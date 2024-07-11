// Copyright 2005-2021 Michael E. Stillman

#include "f4/f4.hpp"
#include "freemod.hpp"                 // for FreeModule
#include "mat.hpp"                     // for MutableMatrix
#include "text-io.hpp"                 // for emit
#include "buffer.hpp"                  // for buffer
#include "error.h"                     // for error
#include "f4/f4-m2-interface.hpp"      // for F4toM2Interface
#include "f4/f4-spairs.hpp"            // for F4SPairSet
#include "f4/f4-types.hpp"             // for row_elem, coefficient_matrix
#include "f4/hilb-fcn.hpp"             // for HilbertController
#include "f4/memblock.hpp"             // for F4MemoryBlock
#include "f4/monhashtable.hpp"         // for MonomialHashTable
#include "f4/moninfo.hpp"              // for MonomialInfo, monomial_word
#include "f4/varpower-monomial.hpp"    // for varpower_word, varpower_monomial
#include "interface/mutable-matrix.h"  // for IM2_MutableMatrix_make
#include "interrupted.hpp"             // for system_interrupted
#include "ring.hpp"                    // for Ring
#include "ringelem.hpp"                // for ring_elem, vec
#include "style.hpp"                   // for INTSIZE

#include <gc/gc_allocator.h>           // for gc_allocator
#include <cstdint>                     // for int32_t
#include <cstdio>                      // for fprintf, stderr
#include <algorithm>                   // for stable_sort
#include <vector>                      // for swap, vector
#include <atomic>                      // for atomic ints in gauss_reduce

class RingElement;

F4GB::F4GB(const VectorArithmetic* VA,
           const MonomialInfo *M0,
           const FreeModule *F0,
           M2_bool collect_syz,
           int n_rows_to_keep,
           M2_arrayint weights0,
           int strategy,
           M2_bool use_max_degree,
           int max_degree,
           int numThreads)
    : mVectorArithmetic(VA),
      mMonomialInfo(M0),
      mFreeModule(F0),
      weights(weights0),
      component_degrees(nullptr),  // need to put this in
      n_pairs_computed(0),
      n_reduction_steps(0),
      n_gens_left(0),
      n_subring(0),
      complete_thru_this_degree(-1),  // need to reset this in the body
      this_degree(),
      is_ideal(F0->rank() == 1),
      hilbert(nullptr),
      mGenerators(),
      mGroebnerBasis(),
      mLookupTable(mMonomialInfo->n_vars()),
      mSPairSet(mMonomialInfo, mGroebnerBasis),
      next_col_to_process(0),
      mat(nullptr),
      mMonomialHashTable(M0, 17),
      mMonomialMemoryBlock(),
      next_monom(),
      clock_sort_columns(0),
      clock_gauss(0),
      mGaussTime(0),
      mParallelGaussTime(0),
      mSerialGaussTime(0),
      mTailReduceTime(0),
      mNewSPairTime(0),
      mInsertGBTime(0),
      clock_make_matrix(0),
      mNumThreads(mtbb::numThreads(numThreads))
#if defined(WITH_TBB)
    , mScheduler(mNumThreads)
#endif
{
  //  mLookupTable = new MonomialLookupTable(mMonomialInfo->n_vars());
  //  mSPairSet = new F4SPairSet(mMonomialInfo, mGroebnerBasis);
  mat = new coefficient_matrix;

  // TODO: set status?
  if (M2_gbTrace >= 3) mMonomialInfo->show();
}

void F4GB::set_hilbert_function(const RingElement *hf)
{
  hilbert = new HilbertController(mFreeModule, hf); // TODO MES: GC problem?
}

void F4GB::delete_gb_array(gb_array &g)
{
  for (auto g0 : g)
    {
      if (! g0->f.coeffs.isNull())
        mVectorArithmetic->deallocateElementArray(g0->f.coeffs);
      // Note: monomials will be cleared en-mass and so don't need to be freed.
      delete g0;
    }
}

F4GB::~F4GB()
{
  //  delete mSPairSet;
  // delete mLookupTable;
  delete mat;

  // Now delete the gens, gb arrays.
  delete_gb_array(mGenerators);
  delete_gb_array(mGroebnerBasis);
}

void F4GB::new_generators(int lo, int hi)
{
  for (int i = lo; i <= hi; i++)
    {
      gbelem *g = mGenerators[i];
      if (g->f.len == 0) continue;
      mSPairSet.insert_generator(g->deg, g->f.monoms, i);
    }
}

///////////////////////////////////////////////////
// Creation of the matrix over K //////////////////
///////////////////////////////////////////////////
int F4GB::new_column(packed_monomial m)
{
  // m is a packed monomial, unique via the hash table H, B.
  column_elem c{};
  int next_column = INTSIZE(mat->columns);
  m[-1] = next_column;
  c.monom = m;
  c.head = -2;
  mat->columns.push_back(c);
  return next_column;
}

int F4GB::find_or_append_column(packed_monomial m)
{
  packed_monomial new_m;
  if (mMonomialHashTable.find_or_insert(m, new_m)) return static_cast<int>(new_m[-1]);
  // At this point, m is a new monomial to be placed as a column
  m = next_monom;
  mMonomialMemoryBlock.intern(1 + mMonomialInfo->monomial_size(m));
  next_monom = mMonomialMemoryBlock.reserve(1 + mMonomialInfo->max_monomial_size());
  next_monom++;
  return new_column(m);
}

int F4GB::mult_monomials(packed_monomial m, packed_monomial n)
{
  // We already have allocated space for a monomial
  // Do the multiply
  // Look it up in the hashtable
  // If it is there, return its column
  // If not, increment our memory block, and insert a new column.
  packed_monomial new_m;
  mMonomialInfo->unchecked_mult(m, n, next_monom);
  if (mMonomialHashTable.find_or_insert(next_monom, new_m))
    return static_cast<int>(
        new_m[-1]);  // monom exists, don't save monomial space
  m = next_monom;
  mMonomialMemoryBlock.intern(1 + mMonomialInfo->monomial_size(m));
  next_monom = mMonomialMemoryBlock.reserve(1 + mMonomialInfo->max_monomial_size());
  next_monom++;
  return new_column(m);
}

void F4GB::load_gen(int which)
{
  GBF4Polynomial &g = mGenerators[which]->f;

  row_elem r{};
  r.monom = nullptr;  // This says that this element corresponds to a generator
  r.elem = which;
  r.len = g.len;
  // r.comps = Mem->components.allocate(g.len);
  r.comps = new int[g.len];
  // r.coeffs is already initialized to [nullptr].

  monomial_word *w = g.monoms;
  for (int i = 0; i < g.len; i++)
    {
      mMonomialInfo->copy(w, next_monom);
      r.comps[i] = find_or_append_column(next_monom);
      w += mMonomialInfo->monomial_size(w);
    }

  mat->rows.push_back(r);
}

void F4GB::load_row(packed_monomial monom, int which)
{
  GBF4Polynomial &g = mGroebnerBasis[which]->f;

  row_elem r{};
  r.monom = monom;
  r.elem = which;
  r.len = g.len;
  // r.comps = Mem->components.allocate(g.len);
  r.comps = new int[g.len];
  // r.coeffs is already initialized to [nullptr].
  
  monomial_word *w = g.monoms;
  for (int i = 0; i < g.len; i++)
    {
      r.comps[i] = mult_monomials(monom, w);
      w += mMonomialInfo->monomial_size(w);
    }

  mat->rows.push_back(r);
}

void F4GB::process_column(int c)
{
  /* If this column has been handled before, return.
     Otherwise, find a GB element whose lead term
     divides this monomial, and either mark this column
     as not an initial element, OR append a row
  */

  column_elem &ce = mat->columns[c];
  if (ce.head >= -1) return;
  int32_t which;
  bool found = mLookupTable.find_one_divisor_packed(mMonomialInfo, ce.monom, which);
  if (found)
    {
      packed_monomial n = next_monom;
      mMonomialInfo->unchecked_divide(ce.monom, mGroebnerBasis[which]->f.monoms, n);
      mMonomialMemoryBlock.intern(1 + mMonomialInfo->monomial_size(n));
      next_monom = mMonomialMemoryBlock.reserve(1 + mMonomialInfo->max_monomial_size());
      next_monom++;
      // M->set_component(which, n);
      ce.head = INTSIZE(mat->rows);
      load_row(n, which);
    }
  else
    ce.head = -1;
}

void F4GB::process_s_pair(spair *p)
{
  int c;

  switch (p->type)
    {
      case F4_SPAIR_SPAIR:
        {
          packed_monomial n = next_monom;
          mMonomialInfo->unchecked_divide(p->lcm, mGroebnerBasis[p->i]->f.monoms, n);
          mMonomialMemoryBlock.intern(1 + mMonomialInfo->monomial_size(n));
          next_monom = mMonomialMemoryBlock.reserve(1 + mMonomialInfo->max_monomial_size());
          next_monom++;

          load_row(n, p->i);
          c = mat->rows[mat->rows.size() - 1].comps[0];

          if (mat->columns[c].head >= -1)
            n_lcmdups++;
          else
            {
              // In this situation, we load the other half as a reducer
              n = next_monom;
              mMonomialInfo->unchecked_divide(p->lcm, mGroebnerBasis[p->j]->f.monoms, n);
              mMonomialMemoryBlock.intern(1 + mMonomialInfo->monomial_size(n));
              next_monom = mMonomialMemoryBlock.reserve(1 + mMonomialInfo->max_monomial_size());
              next_monom++;
              load_row(n, p->j);

              mat->columns[c].head = INTSIZE(mat->rows) - 1;
            }
          break;
        }
      case F4_SPAIR_GEN:
        load_gen(p->i);
        break;
      default:
        break;
    }
}

long ColumnsSorter::ncmps0 = 0;
long ColumnsSorter::ncmps = 0;

void F4GB::reorder_columns()
{
  // Set up to sort the columns.
  // Result is an array 0..ncols-1, giving the new order.
  // Find the inverse of this permutation: place values into "ord" column
  // fields.
  // Loop through every element of the matrix, changing its comp array.

  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);

  // sort the columns

  //int *column_order = Mem->components.allocate(ncols);
  //int *ord = Mem->components.allocate(ncols);
  int *column_order = new int[ncols];
  int *ord = new int[ncols];

  ColumnsSorter C(mMonomialInfo, mat);

// Actual sort of columns /////////////////

  C.reset_ncomparisons();

  clock_t begin_time0 = clock();
  for (int i = 0; i < ncols; i++)
    {
      column_order[i] = i;
    }

  if (M2_gbTrace >= 2) fprintf(stderr, "ncomparisons = ");

  std::stable_sort(column_order, column_order + ncols, C);

  clock_t end_time0 = clock();
  if (M2_gbTrace >= 2) fprintf(stderr, "%ld, ", C.ncomparisons0());
  double nsecs0 = (double)(end_time0 - begin_time0) / CLOCKS_PER_SEC;
  clock_sort_columns += nsecs0;
  if (M2_gbTrace >= 2) fprintf(stderr, " time = %f\n", nsecs0);

  ////////////////////////////

  for (int i = 0; i < ncols; i++)
    {
      ord[column_order[i]] = i;
    }

  // Now move the columns into position
  coefficient_matrix::column_array newcols;
  newcols.reserve(ncols);
  for (int i = 0; i < ncols; i++)
    {
      long newc = column_order[i];
      newcols.push_back(mat->columns[newc]);
    }

  // Now reset the components in each row
  for (int r = 0; r < nrows; r++)
    {
      row_elem &row = mat->rows[r];
      for (int i = 0; i < row.len; i++)
        {
          int oldcol = row.comps[i];
          int newcol = ord[oldcol];
          row.comps[i] = newcol;
        }
      for (int i = 1; i < row.len; i++)
        {
          if (row.comps[i] <= row.comps[i - 1])
            {
              fprintf(stderr, "Internal error: array out of order\n");
              break;
            }
        }
    }

  std::swap(mat->columns, newcols);
  delete [] column_order;
  delete [] ord;
  //Mem->components.deallocate(column_order);
  //Mem->components.deallocate(ord);
}

void F4GB::reorder_rows()
{
  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);
  coefficient_matrix::row_array newrows;
  std::vector<long> rowlocs;  // 0..nrows-1 of where row as been placed

  newrows.reserve(nrows);
  rowlocs.reserve(nrows);
  for (int r = 0; r < nrows; r++) rowlocs.push_back(-1);

  for (int c = 0; c < ncols; c++)
    {
      int oldrow = mat->columns[c].head;
      if (oldrow >= 0)
        {
          // Move the row into place
          int newrow = INTSIZE(newrows);
          newrows.push_back(mat->rows[oldrow]);
          rowlocs[oldrow] = newrow;
          mat->columns[c].head = newrow;
          if (mat->columns[c].head == oldrow) mat->columns[c].head = newrow;
        }
    }
  for (int r = 0; r < nrows; r++)
    if (rowlocs[r] < 0) newrows.push_back(mat->rows[r]);
  std::swap(mat->rows, newrows);
}

void F4GB::reset_matrix()
{
  // mat
  next_col_to_process = 0;
  next_monom = mMonomialMemoryBlock.reserve(1 + mMonomialInfo->max_monomial_size());
  next_monom++;
}

void F4GB::clear_matrix()
{
  // Clear the rows first
  for (auto & r : mat->rows)
    {
      if (not r.coeffs.isNull())
        mVectorArithmetic->deallocateElementArray(r.coeffs);
      // Mem->components.deallocate(r.comps);
      delete [] r.comps;
      r.len = 0;
      r.elem = -1;
      r.monom = nullptr;
    }
  mat->rows.clear();
  mat->columns.clear();
  mMonomialHashTable.reset();
  mMonomialMemoryBlock.reset();
}

void F4GB::make_matrix()
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */

  spair *p;
  while ((p = mSPairSet.get_next_pair()))
    {
      process_s_pair(p);
    }

  while (next_col_to_process < mat->columns.size())
    process_column(next_col_to_process++);

  // DEBUGGING:
  if (M2_gbTrace >= 2)
    {
      fprintf(stderr,
              "--matrix--%ld by %ld\n",
              (long)mat->rows.size(),
              (long)mat->columns.size());
    }
  //  show_row_info();
  //  show_column_info();
  //  show_matrix();

  // Now we reorder the columns, possibly rows?
  reorder_columns();

  // reorder_rows();  // This is only here so we can see what we are doing...?
}

///////////////////////////////////////////////////
// Gaussian elimination ///////////////////////////
///////////////////////////////////////////////////
const ElementArray& F4GB::get_coeffs_array(row_elem &r)
{
  // If r.coeffs is set, returns that, otherwise returns the coeffs array from
  // the generator or GB element.  The resulting value should not be modified.
  if (not r.coeffs.isNull() || r.len == 0) return r.coeffs;

  // At this point, we must go find the coeff array
  if (r.monom == nullptr)  // i.e. a generator
    return mGenerators[r.elem]->f.coeffs;
  return mGroebnerBasis[r.elem]->f.coeffs;
}

bool F4GB::is_new_GB_row(int row) const
// returns true if the r-th row has its lead term not in the current GB
// This can be used to determine which elements should be reduced in the first
// place and also to determine if an element (row) needs to be tail reduced
{
  row_elem &r = mat->rows[row];
  return (r.len > 0 && not r.coeffs.isNull());
}

void F4GB::gauss_reduce(bool diagonalize)
// This reduces the matrix to a triangular form
{
  // For each row which is a non-pivot row:
  //  note that the row must be reducible, since the lead term corresponds to an
  //  spair cancellation
  //   actually: not true: generators will often not be reducible...
  //  also each such row must be non-zero, for the same reason
  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);

  std::atomic<int> n_newpivots = -1;  // the number of new GB elements in this degree
  std::atomic<int> n_zero_reductions = 0;
  if (hilbert)
    {
      n_newpivots = hilbert->nRemainingExpected();
      if (n_newpivots == 0) return;
    }


  mtbb::tick_count t0;
  mtbb::tick_count t1;

#if defined(WITH_TBB)
//#if 0
  t0 = mtbb::tick_count::now();
  using threadLocalDense_t = mtbb::enumerable_thread_specific<ElementArray>;
  // create a dense array for each thread
  threadLocalDense_t threadLocalDense([&]() { 
    return mVectorArithmetic->allocateElementArray(ncols);
  });
  std::cout << "mNumThreads: " << mNumThreads << std::endl;
  mScheduler.execute([&] {
    mtbb::parallel_for(mtbb::blocked_range<int>{0,nrows},
                       [&](const mtbb::blocked_range<int>& r)
                       {
                          threadLocalDense_t::reference my_dense = threadLocalDense.local();
                          for (auto i = r.begin(); i != r.end(); ++i)
                          {
                             // these lines are commented out to avoid the hilbert hint for now...
                             //if ((not hilbert) or (n_newpivots > 0))
                             //{
                                bool newNonzeroReduction = gauss_reduce_row(i, my_dense);
                             //   if (not newNonzeroReduction) n_zero_reductions++;
                             //   if (hilbert && newNonzeroReduction)
                             //     --n_newpivots;
                             //}
                          }
                       });
  });
  for (auto tlDense : threadLocalDense)
    mVectorArithmetic->deallocateElementArray(tlDense);
  t1 = mtbb::tick_count::now();
  mParallelGaussTime += (t1-t0).seconds();
#endif 

  t0 = mtbb::tick_count::now();
  ElementArray gauss_row { mVectorArithmetic->allocateElementArray(ncols) };
  for (int i = 0; i < nrows; i++)
    {
      // TODO: Do we need access to n_newpivots to be thread-safe?
      if ((not hilbert) or (n_newpivots > 0))
      {
         bool newNonzeroReduction = gauss_reduce_row(i, gauss_row);
         if (newNonzeroReduction)
         {
            row_elem &r = mat->rows[i];
            mVectorArithmetic->makeMonic(r.coeffs);
            mat->columns[r.comps[0]].head = i;
         }
         else
            n_zero_reductions++;
         if (hilbert && newNonzeroReduction)
            --n_newpivots;
      }
    }
  mVectorArithmetic->deallocateElementArray(gauss_row);
  t1 = mtbb::tick_count::now();
  mSerialGaussTime += (t1-t0).seconds();

  if (M2_gbTrace >= 3)
    fprintf(stderr, "-- #zeroreductions %d\n", n_zero_reductions.load());

  t0 = mtbb::tick_count::now();
  if (diagonalize) tail_reduce();
  t1 = mtbb::tick_count::now();
  mTailReduceTime += (t1-t0).seconds();
}

bool F4GB::gauss_reduce_row(int index,
                            ElementArray& gauss_row)
{
   // returns true if the row reduces to a "new" nonzero row
   // returns false otherwise
      row_elem &r = mat->rows[index];
      if (r.len == 0) return false;  // could happen once we include syzygies...
      int pivotcol = r.comps[0];
      int pivotrow = mat->columns[pivotcol].head;
      if (pivotrow == index) return false;  // this is a pivot row, so leave it alone

      const ElementArray& rcoeffs = get_coeffs_array(r);
      n_pairs_computed++;
      mVectorArithmetic->fillDenseArray(gauss_row, rcoeffs, Range(r.comps, r.comps + r.len));

      int firstnonzero = -1;
      int first = r.comps[0];
      int last = r.comps[r.len - 1];
      do
        {
          pivotrow = mat->columns[first].head;
          if (pivotrow >= 0)
            {
              row_elem &pivot_rowelem = mat->rows[pivotrow];
              auto& pivot_coeffs = get_coeffs_array(pivot_rowelem);
              n_reduction_steps++;
              mVectorArithmetic->denseCancelFromSparse(gauss_row,
                                                       pivot_coeffs,
                                                       Range(pivot_rowelem.comps,
                                                             pivot_rowelem.comps + pivot_rowelem.len)
                                                       );
              int last1 = pivot_rowelem.comps[pivot_rowelem.len - 1];
              if (last1 > last) last = last1;
            }
          else if (firstnonzero == -1)
            firstnonzero = first;
          first = mVectorArithmetic->denseNextNonzero(gauss_row, first+1, last);
        }
      while (first <= last);  // end do

      if (not r.coeffs.isNull())
        mVectorArithmetic->deallocateElementArray(r.coeffs);
      //Mem->components.deallocate(r.comps);
      delete [] r.comps;
      r.len = 0;
      //Range<int> monomRange;
      //mVectorArithmetic->denseToSparse(gauss_row, r.coeffs, monomRange, firstnonzero, last, mComponentSpace);
      //mVectorArithmetic->denseToSparse(gauss_row, r.coeffs, r.comps, firstnonzero, last, Mem->components);
      mVectorArithmetic->denseToSparse(gauss_row, r.coeffs, r.comps, firstnonzero, last);

      // TODO set r.comps from monomRange.  Question: who has allocated this space??
      // Maybe for now it is just the following line:
      r.len = mVectorArithmetic->size(r.coeffs);
      //r.len = monomRange.size();
      //r.comps = Mem->components.allocate(r.len);
      //std::copy(monomRange.begin(), monomRange.end(), r.comps);
      //mComponentSpace.freeTopArray(monomRange.begin(), monomRange.end());
      //assert(r.len == mVectorArithmetic->size(r.coeffs));
      
      // the above line leaves gauss_row zero, and also handles the case when
      // r.len is 0 (TODO: check this!!)
      // it also potentially frees the old r.coeffs and r.comps (TODO: ?? r.comps??)
      return (r.len > 0);
}
                            

void F4GB::tail_reduce()
{
  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);

  ElementArray gauss_row { mVectorArithmetic->allocateElementArray(ncols) };
  for (int i = nrows - 1; i >= 0; i--)
    {
      row_elem &r = mat->rows[i];
      if (r.len <= 1 || r.coeffs.isNull())
        continue;  // row reduced to zero, ignore it.
      // At this point, we should have an element to reduce
      bool anychange = false;
      mVectorArithmetic->fillDenseArray(gauss_row, r.coeffs, Range(r.comps,
                                                                   r.comps + r.len));
      int firstnonzero = r.comps[0];
      int first = (r.len == 1 ? ncols : r.comps[1]);
      int last = r.comps[r.len - 1];
      while (first <= last)
        {
          int pivotrow = mat->columns[first].head;
          if (pivotrow >= 0)
            {
              anychange = true;
              row_elem &pivot_rowelem =
                  mat->rows[pivotrow];  // pivot_rowelems.coeffs is set at this
              mVectorArithmetic->denseCancelFromSparse(gauss_row,
                                                       pivot_rowelem.coeffs,
                                                       Range(pivot_rowelem.comps,
                                                             pivot_rowelem.comps + pivot_rowelem.len));
              int last1 = pivot_rowelem.comps[pivot_rowelem.len - 1];
              if (last1 > last) last = last1;
            }
          else if (firstnonzero == ncols)
            firstnonzero = first;
          first = mVectorArithmetic->denseNextNonzero(gauss_row, first+1, last);
        };
      if (anychange)
        {
          //Mem->components.deallocate(r.comps);
          delete [] r.comps;
          mVectorArithmetic->deallocateElementArray(r.coeffs);
          r.len = 0;
          //Range<int> monomRange;
          //mVectorArithmetic->denseToSparse(gauss_row,
          //                                       r.coeffs,
          //                                       monomRange,
          //                                       firstnonzero, last,
          //                                       mComponentSpace);
          // mVectorArithmetic->denseToSparse(gauss_row,
          //                                        r.coeffs,
          //                                        r.comps,
          //                                        firstnonzero, last,
          //                                        Mem->components);
          mVectorArithmetic->denseToSparse(gauss_row,
                                                 r.coeffs,
                                                 r.comps,
                                                 firstnonzero, last);
          r.len = mVectorArithmetic->size(r.coeffs);
          //r.len = monomRange.size();
          //r.comps = Mem->components.allocate(r.len);
          //std::copy(monomRange.begin(), monomRange.end(), r.comps);
          //mComponentSpace.freeTopArray(monomRange.begin(), monomRange.end());
          assert(r.len == mVectorArithmetic->size(r.coeffs));
        }
      else
        {
          mVectorArithmetic->setZeroInRange(gauss_row, firstnonzero, last);
        }
      if (r.len > 0)
        {
          mVectorArithmetic->makeMonic(r.coeffs);
        }
    }

  mVectorArithmetic->deallocateElementArray(gauss_row);
}

///////////////////////////////////////////////////
// Extracting new GB elements    //////////////////
///////////////////////////////////////////////////

void F4GB::insert_gb_element(row_elem &r)
{
  // Insert row as gb element.
  // Actions to do:
  //  translate row to a gbelem + poly
  //    set degrees as needed
  //  insert the monomial into the lookup table
  //  find new pairs associated to this new element

  int nslots = mMonomialInfo->max_monomial_size();
  int nlongs = r.len * nslots;

  gbelem *result = new gbelem;
  result->f.len = r.len;

  // If the coeff array is null, then that means the coeffs come from the
  // original array
  // Here we copy it over.

  //  const ElementArray& v = (not r.coeffs.isNull() ? r.coeffs : mVectorArithmetic->copyElementArray(get_coeffs_array(r)));
  //  result->f.coeffs.swap(v);
  if (r.coeffs.isNull())
    {
      // this means the actual coeff vector is coming from a GB element.  Copy it into result->f.coeffs
      ElementArray v { mVectorArithmetic->copyElementArray(get_coeffs_array(r)) };
      result->f.coeffs.swap(v);
    }
  else
    {
      result->f.coeffs.swap(r.coeffs);
    }
  
  //result->f.monoms = Mem->allocate_monomial_array(nlongs);
  result->f.monoms = new monomial_word[nlongs];

  monomial_word *nextmonom = result->f.monoms;
  for (int i = 0; i < r.len; i++)
    {
      mMonomialInfo->copy(mat->columns[r.comps[i]].monom, nextmonom);
      nextmonom += nslots;
    }
  // Mem->components.deallocate(r.comps);
  delete [] r.comps;
  r.comps = nullptr;
  r.len = 0;
  result->deg = this_degree;
  result->alpha = static_cast<int>(mMonomialInfo->last_exponent(result->f.monoms));
  result->minlevel = ELEM_MIN_GB;  // MES: How do
  // we distinguish between ELEM_MIN_GB, ELEM_POSSIBLE_MINGEN?

  int which = INTSIZE(mGroebnerBasis);
  mGroebnerBasis.push_back(result);

  if (hilbert)
    {
      int x;
      //int *exp = newarray_atomic(int, M->n_vars());
      int *exp = new int[mMonomialInfo->n_vars()];
      mMonomialInfo->to_intstar_vector(result->f.monoms, exp, x);
      hilbert->addMonomial(exp, x + 1);
      delete [] exp;
      //freemem(exp);
    }
  // now insert the lead monomial into the lookup table
  //varpower_monomial vp = newarray_atomic(varpower_word, 2 * M->n_vars() + 1);
  varpower_monomial vp = new varpower_word[2 * mMonomialInfo->n_vars() + 1];
  mMonomialInfo->to_varpower_monomial(result->f.monoms, vp);
  mLookupTable.insert_minimal_vp(mMonomialInfo->get_component(result->f.monoms), vp, which);
  delete [] vp;
  //freemem(vp);
  // now go forth and find those new pairs
  mtbb::tick_count t0 = mtbb::tick_count::now();
  mSPairSet.find_new_pairs(is_ideal);
  mtbb::tick_count t1 = mtbb::tick_count::now();
  mNewSPairTime += (t1-t0).seconds();
}

void F4GB::new_GB_elements()
{
  /* After LU decomposition, loop through each
     row of the matrix.  If the corresponding
     lead term is not in the initial ideal (or, at least,
     wasn't) then insert GB element (and so update spairs, etc,
     but don't do auto_reduce...)

     If instead the lead term is not new, then keep track of this
     information somehow: place ... into a monheap...
  */

  /* If we can place the possible new elements first, or in a separate place,
     then
     we don't need to loop through all of these */

  mtbb::tick_count t0 = mtbb::tick_count::now();
  for (int r = 0; r < mat->rows.size(); r++)
    {
      if (is_new_GB_row(r))
        {
          insert_gb_element(mat->rows[r]);
        }
    }
  mtbb::tick_count t1 = mtbb::tick_count::now();
  mInsertGBTime += (t1-t0).seconds();
}

///////////////////////////////////////////////////
// Top level algorithm logic     //////////////////
///////////////////////////////////////////////////

void F4GB::do_spairs()
{
  if (hilbert && hilbert->nRemainingExpected() == 0)
    {
      if (M2_gbTrace >= 1)
        fprintf(stderr,
                "-- skipping degree...no elements expected in this degree\n");
      return;
    }
  reset_matrix();
  clock_t begin_time = clock();

  n_lcmdups = 0;
  make_matrix();

  if (M2_gbTrace >= 6)
    {
      fprintf(stderr, "---------\n");
      show_matrix();
      fprintf(stderr, "---------\n");
    }

  clock_t end_time = clock();
  clock_make_matrix += end_time - begin_time;
  double nsecs = static_cast<double>(end_time - begin_time);
  nsecs /= CLOCKS_PER_SEC;
  if (M2_gbTrace >= 2) fprintf(stderr, " make matrix time = %f\n", nsecs);

  if (M2_gbTrace >= 3) mMonomialHashTable.dump();

  double oldParallelGauss = mParallelGaussTime;
  double oldSerialGauss = mSerialGaussTime;
  double oldTailReduce = mTailReduceTime;
  double oldNewSPair = mNewSPairTime;
  double oldInsertNewGB = mInsertGBTime;

  begin_time = clock();
  gauss_reduce(true);
  end_time = clock();
  clock_gauss += end_time - begin_time;

  //  fprintf(stderr, "---------\n");
  //  show_matrix();
  //  fprintf(stderr, "---------\n");

  nsecs = static_cast<double>(end_time - begin_time);
  nsecs /= CLOCKS_PER_SEC;
  if (M2_gbTrace >= 2)
    {
      fprintf(stderr, " gauss time          = %f\n", nsecs);
      fprintf(stderr, " parallel gauss time          = %g\n", mParallelGaussTime - oldParallelGauss);
      fprintf(stderr, " serial gauss time            = %g\n", mSerialGaussTime - oldSerialGauss);
      fprintf(stderr, " tail reduce time             = %g\n", mTailReduceTime - oldTailReduce);

      fprintf(stderr, " lcm dups            = %ld\n", n_lcmdups);
      if (M2_gbTrace >= 6)
        {
          fprintf(stderr, "---------\n");
          show_matrix();
          fprintf(stderr, "---------\n");
        }
    }
  new_GB_elements();
  fprintf(stderr, " finding new spair time             = %g\n", mNewSPairTime - oldNewSPair);
  fprintf(stderr, " insert new gb time                 = %g\n", mInsertGBTime - oldInsertNewGB - (mNewSPairTime - oldNewSPair));
  
  int ngb = INTSIZE(mGroebnerBasis);
  if (M2_gbTrace >= 1)
    {
      fprintf(stderr, " # GB elements   = %d\n", ngb);
      if (M2_gbTrace >= 5) {
        show_gb_array(mGroebnerBasis);
        mSPairSet.display();
      }
    }

  clear_matrix();
}

enum ComputationStatusCode F4GB::computation_is_complete(StopConditions &stop_)
{
  // This handles everything but stop_.always, stop_.degree_limit
  if (stop_.basis_element_limit > 0 && mGroebnerBasis.size() >= stop_.basis_element_limit)
    return COMP_DONE_GB_LIMIT;
  if (stop_.pair_limit > 0 && n_pairs_computed >= stop_.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (stop_.just_min_gens && n_gens_left == 0) return COMP_DONE_MIN_GENS;
  if (stop_.subring_limit > 0 && n_subring >= stop_.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  if (stop_.use_codim_limit)
    {
#ifdef DEVELOPMENT
#warning "compute the codimension"
#endif
      int c = 0;  // replace this line
      // int c = codim_of_lead_terms();
      if (c >= stop_.codim_limit) return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

void F4GB::test_spair_code()
{
  // This starts out with taking each generator and placing it into the
  // gb matrix, and then calling find_new_pairs after each one.
  // It displays the list of spairs after each generator.

  mGroebnerBasis.push_back(mGenerators[0]);
  for (int i = 1; i < mGenerators.size(); i++)
    {
      mGroebnerBasis.push_back(mGenerators[i]);
      mSPairSet.find_new_pairs(false);
      fprintf(stderr, "---Just inserted element %d---\n", i);
      mSPairSet.display();
    }
}

enum ComputationStatusCode F4GB::start_computation(StopConditions &stop_)
{
  clock_sort_columns = 0;
  clock_gauss = 0;
  clock_make_matrix = 0;
  int npairs;

  //  test_spair_code();

  enum ComputationStatusCode is_done = COMP_COMPUTING;

  for (;;)
    {
      if (system_interrupted())
        {
          is_done = COMP_INTERRUPTED;
          break;
        }

      is_done = computation_is_complete(stop_);
      if (is_done != COMP_COMPUTING) break;

      this_degree = mSPairSet.prepare_next_degree(-1, npairs);

      if (npairs == 0)
        {
          is_done = COMP_DONE;
          break;
        }
      if (stop_.stop_after_degree && this_degree > stop_.degree_limit->array[0])
        {
          is_done = COMP_DONE_DEGREE_LIMIT;
          break;
        }

      if (hilbert)
        {
          if (!hilbert->setDegree(this_degree))
            {
              if (error())
                is_done = COMP_ERROR;
              else
                is_done = COMP_INTERRUPTED;
              break;
            }
        }

      if (M2_gbTrace >= 1)
        {
          if (hilbert)
            fprintf(stderr,
                    "DEGREE %d (nexpected %d npairs %d)\n",
                    this_degree,
                    hilbert->nRemainingExpected(),
                    npairs);
          else
            fprintf(stderr, "DEGREE %d (npairs %d)\n", this_degree, npairs);
        }
      do_spairs();
      complete_thru_this_degree = this_degree;
    }

  if (M2_gbTrace >= 2)
    {
      // fprintf(stderr,
      //         "number of calls to cancel row       : %ld\n",
      //         mGausser->n_dense_row_cancel);
      // fprintf(stderr,
      //         "number of calls to subtract_multiple: %ld\n",
      //         mGausser->n_subtract_multiple);
      fprintf(
          stderr, "total time for sorting columns: %f\n", clock_sort_columns);
      fprintf(stderr,
              "total time for making matrix (includes sort): %f\n",
              ((double)clock_make_matrix) / CLOCKS_PER_SEC);
      fprintf(stderr,
              "total time for gauss: %f\n",
              ((double)clock_gauss) / CLOCKS_PER_SEC);
      fprintf(stderr,
              "parallel tbb time for gauss: %g\n",
              mParallelGaussTime);
      fprintf(stderr,
              "serial tbb time for gauss: %g\n",
              mSerialGaussTime);
      fprintf(stderr,
              "total time for finding new spairs: %g\n",
              mNewSPairTime);
      fprintf(stderr,
              "total time for inserting new gb elements: %g\n",
              mInsertGBTime);
      fprintf(stderr,
              "number of spairs computed           : %ld\n",
              n_pairs_computed);
      fprintf(stderr,
              "number of reduction steps           : %ld\n",
              n_reduction_steps);
      if (M2_gbTrace >= 3)
      {
        fprintf(stderr,
              "number of spairs removed by criterion = %ld\n",
              mSPairSet.n_unneeded_pairs());
        mMonomialInfo->show();
      }
    }

  return is_done;
}

//////////////////////////////////
// Debugging routines only ///////
//////////////////////////////////


void F4GB::show_gb_array(const gb_array &g) const
{
  // Debugging routine
  // Display the array, and all of the internal information in it too.
  buffer o;
  for (int i = 0; i < g.size(); i++)
    {
      vec v = F4toM2Interface::to_M2_vec(
          mVectorArithmetic, mMonomialInfo, g[i]->f, mFreeModule);
      o << "element " << i << " degree " << g[i]->deg << " alpha "
        << g[i]->alpha << newline << "    ";
      mFreeModule->get_ring()->vec_text_out(o, v);
      o << newline;
    }
  emit(o.str());
}

void F4GB::show_row_info() const
{
  // Debugging routine
  for (int i = 0; i < mat->rows.size(); i++)
    {
      fprintf(stderr, "%4d ", mat->rows[i].elem);
      if (mat->rows[i].monom == nullptr)
        fprintf(stderr, "generator");
      else
        mMonomialInfo->show(mat->rows[i].monom);
      fprintf(stderr, "\n");
    }
}

void F4GB::show_column_info() const
{
  // Debugging routine
  for (int i = 0; i < mat->columns.size(); i++)
    {
      fprintf(stderr, "head %4d monomial ", mat->columns[i].head);
      mMonomialInfo->show(mat->columns[i].monom);
      fprintf(stderr, "\n");
    }
}

void F4GB::show_matrix()
{
  // Debugging routine
  MutableMatrix *q = F4toM2Interface::to_M2_MutableMatrix(
      mVectorArithmetic, mat, mGenerators, mGroebnerBasis);
  buffer o;
  q->text_out(o);
  emit(o.str());
}

void F4GB::show_new_rows_matrix()
{
  int ncols = INTSIZE(mat->columns);
  int nrows = 0;
  for (int nr = 0; nr < mat->rows.size(); nr++)
    if (is_new_GB_row(nr)) nrows++;

  MutableMatrix *gbM =
      IM2_MutableMatrix_make(mVectorArithmetic->ring(), nrows, ncols, false);

  int r = -1;
  for (int nr = 0; nr < mat->rows.size(); nr++)
    if (is_new_GB_row(nr))
      {
        r++;
        row_elem &row = mat->rows[nr];
        auto& coeffs = get_coeffs_array(row);
        for (int i = 0; i < row.len; i++)
          {
            int c = row.comps[i];
            ring_elem a = mVectorArithmetic->ringElemFromElementArray(coeffs, i);
            gbM->set_entry(r, c, a);
          }
      }

  buffer o;
  gbM->text_out(o);
  emit(o.str());
}

template class F4MemoryBlock<monomial_word>;
template class F4MemoryBlock<pre_spair>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
