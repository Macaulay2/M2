// Copyright 2014 Michael E. Stillman

#include "res-f4.hpp"
#include "res-gausser.hpp"

#include <iostream>
#include <ctime>
#include <algorithm>

F4Res::F4Res(
             ResF4Mem* Mem,
             const ResGausser* KK0,
             const MonomialInfo* MI,
             int max_level
             )
  : mFrame(*MI,max_level),
    mResGausser(KK0),
    mMonoid(MI),
    mMem(Mem),
    mSchreyerRes(new MonomialsWithComponent(*mMonoid)),
    mHashTable(mSchreyerRes)
{
}

M2_arrayint F4Res::getBetti(int type) const
{
  if (type == 1)
    return mFrame.getBettiFrame();
  ERROR("betti display not implemenented yet");
  return 0;
}

void F4Res::resetMatrix(int lev, int degree)
{
  mThisLevel = lev;
  mThisDegree = degree;
  mNextReducerToProcess = 0;

  // mNextMonom[-1] is the reducer corresponding to this monomial
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;
}

void F4Res::clearMatrix()
{
  mThisLevel = -1;
  mThisDegree = -1;
  mNextReducerToProcess = 0;

  mMonomSpace.reset();

  mNextMonom = nullptr;
  // TODO: write the rest of me.  What needs to be removed?
}

/// findDivisor
//    m: monomial at level mThisLevel-1
//    result: monomial at level mThisLevel, IF true is returned
//  returns true if 'm' - inG(result), for some (unique) 'result'.
bool F4Res::findDivisor(packed_monomial m, packed_monomial result)
{
  // get component of m
  // find the range of monomials to check
  // for each of these, check divisibility in turn
  //   if one works, then return true, and set result.
  long comp = mMonoid->get_component(m); // component is an index into level mLevel-2
  auto& elem = mFrame.level(mThisLevel-2)[comp];
  auto& lev = mFrame.level(mThisLevel-1);
  for (auto j=elem.mBegin; j<elem.mEnd; ++j)
    {
      // Check divisibility of m by this element
      packed_monomial pj = lev[j].mMonom;
      if (mMonoid->divide(m, pj, result)) // this sets the component to be 0
        {
          mMonoid->set_component(j, result); // this sets component correctly
          return true;
        }
    }
  
  return false;
}

// A monomial at level lev has the following form:
// m[-1] index of a divisor for this monomial, -1 if no divisor exists
//    this is only used for monomials being placed into the hash table...
// m[0] is a hash value
// m[1] is the component, an index into the lev-1 part of the frame.
// m[2] is the degree,
// m[3..3+#vars-1] is the monomial.
//   Is m[-1] always present

// processMonomialProduct
//     m is a monomial, component is ignored (it determined the possible n's being used here)
//     n is a monomial at level 'mThisLevel-1'
//     compute their product, and return the column index of this product
//       or -1, if the monomial is not needed.
//     additionally: the product monomial is inserted into the hash table
//     and column array (if it is not already there).
// caveats: this function is only to be used during construction 
//     of the coeff matrices.  It used mThisLevel
long F4Res::processMonomialProduct(packed_monomial m, packed_monomial n)
{
  auto& p = mFrame.level(mThisLevel-2)[mMonoid->get_component(n)];
  if (p.mBegin == p.mEnd) return -1;

#if 0
  std::cout << "monomialProduct(";
  mMonoid->showAlpha(m);
  std::cout << ", ";
  mMonoid->showAlpha(n);
  std::cout << ")" << std::endl;
#endif
  
  packed_monomial new_m; // a pointer to a monomial we are visiting
  mMonoid->unchecked_mult(m,n,mNextMonom);
  // the component is wrong, after this operation, as it adds components
  // So fix that:
  mMonoid->set_component(mMonoid->get_component(n), mNextMonom);
  if (mHashTable.find_or_insert(mNextMonom, new_m))
    return new_m[-1]; // monom exists, don't save monomial space

#if 0
  std::cout << "find_or_insert returned:  ";
  mMonoid->showAlpha(new_m);
  std::cout << std::endl;
#endif
  
  // intern the monomial just inserted into the hash table
  mMonomSpace.intern(1+mMonoid->monomial_size(mNextMonom));
  
  // At this point, we have a new monomial, and a pointer to mNextMonom
  // has been placed into mHashTable.
  // We need to see now what the divisor monomial for mNextMonom is.
  // and if there is one, we need to create a new row for the mReducers matrix.
  // (at least, put it on the queue to be created).
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;
  
  bool has_divisor = findDivisor(new_m, mNextMonom);
  if (!has_divisor)
    {
      new_m[-1] = -1; // no divisor exists
      return -1;
    }

#if 0
  std::cout << "findDivisor returned:  ";
  mMonoid->showAlpha(mNextMonom);
  std::cout << std::endl;
#endif
  
  mMonomSpace.intern(1+mMonoid->monomial_size(mNextMonom));
  
  long thiscol = mColumns.size();
  new_m[-1] = thiscol; // this is a HACK: where we keep the divisor
  mColumns.push_back(new_m);

  Row row;
  row.mLeadTerm = mNextMonom;
  mReducers.push_back(row);

#if 0
  std::cout << "added ";
  mMonoid->showAlpha(row.mLeadTerm);
  std::cout << " to reducer todo list" << std::endl;
#endif
  
  // Now we increment mNextMonom, for the next time
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;

  return thiscol;
}

void F4Res::loadRow(Row& r)
{
  long comp = mMonoid->get_component(r.mLeadTerm);
  auto& p = mFrame.level(mThisLevel-1)[comp].mSyzygy;
  auto end = poly_iter(*mResGausser, *mMonoid, p, 1);
  auto i = poly_iter(*mResGausser, *mMonoid, p);
  for ( ; i != end; ++i)
    {
      long val = processMonomialProduct(r.mLeadTerm, i.monomial());

      if (val < 0) continue;
      r.mComponents.push_back(val);
      r.mCoeffs.push_back(i.coefficient());
    }
} 

// IMPORTANT TODO!!!!
// This class needs to implement Schreyer order comparison.
class ResColumnsSorter
{
public:
  typedef MonomialInfo::value monomial;
  typedef int value;
private:
  const MonomialInfo *M;
  const F4Res& mComputation;
  const std::vector<packed_monomial>& cols;

  static long ncmps;
  static long ncmps0;
public:
  int compare(value a, value b)
  {
    ncmps ++;
    return M->compare_grevlex(cols[a],cols[b]);
  }

  bool operator()(value a, value b)
  {
    ncmps0++;
    return (M->compare_grevlex(cols[a],cols[b]) == LT);
  }

  ResColumnsSorter(const MonomialInfo *M0, const F4Res& comp)
    : M(M0),
      mComputation(comp),
      cols(comp.mColumns)
  {
  }

  long ncomparisons() const { return ncmps; }
  long ncomparisons0() const { return ncmps0; }
  void reset_ncomparisons() { ncmps0 = 0; ncmps = 0; }

  ~ResColumnsSorter() {}
};

static void applyPermutation(int* permutation, std::vector<int>& entries)
{
  // TODO: permutation should be a std::vector too,
  // and we should check the values of the permutation.
  for (int i=0; i<entries.size(); i++)
    entries[i] = permutation[entries[i]];

  // The next is just a consistency check, that maybe can be removed later.
  for (int i=1; i<entries.size(); i++)
    {
      if (entries[i] <= entries[i-1])
        {
          fprintf(stderr, "Internal error: array out of order\n");
          break;
        }
      
    }
}

long ResColumnsSorter::ncmps = 0;
long ResColumnsSorter::ncmps0 = 0;

void F4Res::reorderColumns()
{
  // Set up to sort the columns.
  // Result is an array 0..ncols-1, giving the new order.
  // Find the inverse of this permutation: place values into "ord" column fields.
  // Loop through every element of the matrix, changing its comp array.

  std::cout << "-- rows --" << std::endl;
  debugOutputReducers();
  std::cout << "-- columns --" << std::endl;
  debugOutputColumns();
  
  std::cout << "reorderColumns" << std::endl;
  long ncols = mColumns.size();

  // sort the columns

  int *column_order = mMem->components.allocate(ncols);
  int *ord = mMem->components.allocate(ncols);

  ResColumnsSorter C(mMonoid, *this);

  C.reset_ncomparisons();

  clock_t begin_time0 = clock();
  for (int i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  if (M2_gbTrace >= 2)
    fprintf(stderr, "ncomparisons = ");

  std::sort(column_order, column_order+ncols, C);

  clock_t end_time0 = clock();
  if (M2_gbTrace >= 2)
    fprintf(stderr, "%ld, ", C.ncomparisons0());
  double nsecs0 = (double)(end_time0 - begin_time0)/CLOCKS_PER_SEC;
  //clock_sort_columns += nsecs0;
  if (M2_gbTrace >= 2)
    fprintf(stderr, " time = %f\n", nsecs0);

  ////////////////////////////

  for (int i=0; i<ncols; i++)
    {
      ord[column_order[i]] = i;
    }

  std::cout << "column_order: ";
  for (int i=0; i<ncols; i++) std::cout << " " << column_order[i];
  std::cout <<  std::endl;
  std::cout << "ord: ";
  for (int i=0; i<ncols; i++) std::cout << " " << ord[i];
  std::cout <<  std::endl;

  // Now move the columns into position
  std::vector<packed_monomial> sortedColumnArray;
  std::vector<Row> sortedRowArray;

  sortedColumnArray.reserve(ncols);
  sortedRowArray.reserve(ncols);

  for (int i=0; i<ncols; i++)
    {
      long newc = column_order[i];
      sortedColumnArray.push_back(mColumns[newc]);
      sortedRowArray.push_back(Row());
      std::swap(sortedRowArray[i], mReducers[newc]);
    }

  std::swap(mColumns, sortedColumnArray);
  std::swap(mReducers, sortedRowArray);
  
  for (int i=0; i<mReducers.size(); i++)
    {
      applyPermutation(ord, mReducers[i].mComponents);
    }
  for (int i=0; i<mSPairs.size(); i++)
    {
      applyPermutation(ord, mSPairs[i].mComponents);
    }

  mMem->components.deallocate(column_order);
  mMem->components.deallocate(ord);
}

void F4Res::makeMatrix()
{
  auto& myframe = mFrame.level(mThisLevel);
  long r = 0;
  long comp = 0;
  for (auto it = myframe.begin(); it != myframe.end(); ++it)
    {
      if (it->mDegree == mThisDegree)
        {
          mSPairs.push_back(Row());
          mSPairComponents.push_back(comp);
          Row& row = mSPairs[r];
          r++;
          row.mLeadTerm = it->mMonom;
          loadRow(row);
        }
      comp++;
    }
  // Now we process all monomials in the columns array
  while (mNextReducerToProcess < mColumns.size())
    loadRow(mReducers[mNextReducerToProcess++]);

  reorderColumns();
}

void F4Res::gaussReduce()
{
  // Reduce to zero every spair. Recording creates the
  // corresponding syzygy, which is auto-reduced and correctly ordered.

  // allocate a dense row, of correct size  
  ResGausser::dense_row gauss_row;
  mResGausser->dense_row_allocate(gauss_row, mColumns.size());
  FieldElement one;
  mResGausser->set_one(one);

  for (long i=0; i<mSPairs.size(); i++)
    {
      std::cout << "reducing row " << i << std::endl;
      // Reduce spair #i
      // fill in dense row with this element.

      poly_constructor result(*mMonoid);
      // TODO: add in the first monomial

      Row& r = mSPairs[i]; // row to be reduced.
      long comp = mSPairComponents[i];
      result.appendTerm(mFrame.level(mThisLevel)[comp].mMonom,
                        one);
      auto& syz = mFrame.level(mThisLevel)[comp].mSyzygy; // this is the element we will fill out

      std::cout << "about to fill from sparse " << i << std::endl;
      
      long firstcol = r.mComponents[0];
      long lastcol = mColumns.size()-1; // maybe: r.mComponents[r.mComponents.size()-1];
      mResGausser->dense_row_fill_from_sparse(gauss_row,
                                              r.mComponents.size(),
                                              & r.mCoeffs[0],
                                              & r.mComponents[0]); // FIX: not correct call

      while (firstcol <= lastcol)
        {
          std::cout << "about to reduce with col " << firstcol << std::endl;      
          FieldElement elem;

          #if 0
          for (int p=0; p<mColumns.size(); p++)
            {
              fprintf(stdout, " %d", mResGausser->coeff_to_int(gauss_row.coeffs[p]));
            }
          fprintf(stdout, "\n");
          #endif
          
          mResGausser->negate(gauss_row.coeffs[firstcol], elem);
          result.appendTerm(mReducers[firstcol].mLeadTerm, elem);
          mResGausser->dense_row_cancel_sparse(gauss_row,
                                               mReducers[firstcol].mCoeffs.size(),
                                               & mReducers[firstcol].mCoeffs[0],
                                               & mReducers[firstcol].mComponents[0]
                                               );
          firstcol = mResGausser->dense_row_next_nonzero(gauss_row, firstcol+1, lastcol);
        }
      result.setPoly(syz);
    }

  mResGausser->dense_row_deallocate(gauss_row);
}

void F4Res::construct(int lev, int degree)
{
  resetMatrix(lev, degree);
  makeMatrix();
  //  newSyzElems();

  std::cout << "-- rows --" << std::endl;
  debugOutputReducers();
  std::cout << "-- columns --" << std::endl;
  debugOutputColumns();
  std :: cout << "-- reducer matrix --" << std::endl;
  debugOutputMatrix(mReducers);
  std :: cout << "-- spair matrix --" << std::endl;
  debugOutputMatrix(mSPairs);

  gaussReduce();

  mFrame.show(-1, *mResGausser);
  clearMatrix();
}

void F4Res::debugOutputReducers()
{
  auto end = mReducers.cend();
  for (auto i=mReducers.cbegin(); i != end; ++i)
    {
      mMonoid->showAlpha((*i).mLeadTerm);
      std::cout << std::endl;
    }
}
void F4Res::debugOutputColumns()
{
  auto end = mColumns.cend();
  for (auto i=mColumns.cbegin(); i != end; ++i)
    {
      mMonoid->showAlpha((*i));
      std::cout << std::endl;
    }
}

#if 0
void F4Res::debugOutputMatrix()
{
  std::vector<Row>& rows = mReducers;
  for (long i=0; i<rows.size(); i++)
    {
      fprintf(stdout, "coeffs = ");
      for (long j=0; j<rows[i].mCoeffs.size(); ++j)
        fprintf(stdout, " %d", mResGausser->coeff_to_int(rows[i].mCoeffs[j]));
      fprintf(stdout, "\n comps = ");
      for (long j=0; j<rows[i].mComponents.size(); ++j)
        fprintf(stdout, " %d", rows[i].mComponents[j]);
      fprintf(stdout, "\n");
    }
}
#endif
void F4Res::debugOutputMatrix(std::vector<Row>& rows)
{
  for (long i=0; i<rows.size(); i++)
    {
      auto coeff = rows[i].mCoeffs.begin();
      auto end = rows[i].mCoeffs.end();
      auto monom = rows[i].mComponents.begin();
      for (long c=0; c<mColumns.size(); c++)
        {
          if (coeff == end or *monom != c)
            fprintf(stdout, " .");
          else
            {
              fprintf(stdout, " %d", mResGausser->coeff_to_int(*coeff));
              ++coeff;
              ++monom;
            }
        }
      fprintf(stdout, "\n");
    }
}
void F4Res::debugOutputReducerMatrix()
{
  debugOutputMatrix(mReducers);
}
void F4Res::debugOutputSPairMatrix()
{
  debugOutputMatrix(mSPairs);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

