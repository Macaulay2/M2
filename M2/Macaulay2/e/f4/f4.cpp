// Copyright 2005-2010 Michael E. Stillman

// TODO: this code needs to be worked on
#include <ctime>
#include <algorithm>

#include "f4.hpp"
#include "../freemod.hpp"
#include "interrupted.hpp"

F4GB::F4GB(const Gausser *KK0,
           F4Mem *Mem0,
           const MonomialInfo *M0,
           const FreeModule *F0,
           M2_bool collect_syz,
           int n_rows_to_keep,
           M2_arrayint weights0,
           int strategy,
           M2_bool use_max_degree,
           int max_degree)
    : KK(KK0),
      M(M0),
      F(F0),
      weights(weights0),
      component_degrees(0),  // need to put this in
      n_pairs_computed(0),
      n_reduction_steps(0),
      n_gens_left(0),
      n_subring(0),
      complete_thru_this_degree(-1),  // need to reset this in the body
      this_degree(),
      is_ideal(F0->rank() == 1),
      hilbert(0),
      gens(),
      gb(),
      syz_basis(),
      lookup(0),
      S(0),
      next_col_to_process(0),
      mat(0),
      H(M0, 17),
      Mem(Mem0),
      B(),
      next_monom(),
      syz_next_col_to_process(0),
      syz(0),
      master_syz(0),
      syzH(M0, 17),
      master_syzH(M0, 17),
      syzB(),
      master_syzB(),
      syz_next_monom(),
      // syzF4Vec("syzygy vector manager"),
      gauss_row(),
      clock_sort_columns(0),
      clock_gauss(0),
      clock_make_matrix(0),
      syz_clock_sort_columns(0)
{
  lookup = new MonomialLookupTable(M->n_vars());
  S = new F4SPairSet(M, gb);
  mat = new coefficient_matrix;
  //  mat->rows.reserve(200000);
  //  mat->columns.reserve(200000);

  // set status
  if (M2_gbTrace >= 2) M->show();

  // initialize syzygy stuff
  using_syz = strategy & STRATEGY_USE_SYZ;
  if (M2_gbTrace >= 2) fprintf(stderr, "SYZ: using_syz = %i\n", using_syz);
  syz = new coefficient_matrix;
  master_syz = new coefficient_matrix;
  if (using_syz) syzF = F->get_ring()->make_Schreyer_FreeModule();
}

void F4GB::set_hilbert_function(const RingElement *hf)
{
  if (!using_syz) hilbert = new HilbertController(F, hf);
}

void F4GB::delete_gb_array(gb_array &g)
{
  for (int i = 0; i < g.size(); i++)
    {
      gbelem *g0 = g[i];
      if (g0->f.coeffs)
        KK->deallocate_F4CCoefficientArray(g0->f.coeffs, g0->f.len);
      // Note: monomials will be cleared en-mass and so don't need to be freed.
      delete g0;
    }
}

F4GB::~F4GB()
{
  delete S;
  delete lookup;
  delete mat;
  delete syzF;

  // Now delete the gens, gb arrays.
  delete_gb_array(gens);
  delete_gb_array(gb);
}

void F4GB::new_generators(int lo, int hi)
{
  for (int i = lo; i <= hi; i++)
    {
      gbelem *g = gens[i];
      if (g->f.len == 0) continue;
      S->insert_generator(g->deg, g->f.monoms, i);
    }
}

///////////////////////////////////////////////////
// Creation of the matrix over K //////////////////
///////////////////////////////////////////////////
int F4GB::new_column(packed_monomial m)
{
  // m is a packed monomial, unique via the hash table H, B.
  column_elem c;
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
  if (H.find_or_insert(m, new_m)) return static_cast<int>(new_m[-1]);
  // At this point, m is a new monomial to be placed as a column
  m = next_monom;
  B.intern(1 + M->monomial_size(m));
  next_monom = B.reserve(1 + M->max_monomial_size());
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
  M->unchecked_mult(m, n, next_monom);
  if (H.find_or_insert(next_monom, new_m))
    return static_cast<int>(
        new_m[-1]);  // monom exists, don't save monomial space
  m = next_monom;
  B.intern(1 + M->monomial_size(m));
  next_monom = B.reserve(1 + M->max_monomial_size());
  next_monom++;
  return new_column(m);
}

void F4GB::load_gen(int which)
{
  poly &g = gens[which]->f;

  row_elem r;
  r.monom = NULL;  // This says that this element corresponds to a generator
  r.elem = which;

  r.len = g.len;
  r.coeffs = 0;  // will be fetched when needed via get_coeffs_array
  r.comps = Mem->components.allocate(g.len);

  monomial_word *w = g.monoms;
  for (int i = 0; i < g.len; i++)
    {
      M->copy(w, next_monom);
      r.comps[i] = find_or_append_column(next_monom);
      w += M->monomial_size(w);
    }

  mat->rows.push_back(r);

  syz_load_gen(which);
}

void F4GB::load_row(packed_monomial monom, int which)
{
  poly &g = gb[which]->f;

  row_elem r;
  r.monom = monom;
  r.elem = which;

  r.len = g.len;
  r.coeffs = 0;  // will be fetched when needed via get_coeffs_array
  r.comps = Mem->components.allocate(g.len);

  monomial_word *w = g.monoms;
  for (int i = 0; i < g.len; i++)
    {
      r.comps[i] = mult_monomials(monom, w);
      w += M->monomial_size(w);
    }

  mat->rows.push_back(r);

  syz_load_row(monom, which);
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
  bool found = lookup->find_one_divisor_packed(M, ce.monom, which);
  if (found)
    {
      packed_monomial n = next_monom;
      M->unchecked_divide(ce.monom, gb[which]->f.monoms, n);
      B.intern(1 + M->monomial_size(n));
      next_monom = B.reserve(1 + M->max_monomial_size());
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
          M->unchecked_divide(p->lcm, gb[p->i]->f.monoms, n);
          B.intern(1 + M->monomial_size(n));
          next_monom = B.reserve(1 + M->max_monomial_size());
          next_monom++;

          load_row(n, p->i);
          c = mat->rows[mat->rows.size() - 1].comps[0];

          if (mat->columns[c].head >= -1)
            n_lcmdups++;
          else
            {
              // In this situation, we load the other half as a reducer
              n = next_monom;
              M->unchecked_divide(p->lcm, gb[p->j]->f.monoms, n);
              B.intern(1 + M->monomial_size(n));
              next_monom = B.reserve(1 + M->max_monomial_size());
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

  int *column_order = Mem->components.allocate(ncols);
  int *ord = Mem->components.allocate(ncols);

  ColumnsSorter C(M, mat);

// Actual sort of columns /////////////////

#if 0
  //TODO: MES remove the code in this ifdef 0
  C.reset_ncomparisons();

  clock_t begin_time = clock();
  for (int i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  if (M2_gbTrace >= 2)
    fprintf(stderr, "ncomparisons = ");

  QuickSorter<ColumnsSorter>::sort(&C, column_order, ncols);

  clock_t end_time = clock();
  if (M2_gbTrace >= 2)
    fprintf(stderr, "%ld, ", C.ncomparisons());
  double nsecs = (double)(end_time - begin_time)/CLOCKS_PER_SEC;
  clock_sort_columns += nsecs;
  if (M2_gbTrace >= 2)
    fprintf(stderr, " time = %f\n", nsecs);
#endif
  // STL version ///////////////

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
  Mem->components.deallocate(column_order);
  Mem->components.deallocate(ord);
}

void F4GB::reorder_rows()
{
  //??? reorder rows in <mat> and <syz> simultaneously?

  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);
  coefficient_matrix::row_array newrows;
  VECTOR(long) rowlocs;  // 0..nrows-1 of where row as been placed

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
  next_monom = B.reserve(1 + M->max_monomial_size());
  next_monom++;
}

void F4GB::clear_matrix()
{
  // Clear the rows first
  for (int i = 0; i < mat->rows.size(); i++)
    {
      row_elem &r = mat->rows[i];
      if (r.coeffs) KK->deallocate_F4CCoefficientArray(r.coeffs, r.len);
      Mem->components.deallocate(r.comps);
      r.len = 0;
      r.elem = -1;
      r.monom = 0;
    }
  mat->rows.clear();
  mat->columns.clear();
  H.reset();
  B.reset();
  if (M2_gbTrace >= 4)
    {
      Mem->components.show();
      Mem->coefficients.show();
      Mem->show();
    }
}

void F4GB::make_matrix()
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */

  spair *p;
  while ((p = S->get_next_pair()))
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
      if (using_syz)
        fprintf(stderr,
                "-syzygies-%ld by %ld\n",
                (long)syz->rows.size(),
                (long)syz->columns.size());
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
F4CoefficientArray F4GB::get_coeffs_array(row_elem &r)
{
  // If r.coeffs is set, returns that, otherwise returns the coeffs array from
  // the generator or GB element.  The resulting value should not be modified.
  if (r.coeffs || r.len == 0) return r.coeffs;

  // At this point, we must go find the coeff array
  if (r.monom == 0)  // i.e. a generator
    return gens[r.elem]->f.coeffs;
  return gb[r.elem]->f.coeffs;
}

bool F4GB::is_new_GB_row(int row) const
// returns true if the r-th row has its lead term not in the current GB
// This can be used to determine which elements should be reduced in the first
// place
// and also to determine if an element (row) needs to be tail reduced
{
  row_elem &r = mat->rows[row];
  return (r.len > 0 && r.coeffs);
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

  int n_newpivots = -1;  // the number of new GB elements in this degree
  int n_zero_reductions = 0;
  if (hilbert)
    {
      n_newpivots = hilbert->nRemainingExpected();
      if (n_newpivots == 0) return;
    }

  KK->dense_row_allocate(gauss_row, ncols);
  if (using_syz) KK->dense_row_allocate(syz_row, nrows);
  for (int i = 0; i < nrows; i++)
    {
      row_elem &r = mat->rows[i];
      if (r.len == 0) continue;  // could happen once we include syzygies...
      int pivotcol = r.comps[0];
      int pivotrow = mat->columns[pivotcol].head;
      if (pivotrow == i) continue;  // this is a pivot row, so leave it alone

      F4CoefficientArray rcoeffs = get_coeffs_array(r);
      n_pairs_computed++;
      KK->dense_row_fill_from_sparse(gauss_row, r.len, rcoeffs, r.comps);
      syz_dense_row_fill_from_sparse(i);  // fill syz_row from row[i]

      int firstnonzero = ncols;
      int first = r.comps[0];
      int last = r.comps[r.len - 1];
      do
        {
          pivotrow = mat->columns[first].head;
          if (pivotrow >= 0)
            {
              row_elem &pivot_rowelem = mat->rows[pivotrow];
              F4CoefficientArray pivot_coeffs = get_coeffs_array(pivot_rowelem);
              syzygy_row_record_reduction(pivotrow,
                                          KK->lead_coeff(rcoeffs),
                                          KK->lead_coeff(pivot_coeffs));
              n_reduction_steps++;
              KK->dense_row_cancel_sparse(gauss_row,
                                          pivot_rowelem.len,
                                          pivot_coeffs,
                                          pivot_rowelem.comps);
              int last1 = pivot_rowelem.comps[pivot_rowelem.len - 1];
              if (last1 > last) last = last1;
            }
          else if (firstnonzero == ncols)
            firstnonzero = first;
          first = KK->dense_row_next_nonzero(gauss_row, first + 1, last);
        }
      while (first <= last);
      if (r.coeffs) KK->deallocate_F4CCoefficientArray(r.coeffs, r.len);
      Mem->components.deallocate(r.comps);
      r.len = 0;
      KK->dense_row_to_sparse_row(
          gauss_row, r.len, r.coeffs, r.comps, firstnonzero, last);
      // the above line leaves gauss_row zero, and also handles the case when
      // r.len is 0
      // it also potentially frees the old r.coeffs and r.comps
      if (using_syz)
        {
          row_elem &s = syz->rows[i];
          if (s.len > 0)  // the opposite should not happen
            {
              int *scoeffs = static_cast<int *>(s.coeffs);
              Mem->coefficients.deallocate(scoeffs);
              Mem->components.deallocate(s.comps);
              s.len = 0;
            }
          syz_dense_row_to_sparse_row(syz->rows[i]);
          // the above line leaves syz_row zero
        }
      if (r.len > 0)
        {
          syzygy_row_divide(i, static_cast<int *>(r.coeffs)[0]);
          KK->sparse_row_make_monic(r.len, r.coeffs);
          mat->columns[r.comps[0]].head = i;
          if (--n_newpivots == 0) break;
        }
      else
        n_zero_reductions++;
    }
  KK->dense_row_deallocate(gauss_row);
  if (using_syz) KK->dense_row_deallocate(syz_row);

  if (M2_gbTrace >= 3)
    fprintf(stderr, "-- #zeroreductions %d\n", n_zero_reductions);

  if (diagonalize) tail_reduce();
}

void F4GB::tail_reduce()
{
  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);

  KK->dense_row_allocate(gauss_row, ncols);
  if (using_syz) KK->dense_row_allocate(syz_row, nrows);
  for (int i = nrows - 1; i >= 0; i--)
    {
      row_elem &r = mat->rows[i];
      if (r.len <= 1 || r.coeffs == 0)
        continue;  // row reduced to zero, ignore it.
      // At this point, we should have an element to reduce
      bool anychange = false;
      KK->dense_row_fill_from_sparse(gauss_row, r.len, r.coeffs, r.comps);
      syz_dense_row_fill_from_sparse(i);  // fill syz_row from row[i]
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
                                        // point
              syzygy_row_record_reduction(pivotrow,
                                          KK->lead_coeff(r.coeffs),
                                          KK->lead_coeff(pivot_rowelem.coeffs));
              KK->dense_row_cancel_sparse(gauss_row,
                                          pivot_rowelem.len,
                                          pivot_rowelem.coeffs,
                                          pivot_rowelem.comps);
              int last1 = pivot_rowelem.comps[pivot_rowelem.len - 1];
              if (last1 > last) last = last1;
            }
          else if (firstnonzero == ncols)
            firstnonzero = first;
          first = KK->dense_row_next_nonzero(gauss_row, first + 1, last);
        };
      if (anychange)
        {
          Mem->components.deallocate(r.comps);
          KK->deallocate_F4CCoefficientArray(
              r.coeffs, r.len);  // the coeff array is owned by the row here
          r.len = 0;
          KK->dense_row_to_sparse_row(
              gauss_row, r.len, r.coeffs, r.comps, firstnonzero, last);
          if (using_syz)
            {
              row_elem &s = syz->rows[i];
              if (s.len > 0)  // the opposite should not happen
                {
                  int *scoeffs = static_cast<int *>(s.coeffs);
                  Mem->coefficients.deallocate(scoeffs);
                  Mem->components.deallocate(s.comps);
                  s.len = 0;
                }
              syz_dense_row_to_sparse_row(syz->rows[i]);
              // the above line leaves syz_row zero
            }
        }
      else
        {
          KK->dense_row_clear(gauss_row, firstnonzero, last);
          KK->dense_row_clear(
              gauss_row, 0, INTSIZE(syz->columns) - 1);  //!!!lazy
        }
      if (r.len > 0)
        {
          syzygy_row_divide(i, static_cast<int *>(r.coeffs)[0]);
          KK->sparse_row_make_monic(r.len, r.coeffs);
        }
    }

  KK->dense_row_deallocate(gauss_row);
  if (using_syz) KK->dense_row_deallocate(syz_row);
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

  int nslots = M->max_monomial_size();
  int nlongs = r.len * nslots;

  gbelem *result = new gbelem;
  result->f.len = r.len;

  // If the coeff array is null, then that means the coeffs come from the
  // original array
  // Here we copy it over.

  result->f.coeffs = (r.coeffs ? r.coeffs : KK->copy_F4CoefficientArray(
                                                r.len, get_coeffs_array(r)));
  r.coeffs = 0;
  ;

  result->f.monoms = Mem->allocate_monomial_array(nlongs);

  monomial_word *nextmonom = result->f.monoms;
  for (int i = 0; i < r.len; i++)
    {
      M->copy(mat->columns[r.comps[i]].monom, nextmonom);
      nextmonom += nslots;
    }
  Mem->components.deallocate(r.comps);
  r.len = 0;
  result->deg = this_degree;
  result->alpha = static_cast<int>(M->last_exponent(result->f.monoms));
  result->minlevel = ELEM_MIN_GB;  // MES: How do
  // we distinguish between ELEM_MIN_GB, ELEM_POSSIBLE_MINGEN?

  int which = INTSIZE(gb);
  gb.push_back(result);

  if (hilbert)
    {
      int x;
      int *exp = newarray_atomic(int, M->n_vars());
      M->to_intstar_vector(result->f.monoms, exp, x);
      hilbert->addMonomial(exp, x + 1);
      deletearray(exp);
    }

  // now insert the lead monomial into the lookup table
  varpower_monomial vp = newarray_atomic(varpower_word, 2 * M->n_vars() + 1);
  M->to_varpower_monomial(result->f.monoms, vp);
  lookup->insert_minimal_vp(M->get_component(result->f.monoms), vp, which);
  deleteitem(vp);
  // now go forth and find those new pairs
  S->find_new_pairs(is_ideal);
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

  for (int r = 0; r < mat->rows.size(); r++)
    {
      if (is_new_GB_row(r))
        {
          insert_syz(syz->rows[r], INTSIZE(gb));
          insert_gb_element(mat->rows[r]);
        }
      else
        {
          insert_syz(syz->rows[r]);
        }
    }
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
  reset_syz_matrix();
  clock_t begin_time = clock();

  n_lcmdups = 0;
  make_matrix();

  if (M2_gbTrace >= 5)
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

  if (M2_gbTrace >= 2) H.dump();

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

      fprintf(stderr, " lcm dups            = %ld\n", n_lcmdups);
      if (M2_gbTrace >= 5)
        {
          fprintf(stderr, "---------\n");
          show_matrix();
          fprintf(stderr, "---------\n");
          show_syz_matrix();
          //  show_new_rows_matrix();
        }
    }
  new_GB_elements();
  int ngb = INTSIZE(gb);
  if (M2_gbTrace >= 1)
    {
      fprintf(stderr, " # GB elements   = %d\n", ngb);
      if (M2_gbTrace >= 5) show_gb_array(gb);
      if (using_syz)
        fprintf(stderr,
                " # syzygies      = %ld\n",
                static_cast<long>(syz_basis.size()));
      if (M2_gbTrace >= 5) show_syz_basis();
    }

  clear_matrix();
  clear_syz_matrix();
}

enum ComputationStatusCode F4GB::computation_is_complete(StopConditions &stop_)
{
  // This handles everything but stop_.always, stop_.degree_limit
  if (stop_.basis_element_limit > 0 && gb.size() >= stop_.basis_element_limit)
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

  gb.push_back(gens[0]);
  for (int i = 1; i < gens.size(); i++)
    {
      gb.push_back(gens[i]);
      S->find_new_pairs(false);
      fprintf(stderr, "---Just inserted element %d---\n", i);
      S->display();
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

  reset_master_syz();

  for (;;)
    {
      if (system_interrupted())
        {
          is_done = COMP_INTERRUPTED;
          break;
        }

      is_done = computation_is_complete(stop_);
      if (is_done != COMP_COMPUTING) break;

      this_degree = S->prepare_next_degree(-1, npairs);

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

  clear_master_syz();

  if (M2_gbTrace >= 2)
    {
      fprintf(stderr,
              "number of calls to cancel row       : %ld\n",
              KK->n_dense_row_cancel);
      fprintf(stderr,
              "number of calls to subtract_multiple: %ld\n",
              KK->n_subtract_multiple);
      fprintf(
          stderr, "total time for sorting columns: %f\n", clock_sort_columns);
      if (using_syz)
        fprintf(stderr,
                "total time for sorting syz columns: %f\n",
                syz_clock_sort_columns);
      fprintf(stderr,
              "total time for making matrix (includes sort): %f\n",
              ((double)clock_make_matrix) / CLOCKS_PER_SEC);
      fprintf(stderr,
              "total time for gauss: %f\n",
              ((double)clock_gauss) / CLOCKS_PER_SEC);
      fprintf(stderr,
              "number of spairs computed           : %ld\n",
              n_pairs_computed);
      fprintf(stderr,
              "number of reduction steps           : %ld\n",
              n_reduction_steps);
    }

  fprintf(stderr,
          "number of spairs removed by criterion = %ld\n",
          S->n_unneeded_pairs());
  M->show();
  return is_done;
}

//////////////////////////////////
// Debugging routines only ///////
//////////////////////////////////

#include "f4-m2-interface.hpp"
#include "../text-io.hpp"
#include "../mat.hpp"
#include "../freemod.hpp"

void F4GB::show_gb_array(const gb_array &g) const
{
  // Debugging routine
  // Display the array, and all of the internal information in it too.
  buffer o;
  for (int i = 0; i < g.size(); i++)
    {
      vec v = F4toM2Interface::to_M2_vec(KK, M, g[i]->f, F);
      o << "element " << i << " degree " << g[i]->deg << " alpha "
        << g[i]->alpha << newline << "    ";
      F->get_ring()->vec_text_out(o, v);
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
      if (mat->rows[i].monom == 0)
        fprintf(stderr, "generator");
      else
        M->show(mat->rows[i].monom);
      fprintf(stderr, "\n");
    }
}

void F4GB::show_column_info() const
{
  // Debugging routine
  for (int i = 0; i < mat->columns.size(); i++)
    {
      fprintf(stderr, "head %4d monomial ", mat->columns[i].head);
      M->show(mat->columns[i].monom);
      fprintf(stderr, "\n");
    }
}

void F4GB::show_matrix()
{
  // Debugging routine
  MutableMatrix *q = F4toM2Interface::to_M2_MutableMatrix(KK, mat, gens, gb);
  buffer o;
  q->text_out(o);
  emit(o.str());
}

//////////////////// LINBOX includes //////////////////////////////////////
//#include <linbox/field/modular.h>
//#include <linbox/blackbox/sparse.h>
//#include <linbox/solutions/rank.h>
//#include <linbox/util/timer.h>
// using namespace LinBox;

/////////////////// linbox ////////////////////////////////////

void F4GB::gauss_reduce_linbox()
// dumps the current matrix into a file in the linbox sparse matrix format
{
  // dump the matrix in a file
  char fname[30];
  sprintf(fname, "tmp.%i.matrix", this_degree);
  FILE *mfile = fopen(fname, "w");
  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);

  fprintf(mfile, "%i %i M\n", nrows, ncols);
  for (int i = 0; i < nrows; i++)
    {
      row_elem &r = mat->rows[i];
      int *sparseelems = static_cast<int *>(r.coeffs);
      for (int j = 0; j < r.len; j++)
        {
          fprintf(
              mfile,
              "%i %i %i\n",
              i + 1,
              r.comps[j] + 1,
              KK->coeff_to_int(*sparseelems++));  // is *sparseelems integer?
        }
    }
  fprintf(mfile, "0 0 0\n");
  fclose(mfile);
}

/////////////////// end linbox ///////////////////////////////

void F4GB::show_new_rows_matrix()
{
  int ncols = INTSIZE(mat->columns);
  int nrows = 0;
  for (int nr = 0; nr < mat->rows.size(); nr++)
    if (is_new_GB_row(nr)) nrows++;

  MutableMatrix *gbM =
      IM2_MutableMatrix_make(KK->get_ring(), nrows, ncols, false);

  int r = -1;
  for (int nr = 0; nr < mat->rows.size(); nr++)
    if (is_new_GB_row(nr))
      {
        r++;
        row_elem &row = mat->rows[nr];
        ring_elem *rowelems = newarray(ring_elem, row.len);
        if (row.coeffs == 0)
          {
            if (row.monom == 0)
              KK->to_ringelem_array(
                  row.len, gens[row.elem]->f.coeffs, rowelems);
            else
              KK->to_ringelem_array(row.len, gb[row.elem]->f.coeffs, rowelems);
          }
        else
          {
            KK->to_ringelem_array(row.len, row.coeffs, rowelems);
          }
        for (int i = 0; i < row.len; i++)
          {
            int c = row.comps[i];
            gbM->set_entry(r, c, rowelems[i]);
          }
        deletearray(rowelems);
      }

  buffer o;
  gbM->text_out(o);
  emit(o.str());
}

#include "moninfo.hpp"
#include "../coeffrings.hpp"
template class MemoryBlock<monomial_word>;
template class MemoryBlock<pre_spair>;
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e f4/f4.o "
// indent-tabs-mode: nil
// End:
