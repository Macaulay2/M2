// Copyright 2005 Michael E. Stillman

#include <ctime>

#include "f4.hpp"
#include "monsort.hpp"

static clock_t clock_sort_columns = 0;
static clock_t clock_gauss = 0;
static clock_t clock_make_matrix = 0;

F4GB::F4GB(const Gausser *KK0,
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
    component_degrees(0), // need to put this in
    n_pairs_computed(0),
    n_gens_left(0),
    n_subring(0),
    complete_thru_this_degree(-1), // need to reset this in the body
    this_degree(), 
    gens(), 
    gb(), 
    lookup(0),
    S(0),
    next_col_to_process(0),
    mat(0),
    H(M0,17),
    B(), 
    next_monom(), 
    syz_next_col_to_process(0),
    syz(0),
    syzH(M0,17),
    syzB(), 
    syz_next_monom(), 
    gauss_row()
{
  lookup = new MonomialLookupTable(M->n_vars());
  S = new F4SPairSet(M, gb);
  mat = new coefficient_matrix;
  syz = new coefficient_matrix;
  // set status
  M->show();
}

F4GB::~F4GB()
{
}

void F4GB::new_generators(int lo, int hi)
{
  for (int i=lo; i<=hi; i++)
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
  int next_column = mat->columns.size();
  m[-1] = next_column;
  c.monom = m;
  c.gb_divisor = -2;
  c.head = -1;
  c.ord = 0;
  mat->columns.push_back(c);
  return next_column;
}

int F4GB::syz_new_column(packed_monomial m)
{
  // m is a packed monomial (with component), 
  // unique via the hash table syzH, syzB.
  column_elem c;
  int next_column = syz->columns.size();
  m[-1] = next_column;
  c.monom = m;
  c.gb_divisor = -2;
  c.head = -1;
  c.ord = 0;
  syz->columns.push_back(c);
  return next_column;
}

int F4GB::find_or_append_column(packed_monomial m)
{
  packed_monomial new_m;
  if (H.find_or_insert(m, new_m))
    return new_m[-1];
  // At this point, m is a new monomial to be placed as a column
  m = next_monom;
  B.intern(1+M->monomial_size(m));
  next_monom = B.reserve(1+M->max_monomial_size());
  next_monom++;
  return new_column(m);
}

int F4GB::syz_find_or_append_column(packed_monomial m)
{
  packed_monomial new_m;
  if (syzH.find_or_insert(m, new_m))
    return new_m[-1];
  m = syz_next_monom;
  // At this point, m is a new monomial to be placed as a column
  syzB.intern(1+M->monomial_size(m));
  syz_next_monom = B.reserve(1+M->max_monomial_size());
  syz_next_monom++;
  return syz_new_column(m);
}


int F4GB::mult_monomials(packed_monomial m, packed_monomial n)
{
  // We already have allocated space for a monomial
  // Do the multiply
  // Look it up in the hashtable
  // If it is there, return its column
  // If not, increment our memory block, and insert a new column.
  packed_monomial new_m;
  M->unchecked_mult(m,n,next_monom);
  if (H.find_or_insert(next_monom, new_m))
    return new_m[-1]; // monom exists, don't save monomial space
  m = next_monom;
  B.intern(1+M->monomial_size(m));
  next_monom = B.reserve(1+M->max_monomial_size());
  next_monom++;
  return new_column(m);
}


void F4GB::load_gen(int which)
{
  poly &g = gens[which]->f;

  row_elem r;
  r.monom = NULL; // This says that this element corresponds to a generator
  r.elem = which;
  
  r.len = g.len;
  r.coeffs = g.coeffs; // we don't "own" these elements??
  r.comps = newarray_atomic(int, g.len);

  monomial_word *w = g.monoms;
  for (int i=0; i<g.len; i++)
    {
      M->copy(w, next_monom);
      r.comps[i] = find_or_append_column(next_monom);
      w += M->monomial_size(w);
    }

  mat->rows.push_back(r);
}


void F4GB::load_row(packed_monomial monom, int which)
{
  poly &g = gb[which]->f;

  row_elem r;
  r.monom = monom;
  r.elem = which;

  r.len = g.len;
  r.coeffs = g.coeffs; // We do not own this array
  r.comps = newarray_atomic(int, g.len);

  monomial_word *w = g.monoms;
  for (int i=0; i<g.len; i++)
    {
      r.comps[i] = mult_monomials(monom, w);
      w += M->monomial_size(w);
    }

  mat->rows.push_back(r);
  
  // Append a new row to the syzygy matrix
  M->unchecked_mult(monom, gb[which]->f.monoms, syz_next_monom);  
  M->set_component(which, syz_next_monom);
  packed_monomial m;
  if (!H.find_or_insert(syz_next_monom, m)) //use the old copy if already there
    {
      m = syz_next_monom;
      syzB.intern(1+M->monomial_size(m));
      syz_next_monom = syzB.reserve(1+M->max_monomial_size());
      syz_next_monom++;     
    }
  row_elem syz_r;
  syz_r.monom = m;
  syz_r.elem = which; // here info is duplicated: which == M->get_component(m)

  syz_r.len = 1;
  syz_r.coeffs = newarray_atomic(int, 1); 
  static_cast<int *>(syz_r.coeffs)[0] = 0; // this represents 1 in the coefficient field 
  syz_r.comps = newarray_atomic(int, 1);
  static_cast<int *>(syz_r.comps)[0] = syz_find_or_append_column(m);
  syz->rows.push_back(syz_r);
}


void F4GB::process_column(int c)
{
  /* If this column has been handled before, return.
     Otherwise, find a GB element whose lead term
     divides this monomial, and either mark this colum
     as not an initial element, OR append a row
  */

  column_elem &ce = mat->columns[c];
  if (ce.gb_divisor >= -1)
    return;
  int32_t which;
  bool found = lookup->find_one_divisor_packed(M, ce.monom, which);
  if (found)
    {
      packed_monomial n = next_monom;
      M->unchecked_divide(ce.monom, gb[which]->f.monoms, n);
      B.intern(1+M->monomial_size(n));
      next_monom = B.reserve(1+M->max_monomial_size());
      next_monom++;
      //M->set_component(which, n);
      ce.gb_divisor = mat->rows.size();
      ce.head = ce.gb_divisor;
      load_row(n,which);
    }
  else 
    ce.gb_divisor = -1;
}


void F4GB::process_s_pair(spair *p)
{
  int c;

  switch (p->type) {
  case F4_SPAIR_SPAIR: {
    packed_monomial n = next_monom;
    M->unchecked_divide(p->lcm, gb[p->i]->f.monoms, n);
    B.intern(1+M->monomial_size(n));
    next_monom = B.reserve(1+M->max_monomial_size());
    next_monom++;

    load_row(n, p->i);
    c = mat->rows[mat->rows.size()-1].comps[0];

    if (mat->columns[c].gb_divisor != -2)
      n_lcmdups++;
    else {
      // In this situation, we load the other half as a reducer
      n = next_monom;
      M->unchecked_divide(p->lcm, gb[p->j]->f.monoms, n);
      B.intern(1+M->monomial_size(n));
      next_monom = B.reserve(1+M->max_monomial_size());
      next_monom++;
      load_row(n, p->j);

      mat->columns[c].gb_divisor = mat->rows.size()-1;
      mat->columns[c].head = mat->columns[c].gb_divisor;
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

template class QuickSorter<ColumnsSorter>;

void F4GB::reorder_columns()
{
  // Set up to sort the columns.
  // Result is an array 0..ncols-1, giving the new order.
  // Find the inverse of this permutation: place values into "ord" column fields.
  // Loop through every element of the matrix, changing its comp array.

  long nrows = mat->rows.size();
  long ncols = mat->columns.size();

  // sort the columns

  long *column_order = newarray_atomic(long, ncols);

  clock_t begin_time = clock();
  for (long i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  fprintf(stderr, "ncomparisons = ");

  ColumnsSorter C(M, mat);
  QuickSorter<ColumnsSorter>::sort(&C, column_order, ncols);

  fprintf(stderr, "%ld, ", C.ncomparisons());
  clock_t end_time = clock();
  clock_sort_columns += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " time = %f\n", nsecs);
  //  fprintf(stdout, "column order: ");
  //  for (int i=0; i<column_order.size(); i++)
  //      fprintf(stdout, "%d ", column_order[i]);
  //    fprintf(stdout, "\n");

  for (long i=0; i<ncols; i++)
    {
      mat->columns[column_order[i]].ord = i;
    }

  // Now move the columns into position
  coefficient_matrix::column_array newcols;
  newcols.reserve(ncols);
  for (long i=0; i<ncols; i++)
    {
      long newc = column_order[i];
      newcols.push_back(mat->columns[newc]);
    }

  // Now reset the components in each row
  for (long r=0; r<nrows; r++)
    {
      row_elem &row = mat->rows[r];
      for (long i=0; i<row.len; i++)
	{
	  long oldcol = row.comps[i];
	  long newcol = mat->columns[oldcol].ord;
	  row.comps[i] = newcol;
	}
    }

  std::swap(mat->columns, newcols);
}

void F4GB::syz_reorder_columns()
{
  // Same as reorder_columns(), but for syzygy matrix

  long nrows = syz->rows.size();
  long ncols = syz->columns.size();

  // sort the columns

  long *column_order = newarray_atomic(long, ncols);

  clock_t begin_time = clock();
  for (long i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  fprintf(stderr, "ncomparisons = ");

  ColumnsSorter C(M, syz);
  QuickSorter<ColumnsSorter>::sort(&C, column_order, ncols);

  fprintf(stderr, "%ld, ", C.ncomparisons());
  clock_t end_time = clock();
  clock_sort_columns += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " time = %f\n", nsecs);
  //  fprintf(stdout, "column order: ");
  //  for (int i=0; i<column_order.size(); i++)
  //      fprintf(stdout, "%d ", column_order[i]);
  //    fprintf(stdout, "\n");

  for (long i=0; i<ncols; i++)
    {
      syz->columns[column_order[i]].ord = i;
    }

  // Now move the columns into position
  coefficient_matrix::column_array newcols;
  newcols.reserve(ncols);
  for (long i=0; i<ncols; i++)
    {
      long newc = column_order[i];
      newcols.push_back(syz->columns[newc]);
    }

  // Now reset the components in each row
  for (long r=0; r<nrows; r++)
    {
      row_elem &row = syz->rows[r];
      for (long i=0; i<row.len; i++)
	{
	  long oldcol = row.comps[i];
	  long newcol = syz->columns[oldcol].ord;
	  row.comps[i] = newcol;
	}
    }

  std::swap(syz->columns, newcols);
}


void F4GB::reorder_rows()
{
  //??? reorder rows in <mat> and <syz> simultaneously?

  long nrows = mat->rows.size();
  long ncols = mat->columns.size();
  coefficient_matrix::row_array newrows;
  VECTOR(long) rowlocs; // 0..nrows-1 of where row as been placed

  newrows.reserve(nrows);
  rowlocs.reserve(nrows);
  for (long r = 0; r < nrows; r++)
    rowlocs.push_back(-1);

  for (long c = 0; c < ncols; c++)
    {
      int oldrow = mat->columns[c].head;
      if (oldrow >= 0)
	{
	  // Move the row into place
	  long newrow = newrows.size();
	  newrows.push_back(mat->rows[oldrow]);
	  rowlocs[oldrow] = newrow;
	  mat->columns[c].head = newrow;
	  if (mat->columns[c].gb_divisor == oldrow)
	    mat->columns[c].gb_divisor = newrow;
	}
      
    }
  for (long r = 0; r < nrows; r++)
    if (rowlocs[r] < 0)
      newrows.push_back(mat->rows[r]);
  std::swap(mat->rows, newrows);
}

void F4GB::reset_matrix()
{
  // mat 
  next_col_to_process = 0;
  mat->rows.clear();
  mat->columns.clear();
  mat->column_order.clear();

  H.reset();
  B.reset();
  next_monom = B.reserve(1+M->max_monomial_size());
  next_monom++;
  
  // syz
  syz_next_col_to_process = 0;
  syz->rows.clear();
  syz->columns.clear();
  syz->column_order.clear();

  syzH.reset();
  syzB.reset();
  syz_next_monom = syzB.reserve(1+M->max_monomial_size());
  syz_next_monom++;
}

void F4GB::make_matrix()
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */

  reset_matrix();
  spair *p;
  while (p = S->get_next_pair())
    {
      process_s_pair(p);
    }

  while (next_col_to_process < mat->columns.size())
    process_column(next_col_to_process++);

  // DEBUGGING:
  fprintf(stderr, "--matrix--%ld by %ld\n", 
	  (long)mat->rows.size(), (long)mat->columns.size());
  fprintf(stderr, "-syzygies-%ld by %ld\n", 
	  (long)syz->rows.size(), (long)syz->columns.size());

  //  show_row_info();
  //  show_column_info();
  //  show_matrix();

  // Now we reorder the columns, rows?
  reorder_columns();
  syz_reorder_columns();
  //reorder_rows();  // This is only here so we can see what we are doing...?
}

///////////////////////////////////////////////////
// Gaussian elimination ///////////////////////////
///////////////////////////////////////////////////
bool F4GB::is_new_GB_row(int row)
// returns true if the r-th row has its lead term not in the current GB
// This can be used to determine which elements should be reduced in the first place
// and also to determine if an element (row) needs to be tail reduced
{
  row_elem &r = mat->rows[row];
  if (r.len == 0) return false;
  int pivotcol = r.comps[0];
  return(mat->columns[pivotcol].gb_divisor < 0);
}

void F4GB::gauss_reduce()
  // This is the one I am working on...
{
  // gauss_reduce_linbox(); //dump <mat> in a file
  
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  
  int first, last;
  int pivotrow, pivotcol;

  KK->dense_row_allocate(gauss_row, ncols);
  for (int i=0; i<nrows; i++)
    {
      row_elem &r = mat->rows[i];
      if (r.len == 0) continue; // should not happen MES?
      pivotcol = r.comps[0];
      pivotrow = mat->columns[pivotcol].head;
      if (pivotrow == i) continue; // this is the pivot row, so leave it alone
      if (pivotrow < 0)
	{
	  // In this case the element canot be reduced at all
	  KK->sparse_row_make_monic(r.len, r.coeffs);
	  mat->columns[pivotcol].head = i;
	  continue;
	}
      KK->dense_row_fill_from_sparse(gauss_row, r.len, r.coeffs, r.comps);
      first = r.comps[0];
      last = r.comps[r.len-1];
      do {
	row_elem &pivot_rowelem = mat->rows[pivotrow];
	KK->dense_row_cancel_sparse(gauss_row, pivot_rowelem.len, pivot_rowelem.coeffs, pivot_rowelem.comps);
	int last1 = pivot_rowelem.comps[pivot_rowelem.len-1];
	if (last1 > last) last = last1;
	first = KK->dense_row_next_nonzero(gauss_row, first, last);
	if (first > last) break;
	pivotrow = mat->columns[first].head;
      } while (pivotrow >= 0);
      KK->dense_row_to_sparse_row(gauss_row, r.len, r.coeffs, r.comps, first, last); 
        // the above line leavs gauss_row zero, and also handles the case when r.len is 0
        // it also potentially frees the old r.coeffs and r.comps
      if (r.len > 0)
	{
	  KK->sparse_row_make_monic(r.len, r.coeffs);
	  mat->columns[r.comps[0]].head = i;
	}
    }
}

void F4GB::gauss_reduce_triangular(bool diagonalize)
  // This reduces the matrix to a triangular form
  // It seems that gauss_reduce does NOT do that?
{
  // For each row which is a non-pivot row:
  //  note that the row must be reducible, since the lead term corresponds to an spair cancellation
  //   actually: not true: generators will often not be reducible...
  //  also each such row must be non-zero, for the same reason
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  
  KK->dense_row_allocate(gauss_row, ncols);
  for (int i=0; i<nrows; i++)
    {
      row_elem &r = mat->rows[i];
      if (r.len == 0) continue; // could happen once we include syzygies...
      int pivotcol = r.comps[0];
      int pivotrow = mat->columns[pivotcol].head;
      if (pivotrow == i) continue; // this is a pivot row, so leave it alone

      KK->dense_row_fill_from_sparse(gauss_row, r.len, r.coeffs, r.comps);
      int firstnonzero = ncols;
      int first = r.comps[0];
      int last = r.comps[r.len-1];
      do {
	pivotrow = mat->columns[first].head;
	if (pivotrow >= 0)
	  {
	    row_elem &pivot_rowelem = mat->rows[pivotrow];
	    KK->dense_row_cancel_sparse(gauss_row, pivot_rowelem.len, pivot_rowelem.coeffs, pivot_rowelem.comps);
	    int last1 = pivot_rowelem.comps[pivot_rowelem.len-1];
	    if (last1 > last) last = last1;
	  }
	else if (firstnonzero == ncols)
	  firstnonzero = first;
	first = KK->dense_row_next_nonzero(gauss_row, first+1, last);
      } while (first <= last);
      KK->dense_row_to_sparse_row(gauss_row, r.len, r.coeffs, r.comps, firstnonzero, last); 
      // the above line leavs gauss_row zero, and also handles the case when r.len is 0
      // it also potentially frees the old r.coeffs and r.comps
      if (r.len > 0)
	{
	  KK->sparse_row_make_monic(r.len, r.coeffs);
	  mat->columns[r.comps[0]].head = i;
	}
    }

  if (diagonalize)
    tail_reduce();
}

void F4GB::tail_reduce()
{
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  
  KK->dense_row_allocate(gauss_row, ncols);
  for (int i=nrows-1; i>=0; i--)
    {
      row_elem &r = mat->rows[i];
      if (r.len <= 1) continue; // row reduced to zero, ignore it.
      int pivotcol = r.comps[0];
      int g = mat->columns[pivotcol].gb_divisor;
      if (g >= 0) continue;
      // At this point, we should have an element to reduce

      KK->dense_row_fill_from_sparse(gauss_row, r.len, r.coeffs, r.comps);
      int firstnonzero = r.comps[0];
      int first = (r.len == 1 ? ncols :  r.comps[1]);
      int last = r.comps[r.len-1];
      while (first <= last) {
	int pivotrow = mat->columns[first].head;
	if (pivotrow >= 0)
	  {
	    row_elem &pivot_rowelem = mat->rows[pivotrow];
	    KK->dense_row_cancel_sparse(gauss_row, pivot_rowelem.len, pivot_rowelem.coeffs, pivot_rowelem.comps);
	    int last1 = pivot_rowelem.comps[pivot_rowelem.len-1];
	    if (last1 > last) last = last1;
	  }
	else if (firstnonzero == ncols)
	  firstnonzero = first;
	first = KK->dense_row_next_nonzero(gauss_row, first+1, last);
      };
      KK->dense_row_to_sparse_row(gauss_row, r.len, r.coeffs, r.comps, firstnonzero, last); 
      // the above line leavs gauss_row zero, and also handles the case when r.len is 0
      // it also potentially frees the old r.coeffs and r.comps
      if (r.len > 0)
	{
	  KK->sparse_row_make_monic(r.len, r.coeffs);
	  mat->columns[r.comps[0]].head = i;
	}
    }
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

  long nslots = M->max_monomial_size();
  long nlongs = r.len * nslots;

  gbelem *result = new gbelem;
  result->f.len = r.len;

  // If the coeff array is null, then that means the coeffs come from the original array
  // Here we copy it over.

  if (r.coeffs == 0)
    {
      if (r.monom == 0) // i.e. a generator
	r.coeffs = KK->copy_F4CoefficientArray(r.len,gens[r.elem]->f.coeffs);
      else
	r.coeffs = KK->copy_F4CoefficientArray(r.len,gb[r.elem]->f.coeffs);
    }

  // grab coeffs from the row itself
  F4CoefficientArray tmp = r.coeffs;
  r.coeffs = result->f.coeffs;
  result->f.coeffs = tmp;

  result->f.monoms = newarray_atomic(monomial_word, nlongs);

  monomial_word *nextmonom = result->f.monoms;
  for (int i=0; i<r.len; i++)
    {
      M->copy(mat->columns[r.comps[i]].monom, nextmonom);
      nextmonom += nslots;
    }
  result->deg = this_degree;
  result->alpha = M->last_exponent(result->f.monoms);
  result->minlevel = ELEM_MIN_GB; // MES: How do
                                  // we distinguish between ELEM_MIN_GB, ELEM_POSSIBLE_MINGEN?

  int which = gb.size();
  gb.push_back(result);

  // now insert the lead monomial into the lookup table
  varpower_monomial vp = newarray_atomic(varpower_word, 2 * M->n_vars() + 1);
  M->to_varpower_monomial(result->f.monoms, vp);
  lookup->insert_minimal_vp(M->get_component(result->f.monoms), vp, which);
  deleteitem(vp);
  // now go forth and find those new pairs
  S->find_new_pairs(true);
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
  
  /* If we can place the possible new elements first, or in a separate place, then
     we don't need to loop through all of these */
     
  for (int r=0; r<mat->rows.size(); r++)
    if (is_new_GB_row(r))
      insert_gb_element(mat->rows[r]);
}

///////////////////////////////////////////////////
// Top level algorithm logic     //////////////////
///////////////////////////////////////////////////


void F4GB::do_spairs()
{
  clock_t begin_time = clock();

  n_lcmdups = 0;
  make_matrix();

  clock_t end_time = clock();
  clock_make_matrix += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " make matrix time = %f\n", nsecs);

  H.dump();

  //  show_matrix();
  begin_time = clock();
  gauss_reduce_triangular(true);
  //  gauss_reduce();
  end_time = clock();
  clock_gauss += end_time - begin_time;

  nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " gauss time          = %f\n", nsecs);

  fprintf(stderr, " lcm dups            = %d\n", n_lcmdups);
  //show_matrix();
  //  show_new_rows_matrix();

  //  int ngb = gb.size();
  new_GB_elements();
  //  int ngb1 = gb.size();
  //  fprintf(stderr, " # new GB elements   = %d\n", ngb1 - ngb);
  
  // reset rows and columns and other matrix aspects
  mat->rows.clear();
  mat->columns.clear();
  next_col_to_process = 0;
}


enum ComputationStatusCode F4GB::computation_is_complete(StopConditions &stop_)
{
  // This handles everything but stop_.always, stop_.degree_limit
  if (stop_.basis_element_limit > 0 && gb.size() >= stop_.basis_element_limit) 
    return COMP_DONE_GB_LIMIT;
  if (stop_.pair_limit > 0 && n_pairs_computed >= stop_.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (stop_.just_min_gens && n_gens_left == 0)
    return COMP_DONE_MIN_GENS;
  if (stop_.subring_limit > 0 && n_subring >= stop_.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  if (stop_.use_codim_limit)
    {
#ifdef DEVELOPMENT
#warning "compute the codimension"
#endif
      int c = 0; // replace this line
      //int c = codim_of_lead_terms();
      if (c >= stop_.codim_limit)
	return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

void F4GB::test_spair_code()
{
  // This starts out with taking each generator and placing it into the
  // gb matrix, and then calling find_new_pairs after each one.
  // It displays the list of spairs after each generator.

  gb.push_back(gens[0]);
  for (int i=1; i<gens.size(); i++)
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

  for (;;)
    {
      if (system_interruptedFlag) 
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
      fprintf(stdout, "DEGREE %d (npairs %d)\n", this_degree, npairs);
      do_spairs();
      complete_thru_this_degree = this_degree;
    }

  double clock_time = clock_sort_columns;
  clock_time /= CLOCKS_PER_SEC;
  fprintf(stderr, "total time for sorting columns: %f\n", clock_time);

  clock_time = clock_make_matrix;
  clock_time /= CLOCKS_PER_SEC;
  fprintf(stderr, "total time for making matrix (includes sort): %f\n", clock_time);

  clock_time = clock_gauss;
  clock_time /= CLOCKS_PER_SEC;
  fprintf(stderr, "total time for gauss: %f\n", clock_time);

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
  for (int i=0; i<g.size(); i++)
    {
      vec v = F4toM2Interface::to_M2_vec(KK, M, g[i]->f, F);
      o << "element " << i 
	<< " degree " << g[i]->deg 
	<< " alpha " << g[i]->alpha 
	<< newline << "    ";
      F->get_ring()->vec_text_out(o, v);
      o << newline;
    }
  emit(o.str());
}

void F4GB::show_row_info() const 
{
  // Debugging routine
  for (int i=0; i<mat->rows.size(); i++)
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
  for (int i=0; i<mat->columns.size(); i++)
    {
      fprintf(stderr, "gbdivisor %4d ord %4d monomial ", 
	      mat->columns[i].gb_divisor,
	      mat->columns[i].ord);
      M->show(mat->columns[i].monom);
      fprintf(stderr, "\n");
    }
}

void F4GB::show_matrix()
{
  // Debugging routine
  MutableMatrix *q = F4toM2Interface::to_M2_MutableMatrix(KK,mat,gens,gb);
  buffer o;
  q->text_out(o);
  emit(o.str());
}

//////////////////// LINBOX includes //////////////////////////////////////
//#include <linbox/field/modular.h>
//#include <linbox/blackbox/sparse.h>
//#include <linbox/solutions/rank.h>
//#include <linbox/util/timer.h>
//using namespace LinBox;

/////////////////// linbox ////////////////////////////////////


void F4GB::gauss_reduce_linbox()
  // dumps the current matrix into a file in the linbox sparse matrix format
{
  // dump the matrix in a file
  char fname[30];      
  sprintf(fname, "tmp.%i.matrix", this_degree);
  FILE* mfile = fopen(fname, "w");  
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  
  fprintf(mfile, "%i %i M\n", nrows, ncols);
  for (int i=0; i<nrows; i++)
    {
      row_elem &r = mat->rows[i];
      int *sparseelems = static_cast<int *>(r.coeffs);
      for (int j=0; j<r.len; j++) {
	fprintf(mfile, "%i %i %i\n",i+1,r.comps[j]+1,KK->coeff_to_int(*sparseelems++)); //is *sparseelems integer? 
      }
    }  
  fprintf(mfile, "0 0 0\n");
  fclose(mfile);

  /*
  typedef Modular<short> Field;
  
  // Ring& ZP = KK->get_ring();
  int P = 23; // how go I get the characteristic of a ring?
  Field FF(P);
  Field::Element v;
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  
  SparseMatrix<Field> BB(FF,nrows,ncols); //in
  SparseMatrix<Field> E(FF,nrows,ncols); //out
  
  for (int i=0; i<nrows; i++)
    {
      row_elem &r = mat->rows[i];
      int *sparseelems = static_cast<int *>(r.coeffs);
      int* comps = r.comps; 
      for (int j=0; j<r.len; j++) {
	FF.init(v, *sparseelems++); //ZP.coerce_to_int? What is *sparseelems? 
	BB.setEntry(i, *comps++, v);  // put v in i,j position.
      }
    }  
  long unsigned int r;
  rank (r, BB);*/
}

/////////////////// end linbox ///////////////////////////////

void F4GB::show_new_rows_matrix()
{
  int ncols = mat->columns.size();
  int nrows = 0;
  for (int nr=0; nr<mat->rows.size(); nr++)
    if (is_new_GB_row(nr)) nrows++;

  MutableMatrix *gbM = IM2_MutableMatrix_make(KK->get_ring(), nrows, ncols, false);

  int r = -1;
  for (int nr=0; nr<mat->rows.size(); nr++)
    if (is_new_GB_row(nr))
      {
	r++;
	row_elem &row = mat->rows[nr];
	ring_elem *rowelems = newarray(ring_elem, row.len);
	if (row.coeffs == 0)
	  {
	    if (row.monom == 0)
	      KK->to_ringelem_array(row.len, gens[row.elem]->f.coeffs, rowelems);
	    else
	      KK->to_ringelem_array(row.len, gb[row.elem]->f.coeffs, rowelems);
	  }
	else
	  {
	    KK->to_ringelem_array(row.len, row.coeffs, rowelems);
	  }
	for (int i=0; i<row.len; i++)
	  {
	    int c = row.comps[i];
	    gbM->set_entry(r,c,rowelems[i]);
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
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/f4 f4.o "
// End:

