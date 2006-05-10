// Copyright 2005 Michael E. Stillman

#include <ctime>

#include "F4.hpp"
#include "monsort.hpp"
#include "../queue.hpp"

static clock_t clock_sort_columns = 0;
static clock_t clock_gauss = 0;
static clock_t clock_make_matrix = 0;

template<typename CoeffRing>
F4GB<CoeffRing>::F4GB(const Gausser *KK0,
		      const MonomialInfo *M0,
		      M2_bool collect_syz, 
		      int n_rows_to_keep,
		      M2_arrayint weights0,
		      int strategy, 
		      M2_bool use_max_degree,
		      int max_degree)
  : KK(KK0),
    M(M0),
    weights(weights0),
    component_degrees(0), // need to put this in
    n_pairs_computed(0),
    n_gens_left(0),
    n_subring(0),
    complete_thru_this_degree(-1), // need to reset this in the body
    lookup(0),
    S(0),
    next_col_to_process(0),
    mat(0),
    H(0)
{
  lookup = new MonomialLookupTable;
  // set status
}

template<typename CoeffRing>
F4GB<CoeffRing>::~F4GB()
{
}

///////////////////////////////////////////////////
// Creation of the matrix over K //////////////////
///////////////////////////////////////////////////
template<typename CoeffRing>
int F4GB<CoeffRing>::new_column(packed_monomial m)
{
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

template<typename CoeffRing>
int F4GB<CoeffRing>::find_or_append_column(packed_monomial m)
{
  packed_monomial new_m;
  if (H->find_or_insert(m, new_m))
    return new_m[-1];
  // At this point, m is a new monomial to be placed as a column
  return new_column(m);
}

template<typename CoeffRing>
int F4GB<CoeffRing>::mult_monomials(packed_monomial m, packed_monomial n)
{
  // We already have allocated space for a monomial
  // Do the multiply
  // Look it up in the hashtable
  // If it is there, return its column
  // If not, increment our memory block, and insert a new column.
  packed_monomial new_m;
  M->mult(m,n,next_monom);
  if (H->find_or_insert(next_monom, new_m))
    return new_m[-1]; // monom exists, don't save monomial space
  m = next_monom;
  B.intern(M->monomial_size(m));
  next_monom = B.reserve(M->max_monomial_size());
  return new_column(m);
}

template<typename CoeffRing>
void F4GB<CoeffRing>::load_gen(int which)
{
  poly &g = gens[which]->f;

  row_elem r;
  r.monom = NULL; // This says that this element corresponds to a generator
  r.elem = which;
  
  r.len = g.len;
  r.coeffs = 0; // coefficients are grabbed during gauss().
  r.comps = newarray(int, g.len);

  monomial_word *w = g.monom_space;
  for (int i=0; i<g.len; i++)
    {
      r.comps[i] = find_or_append_column(w);
      w += M->monomial_size(w);
    }

  mat->rows.push_back(r);
}

template<typename CoeffRing>
void F4GB<CoeffRing>::load_row(packed_monomial monom, int which)
{
  poly &g = gb[which]->f;

  row_elem r;
  r.monom = monom;
  r.elem = which;

  r.len = g.len;
  r.coeffs = 0; // coefficients are grabbed during gauss().
  r.comps = newarray(int, g.len);

  monomial_word *w = g.monom_space;
  for (int i=0; i<g.len; i++)
    {
      r.comps[i] = mult_monomials(monom, w);
      w += M->monomial_size(w);
    }

  mat->rows.push_back(r);
}

template<typename CoeffRing>
void F4GB<CoeffRing>::process_column(int c)
{
  /* If this column has been handled before, return.
     Otherwise, find a GB element whose lead term
     divides this monomial, and either mark this colum
     as not an initial element, OR append a row
  */

  column_elem &ce = mat->columns[c];
  if (ce.gb_divisor >= -1)
    return;
  int which;
  bool found = lookup->search(ce.monom, which);
  if (found)
    {
      packed_monomial n = next_monom;
      M->unchecked_divide(ce.monom, gb[which]->f.monom_space, n);
      B.intern(M->monomial_size(n));
      next_monom = B.reserve(M->max_monomial_size());
      M->set_component(which, n);
      ce.gb_divisor = mat->rows.size();
      ce.head = ce.gb_divisor;
      load_row(n,which);
    }
  else 
    ce.gb_divisor = -1;
}

template<typename CoeffRing>
void F4GB<CoeffRing>::process_s_pair(spair *p)
{
  int c;

  switch (p->type) {
  case F4_SPAIR_SPAIR:
    load_row(p->s.spair.first_monom, p->s.spair.first_gb_num);
    c = mat->rows[mat->rows.size()-1].comps[0];
    mat->columns[c].gb_divisor = mat->rows.size()-1;
    mat->columns[c].head = mat->columns[c].gb_divisor;
    load_row(p->s.spair.second_monom, p->s.spair.second_gb_num);
    break;
  case F4_SPAIR_GEN:
    load_gen(p->s.poly.column);
    break;
  default:
    break;
  }
}

template<typename CoeffRing>
class ColumnsSorter
{
  INCLUDE_F4_TYPES;
public:
  typedef typename MonomialInfo::value monomial;
  typedef long value;
private:
  const MonomialInfo *M;
  const coefficient_matrix *mat;
  long ncmps;
public:
  int compare(value a, value b)
  {
    ncmps ++;
    return M->compare_grevlex(mat->columns[a].monom,mat->columns[b].monom);
  }

  ColumnsSorter(const MonomialInfo *M0, const coefficient_matrix *mat0)
    : M(M0), mat(mat0), ncmps(0) {}

  long ncomparisons() const { return ncmps; }
  
  ~ColumnsSorter() {} 
};

template<typename CoeffRing>
void F4GB<CoeffRing>::reorder_columns()
{
  // Set up to sort the columns.
  // Result is an array 0..ncols-1, giving the new order.
  // Find the inverse of this permutation: place values into "ord" column fields.
  // Loop through every element of the matrix, changing its comp array.

  long nrows = mat->rows.size();
  long ncols = mat->columns.size();

  // sort the columns

  long *column_order = newarray(long, ncols);

  clock_t begin_time = clock();
  for (long i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  fprintf(stderr, "ncomparisons = ");

  // MES  ColumnsSorter<CoeffRing> C(M, mat);
  // MES  QuickSorter< ColumnsSorter<CoeffRing> >::sort(&C, column_order, ncols);

  // MES fprintf(stderr, "%ld, ", C.ncomparisons());
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
  typename coefficient_matrix::column_array newcols;
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

template<typename CoeffRing>
void F4GB<CoeffRing>::make_matrix()
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */

  H = new MonomialHash(M,18);
  mat = new coefficient_matrix;
  spair *p;
  while (p = S->get_next_pair())
    {
      process_s_pair(p);
    }

  while (next_col_to_process < mat->columns.size())
    process_column(next_col_to_process++);

  // DEBUGGING:
  fprintf(stderr, "--matrix--%u by %u\n", 
	  mat->rows.size(), mat->columns.size());

  // Now we reorder the columns, rows?
  reorder_columns();
  reorder_rows();  // This is only here so we can see what we are doing...?
}

///////////////////////////////////////////////////
// Extracting new GB elements    //////////////////
///////////////////////////////////////////////////

template<typename CoeffRing>
void F4GB<CoeffRing>::insert_gb_element(row_elem &r)
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

  // grab coeffs from the row itself
  F4CoefficientArray tmp = r.coeffs;
  r.coeffs = result->f.coeffs;
  result->f.coeffs = tmp;

  result->f.monom_space = newarray(monomial_word, nlongs);

  // MES: set result->f.monoms too
  monomial_word *nextmonom = result->f.monom_space;
  for (int i=0; i<r.len; i++)
    {
      M->copy(mat->columns[r.comps[i]].monom, nextmonom);
      // MES: should we keep those pointers here too??
      nextmonom += nslots;
    }
  result->deg = this_degree;
  result->alpha = M->last_exponent(result->f.monom_space);
  result->is_minimal = true;
  result->minlevel = ELEM_MIN_GB; // MES: why both is_minimal and minlevel.  Also how do
                                  // we distinguish between ELEM_MIN_GB, ELEM_POSSIBLE_MINGEN?

  int which = gb.size();
  gb.push_back(result);

  // now insert the lead monomial into the lookup table                                
  lookup->insert(result->f.monom_space, which);

  // now go forth and find those new pairs
  S->find_new_pairs(gb, true);
}

template<typename CoeffRing>
void F4GB<CoeffRing>::new_GB_elements()
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
    {
      if (mat->rows[r].len == 0) continue;
      int c = mat->rows[r].comps[0];
      if (mat->columns[c].gb_divisor < 0)
	insert_gb_element(mat->rows[r]);
    }
}

///////////////////////////////////////////////////
// Top level algorithm logic     //////////////////
///////////////////////////////////////////////////

template<typename CoeffRing>
void F4GB<CoeffRing>::do_spairs()
{
  clock_t begin_time = clock();

  make_matrix();

  clock_t end_time = clock();
  clock_make_matrix += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " make matrix time = %f\n", nsecs);

  gauss_reduce();
  new_GB_elements();
  
  // reset rows and columns and other matrix aspects
  mat->rows.clear();
  mat->columns.clear();
  next_col_to_process = 0;
}

template<typename CoeffRing>
enum ComputationStatusCode F4GB<CoeffRing>::computation_is_complete(StopConditions &stop_)
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
#warning "compute the codimension"
      int c = 0; // replace this line
      //int c = codim_of_lead_terms();
      if (c >= stop_.codim_limit)
	return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

template<typename CoeffRing>
enum ComputationStatusCode F4GB<CoeffRing>::start_computation(StopConditions &stop_)
{
  queue<packed_monomial> Q;
  //  lookup = MonomialLookupTable::create<MonomialInfo>(M,Q);
  
  return COMP_DONE;
  clock_sort_columns = 0;
  clock_gauss = 0;
  clock_make_matrix = 0;
  int npairs;
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


#include "moninfo.hpp"
#include "memblock.cpp"
#include "../coeffrings.hpp"
template class F4GB<CoefficientRingZZp>;
template class MemoryBlock<monomial_word,4092l>;


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
