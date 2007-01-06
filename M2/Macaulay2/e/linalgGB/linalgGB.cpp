// Copyright 2005 Michael E. Stillman.

#include <map>
#include <vector>
#include <ctime>

#include "../text_io.hpp"
#include "../debug.hpp"
#include "../z_mod_p.hpp"
#include "../matrix.hpp"
#include "../mat.hpp"
#include "../matrixcon.hpp"
#include "../dmat-LU.hpp"

#include "linalgGB.hpp"

#include "monoms.h"
#include "MonomialOps.hpp"
#include "MonomialTable.hpp"
#include "interface.hpp"

static clock_t clock_sort_columns = 0;
static clock_t clock_gauss = 0;
static clock_t clock_make_matrix = 0;

GBComputation *createLinearAlgebraGB(const Matrix *m,
				     M2_bool collect_syz,
				     int n_rows_to_keep,
				     M2_arrayint gb_weights,
				     int strategy,
				     M2_bool use_max_degree,
				     int max_degree)
{
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  const Ring *K = R->getCoefficients();
  GBComputation *G;

  const Z_mod *KZZp = K->cast_to_Z_mod();
  if (KZZp != 0)
    {
      G = new LinAlgGB<CoefficientRingZZp>(KZZp,m,
					   collect_syz,
					   n_rows_to_keep,
					   gb_weights,
					   use_max_degree,
					   max_degree,
					   strategy);
      return G;
    }
  ERROR("cannot use Strategy=>LinearAlgebra or Strategy=>Faugere with this type of coefficient ring");
  return 0;
}

template<typename CoeffRing>
LinAlgGB<CoeffRing>::LinAlgGB(const RingType *K0,
	   const Matrix *m, 
	   M2_bool collect_syz, 
	   int n_rows_to_keep,
	   M2_arrayint gb_weights,
	   int strategy, 
	   M2_bool use_max_degree,
	   int max_degree)
  : K(K0), S(&H)
{
  originalR = m->get_ring()->cast_to_PolynomialRing();
  F = m->rows();
  coeffK = K0->get_CoeffRing();

  n_subring = 0;
  n_pairs_computed = 0;
  n_gens_left = m->n_cols();

  lookup = new MonomialLookupTable;

  complete_thru_this_degree = -1;
  this_degree = -1;
  next_col_to_process = 0;
  weights = gb_weights;

  // Set the gens array
  M2Interface<CoeffRing>::from_M2_matrix(coeffK, m, &H, weights, gens);

  for (int i=0; i<gens.size(); i++)
    {
      gbelem *g = gens[i];
      S.insert(SPairSet::make_spair_gen(g->deg, g->f.monoms[0], i));
    }

  set_status(COMP_NOT_STARTED);
}

// Problems still to consider
// (a) finish implementation
// (b) set_comparisons DONE
// (c) choosing an alpha value not too high.
//     Also, can we do Mora algorithm this way?
// (d) field arithmetic in the matrix and polys.  Use templates?
// (e) After this is all working, then play with the LU decomposition
//
// To put in later:
//    module orders, Schreyer orders, syzygies, hilbert functions, ZZ,
//    quotients.

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::allocate_poly(poly &result, size_t len)
{
  result.len = len;
  result.coeffs = newarray(COEFF_TYPE, len);
  result.monoms = newarray(monomial, len);
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::allocate_sparse_row(row_elem &result, size_t len)
{
  result.len = len;
  result.coeffs = newarray(COEFF_TYPE, len);
  result.comps = newarray_atomic(int, len);
}

template<typename CoeffRing>
int LinAlgGB<CoeffRing>::find_or_append_column(monomial m)
{
  int next_column = mat->columns.size();
  std::pair<const monomial, int> p(m, next_column);
  typename coefficient_matrix::monomial_map::iterator i = mat->H0.find(m);
  if (i != mat->H0.end())
    return (*i).second;
  mat->H0.insert(p);
  column_elem c;
  c.monom = m;
  c.gb_divisor = -2;
  c.head = -1;
  c.ord = 0;
  mat->columns.push_back(c);
  return next_column;
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::load_gen(int which)
{
  row_elem r;
  r.monom = NULL;
  r.elem = which;

  poly &g = gens[which]->f;
  allocate_sparse_row(r, g.len);

  for (int i=0; i<g.len; i++)
    coeffK->init_set(r.coeffs[i], g.coeffs[i]);
  for (int i=0; i<g.len; i++)
    r.comps[i] = find_or_append_column(g.monoms[i]);

  mat->rows.push_back(r);
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::load_row(monomial monom, int which)
{
  row_elem r;
  r.monom = monom;
  r.elem = which;

  poly &g = gb[which]->f;
  allocate_sparse_row(r, g.len);

  for (int i=0; i<g.len; i++)
    coeffK->init_set(r.coeffs[i], g.coeffs[i]);

  for (int i=0; i<g.len; i++)
    {
      monomial n = MonomialOps::mult(&H, monom, g.monoms[i]);
      r.comps[i] = find_or_append_column(n);
    }

  mat->rows.push_back(r);
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::process_column(int c)
{
  /* If this column has been handled before, return.
     Otherwise, find a GB element whose lead term
     divides this monomial, and either mark this colum
     as not an initial element, OR append a row
  */

  column_elem &ce = mat->columns[c];
  if (ce.gb_divisor >= -1)
    return;
  tagged_monomial *b;
  bool found = lookup->search(ce.monom, b);
  if (found)
    {
      int which = (reinterpret_cast<long>(b->bag));
      uninterned_monomial um = H.reserve(MONOMIAL_LENGTH(ce.monom));
      monomial m;
      monomial_quotient(ce.monom, gb[which]->f.monoms[0], um);
      H.find_or_insert(um, m);
      ce.gb_divisor = mat->rows.size();
      ce.head = ce.gb_divisor;
      load_row(m,which);
    }
  else 
    ce.gb_divisor = -1;
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::process_s_pair(SPairSet::spair *p)
{
  int c;

  switch (p->type) {
  case SPairSet::SPAIR_SPAIR:
    load_row(p->s.spair.first_monom, p->s.spair.first_gb_num);
    c = mat->rows[mat->rows.size()-1].comps[0];
    mat->columns[c].gb_divisor = mat->rows.size()-1;
    mat->columns[c].head = mat->columns[c].gb_divisor;
    load_row(p->s.spair.second_monom, p->s.spair.second_gb_num);
    break;
  case SPairSet::SPAIR_GEN:
    load_gen(p->s.poly.column);
    break;
  default:
    break;
  }
}

static int ncomparisons = 0;
template <typename CoeffRing>
struct monomial_sorter : public std::binary_function<int,int,bool> {
  typedef VECTOR(column_elem) col_array ;

  col_array columns;
  monomial_sorter(col_array &columns0)
    : columns(columns0) {}

  bool operator()(int a, int b)
    {
      ncomparisons++;
      /* return the boolean value a > b */
      column_elem &m = columns[a];
      column_elem &n = columns[b];
      // First compare degrees, then weights, then do revlex lt routine
      int cmp = m.ord - n.ord;
      if (cmp > 0) return true;
      if (cmp < 0) return false;
      // Now compare via revlex
      cmp = monomial_compare(m.monom, n.monom); // This is lex compare
      if (cmp == LT) return true;
      return false;
    }
};

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::make_matrix()
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */

  mat = new coefficient_matrix;
  SPairSet::spair *p;
  while (p = S.get_next_pair())
    {
      process_s_pair(p);
      //remove_s_pair(p);
    }

  while (next_col_to_process < mat->columns.size())
    process_column(next_col_to_process++);

  // DEBUGGING:
  fprintf(stderr, "--matrix--%d by %d\n", 
	  mat->rows.size(), mat->columns.size());
  fflush(stderr);
  //  show_row_info();
  //show_column_info();
  //  H.dump();

  /* Now sort the monomials */
  //  set_comparisons();
  //  show_matrix();
  reorder_columns();
  //show_column_info();
  reorder_rows();
  //show_column_info();
  // DEBUGGING:
  //show_matrix();

  // change all of the components?  This depends on our LU algorithm
  //show_column_info();
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::reorder_columns()
{
  long nrows = mat->rows.size();
  long ncols = mat->columns.size();

  // sort the columns
  VECTOR(long) column_order;
  column_order.reserve(ncols);
  clock_t begin_time = clock();
  for (long i=0; i<ncols; i++)
    {
      // Set the degree, weight
      mat->columns[i].ord = monomial_weight(mat->columns[i].monom, weights);
      column_order.push_back(i);
    }

  fprintf(stderr, "ncomparisons = ");
  fflush(stderr);
  ncomparisons = 0;
  sort(column_order.begin(), 
       column_order.end(), 
       monomial_sorter<CoeffRing>(mat->columns));
  fprintf(stderr, "%d, ", ncomparisons);
  fflush(stderr);
  clock_t end_time = clock();
  clock_sort_columns += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " time = %f\n", nsecs);
  fflush(stderr);
  //  fprintf(stdout, "column order: ");
  //  for (int i=0; i<column_order.size(); i++)
  //      fprintf(stdout, "%d ", column_order[i]);
  //    fprintf(stdout, "\n");

  for (long i=0; i<column_order.size(); i++)
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
      typename coefficient_matrix::row_elem &row = mat->rows[r];
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
void LinAlgGB<CoeffRing>::reorder_rows()
{
  long nrows = mat->rows.size();
  long ncols = mat->columns.size();
  typename coefficient_matrix::row_array newrows;
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

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::sparse_to_dense_row(elem *v, int r)
{
  typename coefficient_matrix::row_elem &row = mat->rows[r];
  long ncols = mat->columns.size();
  for (long i=0; i<ncols; i++)
    coeffK->set_zero(v[i]);
  for (long i=0; i<row.len; i++)
    coeffK->init_set(v[row.comps[i]], row.coeffs[i]);
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::dense_to_sparse_row(int r, elem *v)
{
  typename coefficient_matrix::row_elem &row = mat->rows[r];
  long ncols = mat->columns.size();
  long len = 0;
  for (long i=0; i<ncols; i++)
    if (!coeffK->is_zero(v[i])) len++;

  allocate_sparse_row(row,len);
  long next = 0;
  for (long i=0; i<ncols; i++)
    if (!coeffK->is_zero(v[i]))
      {
	row.coeffs[next] = v[i];
	row.comps[next++] = i;
	coeffK->set_zero(v[i]);
      }
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::normalize_row(long r)
{
  typename coefficient_matrix::row_elem &row = mat->rows[r];
  elem a;
  if (row.len > 0)
    {
      coeffK->invert(a, row.coeffs[0]);
      for (long j=0; j<row.len; j++)
	coeffK->mult(row.coeffs[j], row.coeffs[j], a);
      long c = row.comps[0];
      mat->columns[c].head = r;
    }
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::reduce_row(long r, long first, long last)
{
  // Only reduces row[r], until a pivot occurs

  if (last < first) return;
  typename coefficient_matrix::row_elem &row = mat->rows[r];
  if (row.len == 0) return;

  // for each non-zero element of row r
  //   find row s to divide by, let c be the multiplier
  //   v -= c*(row s)
  // Now replace the row r by the sparse form of this

  //  fprintf(stdout, "reducing row %ld using %ld..%ld\nInput matrix is\n", r, first, last);
  //show_matrix();
  long ncols = mat->columns.size();

  elem *v = newarray(elem,ncols);
  sparse_to_dense_row(v, r);

  elem a;
  for (long c=row.comps[0]; c<ncols; c++)
    {
      if (coeffK->is_zero(v[c]))
	continue;
      long s = mat->columns[c].head;
      if (s < 0) break;
      elem pivot;
      coeffK->init_set(pivot, v[c]);
      if (s >= first && s <= last)
	{
	  typename coefficient_matrix::row_elem &row2 = mat->rows[s];
	  for (long t = 0; t < row2.len; t++)
	    {
	      coeffK->mult(a, pivot, row2.coeffs[t]);
	      long d = row2.comps[t];
	      coeffK->subtract(v[d], v[d], a);
	    }
	}
    }
  // Now we need to make a sparse row again
  dense_to_sparse_row(r, v);
  deletearray(v);
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::tail_reduce_row(long r, long first, long last)
{
  if (last < first) return;
  typename coefficient_matrix::row_elem &row = mat->rows[r];
  if (row.len == 0) return;

  // for each non-zero element of row r
  //   find row s to divide by, let c be the multiplier
  //   v -= c*(row s)
  // Now replace the row r by the sparse form of this

  //  fprintf(stdout, "reducing row %ld using %ld..%ld\nInput matrix is\n", r, first, last);
  //show_matrix();
  long ncols = mat->columns.size();

  elem *v = newarray(elem,ncols);
  sparse_to_dense_row(v, r);

  elem a;
  for (long i=0; i<row.len; i++)
    {
      long c = row.comps[i];
      long s = mat->columns[c].head;
      elem pivot;
      coeffK->init_set(pivot, v[c]);
      if (s >= first && s <= last)
	{
	  typename coefficient_matrix::row_elem &row2 = mat->rows[s];
	  for (long t = 0; t < row2.len; t++)
	    {
	      coeffK->mult(a, pivot, row2.coeffs[t]);
	      long d = row2.comps[t];
	      coeffK->subtract(v[d], v[d], a);
	    }
	}
    }
  dense_to_sparse_row(r,v);
  deletearray(v);

  //fprintf(stdout, "Output matrix is\n");
  //show_matrix();
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::gauss()
{
  //  fprintf(stdout, "Matrix before gauss\n");
  //  show_matrix();
  long nrows = mat->rows.size();

  // Step 1. Auto-reduce the first set of rows
  long last_pivot_row;
  for (last_pivot_row=0; last_pivot_row<nrows; last_pivot_row++)
    {
      long c = mat->rows[last_pivot_row].comps[0];
      if (mat->columns[c].head != last_pivot_row) break;
    }
  last_pivot_row--;
  //fprintf(stdout, "last pivot row = %ld\n",last_pivot_row);

  for (long r = last_pivot_row; r >= 0; r--)
    {
      tail_reduce_row(r,r+1,last_pivot_row);
    }
  // Step 2. Reduce the last set of rows wrt these
  for (long r = last_pivot_row+1; r<nrows; r++)
    {
      tail_reduce_row(r, 0, last_pivot_row);
    }

  // Step 3A. Reduce the last set of rows wrt these
  for (long r = last_pivot_row+1; r<nrows; r++)
    {
      reduce_row(r, last_pivot_row+1, r-1);
      normalize_row(r);
    }

  // Step 3. Inter-reduce those rows
  for (long r = nrows-1; r>=0; r--)
    {
      tail_reduce_row(r, r+1, nrows-1);
    }

  // Later: maybe use solving equations method, so that we can use more 
  // flexible solving methods...

  //  fprintf(stdout, "Matrix after gauss\n");
  //  show_matrix();
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::LU_decompose()
{
  clock_t begin_time = clock();
  gauss();
  clock_t end_time = clock(); 
  clock_gauss += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " gauss time = %f\n", nsecs);
  fflush(stderr);

#if 0
  // Dense LU code to make sure everything is working OK
  MATTYPE<CoeffRing> *A = M2Interface<CoeffRing>::to_M2_Mat(K,mat);
  MATTYPE<CoeffRing> *L = new MATTYPE<CoeffRing>(K,0,0);
  MATTYPE<CoeffRing> *U = new MATTYPE<CoeffRing>(K,0,0);
  M2_arrayint P = DMatLU<CoeffRing>::LU(A,L,U);
  new_GB_elements_dmat(U,P);
  show_gb_array(gb);
  ////// end denseLU code
#endif

  // DEBUGGING:
  //  MutableMat<CoeffRing, MATTYPE<CoeffRing> > *U1 = 
  //    MutableMat<CoeffRing, MATTYPE<CoeffRing> >::grab_Mat(U);
  //  MutableMat<CoeffRing, MATTYPE<CoeffRing> > *L1 = 
  //    MutableMat<CoeffRing, MATTYPE<CoeffRing> >::grab_Mat(L);
  //  fprintf(stdout, "P = ");
  //  dintarray(P);
  //  fprintf(stdout, "\n--- U --- \n");
  //  dmutablemat(U1);
  //  fprintf(stdout, "\n--- L --- \n");
  //  dmutablemat(L1);
  //  fprintf(stdout, "\n");

}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::sparse_row_to_poly(row_elem &r, poly &g)
{
  allocate_poly(g, r.len);
  for (int i=0; i<r.len; i++)
    {
      coeffK->init_set(g.coeffs[i], r.coeffs[i]);
      g.monoms[i] = mat->columns[r.comps[i]].monom;
    }
}

template<typename CoeffRing>
mygbelem<typename CoeffRing::elem> *
LinAlgGB<CoeffRing>::make_gbelem(poly &g)
{
  gbelem *result = new gbelem;
  std::swap(result->f,g);
  M2Interface<CoeffRing>::poly_set_degrees(coeffK,
					   weights,
					   result->f,
					   result->deg,
					   result->alpha);
  result->is_minimal = 1;
  result->minlevel = ELEM_MIN_GB;
  return result;
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::insert_gb_element(poly &g)
{
  // Grabs g.  Needs to set degree, alpha degree.
  gbelem *result = make_gbelem(g);
  void *v = reinterpret_cast<void *>(gb.size());
  tagged_monomial *b = new tagged_monomial(result->f.monoms[0], v);
  lookup->insert(b);
  gb.push_back(result);
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::new_GB_elements()
{
  /* After LU decomposition, loop through each
     row of the matrix.  If the corresponding 
     lead term is not in the initial ideal (or, at least,
     wasn't) then insert GB element (and so update spairs, etc,
     but don't do auto_reduce...)

     If instead the lead term is not new, then keep track of this
     information somehow: place ... into a monheap...
  */
  for (int r=0; r<mat->rows.size(); r++)
    {
      if (mat->rows[r].len == 0) continue;
      int c = mat->rows[r].comps[0];
      if (mat->columns[c].gb_divisor < 0)
	{
	  poly g;
	  sparse_row_to_poly(mat->rows[r], g); // This grabs the row poly
	                                       // leaving a zero poly
	  insert_gb_element(g);
	  S.find_new_pairs(gb, true);
	}
    }
  //  show_gb_array(gb);
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::s_pair_step()
{
  clock_t begin_time = clock();

  make_matrix();

  clock_t end_time = clock();
  clock_make_matrix += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  fprintf(stderr, " make matrix time = %f\n", nsecs);
  fflush(stderr);

  LU_decompose();
  new_GB_elements();
  // reset rows and columns and other matrix aspects
  mat->rows.clear();
  mat->columns.clear();
  mat->H0.clear();
  next_col_to_process = 0;
}

template<typename CoeffRing>
enum ComputationStatusCode LinAlgGB<CoeffRing>::computation_is_complete()
{
  // This handles everything but stop_.always, stop_.degree_limit
  if (stop_.basis_element_limit > 0 && gb.size() > stop_.basis_element_limit) 
    return COMP_DONE_GB_LIMIT;
  if (stop_.pair_limit > 0 && n_pairs_computed > stop_.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (stop_.just_min_gens && n_gens_left == 0)
    return COMP_DONE_MIN_GENS;
  if (stop_.subring_limit > 0 && n_subring > stop_.subring_limit)
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

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::start_computation()
{
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

      is_done = computation_is_complete();
      if (is_done != COMP_COMPUTING) break;

      this_degree = S.prepare_next_degree(-1, npairs);
      
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
      fflush(stderr);

      s_pair_step();
      complete_thru_this_degree = this_degree;
    }
  set_status(is_done);

  double clock_time = clock_sort_columns;
  clock_time /= CLOCKS_PER_SEC;
  fprintf(stderr, "total time for sorting columns: %f\n", clock_time);
  fflush(stderr);

  clock_time = clock_make_matrix;
  clock_time /= CLOCKS_PER_SEC;
  fprintf(stderr, "total time for making matrix (includes sort): %f\n", clock_time);
  fflush(stderr);

  clock_time = clock_gauss;
  clock_time /= CLOCKS_PER_SEC;
  fprintf(stderr, "total time for gauss: %f\n", clock_time);
  fflush(stderr);
}

template<typename CoeffRing>
LinAlgGB<CoeffRing>::~LinAlgGB()
{
#ifdef DEVELOPMENT
#warning "anything to delete?"
#endif
}

/*************************
 ** Top level interface **
 *************************/

template<typename CoeffRing>
ComputationOrNull *
LinAlgGB<CoeffRing>::set_hilbert_function(const RingElement *hf)
{
#if 0
  _hf_orig = hf;
  _hf_diff = RingElement::make_raw(hf->get_ring(), ZERO_RINGELEM);
  _use_hilb = true;
  _hilb_new_elems = true;

  return this;
#endif
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *LinAlgGB<CoeffRing>::get_gb()
{
  MatrixConstructor result(F,0);
  for (int i=0; i<gb.size(); i++)
    {
      vec v = M2Interface<CoeffRing>::to_M2_vec(coeffK,gb[i]->f, F);
      result.append(v);
    }
  return result.to_matrix();
#if 0
  minimalize_gb();
  return minimal_gb->get_gb();
#endif
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *LinAlgGB<CoeffRing>::get_mingens()
{
#if 0
  MatrixConstructor mat(_F,0);
  for (VECTOR(gbelem *)::iterator i = gb.begin(); 
       i != gb.end(); 
       i++)
    if ((*i)->minlevel == ELEM_POSSIBLE_MINGEN)
      mat.append(originalR->translate_gbvector_to_vec(_F, (*i)->g.f));
  return mat.to_matrix();
#endif
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *LinAlgGB<CoeffRing>::get_change()
{
#if 0
  minimalize_gb();
  return minimal_gb->get_change();
#endif
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *LinAlgGB<CoeffRing>::get_syzygies()
{
#if 0
  // The (non-minimal) syzygy matrix
  MatrixConstructor mat(_Fsyz, 0);
  for (VECTOR(gbvector *)::iterator i = _syz.begin(); 
       i != _syz.end(); 
       i++)
    {
      mat.append(originalR->translate_gbvector_to_vec(_Fsyz, *i));
    }
  return mat.to_matrix();
#endif
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *LinAlgGB<CoeffRing>::get_initial(int nparts)
{
#if 0
  minimalize_gb();
  return minimal_gb->get_initial(nparts);
#endif
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *LinAlgGB<CoeffRing>::matrix_remainder(const Matrix *m)
{
#if 0
  minimalize_gb();
  return minimal_gb->matrix_remainder(m);
#endif
  return 0;
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
#if 0
  minimalize_gb();
  minimal_gb->matrix_lift(m, result_remainder, result_quotient);
#endif
}

template<typename CoeffRing>
int LinAlgGB<CoeffRing>::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
#if 0
  minimalize_gb();
  return minimal_gb->contains(m);
#endif
  return 0;
}

template<typename CoeffRing>
int LinAlgGB<CoeffRing>::complete_thru_degree() const
  // The computation is complete up through this degree.
{
  return complete_thru_this_degree;
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     gbTrace value */
{
  o << "# pairs computed = " << n_pairs_computed << newline;
  if (gbTrace >= 5 && gbTrace % 2 == 1)
    for (unsigned int i=0; i<gb.size(); i++)
      {
	o << i << '\t';
	//	R->gbvector_text_out(o, F, gb[i].f);
	o << newline;
      }
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::show_gb_array(const gb_array &g)
{
  // Debugging routine
  // Display the array, and all of the internal information in it too.
  buffer o;
  for (int i=0; i<g.size(); i++)
    {
      vec v = M2Interface<CoeffRing>::to_M2_vec(coeffK,g[i]->f, F);
      o << "element " << i 
	<< " degree " << g[i]->deg 
	<< " alpha " << g[i]->alpha 
	<< newline << "    ";
      originalR->vec_text_out(o, v);
      o << newline;
    }
  emit(o.str());
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::show_row_info()
{
  // Debugging routine
  for (int i=0; i<mat->rows.size(); i++)
    {
      fprintf(stderr, "%4d ", mat->rows[i].elem);
      if (mat->rows[i].monom == 0)
	fprintf(stderr, "generator");
      else
	monomial_elem_text_out(stderr, mat->rows[i].monom);
      fprintf(stderr, "\n");
    }
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::show_column_info()
{
  // Debugging routine
  for (int i=0; i<mat->columns.size(); i++)
    {
      fprintf(stderr, "gbdivisor %4d ord %4d monomial ", 
	      mat->columns[i].gb_divisor,
	      mat->columns[i].ord);
      monomial_elem_text_out(stderr, mat->columns[i].monom);
      fprintf(stderr, "\n");
    }
}

template<typename CoeffRing>
void LinAlgGB<CoeffRing>::show_matrix()
{
  // Debugging routine
  MutableMatrix *q = M2Interface<CoeffRing>::to_M2_MutableMatrix(K,mat);
  buffer o;
  q->text_out(o);
  emit(o.str());
}
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
