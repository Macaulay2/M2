#include "linalgGB.hpp"
#include "../matrix.hpp"
#include "MonomialTable.h"
#include <map>

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
  if (K->cast_to_Z_mod() != 0)
    {
      CoefficientRingZZp *L = new CoefficientRingZZp(K->charac());
      G = new LinAlgGB<CoefficientRingZZp>(L,m,
					   collect_syz,
					   n_rows_to_keep,
					   gb_weights,
					   use_max_degree,
					   max_degree,
					   strategy);
    }
  return G;
}

template<typename CoefficientRing>
LinAlgGB<CoefficientRing>::LinAlgGB(CoefficientRing *K,
	   const Matrix *m, 
	   M2_bool collect_syz, 
	   int n_rows_to_keep,
	   M2_arrayint gb_weights,
	   int strategy, 
	   M2_bool use_max_degree,
	   int max_degree)
  : S(&H)
{
  originalR = m->get_ring()->cast_to_PolynomialRing();
  F = m->rows();
  coeffK = K;

  n_subring = 0;
  n_pairs_computed = 0;
  n_gens_left = m->n_cols();

  lookup = new MonomialLookupTable;

  this_degree = -1;
  next_col_to_process = 0;
  weights = gb_weights;

  // Set the gens array
  M2Interface<CoefficientRing>::from_M2_matrix(K, m, &H, weights, gens);

  for (int i=0; i<gens.size(); i++)
    {
      gbelem *g = gens[i];
      S.insert(SPairSet::make_spair_gen(g->deg, g->f.monoms[0], i));
    }

  set_status(COMP_NOT_STARTED);
}

#include "linalgGB.hpp"
#include <vector>
#include "../text_io.hpp"
#include "monoms.h"
#include "MonomialOps.h"
#include "../matrix.hpp"
#include "../mutablemat.hpp"
#include "../matrixcon.hpp"

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

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::allocate_poly(poly &result, size_t len)
{
  result.len = len;
  result.coeffs = newarray(COEFF_TYPE, len);
  result.monoms = newarray(monomial, len);
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::allocate_sparse_row(row_elem &result, size_t len)
{
  result.len = len;
  result.coeffs = newarray(COEFF_TYPE, len);
  result.comps = newarray(int, len);
}

template<typename CoefficientRing>
int LinAlgGB<CoefficientRing>::find_or_append_column(monomial m)
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
  c.ord = 0;
  mat->columns.push_back(c);
  return next_column;
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::load_gen(int which)
{
  row_elem r;
  r.monom = NULL;
  r.elem = which;

  poly &g = gens[which]->f;
  allocate_sparse_row(r, g.len);

  for (int i=0; i<g.len; i++)
    coeffK->init_set(r.coeffs+i, g.coeffs+i);
  for (int i=0; i<g.len; i++)
    r.comps[i] = find_or_append_column(g.monoms[i]);

  mat->rows.push_back(r);
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::load_row(monomial monom, int which)
{
  row_elem r;
  r.monom = monom;
  r.elem = which;

  poly &g = gens[which]->f;
  allocate_sparse_row(r, g.len);


  for (int i=0; i<g.len; i++)
    coeffK->init_set(r.coeffs+i, g.coeffs+i);

  for (int i=0; i<g.len; i++)
    {
      monomial n = MonomialOps::mult(&H, monom, g.monoms[i]);
      r.comps[i] = find_or_append_column(n);
    }

  mat->rows.push_back(r);
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::process_column(int c)
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
      int which = reinterpret_cast<int>(b->bag);
      uninterned_monomial um = H.reserve(MONOMIAL_LENGTH(ce.monom));
      monomial m;
      monomial_quotient(ce.monom, gb[which]->f.monoms[0], um);
      H.find_or_insert(um, m);
      ce.gb_divisor = mat->rows.size();
      load_row(m,which);
    }
  else 
    ce.gb_divisor = -1;
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::process_s_pair(SPairSet::spair *p)
{
  int c;

  switch (p->type) {
  case SPairSet::SPAIR_SPAIR:
    load_row(p->s.spair.first_monom, p->s.spair.first_gb_num);
    c = mat->rows[mat->rows.size()-1].comps[0];
    mat->columns[c].gb_divisor = mat->rows.size()-1;
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
template <typename CoefficientRing>
struct monomial_sorter : public binary_function<int,int,bool> {
  typedef std::vector<column_elem, 
                               gc_allocator<column_elem> >
            col_array ;

  col_array columns;
  monomial_sorter(col_array &columns0)
    : columns(columns0) {}

  bool operator()(int a, int b)
    {
      ncomparisons++;
      /* return the boolean value a < b */
      column_elem &m = columns[a];
      column_elem &n = columns[b];
      // First compare degrees, then weights, then do revlex lt routine
      int cmp = m.ord - n.ord;
      if (cmp > 0) return false;
      if (cmp < 0) return true;
      // Now compare via revlex
      cmp = monomial_compare(m.monom, n.monom); // This is lex compare
      if (cmp == GT) return true;
      return false;
    }
};

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::set_comparisons()
{
  /* Sort the monomials */
  /* set the comparison values in the current matrix */
  /* Should we also go thru the matrix and set the values? */

  std::vector<int, gc_allocator<int> > a;
  a.reserve(mat->columns.size());
  for (int i=0; i<mat->columns.size(); i++)
    {
      // Set the degree, weight
      mat->columns[i].ord = monomial_weight(mat->columns[i].monom, weights);
      a.push_back(i);
    }

  ncomparisons = 0;
  sort(a.begin(), a.end(), monomial_sorter<CoefficientRing>(mat->columns));
  fprintf(stderr, "ncomparisons = %d\n", ncomparisons);

  for (int i=0; i<a.size(); i++)
    {
      mat->columns[a[i]].ord = i;
    }
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::make_matrix()
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

  fprintf(stderr, "--matrix--%d by %d\n", mat->rows.size(), mat->columns.size());
  show_row_info();
  show_column_info();
  //  show_matrix();
  H.dump();
  /* Now sort the monomials */
  set_comparisons();

  // change all of the components?  This depends on our LU algorithm
  show_column_info();
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::make_dense_row(int r, )
{
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::LU_decompose()
{
#if 0
  int ncols = mat->columns.size();
  int nrows = mat->rows.size();
  COEFF_TYPE *our_row = newarray(COEFF_TYPE, ncols);
  for (int r=0; r<nrows; r++)
    {
      row_elem &re = mat->rows[r];
      if (re.is_pivot) continue;
      // We now reduce the r th row
      // FILLIN:
        for (int c=0; c<ncols; c++)
	  coeffK->set_zero(our_row[c]);
	for (int i=0; i<re.len; i++)
	  {
	    int thisc = re.comps[i].ord;
	    coeffK->init_set(our_row[thisc], re.coeffs[i]);
	  }
      // 
      int nterm = 0;
      int lead_col = -1;
      for (int c=0; c<ncols; c++)
	if (!coeffK->is_zero(our_row[c]))
	  {
	    int rpivot = mat->columns[c].gb_divisor;
	    if (rpivot >= 0)
	      {
		//subtract_row_multiple(our_row, rpivot);
		row_ele &rpivot = mat->rows[rpivot];
		for (int j=0; j<rpivot.len; j++)
		  {
		    int cj = mat->columns[rpivot.comps[j]].ord;
		    coeffK->subtract_multiple(our_row[cj], pivot, 
		  }
	      }
	    if (!coeffK->is_zero(our_row[c]))
	      {
		nterms++;
		if (lead_col < 0)
		  lead_col = c;
	      }
	  }
      // Now copy the row back to row r, making it monic at the same time
      // REALLOCATE ROW
      COEFF_TYPE *a = our_row+lead_col;
      int next = 0;
      for (int c=lead_col; c<ncols; c++)
	if (!coeffK->is_zero(our_row[c]))
	  {
	    coeffK->divide(newr.coeffs+next, our_row+c, a);
	    newr.comps[next] = c; // WARNING: either change all rows, or keep
	    // track of which rows are represented using sorted column order
	    next++;
	  }
    }
#endif
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::sparse_row_to_poly(row_elem &r, poly &g)
{
  allocate_poly(g, r.len);
  for (int i=0; i<r.len; i++)
    {
      coeffK->init_set(g.coeffs+i, r.coeffs+i);
      g.monoms[i] = mat->columns[r.comps[i]].monom;
    }
}

template<typename CoefficientRing>
mygbelem<typename CoefficientRing::elem> *
LinAlgGB<CoefficientRing>::make_gbelem(poly &g)
{
  gbelem *result = new gbelem;
  swap(result->f,g);
  M2Interface<CoefficientRing>::poly_set_degrees(coeffK,weights,result->f,result->deg,result->alpha);
  result->is_minimal = 1;
  result->minlevel = ELEM_MIN_GB;
  return result;
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::insert_gb_element(poly &g)
{
  // Grabs g.  Needs to set degree, alpha degree.
  gbelem *result = make_gbelem(g);
  void *v = reinterpret_cast<void *>(gb.size());
  tagged_monomial *b = new tagged_monomial(result->f.monoms[0], v);
  lookup->insert(b);
  gb.push_back(result);
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::new_GB_elements()
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
	  S.find_new_pairs(gb, false);
	}
    }
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::s_pair_step()
{
  make_matrix();
  LU_decompose();
  new_GB_elements();
  // reset rows and columns and other matrix aspects
  mat->rows.clear();
  mat->columns.clear();
  mat->H0.clear();
  next_col_to_process = 0;
}

template<typename CoefficientRing>
enum ComputationStatusCode LinAlgGB<CoefficientRing>::computation_is_complete()
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
#warning "compute the codimension"
      int c = 0; // replace this line
      //int c = codim_of_lead_terms();
      if (c >= stop_.codim_limit)
	return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::start_computation()
{
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

      s_pair_step();
    }
  set_status(is_done);
}

template<typename CoefficientRing>
LinAlgGB<CoefficientRing>::~LinAlgGB()
{
#warning "anything to delete?"
}

/*************************
 ** Top level interface **
 *************************/

template<typename CoefficientRing>
ComputationOrNull *LinAlgGB<CoefficientRing>::set_hilbert_function(const RingElement *hf)
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

template<typename CoefficientRing>
const MatrixOrNull *LinAlgGB<CoefficientRing>::get_gb()
{
  MatrixConstructor result(F,0);
  for (int i=0; i<gb.size(); i++)
    {
      vec v = M2Interface<CoefficientRing>::to_M2_vec(coeffK,gb[i]->f, F);
      result.append(v);
    }
  return result.to_matrix();
#if 0
  minimalize_gb();
  return minimal_gb->get_gb();
#endif
  return 0;
}

template<typename CoefficientRing>
const MatrixOrNull *LinAlgGB<CoefficientRing>::get_mingens()
{
#if 0
  MatrixConstructor mat(_F,0);
  for (vector<gbelem *, gc_allocator<gbelem *> >::iterator i = gb.begin(); i != gb.end(); i++)
    if ((*i)->minlevel == ELEM_POSSIBLE_MINGEN)
      mat.append(originalR->translate_gbvector_to_vec(_F, (*i)->g.f));
  return mat.to_matrix();
#endif
  return 0;
}

template<typename CoefficientRing>
const MatrixOrNull *LinAlgGB<CoefficientRing>::get_change()
{
#if 0
  minimalize_gb();
  return minimal_gb->get_change();
#endif
  return 0;
}

template<typename CoefficientRing>
const MatrixOrNull *LinAlgGB<CoefficientRing>::get_syzygies()
{
#if 0
  // The (non-minimal) syzygy matrix
  MatrixConstructor mat(_Fsyz, 0);
  for (vector<gbvector *, gc_allocator<gbvector *> >::iterator i = _syz.begin(); i != _syz.end(); i++)
    {
      mat.append(originalR->translate_gbvector_to_vec(_Fsyz, *i));
    }
  return mat.to_matrix();
#endif
  return 0;
}

template<typename CoefficientRing>
const MatrixOrNull *LinAlgGB<CoefficientRing>::get_initial(int nparts)
{
#if 0
  minimalize_gb();
  return minimal_gb->get_initial(nparts);
#endif
  return 0;
}

template<typename CoefficientRing>
const MatrixOrNull *LinAlgGB<CoefficientRing>::matrix_remainder(const Matrix *m)
{
#if 0
  minimalize_gb();
  return minimal_gb->matrix_remainder(m);
#endif
  return 0;
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
#if 0
  minimalize_gb();
  minimal_gb->matrix_lift(m, result_remainder, result_quotient);
#endif
}

template<typename CoefficientRing>
int LinAlgGB<CoefficientRing>::contains(const Matrix *m)
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

template<typename CoefficientRing>
int LinAlgGB<CoefficientRing>::complete_thru_degree() const
  // The computation is complete up through this degree.
{
#if 0
  return _complete_thru_this_degree;
#endif
  return 0;
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::text_out(buffer &o)
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

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::show_gb_array(const gb_array &g)
{
  // Debugging routine
  // Display the array, and all of the internal information in it too.
  buffer o;
  for (int i=0; i<g.size(); i++)
    {
      vec v = M2Interface<CoefficientRing>::to_M2_vec(coeffK,g[i]->f, F);
      o << "element " << i 
	<< " degree " << g[i]->deg 
	<< " alpha " << g[i]->alpha 
	<< newline << "    ";
      originalR->vec_text_out(o, v);
      o << newline;
    }
  emit(o.str());
}

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::show_row_info()
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

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::show_column_info()
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

template<typename CoefficientRing>
void LinAlgGB<CoefficientRing>::show_matrix()
{
  // Debugging routine
  MutableMatrix *q = M2Interface<CoefficientRing>::to_M2_MutableMatrix(originalR->getCoefficients(),mat);
  buffer o;
  q->text_out(o);
  emit(o.str());
}
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
