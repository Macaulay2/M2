#include "linalgGB.hpp"
#include <vector>
#include "../text_io.hpp"
#include "monoms.h"
#include "../matrix.hpp"
#include "../mutablemat.hpp"
#include "../matrixcon.hpp"

// Problems still to consider
// (a) finish implementation
// (b) set_comparisons
// (c) choosing an alpha value not too high.
//     Also, can we do Mora algorithm this way?
// (d) field arithmetic in the matrix and polys.  Use templates?
// (e) After this is all working, then play with the LU decomposition
//
// To put in later:
//    module orders, Schreyer orders, syzygies, hilbert functions, ZZ,
//    quotients.

void LinearAlgebraGB::allocate_poly(poly &result, size_t len)
{
  result.len = len;
  result.coeffs = newarray(COEFF_TYPE, len);
  result.monoms = newarray(monomial, len);
}

void LinearAlgebraGB::allocate_sparse_row(sparse_row &result, size_t len)
{
  result.len = len;
  result.coeffs = newarray(COEFF_TYPE, len);
  result.comps = newarray(int, len);
}

void LinearAlgebraGB::append_row(monomial m, int elem)
{
  /* Make a new row.  Should we also do
     process_row (and therefore process_vector)  here?

  */
  row_elem r;
  r.monom = m;
  r.elem = elem;
  rows.push_back(r);
}

void LinearAlgebraGB::append_column(monomial m)
{
  column_elem c;
  c.comp = 0;
  c.monom = m;
  c.gb_divisor = -2;
  c.ord = 0;
  columns.push_back(c);
}

int LinearAlgebraGB::column(monomial m)
{
  int next_column = columns.size();
  std::pair<const monomial, int> p(m, next_column);
  monomial_map::iterator i = H0.find(m);
  if (i != H0.end())
    return (*i).second;
  H0.insert(p);
  append_column(m);
  return next_column;
}

int LinearAlgebraGB::mult_monomials(monomial m, monomial n)
{
  // Multiply the two monomials m and n, using the monomial set H.
  // Then, look in thisH to see if it exists already.
  // If so: return the corresponding key (which is the column number)
  // If not: insert it into thisH, do an append_column
  // and return that column number.
  
  uninterned_monomial mn;
  monomial result;
  mn = H.reserve(MONOMIAL_LENGTH(m) + MONOMIAL_LENGTH(n));
  monomial_mult(m,n,mn);
  H.find_or_insert(mn,result);
  return column(result);
}

void LinearAlgebraGB::process_row(int r)
{
  /* Do the multiplication, and process the 
     resulting vector.

     ALSO: this is where we improve row[r] = (m,gbelem)...
     search for any (t,newelem)'s for which t divides m have been done in a prev matrix.
       choose the largest t.  now process (m/t, newelem):
         special multiplication routine?  since newelem uses diff monomial encoding.
 */

  // There are 2 cases: the monomial of row r is NULL.
  // This means that we use a generator instead of a GB element
  row_elem &re = rows[r];
  sparse_row &mg = re.row;
  monomial m = re.monom;
  if (m == NULL)
    {
      poly &g = gens[re.elem]->f;
      allocate_sparse_row(mg, g.len);
      for (int i=0; i<g.len; i++)
	coeffK->init_set(mg.coeffs+i, g.coeffs+i);

      for (int i=0; i<g.len; i++)
	{
	  mg.comps[i] = column(g.monoms[i]);
	  // the above routine creates a new column if it is not there yet
	}
    }
  else
    {
      poly &g = gb[re.elem]->f;
      
      // step 1: find size of the polynomial, allocate a new polynomial
      allocate_sparse_row(mg, g.len);
      
      // step 2: do the multiplication.  Note that this does not need
      // multiplication in the base field/ring.
      // This will also insert monomials into the column hash table,
      // and also append columns to 'columns'.
      // Weyl: this will be more complicated
      // Skew: just need to modify signs, don't put elements in that
      //   have squares.  mg.len will decrease.
      
      // It would be interesting to see if using iterators changes speed at all
      for (int i=0; i<g.len; i++)
	coeffK->init_set(mg.coeffs+i, g.coeffs+i);
      for (int i=0; i<g.len; i++)
	{
	  mg.comps[i] = mult_monomials(m, g.monoms[i]);
	  // the above routine creates a new column if it is not there yet
	}
    }
}

void LinearAlgebraGB::process_column(int c)
{
  /* If this column has been handled before, return.
     Otherwise, find a GB element whose lead term
     divides this monomial, and either mark this colum
     as not an initial element, OR append a row
  */

  column_elem &ce = columns[c];
  if (ce.gb_divisor >= -1)
    return;
  tagged_monomial *b;
  bool found = lookup->search(ce.monom, b);
  if (found)
    {
      ce.gb_divisor = reinterpret_cast<int>(b->bag);
      uninterned_monomial um = H.reserve(MONOMIAL_LENGTH(ce.monom));
      monomial m;
      monomial_quotient(ce.monom, gb[ce.gb_divisor]->f.monoms[0], um);
      H.find_or_insert(um, m);
      append_row(m,ce.gb_divisor);
    }
  else 
    ce.gb_divisor = -1;
}

void LinearAlgebraGB::process_s_pair(SPairSet::spair *p)
{
  switch (p->type) {
  case SPairSet::SPAIR_SPAIR:
    append_row(p->s.spair.first_monom, p->s.spair.first_gb_num);
    append_row(p->s.spair.second_monom, p->s.spair.second_gb_num);
    break;
  case SPairSet::SPAIR_GEN:
    append_row(NULL, p->s.poly.column);
    break;
  default:
    break;
  }
    
  /*
    3 cases:
    (a) an spair or ringpair
       find the lcm.
       do append_column for this monomial
       do 2 append_row's
    (b) a skew pair
       find the lcm
       do process_vector (?)
    (c) a generator
       either append_row, or process_vector
  */
     
}

void LinearAlgebraGB::set_comparisons()
{
  /* Sort the monomials */
  /* set the comparison values in the current matrix */
  /* Should we also go thru the matrix and set the values? */
}

void LinearAlgebraGB::make_matrix()
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */

  SPairSet::spair *p;
  while (p = S.get_next_pair())
    {
      process_s_pair(p);
      //remove_s_pair(p);
    }

  for (;;)
    {
      if (next_row_to_process < rows.size())
	process_row(next_row_to_process++);
      else if (next_col_to_process < columns.size())
	process_column(next_col_to_process++);
      else break;
    }

  fprintf(stderr, "--matrix--%d by %d\n", rows.size(), columns.size());
  show_row_info();
  show_column_info();
  //  show_matrix();
  H.dump();
  /* Now sort the monomials */
  set_comparisons();
}

void LinearAlgebraGB::LU_decompose()
{
}

void LinearAlgebraGB::sparse_row_to_poly(sparse_row &r, poly &g)
{
  allocate_poly(g, r.len);
  for (int i=0; i<r.len; i++)
    {
      coeffK->init_set(g.coeffs+i, r.coeffs+i);
      g.monoms[i] = columns[r.comps[i]].monom;
    }
}

gbelem *LinearAlgebraGB::make_gbelem(poly &g)
{
  gbelem *result = new gbelem;
  swap(result->f,g);
  poly_set_degrees(weights,result->f,result->deg,result->alpha);
  result->is_minimal = 1;
  result->minlevel = ELEM_MIN_GB;
  return result;
}

void LinearAlgebraGB::insert_gb_element(poly &g)
{
  // Grabs g.  Needs to set degree, alpha degree.
  gbelem *result = make_gbelem(g);
  void *v = reinterpret_cast<void *>(gb.size());
  tagged_monomial *b = new tagged_monomial(result->f.monoms[0], v);
  lookup->insert(b);
  gb.push_back(result);
}

void LinearAlgebraGB::new_GB_elements()
{
  /* After LU decomposition, loop through each
     row of the matrix.  If the corresponding 
     lead term is not in the initial ideal (or, at least,
     wasn't) then insert GB element (and so update spairs, etc,
     but don't do auto_reduce...)

     If instead the lead term is not new, then keep track of this
     information somehow: place ... into a monheap...
  */
  for (int r=0; r<rows.size(); r++)
    {
      if (rows[r].row.len == 0) continue;
      int c = rows[r].row.comps[0];
      if (columns[c].gb_divisor < 0)
	{
	  poly g;
	  sparse_row_to_poly(rows[r].row, g); // This grabs the row poly
	                                      // leaving a zero poly
	  insert_gb_element(g);
	  S.find_new_pairs(gb, false);
	}
    }
}

void LinearAlgebraGB::s_pair_step()
{
  make_matrix();
  LU_decompose();
  new_GB_elements();
  // reset rows and columns and other matrix aspects
  rows.clear();
  columns.clear();
  H0.clear();
  next_col_to_process = 0;
  next_row_to_process = 0;
}

enum ComputationStatusCode LinearAlgebraGB::computation_is_complete()
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

void LinearAlgebraGB::start_computation()
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

LinearAlgebraGB::LinearAlgebraGB(const Matrix *m,
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
  coeffK = new CoefficientRing(originalR->getCoefficients());

  n_subring = 0;
  n_pairs_computed = 0;
  n_gens_left = m->n_cols();

  lookup = new MonomialLookupTable;

  this_degree = -1;
  next_col_to_process = 0;
  next_row_to_process = 0;
  weights = gb_weights;

  // Set the gens array
  from_M2_matrix(m, &H, weights, gens);

  for (int i=0; i<gens.size(); i++)
    {
      gbelem *g = gens[i];
      S.insert(SPairSet::make_spair_gen(g->deg, g->f.monoms[0], i));
    }

  show_gb_array(gens);
  set_status(COMP_NOT_STARTED);
}

LinearAlgebraGB::~LinearAlgebraGB()
{
#warning "anything to delete?"
}

LinearAlgebraGB * LinearAlgebraGB::create(const Matrix *m, 
				  M2_bool collect_syz, 
				  int n_rows_to_keep,
				  M2_arrayint gb_weights,
				  int strategy, 
				  M2_bool use_max_degree,
				  int max_degree)
{
  buffer o;
  o << "entering LinearAlgebraGB::create" << newline;
  emit(o.str());
  LinearAlgebraGB *result = new LinearAlgebraGB(m,
						collect_syz,
						n_rows_to_keep,
						gb_weights,
						strategy,
						use_max_degree,
						max_degree);
  return result;
}

/*************************
 ** Top level interface **
 *************************/

ComputationOrNull *LinearAlgebraGB::set_hilbert_function(const RingElement *hf)
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

const MatrixOrNull *LinearAlgebraGB::get_gb()
{
  MatrixConstructor mat(F,0);
  for (int i=0; i<gb.size(); i++)
    {
      vec v = to_M2_vec(gb[i]->f, F);
      mat.append(v);
    }
  return mat.to_matrix();
#if 0
  minimalize_gb();
  return minimal_gb->get_gb();
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::get_mingens()
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

const MatrixOrNull *LinearAlgebraGB::get_change()
{
#if 0
  minimalize_gb();
  return minimal_gb->get_change();
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::get_syzygies()
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

const MatrixOrNull *LinearAlgebraGB::get_initial(int nparts)
{
#if 0
  minimalize_gb();
  return minimal_gb->get_initial(nparts);
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::matrix_remainder(const Matrix *m)
{
#if 0
  minimalize_gb();
  return minimal_gb->matrix_remainder(m);
#endif
  return 0;
}

void LinearAlgebraGB::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
#if 0
  minimalize_gb();
  minimal_gb->matrix_lift(m, result_remainder, result_quotient);
#endif
}

int LinearAlgebraGB::contains(const Matrix *m)
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

int LinearAlgebraGB::complete_thru_degree() const
  // The computation is complete up through this degree.
{
#if 0
  return _complete_thru_this_degree;
#endif
  return 0;
}

void LinearAlgebraGB::text_out(buffer &o)
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

void LinearAlgebraGB::show_gb_array(const gb_array &g)
{
  // Debugging routine
  // Display the array, and all of the internal information in it too.
  buffer o;
  for (int i=0; i<g.size(); i++)
    {
      vec v = to_M2_vec(g[i]->f, F);
      o << "element " << i 
	<< " degree " << g[i]->deg 
	<< " alpha " << g[i]->alpha 
	<< newline << "    ";
      originalR->vec_text_out(o, v);
      o << newline;
    }
  emit(o.str());
}

void LinearAlgebraGB::show_row_info()
{
  // Debugging routine
  for (int i=0; i<rows.size(); i++)
    {
      fprintf(stderr, "%4d ", rows[i].elem);
      if (rows[i].monom == 0)
	fprintf(stderr, "generator");
      else
	monomial_elem_text_out(stderr, rows[i].monom);
      fprintf(stderr, "\n");
    }
}

void LinearAlgebraGB::show_column_info()
{
  // Debugging routine
  for (int i=0; i<columns.size(); i++)
    {
      fprintf(stderr, "comp %4d gbdivisor %4d ord %4d monomial ", 
	      columns[i].comp,
	      columns[i].gb_divisor,
	      columns[i].ord);
      monomial_elem_text_out(stderr, columns[i].monom);
      fprintf(stderr, "\n");
    }
}

void LinearAlgebraGB::show_matrix()
{
  // Debugging routine
  MutableMatrix *q = to_M2_MutableMatrix(originalR->getCoefficients(),rows,columns);
  buffer o;
  q->text_out(o);
  emit(o.str());
}
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
