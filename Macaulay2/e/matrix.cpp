// Copyright 1995 Michael E. Stillman

#if 0
To do here:
  +k-basis routines
  +monideal routines (done)
  elem(r,c) makes new element, so be sure to remove it if needed

  check error conditions
  lead term routines are a bit messed up

  new routines to add: 
    exterior, symm (how best to do this?)
    random_matrix
    coeffs, 
    inpart, stdpart, mininimalpart (but call them what?? )
#endif

#include "style.hpp"
#include "text_io.hpp"
#include "ring.hpp"
#include "matrix.hpp"
#include "comb.hpp"
#include "det.hpp"
#include "pfaff.hpp"
#include "polyring.hpp"
#include "termideal.hpp"
#include "assprime.hpp"
#include "monideal.hpp"

#include "vector.hpp"
#include "exptable.h"

void Matrix::initialize(const FreeModule *r, 
		     const FreeModule *c,
		     const int *deg)
{
  _rows = (FreeModule *)r;
  _cols = (FreeModule *)c;
  _degree_shift = (int *) deg;

  vec Zero = NULL;
  for (int i=0; i<c->rank(); i++) 
    _entries.append(Zero); 
}

Matrix::Matrix(const FreeModule *r, 
	       const FreeModule *c,
	       const int *deg)
{ 
  initialize(r,
	     c,
	     r->get_ring()->degree_monoid()->make_new(deg));
}

Matrix::Matrix(const FreeModule *r, 
	       const FreeModule *c)
{ 
  initialize(r,
	     c,
	     r->get_ring()->degree_monoid()->make_one());
}

Matrix::Matrix(const FreeModule *r)
{ 
  initialize(r,
	     r->new_free(),
	     r->get_ring()->degree_monoid()->make_one());
}

Matrix::Matrix(const MonomialIdeal *mi) 
{ 
  const FreeModule *r = mi->get_ring()->make_FreeModule(1);
  int *one = r->get_ring()->degree_monoid()->make_one();
  initialize(r, r->new_free(), one);
  append_monideal(mi,0);
}

const MatrixOrNull * Matrix::make(const FreeModule *target,
				  const Vector_array *V)
{
  // Checks to make:
  // each vector in V is over same ring.
  
  const Ring *R = target->get_ring();
  for (unsigned int i=0; i<V->len; i++)
    {
      if (R != V->array[i]->get_ring())
	{
	  ERROR("expected vectors in the same ring");
	  return 0;
	}
      if (target->rank() != V->array[i]->free_of()->rank())
	{
	  ERROR("expected vectors of same length");
	  return 0;
	}
    }
  Matrix *result = new Matrix(target);
  for (unsigned int i=0; i<V->len; i++)
    result->append(target->copy(V->array[i]->get_value()));
  return result;
}

const MatrixOrNull * Matrix::make(const FreeModule *target,
				  const FreeModule *source,
				  const M2_arrayint deg,
				  const Vector_array *V)
{
  // Check that: target, source, V all have the same ring.
  // Check that deg has the right length for a degree in this ring

  const Ring *R = target->get_ring();
  if (source->get_ring() != R)
    {
      ERROR("expected free modules over the same ring");
      return 0;
    }
  if (R->degree_monoid()->n_vars() != (int)deg->len)
    {
      ERROR("expected degree of matrix to have %d entries", 
	    R->degree_monoid()->n_vars());
      return 0;
    }
  for (unsigned int i=0; i<V->len; i++)
    {
      if (R != V->array[i]->get_ring())
	{
	  ERROR("expected vectors in the same ring");
	  return 0;
	}
      if (target->rank() != V->array[i]->free_of()->rank())
	{
	  ERROR("expected vectors of same length");
	  return 0;
	}
    }
  Matrix *result = new Matrix(target,source,deg->array);
  for (unsigned int i=0; i<V->len; i++)
    (*result)[i] = target->copy(V->array[i]->get_value());
  return result;
}

const MatrixOrNull * Matrix::make(const FreeModule *target,
				  const FreeModule *source,
				  const M2_arrayint deg,
				  const Matrix *M)
{
  // Check that the sizes of target, source match that of M.
  // Check that: target, source, M all have the same ring.
  // Check that deg has the right length for a degree in this ring

  const Ring *R = target->get_ring();
  if (source->get_ring() != R || M->get_ring() != R)
    {
      ERROR("expected free modules over the same ring");
      return 0;
    }
  if (target->rank() != M->rows()->rank()
      || source->rank() != M->cols()->rank())
    {
      ERROR("expected free modules to have the same rank");
      return 0;
    }
  if (R->degree_monoid()->n_vars() != (int)deg->len)
    {
      ERROR("expected degree of matrix to have %d entries", 
	    R->degree_monoid()->n_vars());
      return 0;
    }
  Matrix *result = new Matrix(target,source,deg->array);
  for (int i=0; i<source->rank(); i++)
    (*result)[i] = target->copy((*M)[i]);
  return result;
}

const Matrix * Matrix::make(const MonomialIdeal * mi)
{
  return new Matrix(mi);
}

bool Matrix::is_equal(const Matrix &m) const
{
  if (this == &m) return true;
  if (get_ring() != m.get_ring())
    return false;
  if (n_rows() != m.n_rows())
    return false;
  if (n_cols() != m.n_cols())
    return false;
  for (int i=0; i<n_cols(); i++)
    if (! rows()->is_equal(elem(i), m.elem(i))) 
      return false;
  return true;
}

bool Matrix::is_zero() const
{
  for (int i=0; i<n_cols(); i++)
    if (! rows()->is_zero(elem(i))) return false;
  return true;
}

int Matrix::is_homogeneous() const
{
  int *d = degree_monoid()->make_one();
  for (int i=0; i<n_cols(); i++)
    {
      if (rows()->is_zero(elem(i))) continue;
      if (! rows()->is_homogeneous(elem(i)))
	{
	  degree_monoid()->remove(d);
	  return 0;
	}
 
      rows()->degree(elem(i), d);
      degree_monoid()->divide(d, degree_shift(), d);
      if (0 != degree_monoid()->compare(d, cols()->degree(i)))
	{
	  degree_monoid()->remove(d);
	  return 0;
	}
    }
  degree_monoid()->remove(d);
  return 1;
}

Matrix *Matrix::homogenize(int v, const M2_arrayint wts) const
{
  Matrix *result = new Matrix(rows());
  for (int i=0; i<n_cols(); i++)
    result->append(rows()->homogenize(elem(i), v, wts));
  return result;
}

#if 0
void Matrix::schreyer_append(vec v)
{
  if (! rows()->is_zero(v)) 
    {
      int *d = degree_monoid()->make_one();
      rows()->degree(v, d);
      cols()->append(d, v->monom, cols()->rank());
      degree_monoid()->remove(d);
      _entries.append(v);
    }
  else
    append(v);
}
#endif

Matrix *Matrix::zero(const FreeModule *F, const FreeModule *G)
{
  if (F->get_ring() != G->get_ring())
    {
      ERROR("free modules have different base rings");
      return 0;
    }
  Matrix *result = new Matrix(F,G);
  for (int i=0; i<G->rank(); i++)
    (*result)[i] = F->zero();
  return result;
}

Matrix *Matrix::identity(const FreeModule *F)
{
  Matrix *result = new Matrix(F,F);
  for (int i=0; i<F->rank(); i++)
    (*result)[i] = F->e_sub_i(i);
  return result;
}

Matrix *Matrix::operator+(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("matrices have different base rings");
      return 0;
    }
  if (rows()->rank() != m.rows()->rank()
      || cols()->rank() != m.cols()->rank())
    {
      ERROR("matrices have different shapes");
      return 0;
    }
  
  Matrix *result;
  const Ring *R = get_ring();
  const FreeModule *F = rows();
  const FreeModule *G = cols();
  
  if (!rows()->is_equal(m.rows()))
    F = R->make_FreeModule(n_rows());
  
  if (!cols()->is_equal(m.cols()))
    G = R->make_FreeModule(n_cols());
  
  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    result = new Matrix(F,G,degree_shift());
  else
    result = new Matrix(F,G);
  
  for (int i=0; i<n_cols(); i++)
    (*result)[i] = F->add(elem(i),m[i]);
  return result;
}

Matrix *Matrix::operator-(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("matrices have different base rings");
      return 0;
    }
  if (rows()->rank() != m.rows()->rank()
      || cols()->rank() != m.cols()->rank())
    {
      ERROR("matrices have different shapes");
      return 0;
    }

  Matrix *result;
  const Ring *R = get_ring();
  const FreeModule *F = rows();
  const FreeModule *G = cols();
  
  if (!rows()->is_equal(m.rows()))
    F = R->make_FreeModule(n_rows());
  
  if (!cols()->is_equal(m.cols()))
    G = R->make_FreeModule(n_cols());
  
  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    result = new Matrix(F,G,degree_shift());
  else
    result = new Matrix(F,G);
  
  for (int i=0; i<n_cols(); i++)
    (*result)[i] = F->subtract(elem(i),m[i]);
  return result;
}

Matrix *Matrix::operator-() const
{
  Matrix *result = new Matrix(rows(), cols(), degree_shift());
  for (int i=0; i<n_cols(); i++)
    (*result)[i] = rows()->negate(elem(i));
  return result;
}

MatrixOrNull *Matrix::sub_matrix(const M2_arrayint r, const M2_arrayint c) const
{
  const FreeModule *F = rows()->sub_space(r);
  const FreeModule *G = cols()->sub_space(c);
  if (F == NULL || G == NULL)
    return 0;

  Matrix *result = new Matrix(F, G, degree_shift());
  for (unsigned int i=0; i<c->len; i++)
    (*result)[i] = F->sub_vector(rows(), elem(c->array[i]), r);
  return result;
}

MatrixOrNull *Matrix::sub_matrix(const M2_arrayint c) const
{
  const FreeModule *G = cols()->sub_space(c);
  if (G == NULL)
    return 0;

  Matrix *result = new Matrix(rows(), G, degree_shift());
  for (unsigned int i=0; i<c->len; i++)
    (*result)[i] = rows()->copy(elem(c->array[i]));
  return result;

}

MatrixOrNull *Matrix::reshape(const FreeModule *F, const FreeModule *G) const
  // Reshape 'this' : F <--- G, where 
  // (rank F)(rank G) = (nrows this)(ncols this)
{
  if (F->get_ring() != get_ring() || G->get_ring() != get_ring())
    {
      ERROR("reshape: expected same ring");
      return 0;
    }
  if (n_rows() * n_cols() != F->rank() * G->rank())
    {
      ERROR("reshape: ranks of freemodules incorrect");
      return 0;
    }

  Matrix *result = new Matrix(F,G);
  F->reshape(*this, *result);
  return result;
}

MatrixOrNull *Matrix::flip(const FreeModule *F, const FreeModule *G)
{
  if (F->get_ring() != G->get_ring())
    {
      ERROR("flip: expected same ring");
      return 0;
    }
  const FreeModule *H = F->tensor(G);
  const FreeModule *K = G->tensor(F);
  Matrix *result = new Matrix(K, H);
  int next = 0;
  for (int f=0; f<F->rank(); f++)
    for (int g=0; g<G->rank(); g++)
      (*result)[next++] = H->e_sub_i(f + g * F->rank());
  return result;
}

Matrix *Matrix::transpose() const
{
  const FreeModule *F = cols()->transpose();
  const FreeModule *G = rows()->transpose();
  int *deg = degree_monoid()->make_one();
  degree_monoid()->divide(deg, degree_shift(), deg);
  Matrix *result = new Matrix(F, G, deg);
  degree_monoid()->remove(deg);
  F->transpose_matrix(*this, *result);
  return result;
}

Matrix *Matrix::operator*(const ring_elem r) const
{
  int *deg = degree_monoid()->make_one();
  if (!get_ring()->is_zero(r))
    get_ring()->degree(r, deg);
  degree_monoid()->mult(deg, degree_shift(), deg);
  Matrix *result = new Matrix(rows(), cols(), deg);
  for (int i=0; i<n_cols(); i++)
    (*result)[i] = rows()->mult(r, elem(i));
  return result;
}

Matrix *Matrix::concat(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("concat: different base rings");
      return 0;
    }
  if (n_rows() != m.n_rows())
    {
      ERROR("concat: matrices have different numbers of rows");
      return 0;
    }

  const FreeModule *G = cols()->direct_sum(m.cols());
  Matrix *result = new Matrix(rows(), G);
  int i;
  int nc = n_cols();
  for (i=0; i<nc; i++)
    (*result)[i] = rows()->copy(elem(i));
  for (i=0; i<m.n_cols(); i++)
    (*result)[nc+i] = rows()->copy(m.elem(i));

  return result;
}

Matrix *Matrix::direct_sum(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("concat: different base rings");
      return 0;
    }
  int *deg;
  if (EQ == degree_monoid()->compare(degree_shift(), m->degree_shift()))
    deg = degree_monoid()->make_new(degree_shift());
  else
    deg = degree_monoid()->make_one();

  const FreeModule *F = rows()->direct_sum(m->rows());
  const FreeModule *G = cols()->direct_sum(m->cols());
  Matrix *result = new Matrix(F, G, deg);

  degree_monoid()->remove(deg);

  int i;
  int nr = n_rows();
  int nc = n_cols();
  for (i=0; i<nc; i++) (*result)[i] = F->copy(elem(i));
  for (i=0; i<m->n_cols(); i++)
    (*result)[nc+i] = F->component_shift(nr, m->rows(), (*m)[i]);
  return result;
}

Matrix *Matrix::operator*(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("matrix mult: different base rings");
      return 0;
    }
  if (n_cols() != m.n_rows())
    {
      ERROR("matrix mult: matrix sizes don't match");
      return 0;
    }

  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m.degree_shift(), deg);

  Matrix *result = new Matrix(rows(), m.cols(), deg);
  degree_monoid()->remove(deg);

  for (int i=0; i<m.n_cols(); i++)
    (*result)[i] = rows()->mult_by_matrix(this, m.rows(), m[i]);
  return result;
}

Matrix *Matrix::module_tensor(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("module tensor: different base rings");
      return 0;
    }
  FreeModule *F = rows()->tensor(m->rows());
  FreeModule *G = rows()->tensor(m->cols());
  FreeModule *G1 = m->rows()->tensor(cols());
  G->direct_sum_to(G1);
  delete G1;
  Matrix *result = new Matrix(F, G);

  int i, j, next=0;

  for (i=0; i<n_rows(); i++)
    for (j=0; j<m->n_cols(); j++)
      (*result)[next++] = F->component_shift(i * m->n_rows(), m->rows(), (*m)[j]);

  for (i=0; i<m->n_rows(); i++)
    for (j=0; j<n_cols(); j++)
      (*result)[next++] = F->tensor_shift(m->n_rows(), i, rows(), elem(j));

  return result;
}
Matrix *Matrix::random(const Ring *R, int r, int c)
{
  FreeModule *F = R->make_FreeModule(r);
  FreeModule *G = R->make_FreeModule(c);
  Matrix *result = new Matrix(F,G);
  for (int i=0; i<c; i++)
    (*result)[i] = F->random();
  return result;
}
#if 0
Matrix Matrix::random(const FreeModule *F, const FreeModule *G, 
		      int *mapdeg,	// Degree of the map, not the elements
		      int ishomog)	// Whether the map is homog.  If not
					// then degrees < given degree in any
					// entry are allowed.
{
  // For each degree in the matrix, generate a basis in that degree, or in
  // degrees <= that degree (if ishomog is false).  Don't duplicate these:
  // It is probably sufficient to check linearly the ones that we have.
  // In any case, for each column, place all of the terms on a list, and sort
  // them afterwords.
  Matrix result(F,G,mapdeg);
  array<intarray> degs;
  array<Matrix> bases;
  for (i=0; i<G->rank(); i++)
    {
      vec f = NULL;
      for (j=0; j<F->rank(); j++)
	{
	  // The degree we need:

	  // Determine whether this basis exists

	  // If not, create it, and insert into list.
	  // If ishomog is false, then do that basis.

	  // For each term in this matrix, call 'random' on
	  // the coefficient ring, and add to the 'f' list this term.
	  // Fon't forget to handle the case when F is Schreyer, or
	  // R has no monoid.

	  result[j] = F->sort(f);
	}
    }
}
#endif		      
Matrix *Matrix::tensor(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("matrix tensor: different base rings");
      return 0;
    }

  const FreeModule *F = rows()->tensor(m->rows());
  const FreeModule *G = cols()->tensor(m->cols());
  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m->degree_shift(), deg);

  Matrix *result = new Matrix(F, G, deg);

  degree_monoid()->remove(deg);

  int i, j, next = 0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m->n_cols(); j++)
      (*result)[next++] = F->tensor(rows(), elem(i), 
				 m->rows(), (*m)[j]);
				 
  return result;
}

Matrix *Matrix::diff(const Matrix *m, int use_coef) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("matrix diff: different base rings");
      return 0;
    }
  FreeModule *F1 = rows()->transpose();
  const FreeModule *F = F1->tensor(m->rows());
  FreeModule *G1 = cols()->transpose();
  const FreeModule *G = G1->tensor(m->cols());
  int *deg = degree_monoid()->make_one();
  degree_monoid()->divide(m->degree_shift(), degree_shift(), deg);
  delete F1;
  delete G1;
  Matrix *result = new Matrix(F, G, deg);
  degree_monoid()->remove(deg);
  int i, j, next=0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m->n_cols(); j++)
      (*result)[next++] = F->diff(rows(), elem(i), 
				  m->rows(), (*m)[j],
				  use_coef);

  return result;
}

Matrix *Matrix::lead_term(int n) const
    // Select those monomials in each column
    // which are maximal in the order under
    // the first n weight vectors, where the
    // component slot is considered as the nvars+1 st weight
    // vector.
{
#warning "implement Matrix::lead_term(int n)"
#if 0
  // MES aug 2002
  Matrix *result = new Matrix(rows(), cols(), degree_shift());
  for (int i=0; i<n_cols(); i++)
    (*result)[i] = rows()->lead_term(n, elem(i));
  return result;
#endif
  return 0;
}

#if 0
void Matrix::minimal_lead_terms_ZZ(intarray &result) const
{
  int x;
  M2_arrayint indices;
  array<TermIdeal *> mis;
  const array<vec> vecs = _entries;
  indices = rows()->sort(vecs, NULL, 0, 1);
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  const FreeModule *Rsyz = P->get_Rsyz(); // NULL if not a quotient ring.
  FreeModule *Gsyz = P->make_FreeModule(vecs.length());
  for (x=0; x<n_cols(); x++)
    mis.append(new TermIdeal(P,Gsyz));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices->array[i]];
      vec gsyz, rsyz;
      if (v == NULL) continue;
      if (TI_TERM != mis[v->comp]->search(v->coeff, v->monom, gsyz, rsyz))
	{
	  mis[v->comp]->insert_minimal(
				       new tagged_term(P->Ncoeffs()->copy(v->coeff),
						       P->Nmonoms()->make_new(v->monom),
						       NULL,
						       NULL));
	  result.append(indices->array[i]);
	}
      Gsyz->remove(gsyz);
      if (rsyz != NULL) Rsyz->remove(rsyz);
    }
  for (x=0; x<n_cols(); x++)
    delete mis[x];
}
#endif
#if 0
// OLDER THAN THE PREVIOUS VERSION!!
Matrix Matrix::minimal_lead_terms_ZZ() const
{
  int x;
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  FreeModule *Gsyz = P->make_FreeModule(n_cols());
  bump_up(Gsyz);
  array< queue<tagged_term *> > allterms;
  for (int i=0; i<n_cols(); i++)
    {
      vec v = elem(i);
      if (v == NULL) continue;
      allterms[v->comp].insert(
			       new tagged_term(P->Ncoeffs()->copy(v->coeff),
					       P->Nmonoms()->make_new(v->monom),
					       Gsyz->e_sub_i(i),
					       NULL));
    }
  Matrix result(rows());
  for (x=0; x<n_cols(); x++)
    {
      if (allterms[x].length() > 0)
	{
	  TermIdeal *ti = TermIdeal::make_termideal(P,Gsyz,allterms[x]);
	  // Loop through and add the corresponding elements in...
	  for (cursor_TermIdeal k(ti); k.valid(); ++k)
	    {
	      tagged_term *t = *k;
	      vec gsyz = t->_gsyz;
	      vec v = NULL;
	      rows()->apply_map(v, gsyz, entries);
	      rows()->negate_to(v);
	      result.append(v);
	    }
	  delete ti;
	}
    }
  bump_down(Gsyz);
  return result;
}
#endif

#if 0
void Matrix::minimal_lead_terms(intarray &result) const
{
  if (get_ring()->Ncoeffs()->is_ZZ())
    {
      minimal_lead_terms_ZZ(result);
      return;
    }
  M2_arrayint indices;
  intarray vp;
  array<MonomialIdeal *> mis;
  const array<vec> vecs = _entries;
  indices = rows()->sort(vecs, NULL, 0, 1);
  for (int x=0; x<n_rows(); x++)
    mis.append(new MonomialIdeal(get_ring()));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices->array[i]];
      if (v == NULL) continue;
      // Reduce each one in turn, and replace.
      Bag *junk_bag;
      vp.shrink(0);
      rows()->lead_varpower(v, vp);
      if (!mis[v->comp]->search(vp.raw(),junk_bag))
	{
	  Bag *b = new Bag(indices->array[i], vp);
	  mis[v->comp]->insert(b);
	  result.append(indices->array[i]);
	}
    }
}

Matrix *Matrix::lead_var_coefficient(Matrix *&monoms) const
{
  Matrix *result = new Matrix(rows());
  monoms = new Matrix(get_ring()->make_FreeModule(1));
  int var, exp;
  for (int i=0; i<n_cols(); i++)
    {
      vec u = elem(i);
      vec v = rows()->lead_var_coefficient(u, var, exp);
      result->append(v);
      ring_elem a = get_ring()->var(var,exp);
      vec w = monoms->rows()->term(0,a);
      get_ring()->remove(a);
      monoms->append(w);
    }
  return result;
}
#endif

M2_arrayint Matrix::elim_vars(int nparts) const
{
  intarray keep;
  for (int i=0; i<n_cols(); i++)
    if (rows()->in_subring(nparts, elem(i)))
      keep.append(i);
  M2_arrayint result = makearrayint(keep.length());
  for (unsigned int i=0; i<result->len; i++)
    result->array[i] = keep[i];
  return result;
}

M2_arrayint Matrix::elim_keep(int nparts) const
{
  intarray keep;
  for (int i=0; i<n_cols(); i++)
    if (!rows()->in_subring(nparts, elem(i)))
      keep.append(i);
  M2_arrayint result = makearrayint(keep.length());
  for (unsigned int i=0; i<result->len; i++)
    result->array[i] = keep[i];
  return result;
}

Matrix *Matrix::divide_by_var(int n, int maxd, int &maxdivided) const
{
  Matrix *result = new Matrix(rows(), cols(), degree_shift());
  int *newdeg = degree_monoid()->make_one();
  maxdivided = 0;
  for (int i=0; i<n_cols(); i++)
    {
      if (elem(i) == NULL)
	(*result)[i] = elem(i);
      else
	{
	  int lo,hi;
	  rows()->degree_of_var(n, elem(i), lo,hi);
	  if (maxd >= 0 && lo > maxd)
	    lo = maxd;
	  if (lo > maxdivided)
	    maxdivided = lo;
	  degree_monoid()->power(get_ring()->Nmonoms()->degree_of_var(n), lo, newdeg);
	  degree_monoid()->divide(cols()->degree(i), newdeg, newdeg);
	  result->cols()->change_degree(i, newdeg);
	  (*result)[i] = rows()->divide_by_var(n, lo, elem(i));
	}
    }
  degree_monoid()->remove(newdeg);
  return result;
}

// ideal operations
MatrixOrNull *Matrix::koszul(int p) const
{
  if (n_rows() != 1)
    {
      ERROR("expected a matrix with one row");
      return 0;
    }
  
  FreeModule *F = cols()->exterior(p-1);
  FreeModule *G = cols()->exterior(p);
  Matrix *result = new Matrix(F, G, degree_shift());
  if (p <= 0 || p > n_cols()) return result;
  intarray carray(p);
  int *a = carray.alloc(p);
  for (int c=0; c < result->n_cols(); c++)
    {
      comb::decode(c, a,p);
      vec v = F->zero();
      int negate = ((p % 2) != 0);
      for (int r=p-1; r>=0; r--)
	{
	  negate = !negate;
#if 0
	  int tmp = a[r];
	  a[r] = a[p-1];
	  a[p-1] = tmp;
#else
	  swap(a[p-1], a[r]);
#endif
	  int x = comb::encode(a, p-1);
	  vec temp = F->raw_term(elem(0, a[p-1]), x);

	  if (negate)
	    F->subtract_to(v, temp);
	  else 
	    F->add_to(v, temp);
	}
      (*result)[c] = v;
    }
  return result;
}

static int signdivide(int n, const int *a, const int *b, int *exp)
{
  int sign = 0;
  int sum = 0;
  for (int i=0; i<n; i++)
    {
      int e = a[i] - b[i];
      if (e < 0) return 0;
      exp[i] = e;
      sign += sum*e;
      sum += b[i];
    }
  sign %= 2;
  if (sign == 0) return 1;
  return -1;
}
Matrix *Matrix::koszul(const Matrix *r, const Matrix *c)
{
  // First check rings: r,c,'this' should be row vectors.
  // and the ring should be a polynomial ring
  const FreeModule *F = r->cols();

  // Create result matrix
  Matrix *result = new Matrix(F, c->cols());

  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  if (P == NULL) return result;
  const Monoid *M = P->Nmonoms();

  int nvars = F->get_ring()->n_vars();
  int nrows = r->n_cols();
  int ncols = c->n_cols();
  int *aexp = new int[nvars];
  int *bexp = new int[nvars];
  int *result_exp = new int[nvars];
  for (int i=0; i<ncols; i++)
    {
      if ((*c)[i] == NULL) continue;
      const int *a = P->lead_monomial((*c)[i]->coeff);
      M->to_expvector(a, aexp);
      vec v = NULL;
      for (int j=0; j<nrows; j++)
	{
	  if ((*r)[j] == NULL) continue;
	  const int *b = P->lead_monomial((*r)[j]->coeff);
	  M->to_expvector(b, bexp);
	  int sign = signdivide(nvars, aexp, bexp, result_exp);
	  if (sign != 0)
	    {
	      ring_elem f = P->from_int(sign);
	      M->from_expvector(result_exp, f.poly_val->monom);
	      vec p = F->raw_term(f,j);
	      p->next = v;
	      v = p;
	    }
	}
      F->sort(v);
      (*result)[i] = v;
    }
  delete [] aexp;
  delete [] bexp;
  delete [] result_exp;
  return result;
}

Matrix *Matrix::exterior(int p,int strategy) const
{
  DetComputation *d = new DetComputation(this,p,1,strategy);
  d->calc(-1);
  Matrix *result = d->determinants();
  delete d;
  return result;
}

Matrix *Matrix::minors(int p,int strategy) const
{
  DetComputation *d = new DetComputation(this,p,0,strategy);
  d->calc(-1);
  Matrix *result = d->determinants();
  delete d;
  return result;
}

Matrix *Matrix::pfaffians(int p) const
{
  PfaffianComputation *d = new PfaffianComputation(this,p);
  d->calc(-1);
  Matrix *result = d->pfaffians();
  delete d;
  return result;
}

Matrix *Matrix::wedge_product(int p, int q, const FreeModule *F)
{
  const FreeModule *Fp = F->exterior(p);
  const FreeModule *Fq = F->exterior(q);
  const FreeModule *Fn = F->exterior(p+q);
  const FreeModule *G = Fp->tensor(Fq);

  Matrix *result = new Matrix(Fn, G);

  if (p < 0 || q < 0 || p+q >F->rank())
    return result;

  if (p == 0 || q == 0)
    {
      for (int i=0; i<G->rank(); i++)
	(*result)[i] = Fn->e_sub_i(i);
      return result;
    }

  int *a = new int[p];
  int *b = new int[q];
  int *c = new int[p+q];
  int col = 0;

  for (int i=0; i<Fp->rank(); i++)
    {
      comb::decode(i, a, p);
      for (int j=0; j<Fq->rank(); j++)
	{
	  comb::decode(j, b, q);
	  int sgn = comb::mult_subsets(p,a,q,b,c);
	  if (sgn == 0)
	    {
	      col++;
	      continue;
	    }
	  ring_elem r = F->get_ring()->from_int(sgn);
	  int row = comb::encode(c,p+q);
	  (*result)[col++] = Fn->raw_term(r,row);
	}
    }

  delete [] a;
  delete [] b;
  delete [] c;
  return result;
}

void Matrix::text_out(buffer &o) const
{
  int nrows = n_rows();
  int ncols = n_cols();
//  o << "#rows = " << nrows << " and #cols = " << ncols << endl;
//  o << "rows = ";
//  rows().text_out(o);
//  o << endl << "cols = ";
//  cols().text_out(o);
//  o << endl;

  buffer *p = new buffer[nrows];
  int r;
  for (int c=0; c<ncols; c++)
    {
      int maxcount = 0;
      for (r=0; r<nrows; r++)
	{
	  ring_elem f = elem(r,c);
	  get_ring()->elem_text_out(p[r], f);
	  get_ring()->remove(f);
	  if (p[r].size() > maxcount)
	    maxcount = p[r].size();
	}
      for (r=0; r<nrows; r++)
	for (int k=maxcount+1-p[r].size(); k > 0; k--)
	  p[r] << ' ';
    }
  for (r=0; r<nrows; r++)
    {
      p[r] << '\0';
      char *s = p[r].str();
      o << s << newline;
    }
  delete [] p;
}

#if 0
int Matrix::moneq(const int *exp, int *m, const int *vars, int *exp2) const
    // Internal private routine for 'coeffs'.
    // exp2 is a scratch value.  It is a paramter so we only have to allocate 
    // it once...
{
  get_ring()->Nmonoms()->to_expvector(m, exp2);
  int nvars = get_ring()->n_vars();
  for (int i=0; i<nvars; i++)
    {
      if (vars[i] == 0) continue;
      if (exp[i] != exp2[i]) 
	return 0;
      else 
	exp2[i] = 0;
    }
  get_ring()->Nmonoms()->from_expvector(exp2, m);
  return 1;
}
vec Matrix::strip_vector(vec &f, const int *vars, 
			      const FreeModule *F, vec &vmonom) const
    // private routine for 'coeffs'
{
  if (f == NULL) 
    {
      vmonom = NULL;
      return NULL;
    }
  if (get_ring()->Nmonoms() == NULL)
    {
      vmonom = F->e_sub_i(0);
      vec result = f;
      f = NULL;
      return result;
    }
  // At this point, we know that we have a polynomial ring
  int nvars = get_ring()->n_vars();
  int *exp = new int[nvars];
  int *scratch_exp = new int[nvars];
  const Monoid *M = get_ring()->Nmonoms();

  M->to_expvector(f->monom, exp);
  for (int i=0; i<nvars; i++)
    if (vars[i] == 0) exp[i] = 0;

  // the following two lines do NOT work if 'F' is a Schreyer free module,
  // but this routine is private to 'coeffs', where this is not the case.
  vmonom = F->e_sub_i(0);
  M->from_expvector(exp, vmonom->monom);

  vecterm head;
  vecterm *newf = &head;
  vec result = NULL;

  // Loop through f: if monomial matches 'exp', strip and add to result,
  // otherwise leave alone, and place on head list.
  while (f != NULL)
    {
      if (moneq(exp, f->monom, vars, scratch_exp))
	{
	  vec temp = f;
	  f = f->next;
	  temp->next = NULL;
	  rows()->add_to(result, temp);
	}
      else
	{
	  newf->next = f;
	  f = f->next;
	  newf = newf->next;
	  newf->next = NULL;
	}
    }
  newf->next = NULL;
  f = head.next;

  delete [] exp;
  delete [] scratch_exp;
  return result;
}
#endif
#if 0
// MES Aug 2002
Matrix *Matrix::simplify(int n) const
{
  int i,j, keep;
  Matrix *result = new Matrix(rows());

  switch (n) {
  case 1:
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result->append(rows()->copy(f));
      }
    break;
    //  case SIMP_SCALAR_MULTIPLES:
  case 2:
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	keep = 1;
	for (j=i+1; j<n_cols(); j++)
	  {
	    vec g = elem(j);
	    if (g == NULL) continue;
	    if (rows()->is_scalar_multiple(f, g))
	      {
		keep = 0;
		break;
	      }
	  }
	if (keep) result->append(rows()->copy(f));
      }
    break;
  case 3:
    // Remove multiple monomial divisors (i.e. x^2*f --> x*f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result->append(rows()->monomial_squarefree(f));
      }
    break;
  case 4:
    // Remove monomial divisors (i.e. x*f --> f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result->append(rows()->remove_monomial_divisors(f));
      }
    break;
#if 0
  case SIMP_ZEROS:
    break;
  case SIMP_MULTIPLES:
    break;
  case SIMP_AUTO_REDUCE:
    break;
  case SIMP_SQUAREFREE:
    break;
  case SIMP_MONOMIAL_DIVISORS:
    break;
#endif
  default:
    ERROR("bad simplification type");
    return 0;
  }

  return result;
}
#endif
#if 0
// MES Aug 2002
Matrix *Matrix::auto_reduce() const
{
  array<vec> vecs;
  int i;
  for (i=0; i<n_cols(); i++)
    vecs.append(rows()->copy(elem(i)));
  rows()->auto_reduce(vecs);
  Matrix *result = new Matrix(rows());
  for (i=0; i<vecs.length(); i++)
    result->append(vecs[i]);
  return result;
}
#endif

M2_arrayint Matrix::sort(int degorder, int monorder) const
  // Sort the columns of 'this': Place the column indices into 'result'.
  // If degorder < 0, sort in descending degree order, if >0 ascending degree
  // If ==0, or in the event that two columns have the same (simple) degree,
  // use the monomial order: monorder > 0 means ascending, <0 means descending.
{
  M2_arrayint_OrNull degs = 0;

  if (degorder != 0)
    {
      degs = makearrayint(n_cols());
      for (int i=0; i<n_cols(); i++)
	degs->array[i] = cols()->primary_degree(i);
    }

  return rows()->sort(_entries, degs, degorder, monorder);
}

#if 0
Matrix *Matrix::coeffs(const int *vars, Matrix * &result_monoms) const
{
  Matrix *result_coeffs = new Matrix(rows());
  result_monoms = new Matrix(get_ring()->make_FreeModule(1));	// One row matrix
  for (int j=0; j<n_cols(); j++)
    {
      vec f = rows()->copy(elem(j));
      vec vmonom;
      while (f != NULL)
	{
	  vec g = strip_vector(f, vars, result_monoms->rows(), vmonom);
	  result_coeffs->append(g);
	  result_monoms->append(vmonom);
	}
    }
  // MES: now sort both matrices...
  return result_coeffs;
}
#endif

MatrixOrNull *Matrix::monomials(M2_arrayint vars) const
  // Returns a one row matrix of all of the monomials in the variable subset 'vars'
  // which occur in 'this'.  These monomials are sorted into increasing degree order.
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a matrix over a polynomial ring");
      return 0;
    }
  const Monoid *M = P->Nmonoms();
  int nvars = M->n_vars();
  // Check that 'vars' is valid
  for (unsigned int i=0; i<vars->len; i++)
    if (vars->array[i] < 0 || vars->array[i] >= nvars)
      {
	ERROR("expected a list of indices of indeterminates");
	return 0;
      }

  // Now collect all of the monomials
  int *mon = M->make_one();
  int *exp = new int[M->n_vars()];
  ring_elem one = P->Ncoeffs()->from_int(1);
  exponent_table *E = exponent_table_new(50000, vars->len);

  for (int c=0; c<n_cols(); c++)
    {
      vec v = elem(c);
      for ( ; v != 0; v = v->next)
	{
	  for (Nterm *t = v->coeff; t != 0; t = t->next)
	    {
	      int *exp1 = new int[vars->len];
	      M->to_expvector(t->monom, exp);
	      for (unsigned int i=0; i<vars->len; i++)
		exp1[i] = exp[vars->array[i]];
	      exponent_table_put(E, exp1, 1);
	    }
	}
    }

  // Take all of these monomials and make an array out of them
  Matrix *result = new Matrix(get_ring()->make_FreeModule(1));
  const void ** monoms = exponent_table_to_array(E);
  for (int i=0; i<nvars; i++) exp[i] = 0;
  for (int i=0; monoms[i] != 0; i += 2)
    {
      const int * exp1 = (const int *) monoms[i];
      for (unsigned int j=0; j<vars->len; j++)
	exp[vars->array[j]] = exp1[j];
      M->from_expvector(exp, mon);
      ring_elem a = P->term(one, mon);
      result->append(rows()->raw_term(a,0));
    }
  
  // Remove the garbage memory
  delete [] exp;
  M->remove(mon);
  exponent_table_free(&E);

  // Finally, we sort them
  M2_arrayint perm = result->sort(1,1);
  return result->sub_matrix(perm);
}

static vec coeffs_of_vec(exponent_table *E, M2_arrayint vars,
			 const FreeModule *F, vec f)
    // private routine for 'coeffs'.
{
  if (f == NULL) return 0;
  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    return 0;
  const Monoid *M = P->Nmonoms();
  int *mon = M->make_one();

  // At this point, we know that we have a polynomial ring
  int nvars = M->n_vars();
  int *exp = new int[nvars];
  int *scratch_exp = new int[nvars];

  vec result = 0;
  for (vec g = f ; g != 0; g = g->next)
    {
      for (Nterm *h = g->coeff; h != 0; h = h->next)
	{
	  M->to_expvector(h->monom, exp);
	  for (unsigned int i=0; i<vars->len; i++)
	    {
	      int v = vars->array[i];
	      scratch_exp[i] = exp[v];
	      exp[v] = 0;
	    }
	  int val = exponent_table_get(E, scratch_exp);
	  if (val > 0)
	    {
	      M->from_expvector(exp, mon);
	      ring_elem t = P->term(h->coeff, mon);
	      vec v = F->raw_term(t,val-1);
	      v->next = result;
	      result = v;
	    }
	}
    }
  delete [] exp;
  delete [] scratch_exp;
  M->remove(mon);
  F->sort(result);
  return result;
}

MatrixOrNull *Matrix::coeffs(M2_arrayint vars, const M2_arrayint monoms) const
{
  // Given an array of variable indices, 'vars', and given
  // that 'monoms' and 'this' both have one row, makes a matrix
  // having number of rows = ncols(monoms), 
  //        number of cols = ncols(this),
  // whose (r,c) entry is the coefficient (in the other variables)
  // of this[0,c] in the monomial monoms[0,r].


  // Step 0: Do some error checking
  int nvars = get_ring()->n_vars();
  int nelements = monoms->len / vars->len;
  if (nelements * vars->len != monoms->len)
    {
      ERROR("coeffs: expected an array of exponents");
      return 0;
    }
  for (unsigned int i=0; i<vars->len; i++)
    if (vars->array[i] < 0 || vars->array[i] >= nvars)
      {
	ERROR("coeffs: expected a set of variable indices");
	return 0;
      }

  // Step 1: Make an exponent_table of all of the monoms.
  // We set the value of the i th monomial to be 'i+1', since 0
  // indicates a non-existent entry.

  exponent_table *E = exponent_table_new(nelements, vars->len);
  for (int i=0; i<nelements; i++)
    exponent_table_put(E, monoms->array + i*(vars->len), i+1);

  // Step 2: for each vector column of 'this'
  //     create a column, and put this vector into result.

  Matrix *result = new Matrix(get_ring()->make_FreeModule(nelements));
  for (int i=0; i<n_cols(); i++)
    result->append(coeffs_of_vec(E, vars, rows(), elem(i)));

  return result;
}

MonomialIdeal *Matrix::make_monideal(int n) const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  const Monoid *M = P->Nmonoms();
  queue <Bag *> new_elems;
  for (int i=0; i<n_cols(); i++)
    {
      vec v = elem(i);
      if (rows()->is_zero(v)) continue;
      if (v->comp != n) continue;
      Bag *b = new Bag(i);
      M->to_varpower(P->lead_monomial(v->coeff), b->monom());
      new_elems.insert(b);      
    }

#warning "make_monideal doesn't handle quotient rings"
#if 0
  // If the base ring is a quotient ring, include these lead monomials.
  if (P->is_quotient_ring())
    {
      const MonomialIdeal *Rideal = P->get_quotient_monomials();
      for (Index<MonomialIdeal> j = Rideal->first(); j.valid(); j++)
	{
	  Bag *b = new Bag(-1, (*Rideal)[j]->monom());
	  new_elems.insert(b);
	}
    }
#endif

  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *Matrix::make_skew_monideal(int n) const
{
  MonomialIdeal *result = make_monideal(n);
  const Monoid *M = get_ring()->Nmonoms();
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P != 0 && P->is_skew_commutative())
    {
      intarray vp;
      for (int i=0; i<M->n_vars(); i++)
	if (P->is_skew_var(i))
	  {
	    vp.shrink(0);
	    varpower::var(i,2,vp);
	    Bag *b = new Bag(-1, vp);
	    result->insert_minimal(b);
	  }
    }
  return result;
}

void Matrix::append_monideal(const MonomialIdeal *mi, int k)
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a matrix over a polynomial ring");
      return;
    }
  const Monoid *M = P->Nmonoms();
  int *mon = M->make_one();
  for (Index<MonomialIdeal> i = mi->last(); i.valid(); i--)
    {
      M->from_varpower((*mi)[i]->monom().raw(), mon);
      ring_elem f = P->term(P->Ncoeffs()->from_int(1), mon);
      vec v = rows()->raw_term(f, 0);
      append(v);
    }
  M->remove(mon);
}


int Matrix::dimension() const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  const Ring *K = get_ring()->Ncoeffs();
  bool is_ZZ = K->is_ZZ();
  int base = (is_ZZ ? 1 : 0);
  int result = -1;
  if (P != 0)
    {
      int n = get_ring()->n_vars();
      for (int i=0; i<n_rows(); i++)
	{
	  MonomialIdeal *mi = make_skew_monideal(i);
	  AssociatedPrimes ap(mi);
	  int d = n - ap.codimension();
	  if (d > result) result = d;
	}
      if (result != -1) result += base;
      return result;
    }
  else
    {
      // This handles the case when the coefficients are a field, or ZZ
      int i,j;
      int *dims = new int[n_rows()];
      for (i=0; i<n_rows(); i++)
	dims[i] = base;
      for (j=0; j<n_cols(); j++)
	{
	  vec f = elem(j);
	  if (f == 0) continue;
	  if (dims[f->comp] == -1) continue;
	  if (K->is_unit(f->coeff))
	    dims[f->comp] = -1;
	  else
	    dims[f->comp] = 0;
	}
      for (i=0; i<n_rows(); i++)
	if (dims[i] > result) result = dims[i];
      delete [] dims;
      return result;
    }
}


static int symm1_next = 0;

void Matrix::symm1(Matrix * &result, 
		   vec f,	     // product so far generated, consumed here
		   int lastn,	     // can use lastn..n_cols()-1 in product
		   int pow) const   // remaining power to take
{
  if (pow == 0)
    (*result)[symm1_next++] = f;
  else
    {
      for (int i=lastn; i<n_cols(); i++)
	{
	  ring_elem g = elem(0,i);
	  vec h = result->rows()->mult(g, f);
	  get_ring()->remove(g);
	  symm1(result, h, i, pow-1);
	}
      result->rows()->remove(f);
    }
}

MatrixOrNull *Matrix::symm(int n) const
    // Assumption: 'this' has one row.
    // Return the 'n'th power of the ideal
{
  if (this->n_rows() != 1)
    {
      ERROR("expected one row");
      return 0;
    }
  const FreeModule *G = cols()->symm(n);
  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->power(deg, n, deg);
  Matrix *result = new Matrix(rows(), G, deg);
  degree_monoid()->remove(deg);

  if (n >= 0)
    {
      vec f = rows()->e_sub_i(0);
      symm1_next = 0;
      symm1(result, f, 0, n);	  // consumes f
    }
  return result;
}

static int            kb_do_trunc;
static Matrix       * kb_result;
static MonomialIdeal* kb_monideal;
static int          * kb_deg;
static vec            kb_vec;
static int            kb_n_vars;
static int          * kb_exp;
static int          * kb_mon;
static int          * kb_vec_monom;
static int          * kb_exp_degree;
static const Monoid * kb_D;
static const PolynomialRing * kb_P;

void Matrix::k_basis_insert() const
{
  get_ring()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  get_ring()->Nmonoms()->divide(kb_mon, kb_vec_monom, kb_mon);
  ring_elem tmp = get_ring()->term(get_ring()->Ncoeffs()->from_int(1), kb_mon);
  kb_result->append(rows()->mult(tmp, kb_vec));
  get_ring()->remove(tmp);
}
void Matrix::k_basis0(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (kb_P->is_skew_commutative() &&
	    kb_P->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      kb_D->mult(kb_exp_degree, get_ring()->Nmonoms()->degree_of_var(i),
		     kb_exp_degree);

      int cmp = kb_D->primary_value(kb_exp_degree) - kb_D->primary_value(kb_deg);
      Bag *b;
      if (cmp > 0 
	  && kb_do_trunc 
	  && !kb_monideal->search_expvector(kb_exp,b))
	{
	  k_basis_insert();
	}

      if (cmp <= 0 && !kb_monideal->search_expvector(kb_exp,b))
	{
	  if (cmp == 0)
	    {
	      if (kb_D->compare(kb_exp_degree, kb_deg) == EQ)
		{
		  k_basis_insert();
		}
	    }
	  else
	    k_basis0(i);
	}

      kb_exp[i]--;
      kb_D->divide(kb_exp_degree, get_ring()->Nmonoms()->degree_of_var(i),
		   kb_exp_degree);
    }
}

Matrix *Matrix::k_basis(Matrix &bot, const int *d, int do_trunc) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // Find a basis for (image this)/(image bottom) in degree d.
    // If 'd' is NULL, first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
    // If 'd' is not NULL, it is an element of the degree monoid.
{
  kb_do_trunc = do_trunc;
  kb_result = new Matrix(rows());
  kb_n_vars = get_ring()->n_vars();
  kb_D = get_ring()->degree_monoid();

  kb_mon = get_ring()->Nmonoms()->make_one();
  kb_vec_monom = get_ring()->Nmonoms()->make_one();
  kb_deg = kb_D->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);
  kb_exp_degree = kb_D->make_one();

  int *e = kb_D->make_one();
  kb_D->from_expvector(d, e);

  kb_P = get_ring()->cast_to_PolynomialRing();
  if (kb_P != 0)
    {
      for (int i=0; i<n_rows(); i++)
	{
	  degree_monoid()->divide(e, rows()->degree(i), kb_deg);
	  
	  // get the two monomial ideals
	  MonomialIdeal *top = make_monideal(i);
	  MonomialIdeal *bottom = bot.make_monideal(i);
	  top = *top - *bottom;
	  
	  Bag *b;
	  while (top->remove(b))
	    {
	      kb_vec = elem(b->basis_elem());
	      get_ring()->Nmonoms()->from_varpower(b->monom().raw(),kb_vec_monom);
	      
	      MonomialIdeal *miq = top->intersect(b->monom().raw());
	      
	      kb_monideal = *miq + *bottom;
	      
	      kb_exp_a.shrink(0);
	      varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	      get_ring()->Nmonoms()->degree_of_varpower(b->monom().raw(), 
							kb_exp_degree);
	      
	      int cmp = kb_D->primary_value(kb_exp_degree) 
		- kb_D->primary_value(kb_deg);
	      if ((cmp > 0 && do_trunc)
		  || (0 == kb_D->compare(kb_deg, kb_exp_degree)))
		kb_result->append(rows()->copy(kb_vec));
	      else if (cmp < 0)
		k_basis0(0);
	      
	      delete b;
	    }
	  //	}
	  //      else if (do_trunc)
	  //	{
	  //	  kb_result.append(rows()->copy(elem(i)));
	  //	}
	}
    }
  
  Matrix *result = kb_result;

  kb_D->remove(kb_deg);
  kb_D->remove(kb_exp_degree);
  kb_D->remove(e);
  get_ring()->Nmonoms()->remove(kb_mon);
  get_ring()->Nmonoms()->remove(kb_vec_monom);  
  kb_result = 0;
  kb_P = 0;

  return result;
}


void Matrix::k_basis1(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  get_ring()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  get_ring()->Nmonoms()->divide(kb_mon, kb_vec_monom, kb_mon);
  ring_elem tmp = get_ring()->term(get_ring()->Ncoeffs()->from_int(1), kb_mon);
  kb_result->append(rows()->mult(tmp, kb_vec));
  get_ring()->remove(tmp);

  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (kb_P->is_skew_commutative() &&
	    kb_P->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      Bag *b;
      if (!kb_monideal->search_expvector(kb_exp,b))
	k_basis1(i);

      kb_exp[i]--;
    }
}

Matrix *Matrix::k_basis(Matrix &bot) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
{
  kb_result = new Matrix(rows());
  kb_n_vars = get_ring()->n_vars();

  kb_mon = get_ring()->Nmonoms()->make_one();
  kb_vec_monom = get_ring()->Nmonoms()->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);

  kb_P = get_ring()->cast_to_PolynomialRing();
  if (kb_P != 0)
    {
      for (int i=0; i<n_rows(); i++)
	{
	  // get the two monomial ideals
	  MonomialIdeal *top = make_monideal(i);
	  MonomialIdeal *bottom = bot.make_monideal(i);
	  
	  Bag *b, *c;
	  while (top->remove(b))
	    {
	      kb_vec = elem(b->basis_elem());
	      get_ring()->Nmonoms()->from_varpower(b->monom().raw(),kb_vec_monom);
	      
	      MonomialIdeal *miq = top->intersect(b->monom().raw());
	      kb_monideal = *miq + *bottom;
	      
	      kb_exp_a.shrink(0);
	      varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	      if (!kb_monideal->search(b->monom().raw(), c))
		k_basis1(0);
	      
	      delete b;
	    }
	}
    }
  get_ring()->Nmonoms()->remove(kb_mon);  
  get_ring()->Nmonoms()->remove(kb_vec_monom);  
  Matrix *result = kb_result;
  kb_result = 0;
  kb_P = 0;
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
