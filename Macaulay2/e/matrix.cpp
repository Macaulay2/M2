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
    inpart, stdpart, mininimalpart (but call them what??)
#endif

#include "style.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "matrix.hpp"
#include "comb.hpp"
#include "det.hpp"
#include "termideal.hpp"
#include "assprime.hpp"
stash *Matrix_rec::mystash;

Matrix_rec::~Matrix_rec()
{ 
  rows->get_ring()->degree_monoid()->remove(degree_shift);
  for (int i=0; i<cols->rank(); i++) rows->remove(entries[i]);
  bump_down(rows); 
  bump_down(cols); 
}

Matrix::Matrix(const FreeModule *r, 
	       const FreeModule *c,
	       const int *deg)
{
  obj = new Matrix_rec(r,c,deg);
}

Matrix::Matrix(const FreeModule *r, 
	       const FreeModule *c)
{ 
  int *one = r->get_ring()->degree_monoid()->make_one();
  obj = new Matrix_rec(r,c,one);
  r->get_ring()->degree_monoid()->remove(one);
}

Matrix::Matrix(const FreeModule *r)
{ 
  int *one = r->get_ring()->degree_monoid()->make_one();
  obj = new Matrix_rec(r,r->new_free(),one);
  r->get_ring()->degree_monoid()->remove(one);
}

Matrix::Matrix(const MonomialIdeal &mi) 
{ 
  const FreeModule *r = mi.get_ring()->make_FreeModule(1);
  int *one = r->get_ring()->degree_monoid()->make_one();
  obj = new Matrix_rec(r,r->new_free(),one);
  r->get_ring()->degree_monoid()->remove(one);

  append_monideal(mi,0);
}

bool Matrix::is_equal(const Matrix &m) const
{
  if (this == &m) return true;
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

Matrix Matrix::homogenize(int v, const int *wts) const
{
  Matrix result(rows());
  for (int i=0; i<n_cols(); i++)
    result.append(rows()->homogenize(elem(i), v, wts));
  return result;
}

intarray Matrix::get_degree_shift() const
{
  intarray result;
  int *d = result.alloc(degree_monoid()->n_vars());
  degree_monoid()->to_expvector(degree_shift(), d);
  return result;
}

void Matrix::set_degree_shift(const intarray &deg)
{
  if (deg.length() != degree_monoid()->n_vars())
    {
      gError << "improper shift degree";
      return;
    }
  degree_monoid()->from_expvector(deg.raw(), degree_shift());
}

void Matrix::schreyer_append(vec v)
{
  if (! rows()->is_zero(v)) 
    {
      int *d = degree_monoid()->make_one();
      rows()->degree(v, d);
      cols()->append(d, v->monom, cols()->rank());
      degree_monoid()->remove(d);
      obj->entries.append(v);
    }
  else
    append(v);
}

Matrix Matrix::zero(const FreeModule *F, const FreeModule *G)
{
  Matrix result(F,G);
  if (F->get_ring() != G->get_ring())
    gError << "free modules have different base rings";
  for (int i=0; i<G->rank(); i++)
    result[i] = F->zero();
  return result;
}

Matrix Matrix::identity(const FreeModule *F)
{
  Matrix result(F,F);
  for (int i=0; i<F->rank(); i++)
    result[i] = F->e_sub_i(i);
  return result;
}

Matrix Matrix::operator+(const Matrix &m) const
{
  Matrix result(rows(), cols());

  if (get_ring() != m.get_ring())
    gError << "matrices have different base rings";
  else if (rows()->rank() != m.rows()->rank()
	   || cols()->rank() != m.cols()->rank())
    gError << "matrices have different shapes";
  else
    {
      const Ring *R = get_ring();
      const FreeModule *F = rows();
      const FreeModule *G = cols();

      if (!rows()->is_equal(m.rows()))
	F = R->make_FreeModule(n_rows());
      
      if (!cols()->is_equal(m.cols()))
	G = R->make_FreeModule(n_cols());

      if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
	result = Matrix(F,G,degree_shift());
      else
	result = Matrix(F,G);

      for (int i=0; i<n_cols(); i++)
	{
	  vec v = F->translate(rows(), elem(i));
	  vec w = F->translate(m.rows(), m[i]);
	  F->add_to(v,w);
	  result[i] = v;
	}
    }
  return result;
}
Matrix Matrix::operator-(const Matrix &m) const
{
  Matrix result(rows(), cols());

  if (get_ring() != m.get_ring())
    gError << "matrices have different base rings";
  else if (rows()->rank() != m.rows()->rank()
	   || cols()->rank() != m.cols()->rank())
    gError << "matrices have different shapes";
  else
    {
      const Ring *R = get_ring();
      const FreeModule *F = rows();
      const FreeModule *G = cols();

      if (!rows()->is_equal(m.rows()))
	F = R->make_FreeModule(n_rows());
      
      if (!cols()->is_equal(m.cols()))
	G = R->make_FreeModule(n_cols());

      if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
	result = Matrix(F,G,degree_shift());
      else
	result = Matrix(F,G);

      for (int i=0; i<n_cols(); i++)
	{
	  vec v = F->translate(rows(), elem(i));
	  vec w = F->translate(m.rows(), m[i]);
	  F->subtract_to(v,w);
	  result[i] = v;
	}
    }
  return result;
}
Matrix Matrix::operator-() const
{
  Matrix result(rows(), cols(), degree_shift());
  for (int i=0; i<n_cols(); i++)
    result[i] = rows()->negate(elem(i));
  return result;
}

Matrix Matrix::sub_matrix(const intarray &r, const intarray &c) const
{
  const FreeModule *F = rows()->sub_space(r);
  const FreeModule *G = cols()->sub_space(c);
  if (F == NULL || G == NULL)
      return Matrix(rows(), cols());

  Matrix result(F, G, degree_shift());
  for (int i=0; i<c.length(); i++)
    result[i] = F->sub_vector(rows(), elem(c[i]), r);
  return result;
}

Matrix Matrix::sub_matrix(const intarray &c) const
{
  const FreeModule *G = cols()->sub_space(c);
  if (G == NULL)
    return Matrix(rows(), cols());

  Matrix result(rows(), G, degree_shift());
  for (int i=0; i<c.length(); i++)
    result[i] = rows()->copy(elem(c[i]));
  return result;
}

Matrix Matrix::reshape(const FreeModule *F, const FreeModule *G) const
  // Reshape 'this' : F <--- G, where 
  // (rank F)(rank G) = (nrows this)(ncols this)
{
  Matrix result(F,G);
  if (n_rows() * n_cols() != F->rank() * G->rank())
    gError << "reshape: ranks of freemodules incorrect";
  else
    F->reshape(*this, result);
  return result;
}

Matrix Matrix::flip(const FreeModule *F, const FreeModule *G)
{
  const FreeModule *H = F->tensor(G);
  const FreeModule *K = G->tensor(F);
  Matrix result(K, H);
  int next = 0;
  for (int f=0; f<F->rank(); f++)
    for (int g=0; g<G->rank(); g++)
      result[next++] = H->e_sub_i(f + g * F->rank());
  return result;
}

Matrix Matrix::transpose() const
{
  const FreeModule *F = cols()->transpose();
  const FreeModule *G = rows()->transpose();
  int *deg = degree_monoid()->make_one();
  degree_monoid()->divide(deg, degree_shift(), deg);
  Matrix result(F, G, deg);
  degree_monoid()->remove(deg);
  F->transpose_matrix(*this, result);
  return result;
}

Matrix Matrix::operator*(const ring_elem r) const
{
  int *deg = degree_monoid()->make_one();
  if (!get_ring()->is_zero(r))
    get_ring()->degree(r, deg);
  degree_monoid()->mult(deg, degree_shift(), deg);
  Matrix result(rows(), cols(), deg);
  for (int i=0; i<n_cols(); i++)
    result[i] = rows()->mult(r, elem(i));
  return result;
}

Matrix Matrix::concat(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      gError << "concat: different base rings";
      return Matrix(rows(), cols());
    }
  if (n_rows() != m.n_rows())
    {
      gError << "concat: matrices have different numbers of rows";
      return Matrix(rows(), cols());
    }

  const FreeModule *G = cols()->direct_sum(m.cols());
  Matrix result(rows(), G);
  int i;
  int nc = n_cols();
  for (i=0; i<nc; i++)
    result[i] = rows()->copy(elem(i));
  for (i=0; i<m.n_cols(); i++)
    result[nc+i] = rows()->translate(m.rows(), m.elem(i));

  return result;
}

Matrix Matrix::direct_sum(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      gError << "concat: different base rings";
      return Matrix(rows(), cols());
    }
  int *deg;
  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    deg = degree_monoid()->make_new(degree_shift());
  else
    deg = degree_monoid()->make_one();

  const FreeModule *F = rows()->direct_sum(m.rows());
  const FreeModule *G = cols()->direct_sum(m.cols());
  Matrix result(F, G, deg);

  degree_monoid()->remove(deg);

  int i;
  int nr = n_rows();
  int nc = n_cols();
  for (i=0; i<nc; i++) result[i] = F->translate(rows(), elem(i));
  for (i=0; i<m.n_cols(); i++)
    result[nc+i] = F->component_shift(nr, m.rows(), m[i]);
  return result;
}

Matrix Matrix::operator*(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      gError << "matrix mult: different base rings";
      return Matrix(rows(), cols());
    }
  if (n_cols() != m.n_rows())
    {
      gError << "matrix mult: matrix sizes don't match";
      return Matrix(rows(), cols());
    }

  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m.degree_shift(), deg);

  Matrix result(rows(), m.cols(), deg);
  degree_monoid()->remove(deg);

  for (int i=0; i<m.n_cols(); i++)
    result[i] = rows()->mult_by_matrix(*this, m.rows(), m[i]);
  return result;
}

Matrix Matrix::module_tensor(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      gError << "module tensor: different base rings";
      return Matrix(rows(), cols());
    }
  FreeModule *F = rows()->tensor(m.rows());
  FreeModule *G = rows()->tensor(m.cols());
  FreeModule *G1 = m.rows()->tensor(cols());
  G->direct_sum_to(G1);
  delete G1;
  Matrix result(F, G);

  int i, j, next=0;

  for (i=0; i<n_rows(); i++)
    for (j=0; j<m.n_cols(); j++)
      result[next++] = F->component_shift(i * m.n_rows(), m.rows(), m[j]);

  for (i=0; i<m.n_rows(); i++)
    for (j=0; j<n_cols(); j++)
      result[next++] = F->tensor_shift(m.n_rows(), i, rows(), elem(j));

  return result;
}
Matrix Matrix::random(const Ring *R, int r, int c)
{
  FreeModule *F = R->make_FreeModule(r);
  FreeModule *G = R->make_FreeModule(c);
  Matrix result = Matrix(F,G);
  for (int i=0; i<c; i++)
    result[i] = F->random();
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
Matrix Matrix::tensor(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      gError << "matrix tensor: different base rings";
      return Matrix(rows(), cols());
    }

  const FreeModule *F = rows()->tensor(m.rows());
  const FreeModule *G = cols()->tensor(m.cols());
  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m.degree_shift(), deg);

  Matrix result(F, G, deg);

  degree_monoid()->remove(deg);

  int i, j, next = 0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m.n_cols(); j++)
      result[next++] = F->tensor(rows(), elem(i), 
				 m.rows(), m[j]);
				 
  return result;
}

Matrix Matrix::diff(const Matrix &m, int use_coef) const
{
  if (get_ring() != m.get_ring())
    {
      gError << "matrix diff: different base rings";
      return Matrix(rows(), cols());
    }
  FreeModule *F1 = rows()->transpose();
  const FreeModule *F = F1->tensor(m.rows());
  FreeModule *G1 = cols()->transpose();
  const FreeModule *G = G1->tensor(m.cols());
  int *deg = degree_monoid()->make_one();
  degree_monoid()->divide(m.degree_shift(), degree_shift(), deg);
  delete F1;
  delete G1;
  Matrix result(F, G, deg);
  degree_monoid()->remove(deg);
  int i, j, next=0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m.n_cols(); j++)
      result[next++] = F->diff(rows(), elem(i), 
			       m.rows(), m[j],
			       use_coef);

  return result;
}

Matrix Matrix::lead_term(int n) const
    // Select those monomials in each column
    // which are maximal in the order under
    // the first n weight vectors, where the
    // component slot is considered as the nvars+1 st weight
    // vector.
{
  Matrix result(rows(), cols(), degree_shift());
  for (int i=0; i<n_cols(); i++)
    result[i] = rows()->lead_term(n, elem(i));
  return result;
}

void Matrix::minimal_lead_terms_ZZ(intarray &result) const
{
  int x;
  intarray indices;
  intarray degs; // Not used.
  array<TermIdeal *> mis;
  const array<vec> vecs = obj->entries;
  rows()->sort(vecs, degs, 0, 1, indices);
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  const FreeModule *Rsyz = P->get_Rsyz(); // NULL if not a quotient ring.
  FreeModule *Gsyz = P->make_FreeModule(vecs.length());
  bump_up(Gsyz);
  for (x=0; x<n_cols(); x++)
    mis.append(new TermIdeal(P,Gsyz));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices[i]];
      vec gsyz, rsyz;
      if (v == NULL) continue;
      if (TI_TERM != mis[v->comp]->search(v->coeff, v->monom, gsyz, rsyz))
	{
	  mis[v->comp]->insert_minimal(
				       new tagged_term(P->Ncoeffs()->copy(v->coeff),
						       P->Nmonoms()->make_new(v->monom),
						       NULL,
						       NULL));
	  result.append(indices[i]);
	}
      Gsyz->remove(gsyz);
      if (rsyz != NULL) Rsyz->remove(rsyz);
    }
  for (x=0; x<n_cols(); x++)
    delete mis[x];
  bump_down(Gsyz);
}

#if 0
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
	      rows()->apply_map(v, gsyz, obj->entries);
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

void Matrix::minimal_lead_terms(intarray &result) const
{
  if (get_ring()->Ncoeffs()->is_Z())
    {
      minimal_lead_terms_ZZ(result);
      return;
    }
  intarray indices, vp;
  intarray degs; // Not used.
  array<MonomialIdeal> mis;
  const array<vec> vecs = obj->entries;
  rows()->sort(vecs, degs, 0, 1, indices);
  for (int x=0; x<n_rows(); x++)
    mis.append(MonomialIdeal(get_ring()));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices[i]];
      if (v == NULL) continue;
      // Reduce each one in turn, and replace.
      Bag *junk_bag;
      vp.shrink(0);
      rows()->lead_varpower(v, vp);
      if (!mis[v->comp].search(vp.raw(),junk_bag))
	{
	  Bag *b = new Bag(indices[i], vp);
	  mis[v->comp].insert(b);
	  result.append(indices[i]);
	}
    }
}

Matrix Matrix::lead_var_coefficient(Matrix &monoms) const
{
  Matrix result(rows());
  monoms = Matrix(get_ring()->make_FreeModule(1));
  int var, exp;
  for (int i=0; i<n_cols(); i++)
    {
      vec u = elem(i);
      vec v = rows()->lead_var_coefficient(u, var, exp);
      result.append(v);
      ring_elem a = get_ring()->var(var,exp);
      vec w = monoms.rows()->term(0,a);
      get_ring()->remove(a);
      monoms.append(w);
    }
  return result;
}

void Matrix::elim(int n, intarray &result) const
{
  for (int i=0; i<n_cols(); i++)
    if (rows()->in_subring(n, elem(i)))
      result.append(i);
}

Matrix Matrix::sat(int n, int maxd) const
{
  Matrix result(rows(), cols(), degree_shift());
  int *newdeg = degree_monoid()->make_one();
  for (int i=0; i<n_cols(); i++)
    {
      if (elem(i) == NULL)
	result[i] = elem(i);
      else
	{
	  int d = rows()->degree_of_var(n, elem(i));
	  if (maxd >= 0 && d > maxd)
	    d = maxd;
	  degree_monoid()->power(get_ring()->Nmonoms()->degree_of_var(n), d, newdeg);
	  degree_monoid()->divide(cols()->degree(i), newdeg, newdeg);
	  result.cols()->change_degree(i, newdeg);
	  result[i] = rows()->divide_by_var(n, d, elem(i));
	}
    }
  degree_monoid()->remove(newdeg);
  return result;
}

// ideal operations
Matrix Matrix::koszul(int p) const
{
  
  assert(n_rows() == 1);	// koszul should only be used on a row vector
  FreeModule *F = cols()->exterior(p-1);
  FreeModule *G = cols()->exterior(p);
  Matrix result(F, G, degree_shift());
  if (p <= 0 || p > get_ring()->n_vars()) return result;
  intarray carray(p);
  int *a = carray.alloc(p);
  for (int c=0; c < result.n_cols(); c++)
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
	  vec temp = F->term(x, elem(0, a[p-1]));

	  if (negate)
	    F->subtract_to(v, temp);
	  else 
	    F->add_to(v, temp);
	}
      result[c] = v;
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
Matrix Matrix::koszul(const Matrix &r, const Matrix &c)
{
  // First check rings: r,c,'this' should be row vectors.
  const FreeModule *F = r.cols();
  const Ring *R = F->get_ring();
  const Monoid *M = R->Nmonoms();
  
  // Create result matrix
  Matrix result(F, c.cols());

  if (M == NULL) return result;

  int nvars = F->get_ring()->n_vars();
  int nrows = r.n_cols();
  int ncols = c.n_cols();
  int *aexp = new int[nvars];
  int *bexp = new int[nvars];
  int *result_exp = new int[nvars];
  for (int i=0; i<ncols; i++)
    {
      if (c[i] == NULL) continue;
      const int *a = c[i]->monom;
      M->to_expvector(a, aexp);
      vec v = NULL;
      for (int j=0; j<nrows; j++)
	{
	  if (r[j] == NULL) continue;
	  const int *b = r[j]->monom;
	  M->to_expvector(b, bexp);
	  int sign = signdivide(nvars, aexp, bexp, result_exp);
	  if (sign != 0)
	    {
	      ring_elem f = R->from_int(sign);
	      M->from_expvector(result_exp, f.poly_val->monom);
	      vec p = F->term(j,f);
	      p->next = v;
	      v = p;
	    }
	}
      F->sort(v);
      result[i] = v;
    }
  delete [] aexp;
  delete [] bexp;
  delete [] result_exp;
  return result;
}

#if 0

// determinants and Schur functors
Matrix Matrix::symm(int p) const;
#endif

Matrix Matrix::exterior(int p) const
{
  DetComputation *d = new DetComputation(*this,p,1);
  d->calc(-1);
  Matrix result = d->determinants();
  delete d;
  return result;
}

Matrix Matrix::wedge_product(int p, int q, const FreeModule *F)
{
  const FreeModule *Fp = F->exterior(p);
  const FreeModule *Fq = F->exterior(q);
  const FreeModule *Fn = F->exterior(p+q);
  const FreeModule *G = Fp->tensor(Fq);

  Matrix result(Fn, G);

  if (p < 0 || q < 0 || p+q >F->rank())
    return result;

  if (p == 0 || q == 0)
    {
      for (int i=0; i<G->rank(); i++)
	result[i] = Fn->e_sub_i(i);
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
	  result[col++] = Fn->term(row,r);
	  F->get_ring()->remove(r);
	}
    }

  delete [] a;
  delete [] b;
  delete [] c;
  return result;
}

#if 0
Matrix Matrix::wedge_dual(int p, const FreeModule *F)
{
  const FreeModule *Fp = F->exterior(p);
  Matrix result(Fp,Fp);

  if (p <= 0 || p > F->rank())
    return result;

  for (int i=0; i<Fp->rank(); i++)
    {
      
    }
}
#endif

void Matrix_rec::text_out(buffer &o) const
{
  Matrix_rec *M1 = (Matrix_rec *) this;
  Matrix M = M1->cast_to_Matrix();
  M.text_out(o);
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

void Matrix_rec::bin_out(buffer &o) const
{
  assert(cols->rank() == entries.length());
  bin_int_out(o,entries.length());
  for (int i=0; i<entries.length(); i++)
    rows->elem_bin_out(o, entries[i]);
}
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
Matrix Matrix::simplify(int n) const
{
  int i,j, keep;
  Matrix result(rows());

  switch (n) {
  case 1:
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result.append(rows()->copy(f));
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
	if (keep) result.append(rows()->copy(f));
      }
    break;
  case 3:
    // Remove multiple monomial divisors (i.e. x^2*f --> x*f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result.append(rows()->monomial_squarefree(f));
      }
    break;
  case 4:
    // Remove monomial divisors (i.e. x*f --> f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result.append(rows()->remove_monomial_divisors(f));
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
    gError << "bad simplification type";
    break;
  }

  return result;
}

Matrix Matrix::auto_reduce() const
{
  array<vec> vecs;
  int i;
  for (i=0; i<n_cols(); i++)
    vecs.append(rows()->copy(elem(i)));
  rows()->auto_reduce(vecs);
  Matrix result(rows());
  for (i=0; i<vecs.length(); i++)
    result.append(vecs[i]);
  return result;
}

void Matrix::sort(int degorder, int monorder, intarray &result) const
  // Sort the columns of 'this': Place the column indices into 'result'.
  // If degorder < 0, sort in descending degree order, if >0 ascending degree
  // If ==0, or in the event that two columns have the same (simple) degree,
  // use the monomial order: monorder > 0 means ascending, <0 means descending.
{
  intarray degs;

  if (degorder != 0)
    for (int i=0; i<n_cols(); i++)
      degs.append(cols()->primary_degree(i));

  rows()->sort(obj->entries, degs, degorder, monorder, result);
}

Matrix Matrix::coeffs(const int *vars, Matrix &result_monoms) const
{
  Matrix result_coeffs(rows());
  result_monoms = Matrix(get_ring()->make_FreeModule(1));	// One row matrix
  for (int j=0; j<n_cols(); j++)
    {
      vec f = rows()->copy(elem(j));
      vec vmonom;
      while (f != NULL)
	{
	  vec g = strip_vector(f, vars, result_monoms.rows(), vmonom);
	  result_coeffs.append(g);
	  result_monoms.append(vmonom);
	}
    }
  return result_coeffs;
}

MonomialIdeal Matrix::make_monideal(int n) const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      gError << "expected polynomial ring";
      return MonomialIdeal(get_ring());
    }
  queue <Bag *> new_elems;
  for (int i=0; i<n_cols(); i++)
    {
      vec v = elem(i);
      if (rows()->is_zero(v)) continue;
      if (rows()->lead_component(v) != n) continue;
      Bag *b = new Bag(i);
      rows()->lead_varpower(v, b->monom());
      new_elems.insert(b);      
    }

  // If the base ring is a quotient ring, include these lead monomials.
  if (P->is_quotient_ring())
    {
      const MonomialIdeal &Rideal = P->get_quotient_monomials();
      for (Index<MonomialIdeal> j = Rideal.first(); j.valid(); j++)
	{
	  Bag *b = new Bag(-1, Rideal[j]->monom());
	  new_elems.insert(b);
	}
    }

  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

MonomialIdeal Matrix::make_skew_monideal(int n) const
{
  MonomialIdeal result = make_monideal(n);
  const Monoid *M = get_ring()->Nmonoms();
  if (M != NULL && M->is_skew())
    {
      intarray vp;
      for (int i=0; i<M->n_vars(); i++)
	if (M->is_skew_var(i))
	  {
	    vp.shrink(0);
	    varpower::var(i,2,vp);
	    Bag *b = new Bag(-1, vp);
	    result.insert_minimal(b);
	  }
    }
  return result;
}

void Matrix::append_monideal(const MonomialIdeal &mi, int k)
{
  for (Index<MonomialIdeal> i = mi.last(); i.valid(); i--)
    {
      vec v = rows()->from_varpower(mi[i]->monom().raw(), k);
      append(v);
    }
}


int Matrix::dimension() const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  const Ring *K = get_ring()->Ncoeffs();
  bool is_ZZ = K->is_Z();
  int base = (is_ZZ ? 1 : 0);
  int result = -1;
  if (P != 0)
    {
      int n = get_ring()->n_vars();
      for (int i=0; i<n_rows(); i++)
	{
	  MonomialIdeal mi = make_skew_monideal(i);
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

void Matrix::symm1(Matrix &result, 
		   vec f,	     // product so far generated, consumed here
		   int lastn,	     // can use lastn..n_cols()-1 in product
		   int pow) const   // remaining power to take
{
  if (pow == 0)
    result[symm1_next++] = f;
  else
    {
      for (int i=lastn; i<n_cols(); i++)
	{
	  ring_elem g = elem(0,i);
	  vec h = result.rows()->mult(g, f);
	  get_ring()->remove(g);
	  symm1(result, h, i, pow-1);
	}
      result.rows()->remove(f);
    }
}

Matrix Matrix::symm(int n) const
    // Assumption: 'this' has one row.
    // Return the 'n'th power of the ideal
{
  const FreeModule *G = cols()->symm(n);
  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->power(deg, n, deg);
  Matrix result(rows(), G, deg);
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
static Matrix         kb_result;
static MonomialIdeal  kb_monideal;
static int          * kb_deg;
static vec            kb_vec;
static int            kb_n_vars;
static int          * kb_exp;
static int          * kb_mon;
static int          * kb_exp_degree;
static const Monoid * kb_D;

void Matrix::k_basis_insert() const
{
  get_ring()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  get_ring()->Nmonoms()->divide(kb_mon, kb_vec->monom, kb_mon);
  ring_elem tmp = get_ring()->term(get_ring()->Ncoeffs()->from_int(1), kb_mon);
  kb_result.append(rows()->mult(tmp, kb_vec));
  get_ring()->remove(tmp);
}
void Matrix::k_basis0(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (get_ring()->Nmonoms()->is_skew() &&
	    get_ring()->Nmonoms()->is_skew_var(i) &&
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
	  && !kb_monideal.search_expvector(kb_exp,b))
	{
	  k_basis_insert();
	}

      if (cmp <= 0 && !kb_monideal.search_expvector(kb_exp,b))
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

Matrix Matrix::k_basis(Matrix bot, const int *d, int do_trunc) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // Find a basis for (image this)/(image bottom) in degree d.
    // If 'd' is NULL, first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
    // If 'd' is not NULL, it is an element of the degree monoid.
{
  kb_do_trunc = do_trunc;
  kb_result = Matrix(rows());
  kb_n_vars = get_ring()->n_vars();
  kb_D = get_ring()->degree_monoid();

  kb_mon = get_ring()->Nmonoms()->make_one();
  kb_deg = kb_D->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);
  kb_exp_degree = kb_D->make_one();

  int *e = kb_D->make_one();
  kb_D->from_expvector(d, e);

  for (int i=0; i<n_rows(); i++)
    {
      degree_monoid()->divide(e, rows()->degree(i), kb_deg);
      
	  // get the two monomial ideals
	  MonomialIdeal top = make_monideal(i);
	  MonomialIdeal bottom = bot.make_monideal(i);
	  top = top - bottom;

	  Bag *b;
	  while (top.remove(b))
	    {
	      kb_vec = elem(b->basis_elem());
	      MonomialIdeal miq = top.intersect(b->monom().raw());

	      kb_monideal = miq + bottom;
	      
	      kb_exp_a.shrink(0);
	      varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	      get_ring()->Nmonoms()->degree_of_varpower(b->monom().raw(), 
						       kb_exp_degree);

	      int cmp = kb_D->primary_value(kb_exp_degree) 
		        - kb_D->primary_value(kb_deg);
	      if ((cmp > 0 && do_trunc)
                 || (0 == kb_D->compare(kb_deg, kb_exp_degree)))
		kb_result.append(rows()->copy(kb_vec));
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
  
  Matrix result = kb_result;

  kb_D->remove(kb_deg);
  kb_D->remove(kb_exp_degree);
  kb_D->remove(e);
  get_ring()->Nmonoms()->remove(kb_mon);
  kb_result = Matrix(get_ring()); // This is so no large global data will be laying around.

  return result;
}


void Matrix::k_basis1(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  get_ring()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  get_ring()->Nmonoms()->divide(kb_mon, kb_vec->monom, kb_mon);
  ring_elem tmp = get_ring()->term(get_ring()->Ncoeffs()->from_int(1), kb_mon);
  kb_result.append(rows()->mult(tmp, kb_vec));
  get_ring()->remove(tmp);

  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (get_ring()->Nmonoms()->is_skew() &&
	    get_ring()->Nmonoms()->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      Bag *b;
      if (!kb_monideal.search_expvector(kb_exp,b))
	k_basis1(i);

      kb_exp[i]--;
    }
}

Matrix Matrix::k_basis(Matrix bot) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
{
  kb_result = Matrix(rows());
  kb_n_vars = get_ring()->n_vars();

  kb_mon = get_ring()->Nmonoms()->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);

  for (int i=0; i<n_rows(); i++)
    {
      // get the two monomial ideals
      MonomialIdeal top = make_monideal(i);
      MonomialIdeal bottom = bot.make_monideal(i);
      
      Bag *b, *c;
      while (top.remove(b))
	{
	  kb_vec = elem(b->basis_elem());

	  MonomialIdeal miq = top.intersect(b->monom().raw());
	  kb_monideal = miq + bottom;
	  
	  kb_exp_a.shrink(0);
	  varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	  if (!kb_monideal.search(b->monom().raw(), c))
	    k_basis1(0);

	  delete b;
	}
    }

  get_ring()->Nmonoms()->remove(kb_mon);  
  Matrix result = kb_result;
  kb_result = Matrix(get_ring()); // This is so no large global data will be laying around.
  return result;
}
