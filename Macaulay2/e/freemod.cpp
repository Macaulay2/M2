#if defined(__MWERKS__)
#pragma optimization_level 2
#endif
// Copyright 1995  Michael E. Stillman

#include "freemod.hpp"
#include "comb.hpp"
#include "text_io.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "ringmap.hpp"
#include "ntuple.hpp"
#include "termideal.hpp"

#include "geovec.hpp"

extern "C" void debugout(const FreeModule *F, const vec v)
{
  buffer o;
  F->elem_text_out(o,v);
  emit_line(o.str());
}
vec FreeModule::new_term() const
{
  vec result = new vecterm;
  result->next = NULL;
  result->coeff = (Nterm*)0;
  return result;
}
vec FreeModule::copy_term(vec v) const
{
  vec result = new_term();
  result->comp = v->comp;
  result->coeff = R->copy(v->coeff);
  result->next = NULL;
  return result;
}

vec FreeModule::e_sub_i(int i) const
{
  ring_elem a = R->from_int(1);
  return raw_term(a,i);
}

vec FreeModule::raw_term(ring_elem a, int e) const
  // This DOES NOT COPY a.
{
  if (R->is_zero(a))
    return NULL;
  vec result = new_term();
  result->next = NULL;
  result->comp = e;
  result->coeff = a;
  return result;
}

bool FreeModule::is_equal(vec a, vec b) const
{
  for ( ;; a = a->next, b = b->next)
    {
      if (a == NULL)
	{
	  if (b == NULL) return true;
	  return false;
	}
      if (b == NULL) return false;
      if (a->comp != b->comp) return false;
      if (!R->is_equal(a->coeff, b->coeff)) return false;
    }
}


vec FreeModule::copy(vec v) const
{
  vecterm result;
  vecterm *b = &result;
  for ( ; v != NULL; v = v->next, b = b->next)
      b->next = copy_term(v);
  b->next = NULL;
  return result.next;
}

void FreeModule::remove(vec &v) const
{
  R->remove_vector(v);
}

ring_elem FreeModule::get_coefficient(vec v, int e) const
{
  while (v != NULL)
    {
      if (v->comp == e) return R->copy(v->coeff);
      if (v->comp < e) return R->from_int(0);
      v = v->next;
    }
  return R->from_int(0);
}

#if 0
vec FreeModule::lead_term(int n, vec v) const
{
  if (v == NULL) return NULL;
  vecterm head;
  vecterm *result = &head;
  if (M == NULL || n > R->n_vars())
    {
      result->next = copy_term(v);
      result = result->next;
    }
  else 
    for (vecterm *t = v; t != NULL; t = t->next)
      {
	if (M->compare(n, t->monom, v->monom) != 0) break;
	result->next = copy_term(t);
	result = result->next;
      }
  result->next = NULL;
  return head.next;
}

vec FreeModule::lead_term(vec v) const
{
  return lead_term(1 + R->n_vars(), v);
}
#endif

#if 0
// MES Aug 2002
vec FreeModule::lead_term(vec v) const
{
  // MES: THIS DEPENDS ON THE ORDER IN THE FREEMODULE!!
  // This one is written so that the order is mi ei > mj ej
  // iff mi > mj, or = and i > j.
  //  if (schreyer) return schreyer_lead_term(v,a,comp);
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) return copy_term(v);
  // First find the component with the largest monomial
  if (v == 0) return NULL;
  vec largest = v;
  for (vec w = v->next; w != 0; w = w->next)
    if (P->compare(w->coeff, largest->coeff) == GT)
      largest = w;

  ring_elem lt = P->lead_term(largest->coeff);
  return raw_term(lt, largest->comp);
}

const int * FreeModule::schreyer_lead_term(vec v, ring_elem &a, int &comp) const
{
  
}
#endif

int FreeModule::n_terms(vec v) const
{
  int result = 0;
  for ( ; v != NULL; v = v->next)
    result++;
  return result;
}

vec FreeModule::get_terms(vec v, int lo, int hi) const
{
  vecterm head;
  vecterm *result = &head;
  int nmons = n_terms(v);
  if (lo < 0) lo = nmons + lo;
  if (hi < 0) hi = nmons + hi;
  int n = 0;
  while (v != NULL)
    {
      if (n > hi) break;
      if (n >= lo)
	{
	  result->next = copy_term(v);
	  result = result->next;
	}
      v = v->next;
      n++;
    }
  result->next = NULL;
  return head.next;
}

int FreeModule::lead_component(vec v) const
{
  if (v == NULL) return -1;
  return v->comp;
}

ring_elem FreeModule::lead_coefficient(vec v) const
{
  if (v == NULL) 
    return R->from_int(0);
  return v->coeff;
}

//////////////////////////////////////////////
//  Addition, subtraction ////////////////////
//////////////////////////////////////////////

int FreeModule::compare(const vecterm *t, const vecterm *s) const
{
  int cmp = t->comp - s->comp;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}

void FreeModule::add_to(vec &f, vec &g) const
{
  if (g == NULL) return;
  if (f == NULL) { f = g; g = NULL; return; }
  vecterm head;
  vecterm *result = &head;
  while (1)
    switch (compare(f, g))
      {
      case LT:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f; 
	    f = head.next;
	    return;
	  }
	break;
      case GT:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    f = head.next;
	    g = NULL;
	    return;
	  }
	break;
      case EQ:
	vecterm *tmf = f;
	vecterm *tmg = g;
	f = f->next;
	g = g->next;
	R->add_to(tmf->coeff, tmg->coeff);
	if (!R->is_zero(tmf->coeff))
	  {
	    result->next = tmf;
	    result = result->next;
	  }
	if (g == NULL) 
	  {
	    result->next = f; 
	    f = head.next;
	    return;
	  }
	if (f == NULL) 
	  {
	    result->next = g; 
	    f = head.next;
	    g = NULL;
	    return;
	  }
	break;
      }
}

void FreeModule::negate_to(vec &v) const
{
  vec w = v;
  while (w != NULL)
    {
      R->negate_to(w->coeff);
      w = w->next;
    }
}

void FreeModule::subtract_to(vec &v, vec &w) const
{
  negate_to(w);
  add_to(v, w);
}

vec FreeModule::negate(vec v) const
{
  vecterm result;
  vecterm *b = &result;
  for (vecterm *a = v; a != NULL; a = a->next)
    {
      b->next = raw_term(R->negate(a->coeff), a->comp);
      b = b->next;
    }
  b->next = NULL;
  return result.next;
}

vec FreeModule::add(vec v, vec w) const
{
  vec f = copy(v);
  vec g = copy(w);
  add_to(f, g);
  return f;
}

vec FreeModule::subtract(vec v, vec w) const
{
  vec f = copy(v);
  vec g = negate(w);
  add_to(f, g);
  return f;
}


vec FreeModule::mult(int n, vec v) const
{
  ring_elem f = R->from_int(n);
  vec result = mult(f, v);
  R->remove(f);
  return result;
}

vec FreeModule::mult(const ring_elem f, const vec w) const
{
  if (R->is_zero(f)) return NULL;
  vecterm head;
  vec result = &head;
  for (vec v = w ; v != 0; v = v->next)
    {
      ring_elem a = R->mult(f,v->coeff);
      if (!R->is_zero(a))
	{
	  vec t = raw_term(a, v->comp);
	  result->next = t;
	  result = t;
	}
    }
  result->next = NULL;
  return head.next;
}

vec FreeModule::rightmult(const vec w, const ring_elem f) const
{
  if (R->is_zero(f)) return NULL;
  vecterm head;
  vec result = &head;
  for (vec v = w ; v != 0; v = v->next)
    {
      ring_elem a = R->mult(v->coeff,f);
      if (!R->is_zero(a))
	{
	  vec t = raw_term(a, v->comp);
	  result->next = t;
	  result = t;
	}
    }
  result->next = NULL;
  return head.next;
}


//////////////////////////////////////////////
//  Matrix routines //////////////////////////
//////////////////////////////////////////////

vec FreeModule::component_shift(int n, const FreeModule *F, 
			  vec v) const
{
  vecterm head;
  vecterm *result = &head;

  for ( ; v != NULL; v = v->next)
    {
      result->next = copy_term(v);
      result = result->next;
      result->comp += n;
    }
  result->next = NULL;
  return head.next;
}

vec FreeModule::tensor_shift(int n, int m, 
		       const FreeModule *F,
		       vec v) const
{
  vecterm head;
  vecterm *result = &head;

  for ( ; v != NULL; v = v->next)
    {
      result->next = copy_term(v);
      result = result->next;
      result->comp = n * result->comp + m;
    }
  result->next = NULL;
  return head.next;
}

vec FreeModule::sub_vector(const FreeModule *F, vec v, 
				const M2_arrayint r) const
{
  intarray trans(F->rank());
  int i;
  for (i=0; i<F->rank(); i++)
    trans.append(-1);

  for (unsigned j=0; j<r->len; j++)
    if (r->array[j] >= 0 && r->array[j] < F->rank())
      trans[r->array[j]] = j;

  vecterm head;
  vecterm *result = &head;
  for ( ; v != NULL; v = v->next)
    if (trans[v->comp] != -1)
      {
	result->next = copy_term(v);
	result = result->next;
	result->comp = trans[v->comp];
      }
  result->next = NULL;
  result = head.next;

  sort(result);
  return result;
}
void FreeModule::reshape(const Matrix &m, Matrix &result) const
{
  // Assumptions: m and result have the same number of entries.

  int r, c;
  for (c=0; c<m.n_cols(); c++)
    for (vecterm *p = m.elem(c); p != NULL; p = p->next)
      {
	vecterm *q = copy_term(p);
	r = p->comp;

	// Determine new component
	int loc = c * m.n_rows() + r;
	int result_col = loc / result.n_rows();
	int result_row = loc % result.n_rows();
	
	q->comp = result_row;
	q->next = result.elem(result_col);
	result.elem(result_col) = q;
      }
  for (c=0; c<result.n_cols(); c++)
    sort(result.elem(c));
}

void FreeModule::transpose_matrix(const Matrix &m, Matrix &result) const
{
  // Assumption: 'this' == result.rows()
  int r, c;
  for (c=0; c<m.n_cols(); c++)
    for (vecterm *p = m.elem(c); p != NULL; p = p->next)
      {
	vecterm *q = copy_term(p);
	r = q->comp;
	q->comp = c;
	q->next = result.elem(r);
	result.elem(r) = q;
      }
  for (c=0; c<result.n_cols(); c++)
    sort(result.elem(c));
}

vec FreeModule::mult_by_matrix(const Matrix *m,
				 const FreeModule *F, 
				 vec v) const
{
  // Each loop below should read
  // result = 0
  // for each non-zero term f e[component] of the vector v
  //    result += f M[v]
  vec result = NULL, f;
  for ( ; v != NULL; v = v->next)
    {
      f = mult(v->coeff, m->elem(v->comp));
      add_to(result, f);
    }
  return result;
}

vec FreeModule::tensor(const FreeModule *F, vec v, 
			 const FreeModule *G, vec w) const
{
  vecHeap H(this);
  for ( ; v != NULL; v = v->next)
    {
      vec w1 = component_shift(v->comp * G->rank(),
			       G,w);
      vec w2 = mult(v->coeff, w1);
      remove(w1);
      H.add(w2);
    }
  return H.value();
}


///////////////////////////////////////////////////
// Sorting a list of vectors (and maybe degrees) //
///////////////////////////////////////////////////

static int deg_ascending;
static int monorder_ascending;
static int * sort_vals;
static const array<vec> * sort_vecs;
static const int * sort_degs;

int FreeModule::sort_compare(int i, int j) const
{
  if (i == j) return 0;
  if (deg_ascending != 0)
    {
      int d1 = sort_degs[i];
      int d2 = sort_degs[j];
      if (d1 > d2) return -deg_ascending;
      if (d1 < d2) return deg_ascending;
    }
  vec v1 = (*sort_vecs)[i];
  vec v2 = (*sort_vecs)[j];
  if (v1 == NULL) return -monorder_ascending;
  if (v2 == NULL) return monorder_ascending;
  int cmp = compare(v1, v2);
  if (cmp > 0) return -monorder_ascending;
  if (cmp < 0) return monorder_ascending;  
#if 0
  if (K->is_ZZ())
    {
      // Compare coeficients as well.
      cmp = K->cast_to_Z()->compare(v1->coeff, v2->coeff);
      buffer o;
      o << "comparing ";
      K->elem_text_out(o, v1->coeff);
      o << " and ";
      K->elem_text_out(o, v2->coeff);
      o << " result = " << cmp << newline;
      emit(o.str());
      if (cmp < 0) return 1;
      if (cmp > 0) return -1;
    }
#endif
  return 0;
}

int FreeModule::sort_partition(int lo, int hi) const
{
  int pivot = sort_vals[lo];
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
      do { j--; }
      while (sort_compare(sort_vals[j], pivot) < 0);
      do { i++; }
      while (sort_compare(sort_vals[i], pivot) > 0);

      if (i < j)
	{
	  int tmp = sort_vals[j];
	  sort_vals[j] = sort_vals[i];
	  sort_vals[i] = tmp;
	}
      else
	return j;
    }
}

void FreeModule::sort_range(int lo, int hi) const
{
  if (lo < hi)
    {
      int q = sort_partition(lo, hi);
      sort_range(lo, q);
      sort_range(q+1, hi);
    }
}

M2_arrayint_OrNull 
FreeModule::sort(const array<vec> &vecs, 
		 const M2_arrayint degrees, 
		 int degorder, // -1=descending, 0=don't use, 1=ascending
		 int monorder // -1=descending, 1=ascending.
		 ) const
{
  M2_arrayint result = makearrayint(vecs.length());
  if (vecs.length() == 0) return result;

  monorder_ascending = monorder;
  deg_ascending = degorder;

  sort_vals = result->array;
  for (int i=0; i<vecs.length(); i++)
    sort_vals[i] = i;

  sort_vecs = &vecs;
  sort_degs = NULL;
  if (deg_ascending) {
    if (degrees->len != (unsigned int)(vecs.length())) {
	ERROR("sort: specified degree order, without giving degrees");
	return 0;
      }
    sort_degs = degrees->array;
  }

  sort_range(0,vecs.length()-1);
  sort_vals = NULL;
  sort_degs = NULL;
  return result;
}

//////////////////////////////////////////////
//  Divisibility checks               ////////
//                                    ////////
//////////////////////////////////////////////
#if 0
int FreeModule::is_scalar_multiple(vec f, vec g) const
  // is df = cg, some scalars c,d?
{
  if (f == NULL) return 1;
  if (g == NULL) return 1;
  ring_elem c = f->coeff;
  ring_elem d = g->coeff;
  vec p,q;
  for (p=f, q=g; p != NULL && q != NULL; p=p->next, q=q->next)
    {
      if (p->comp != q->comp) return 0;
      if (M->compare(p->monom, q->monom) != 0) return 0;
    }
  for (p=f, q=g; p != NULL && q != NULL; p=p->next, q=q->next)
    {
      ring_elem c1 = K->mult(c, q->coeff);
      ring_elem d1 = K->mult(d, p->coeff);
      int isequal = K->is_equal(c1, d1);
      K->remove(c1);
      K->remove(d1);
      if (!isequal) return 0;
    }
  if (q == NULL && p == NULL) return 1;
  return 0;
}

void FreeModule::monomial_divisor(vec f, int *exp) const
{
  if (f == NULL || M == NULL) return;
  int *m = M->make_one();
  int *exp1 = new int[M->n_vars()];
  M->divide(f->monom, base_monom(f->comp), m);
  M->to_expvector(m, exp);

  for (vec a = f->next; a != NULL; a = a->next)
    {
      M->divide(a->monom, base_monom(a->comp), m);
      M->to_expvector(m, exp1);
      ntuple::gcd(M->n_vars(), exp, exp1, exp);
    }

  delete [] exp1;
  M->remove(m);
}

vec FreeModule::monomial_squarefree(vec f) const
{
  if (M == NULL) return copy(f);
  int *exp = new int[R->n_vars()];
  monomial_divisor(f, exp);
  // Now divide each term by exp[i]-1, if exp[i] >= 2
  for (int i=0; i<M->n_vars(); i++)
    if (exp[i] >= 1) exp[i]--;

  // Divide f by exp:
  vec result = divide_by_expvector(exp, f);

  delete [] exp;
  return result;
}

vec FreeModule::remove_monomial_divisors(vec f) const
{
  if (M == NULL) return copy(f);
  int *exp = new int[R->n_vars()];
  monomial_divisor(f, exp);

  // Divide f by exp:
  vec result = divide_by_expvector(exp, f);

  delete [] exp;
  return result;
}
#endif

vec FreeModule::diff(const FreeModule * F, vec v, 
		       const FreeModule * G, vec w,
		       int use_coeff) const
{
  vec result = NULL;
  for ( ; v != NULL; v = v->next)
    for (vecterm *p = w; p != NULL; p = p->next)
      {
	ring_elem a = R->diff(v->coeff, p->coeff, use_coeff);
	if (R->is_zero(a)) 
	  {
	    R->remove(a);
	    continue;
	  }
	vecterm *t = new_term();
	t->comp = G->rank() * v->comp + p->comp; // MES: is this right??!
	t->coeff = a;
	t->next = result;
	result = t;
      }
  sort(result);
  return result;
}


int FreeModule::in_subring(int n, const vec v) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0 || v == NULL) return true;
  const Monoid *M = P->Nmonoms();
  for (vec w = v ; w != NULL; w = w->next)
    if (!M->in_subring(n, P->lead_monomial(w->coeff)))
      return false;
  return true;
}

#if 0
vec FreeModule::coefficient_of_var(vec v, int var, int exp) const
{
  if (M == NULL) return copy(v);
  vecterm head;
  vecterm *result = &head;
  for (vecterm *t = v; t != NULL; t = t->next)
    {
      M->to_expvector(t->monom, nf_exp);
      if (nf_exp[var] != exp) continue;

      nf_exp[var] = 0;

      result->next = new_term();
      result = result->next;
      result->comp = t->comp;
      result->coeff = K->copy(t->coeff);
      M->from_expvector(nf_exp, result->monom);
    }
  result->next = NULL;
  return head.next;
}

vec FreeModule::lead_var_coefficient(vec &v, int &var, int &exp) const
{
  var = -1;
  exp = 0;
  if (M == NULL) return NULL;
  if (v == NULL)
    {
      return NULL;
    }
  M->divide(v->monom, base_monom(v->comp), nf_1);
  M->to_expvector(nf_1, nf_exp);
  for (int i=0; i<M->n_vars(); i++)
    if (nf_exp[i] > 0) 
      {
	var = i;
	exp = nf_exp[i];
	break;
      }

  for (vecterm *t = v->next; t != NULL; t = t->next)
    {
      M->divide(t->monom, base_monom(t->comp), nf_1);
      M->to_expvector(nf_1, nf_exp);
      for (int j=0; j<var; j++)
	{
	  if (nf_exp[j] > 0)
	    {
	      var = j;
	      exp = nf_exp[j];
	      break;
	    }
	  if (nf_exp[var] > exp)
	    exp = nf_exp[var];
	}
    }
  // Now we have the variable, and its exponent.
  
  return coefficient_of_var(v, var, exp);
}
#endif

void FreeModule::degree_of_var(int n, const vec v, int &lo, int &hi) const
{
  if (v == NULL)
    {
      ERROR("attempting to find degree of zero vector");
      return;
    }
  R->degree_of_var(n, v->coeff, lo, hi);
  for (vec w = v->next; w != 0; w = w->next)
    {
      int lo1,hi1;
      R->degree_of_var(n, w->coeff, lo1, hi1);
      if (lo1 < lo) lo = lo1;
      if (hi1 > hi) hi = hi1;
    }
}

vec FreeModule::divide_by_var(int n, int d, const vec v) const
{
  vecterm head;
  vecterm *result = &head;
  for (vec w = v; w != 0; w = w->next)
    {
      ring_elem a = R->divide_by_var(n, d, w->coeff);
      if (!R->is_zero(a))
	{
	  vec t = raw_term(a, w->comp);
	  result->next = t;
	  result = t;
	}
    }
  result->next = 0;
  return head.next;
}

vec FreeModule::divide_by_expvector(const int *exp, const vec v) const
{
  vecterm head;
  vecterm *result = &head;
  for (vec w = v; w != 0; w = w->next)
    {
      ring_elem a = R->divide_by_expvector(exp, w->coeff);
      if (!R->is_zero(a))
	{
	  vec t = raw_term(a, w->comp);
	  result->next = t;
	  result = t;
	}
    }
  result->next = 0;
  return head.next;
}

//////////////////////////////////////////////
//  Homogeniety and the grading //////////////
//////////////////////////////////////////////

bool FreeModule::multi_degree(const vec f, int *degf) const
  // Returns true if the element is homogeneous
  // Sets degf to be the highest degree found (actually, the join of the 
  //   degree vectors occuring).
{
  int *degv;
  degree_monoid()->one(degf);
  if (f == NULL) return true;
  bool result = R->multi_degree(f->coeff, degf);
  degree_monoid()->mult(degf, degree(f->comp), degf);
  degv = degree_monoid()->make_one();

  for (vec v = f->next; v != 0; v = v->next)
    {
      bool ishom = R->multi_degree(v->coeff, degv);
      result = result && ishom;
      degree_monoid()->mult(degv, degree(v->comp), degv);

      if (0 != degree_monoid()->compare(degf, degv))
	{
	  result = false;
	  degree_monoid()->lcm(degf, degv, degf);
	}
    }
  degree_monoid()->remove(degv);
  return result;
}

void FreeModule::degree(const vec f, int *degf) const
{
  multi_degree(f, degf);
}

#if 0
bool FreeModule::is_homogeneous(const vec f) const
{
  if (f == NULL) return true;
  int *d = degree_monoid()->make_one();
  int *e = degree_monoid()->make_one();
  bool result = R->multi_degree(f->coeff, d);
  if (!result) return false;
  degree_monoid()->mult(d, degree(f->comp), d);
  for (vecterm *t = f->next; (t != NULL) && result; t = t->next)
    {
      bool ishom = R->multi_degree(t->coeff, e);
      if (!ishom) return false;
      degree_monoid()->mult(e, degree(t->comp), e);

      if (0 != degree_monoid()->compare(d,e))
	result = false;
    }
  degree_monoid()->remove(d);
  degree_monoid()->remove(e);
  return result;
}
#endif

bool FreeModule::is_homogeneous(const vec f) const
{
  if (f == NULL) return true;
  int *d = degree_monoid()->make_one();
  int *e = degree_monoid()->make_one();
  bool result = R->multi_degree(f->coeff, d);
  if (result) 
    {
      degree_monoid()->mult(d, degree(f->comp), d);
      for (vecterm *t = f->next; (t != NULL) && result; t = t->next)
	{
	  bool ishom = R->multi_degree(t->coeff, e);
	  result = result && ishom;
	  if (result)
	    {
	      degree_monoid()->mult(e, degree(t->comp), e);
	      if (0 != degree_monoid()->compare(d,e))
		result = false;
	    }
	}
    }
  degree_monoid()->remove(d);
  degree_monoid()->remove(e);
  return result;
}

void FreeModule::change_degree(int i, const int *deg)
{ 
  // WARNING: this modifies the degree, and should only be used during
  // the construction of a free module (or matrix).
  assert(i >= 0);
  assert(i < rank());
  degree_monoid()->copy(deg, components[i]);
}

int FreeModule::primary_degree(const vec f) const
{
  if (f == NULL) return 0;
  int deg = R->primary_degree(f->coeff);

  return primary_degree(f->comp) + deg;
}

void FreeModule::degree_weights(const vec f, const M2_arrayint wts, int &lo, int &hi) const
{
  vecterm *t = f;
  if (t == NULL)
    {
      lo = hi = 0;
      return;
    }
  R->degree_weights(t->coeff, wts, lo, hi);
  lo += primary_degree(t->comp);
  hi += primary_degree(t->comp);
  for (t = t->next; t != NULL; t = t->next)
    {
      int lo1, hi1;
      R->degree_weights(t->coeff, wts, lo1, hi1);
      lo1 += primary_degree(t->comp);
      hi1 += primary_degree(t->comp);
      if (hi1 > hi) hi = hi1;
      if (lo1 < lo) lo = lo1;
    }
}

vec FreeModule::homogenize(const vec f, 
			   int v, int d, const M2_arrayint wts) const
  // Any terms which can't be homogenized are silently set to 0
{
  vecterm head;
  vecterm *result = &head;
  assert(wts->array[v] != 0);
  // If an error occurs, then return 0, and set ERROR

  for (vec w = f; w != 0; w = w->next)
    {
      int e = primary_degree(w->comp);
      ring_elem a = R->homogenize(w->coeff, v, d-e, wts);
      if (!R->is_zero(a))
	{
	  result->next = raw_term(a, w->comp);
	  result = result->next;
	}
    }
  result->next = 0;
  return head.next;
}

vec FreeModule::homogenize(const vec f, int v, const M2_arrayint wts) const
{
  vecterm *result = NULL;
  if (f == NULL) return result;
  int lo, hi;
  degree_weights(f, wts, lo, hi);
  assert(wts->array[v] != 0);
  int d = (wts->array[v] > 0 ? hi : lo);
  return homogenize(f, v, d, wts);
}

vec FreeModule::random() const
{
  vec result = NULL;
  for (int i=0; i<rank(); i++)
    {
      vec v = raw_term(R->random(),i);
      if (v != NULL)
	{
	  v->next = result;
	  result = v;
	}
    }
  return result;
}
//////////////////////////////////////////////
//  Translation and sorting routines /////////
//////////////////////////////////////////////

#if 0
vec FreeModule::resize(const FreeModule *oldF, vec f) const
    // assumptions: (1) f is a polynomial in the ring R.
    // (2) the current ring is the same as 'R', except for
    // the size of monomials (i.e: same base ring, same 
    // monomial order, same degree info)
{
  if (M == NULL) return copy(f);
  vecterm head;
  vecterm *result = &head;
  intarray expa;
  int *exp = expa.alloc(R->n_vars());
  for (vecterm *t = f; t != NULL; t = t->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K->copy(t->coeff);
      oldF->M->to_expvector(t->monom, exp);
      M->from_expvector(exp, result->monom);
    }
  result->next = NULL;
  return head.next;
}
#endif

void FreeModule::sort(vecterm *&f) const
{
  // Divide f into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.
  
  if (f == NULL || f->next == NULL) return;
  vecterm *f1 = NULL;
  vecterm *f2 = NULL;
  while (f != NULL)
    {
      vecterm *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == NULL) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }
  
  sort(f1);
  sort(f2);
  add_to(f1, f2);
  f = f1;
}

vec FreeModule::translate(const FreeModule *oldF, vec v) const
{
  // MES: fix the following aspects of this function.
  // Check the following:
  // K == F->K
  // The number of variables for R, F->R are the same.
  // Determine if sorting is needed.

  return copy(v);
#if 0  
  if (M == NULL || this == oldF) return copy(v);
//  if (rank() != oldF->rank()) {
//       gError << "expected free modules of the same rank";
//       return NULL;
//  }
  int needs_sorting = 1;
  vecterm head;
  vecterm *result = &head;
  intarray expa;
  int *exp = expa.alloc(R->n_vars());
  int *m = oldF->M->make_one();
  for (vecterm *t = v; t != NULL; t = t->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = t->comp;
      result->coeff = K->copy(t->coeff);
      oldF->M->divide(t->monom, oldF->base_monom(t->comp), m);
      oldF->M->to_expvector(m, exp);
      M->from_expvector(exp, result->monom);
      M->mult(base_monom(t->comp),result->monom,result->monom);
    }
  oldF->M->remove(m);

  result->next = NULL;
  if (needs_sorting) sort(head.next);
  return head.next;
#endif
}


//////////////////////////////////////////////
//  Input, output ////////////////////////////
//////////////////////////////////////////////

void FreeModule::elem_text_out(buffer &o, const vec v) const
{
  if (v == NULL)
    {
      o << "0";
      return;
    }

  int old_one = p_one;
  int old_parens = p_parens;
  int old_plus = p_plus;

  p_one = 0;
  for (vecterm *t = v; t != NULL; t = t->next)
    {
      R->elem_text_out(o,t->coeff);
      o << "<" << t->comp << ">";
      p_plus = 1;
    }

  p_one = old_one;
  p_parens = old_parens;
  p_plus = old_plus;
}

vec FreeModule::eval(const RingMap *map, const FreeModule *F,
			  const vec v) const
{
  vecterm head;
  vec result = &head;

  for (vec t = v; t != 0; t = t->next)
    {
      ring_elem a = R->eval(map, t->coeff);
      if (!R->is_zero(a))
	{
	  result->next = raw_term(a,t->comp);
	  result = result->next;
	}
    }
  result->next = 0;
  return head.next;
}
