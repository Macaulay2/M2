// Copyright 1995  Michael E. Stillman

#include "freemod.hpp"
#include "comb.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "ringmap.hpp"
#include "ntuple.hpp"

#include "geovec.hpp"

stash *FreeModule::mystash;

vec FreeModule::new_term() const
{
  return (vec) R->vecstash->new_elem();
}
vec FreeModule::copy_term(vec v) const
{
  vec result = new_term();
  result->comp = v->comp;
  result->coeff = K->copy(v->coeff);
  result->next = NULL;
  if (M != NULL)
    M->copy(v->monom, result->monom);
  return result;
}
vec FreeModule::new_term(int e, ring_elem a, const int *m) const
{
  // This is a low-level routine.  Assumptions:
  // a is not to be copied, and is not 0.
  // the result is not reduced, i.e. it is a single term.

  vec result = new_term();
  result->next = NULL;
  result->comp = e;
  result->coeff = a;
  if (M != NULL)
    M->copy(m, result->monom);
  return result;
}

vec FreeModule::from_varpower(const int *vp, int x) const
{
  if (M == NULL) return NULL;
  ring_elem a = K->from_int(1);
  int *m = M->make_one();
  M->from_varpower(vp, m);
  if (M->is_skew() && M->skew_is_zero(m))
    {
      M->remove(m);
      K->remove(a);
      return NULL;
    }
  vec result = term(x,a,m);
  K->remove(a);
  M->remove(m);
  return result;
}

void FreeModule::lead_varpower(const vec v, intarray &vp) const
{
  assert(v != NULL);
  if (M == NULL)
    varpower::one(vp);
  else
    {
      int *m = M->make_one();
      M->divide(v->monom, base_monom(v->comp), m);
      M->to_varpower(m, vp);
      M->remove(m);
    }
}

vec FreeModule::term(int e, ring_elem a, const int *m) const
{
  // Higher level term construction
  // MES: should this routine: (1) copy a, (2) reduce the result?, 
  // (3) check whether a is NULL.  IE how low level should this be?
  // (4) add base_monom to m, in case of FREE_SCHREYER.

  if (K->is_zero(a)) return NULL;
  vec result = new_term();
  result->next = NULL;
  result->comp = e;
  result->coeff = K->copy(a);
  switch (ty)
    {
    case FREE:
      break;
    case FREE_POLY:
      M->copy(m, result->monom);
      if (is_quotient_ring) normal_form(result);
      break;
    case FREE_SCHREYER:
      M->mult(base_monom(e), m, result->monom);
      if (is_quotient_ring) normal_form(result);
      break;
    }
  return result;
}

vec FreeModule::e_sub_i(int i) const
{
  // MES: if (R->is_zero_ring()) return NULL;
  vec result = new_term();
  result->next = NULL;
  result->comp = i;
  result->coeff = K->from_int(1);
  switch (ty)
    {
    case FREE:
      break;
    case FREE_POLY:
      M->one(result->monom);
      if (is_quotient_ring) normal_form(result);
      break;
    case FREE_SCHREYER:
      M->copy(base_monom(i), result->monom);
      if (is_quotient_ring) normal_form(result);
      break;
    }

  return result;
}

vec FreeModule::term(int e, ring_elem a) const
    // MES: handle this by "mult(a, e_sub_i(e))" ??  Sounds good.  This is not 
    // time critical.
{
  if (R->is_zero(a)) return NULL;
  vec v = e_sub_i(e);
  vec result = mult(a, v);
  remove(v);
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
      if (!K->is_equal(a->coeff, b->coeff)) return false;
      if (M != NULL && M->compare(a->monom, b->monom) != 0)
	return false;
    }
}



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
  while (v != NULL)
    {
      vecterm *tmp = v;
      v = v->next;
      K->remove(tmp->coeff);
      R->vecstash->delete_elem(tmp);
    }
}

ring_elem FreeModule::get_coefficient(vec v, int e) const
{
  switch (ty)
    {
    case FREE:
      while (v != NULL)
	{
	  if (v->comp == e) return K->copy(v->coeff);
	  if (v->comp < e) return K->from_int(0);
	  v = v->next;
	}
      return K->from_int(0);
    case FREE_POLY:
    case FREE_SCHREYER:
      const PolynomialRing *P = R->cast_to_poly_ring();
      assert(P != NULL);
      Nterm head;
      Nterm *result = &head;
      const index_type *x = component(e);
      for ( ; v != NULL; v = v->next)
	{
	  if (v->comp != e) continue;
	  result->next = P->new_term();
	  result = result->next;
	  result->coeff = K->copy(v->coeff);
	  M->divide(v->monom, x->base_monom, result->monom);
	}
      result->next = NULL;
      return head.next;
    }
  return NULL;
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
  if (v == NULL) 
    {
      //throw Exception("lead_component: vector is zero");
      assert(0);
      return 0;
    }
  return v->comp;
}

ring_elem FreeModule::lead_coefficient(vec v) const
{
  if (v == NULL) 
    {
      //throw Exception("lead_coefficient: vector is zero");
      assert(0);
      return K->from_int(0);
    }
  return K->copy(v->coeff);
}

int FreeModule::n_terms(vec v) const
{
  int result = 0;
  for ( ; v != NULL; v = v->next)
    result++;
  return result;
}

//////////////////////////////////////////////
//  Addition, subtraction ////////////////////
//////////////////////////////////////////////

int FreeModule::compare(const vecterm *t, const vecterm *s) const
{
  // MES: this should use compare_num's.
  int cmp;
  if (M != NULL)
    {
      cmp = M->compare(t->monom, s->monom);
      if (cmp != 0) return cmp;
    }
  cmp = component(t->comp)->compare_num - component(s->comp)->compare_num;
  // The following replaced.  We will see how much this breaks things...!
  //cmp = t->comp - s->comp;
  if (cmp < 0) return -1;
  if (cmp > 0) return 1;
  return 0;
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
      case -1:
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
      case 1:
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
      case 0:
	vecterm *tmf = f;
	vecterm *tmg = g;
	f = f->next;
	g = g->next;
	K->add_to(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  {
	    K->remove(tmf->coeff);
	    R->vecstash->delete_elem(tmf);
	  }
	else
	  {
	    result->next = tmf;
	    result = result->next;
	  }
	K->remove(tmg->coeff);
	R->vecstash->delete_elem(tmg);
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
      K->negate_to(w->coeff);
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
      b->next = new_term(a->comp, K->negate(a->coeff), a->monom);
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

//////////////////////////////////////////////
//  Divisibility checks               ////////
//                                    ////////
//////////////////////////////////////////////

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

//////////////////////////////////////////////
//  Multiplication: may be overridden ////////
//  by inheritance                    ////////
//////////////////////////////////////////////

vec FreeModule::imp_mult_by_coeff(const ring_elem c, vec v) const
   // return c*v
{
  vecterm head;
  vecterm *result = &head;
  for (vecterm *a = v; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = a->comp;
      result->coeff = K->mult(a->coeff, c);
      if (M != NULL)
	M->copy(a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

vec FreeModule::imp_skew_mult_by_term(const ring_elem c,
			       const int *m, vec v) const
   // return c*m*v
{
  vecterm head;
  vecterm *result = &head;
  ring_elem minus_c = K->negate(c);
  vecterm *nextterm = new_term();
  for (vecterm *a = v; a != NULL; a = a->next)
    {
      M->divide(a->monom, base_monom(a->comp), nextterm->monom);
      int sign = M->skew_mult(m, nextterm->monom, nextterm->monom);
      M->mult(nextterm->monom, base_monom(a->comp), nextterm->monom);
      if (sign == 0) 
	continue;
      else if (sign > 0)
	nextterm->coeff = K->mult(a->coeff, c);
      else
	nextterm->coeff = K->mult(a->coeff, minus_c);

      nextterm->comp = a->comp;
      result->next = nextterm;
      result = result->next;
      nextterm = new_term();
    }
  nextterm->next = NULL;
  remove(nextterm);
  result->next = NULL;
  return head.next;
}

vec FreeModule::imp_mult_by_term(const ring_elem c, const int *m, const vec v) const
   // return c*m*v
{
  if (M != NULL && M->is_skew())
    {
      return imp_skew_mult_by_term(c,m,v);
    }
  vecterm head;
  vecterm *result = &head;
  for (vecterm *a = v; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = a->comp;
      result->coeff = K->mult(a->coeff, c);
      if (M != NULL)
	M->mult(m, a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

void FreeModule::imp_subtract_multiple_to(vec &v, 
				 ring_elem c, const int *m, const vec w) const
{
  ring_elem b = K->negate(c);
  vec h = imp_mult_by_term(b, m, w);
  add_to(v, h);
  K->remove(b);
}

vec FreeModule::mult_by_coeff(const ring_elem c, vec v) const
   // return c*v
{
  vec result = imp_mult_by_coeff(c,v);
  if (is_quotient_ring) normal_form(result);
  return result;
}

vec FreeModule::mult_by_term(const ring_elem c, const int *m, const vec v) const
   // return c*m*v
{
  vecterm *result = imp_mult_by_term(c, m, v);
  if (is_quotient_ring) normal_form(result);
  return result;
}

vec FreeModule::right_mult_by_term(vec v, ring_elem c, const int *m) const
   // return v*c*m
{
  return mult_by_term(c, m, v);
}

void FreeModule::subtract_multiple_to(vec &v, ring_elem c, 
				    const int *m, const vec w) const
    // v := v - c * m * w
{
  ring_elem minus_c = K->negate(c);
  vec u = mult_by_term(minus_c, m, w);
  add_to(v, u);
  K->remove(minus_c);
}


vec FreeModule::mult(int n, vec v) const
{
  ring_elem f = R->from_int(n);
  vec result = mult(f, v);
  R->remove(f);
  return result;
}

vec FreeModule::mult(const ring_elem f, const vec v) const
{
  if (R->is_zero(f)) return NULL;
  vec result = NULL;
  if (M == NULL)
    result = mult_by_coeff(f, v);
  else
    for (Nterm *a = f; a != NULL; a = a->next)
      {
	vec h = mult_by_term(a->coeff, a->monom, v);
	add_to(result, h);
      }
  return result;
}

vec FreeModule::rightmult(const vec v, const ring_elem f) const
{
  return mult(f,v);
}

//////////////////////////////////////////////
//  Normal forms /////////////////////////////
//////////////////////////////////////////////
void FreeModule::imp_cancel_lead_term(vec &f, 
				      vec g, 
				      ring_elem &coeff, 
				      int *monom) const
{
  if (f == NULL || g == NULL) return;
  coeff = K->divide(f->coeff, g->coeff);
  if (M->is_skew())
    {
      M->divide(f->monom, base_monom(f->comp), f->monom);
      M->divide(g->monom, base_monom(g->comp), g->monom);
      int sign = M->skew_divide(f->monom, g->monom, monom);
      M->mult(f->monom, base_monom(f->comp), f->monom);
      M->mult(g->monom, base_monom(g->comp), g->monom);
      if (sign < 0) K->negate_to(coeff);
      imp_subtract_multiple_to(f, coeff, monom, g);
    }
  else
    {
      M->divide(f->monom, g->monom, monom);
      imp_subtract_multiple_to(f, coeff, monom, g);
    }
}
void FreeModule::imp_ring_cancel_lead_term(vec &f, 
					   ring_elem gg, 
					   ring_elem &coeff, 
					   int *monom) const
{
  Nterm *g = gg;
  if (f == NULL || g == NULL) return;
  coeff = K->divide(f->coeff, g->coeff);
  if (M->is_skew())
    {
      M->divide(f->monom, base_monom(f->comp), f->monom);
      int sign = M->skew_divide(f->monom, g->monom, monom);
      M->mult(f->monom, base_monom(f->comp), f->monom);
      if (sign < 0) K->negate_to(coeff);
      imp_subtract_ring_multiple_to(f, coeff, monom, g);
    }
  else
    {
      M->divide(f->monom, g->monom, monom);
      imp_subtract_ring_multiple_to(f, coeff, monom, g);
    }
}




vec FreeModule::imp_skew_ring_mult_by_term(
		    const ring_elem f,
		    const ring_elem c,
		    const int *m, 
                    int x) const
   // return c*m*f*e_x
{
  vecterm head;
  vecterm *result = &head;
  ring_elem minus_c = K->negate(c);
  vecterm *nextterm = new_term();
  M->divide(m, base_monom(x), (int *) m);
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      int sign = M->skew_mult(m, a->monom, nextterm->monom);
      M->mult(nextterm->monom, base_monom(x), nextterm->monom);
      if (sign == 0) 
	continue;
      else if (sign > 0)
	nextterm->coeff = K->mult(a->coeff, c);
      else
	nextterm->coeff = K->mult(a->coeff, minus_c);

      nextterm->comp = x;
      result->next = nextterm;
      result = result->next;
      nextterm = new_term();
    }
  M->mult(m, base_monom(x), (int *) m);
  nextterm->next = NULL;
  remove(nextterm);
  result->next = NULL;
  return head.next;
}
vec FreeModule::imp_ring_mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m, int x) const
   // return c*m*f*e_x
   // This is an internal routine: 'm' should be the total monomial to multiply
{
  assert(M != NULL);		// This should only be called by normal_form,
				// which will check this first.
  if (M != NULL && M->is_skew())
    {
      return imp_skew_ring_mult_by_term(f,c,m,x);
    }
  vecterm head;
  vecterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K->mult(a->coeff, c);
      result->comp = x;
      M->mult(m, a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}
void FreeModule::imp_subtract_ring_multiple_to
  (vec &f, ring_elem a, const int *m, const Nterm *g) const
    // f := f - a * m * g
{
  // Caution: this returns an element which is not in normal form.
  ring_elem minus_a = K->negate(a);
  Nterm *g1 = (Nterm *) g;
  vec result = imp_ring_mult_by_term(g1, minus_a, m, f->comp);
  add_to(f, result);
  K->remove(minus_a);
}
void FreeModule::normal_form(vec &v) const
{
  assert(M != NULL);		// This routine should only be called
				// if is_quotient_ring is set, which should
				// only be set if M != NULL.
  const PolynomialRing *P = R->cast_to_poly_ring();
  vecterm head;
  vecterm *result = &head;

  vecterm *t = v;
  while (t != NULL)
    {
      if (ty == FREE_POLY)
	M->to_expvector(t->monom, nf_exp);
      else
	{
	  M->divide(t->monom, base_monom(t->comp), nf_1);
	  M->to_expvector(nf_1, nf_exp);
	}

      int_bag *b;
      if (P->Rideal.search_expvector(nf_exp, b))
	{
	  Nterm *s = (Nterm *) (b->basis_ptr());
	  ring_elem coeff;
	  imp_ring_cancel_lead_term(t, s, coeff, nf_1);
	  K->remove(coeff);
	  //M->divide(t->monom, s->monom, nf_1);
	  //imp_subtract_ring_multiple_to(t, t->coeff, nf_1, s);
	}
      else
	{
	  result->next = t;
	  t = t->next;
	  result = result->next;
	}
    }
  result->next = NULL;
  v = head.next;
}

void FreeModule::normal_form(vec &v, 
			     const array<MonomialIdeal> &mis, 
			     const array<vec> &vecs) const
{
  const PolynomialRing *P = R->cast_to_poly_ring();
  vecterm head;
  vecterm *result = &head;
  int_bag *b;
  int *m = M->make_one();
  vecterm *t = v;
  while (t != NULL)
    {
      M->to_expvector(t->monom, nf_exp);
      if (is_quotient_ring && P->Rideal.search_expvector(nf_exp, b))
	{
	  Nterm *s = (Nterm *) (b->basis_ptr());
	  ring_elem coeff;
	  imp_ring_cancel_lead_term(t, s, coeff, nf_1);
	  K->remove(coeff);
	}
      else if (mis[t->comp].search_expvector(nf_exp, b))
	{
	  // Possibly be more careful in the choice of element...
	  int x = b->basis_elem();
	  // MES: reduce by w=vecs[x]: t -= (coeff)*m*w
	  ring_elem c;
	  imp_cancel_lead_term(t, vecs[x], c, m);
	  K->remove(c);
	}
      else
	{
	  result->next = t;
	  t = t->next;
	  result = result->next;
	}
    }
  result->next = NULL;
  v = head.next;
  M->remove(m);
}

//////////////////////////////////////////////
//  Groebner basis support routines //////////
//////////////////////////////////////////////

vec FreeModule::mult_by_monomial(const int *m, vec v) const
   // return m*v.  The result is not nec in normal form.
{
  if (M == NULL) return copy(v);
  if (M->is_skew())
    {
      ring_elem one = K->from_int(1);
      vec f = mult_by_term(one,m,v);
      K->remove(one);
      return f;
    }
  vecterm head;
  vecterm *result = &head;
  for (vecterm *a = v; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = a->comp;
      result->coeff = K->copy(a->coeff);
      M->mult(m, a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

ring_elem FreeModule::coeff_of(const vec v, const int *m, int x) const
{
  if (M == NULL)
    {
      for (vecterm *t = v; t != NULL; t = t->next)
	if (x == t->comp)
	  return K->copy(t->coeff);
    }
  else
    {
      for (vecterm *t = v; t != NULL; t = t->next)
	if (x == t->comp && M->compare(m, t->monom) == 0)
	  return K->copy(t->coeff);
    }
  return K->from_int(0);
}

void FreeModule::auto_reduce(const FreeModule *Fsyz, vec &f, vec &fsyz, 
		 vec g, vec gsyz) const
    // f -= c g, fsyz -= c gsyz, where c = coeff of in(g) in f.
{
  // MES: this doesn't yet work for M == NULL.
  ring_elem c = coeff_of(f, g->monom, g->comp);
  if (!K->is_zero(c))
    {
      if (M != NULL) M->one(nf_1);
      imp_subtract_multiple_to(f, c, nf_1, g);
      Fsyz->imp_subtract_multiple_to(fsyz, c, nf_1, gsyz);
    }
  K->remove(c);
}

void FreeModule::make_monic(vec &v, vec &vsyz) const
{
  vecterm *t;
  if (v == NULL) return;
  ring_elem a = K->invert(v->coeff);
  for (t = v ; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K->mult(a, tmp);
      K->remove(tmp);
    }
  for (t = vsyz ; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K->mult(a, tmp);
      K->remove(tmp);
    }
  K->remove(a);
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
      if (M != NULL)
	{
	  M->divide(result->monom, F->base_monom(v->comp), result->monom);
	  M->mult(result->monom, base_monom(result->comp), result->monom);
	}
    }
  result->next = NULL;
  return head.next;
  // MES: is sorting needed here sometimes?
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
      if (M != NULL)
	{
	  M->divide(result->monom, F->base_monom(v->comp), result->monom);
	  M->mult(result->monom, base_monom(result->comp), result->monom);
	}
    }
  result->next = NULL;
  return head.next;
  // MES: is sorting needed here sometimes?
}

vec FreeModule::sub_vector(const FreeModule *F, vec v, 
				const intarray &r) const
{
  intarray trans(F->rank());
  int i;
  for (i=0; i<F->rank(); i++)
    trans.append(-1);

  for (i=0; i<r.length(); i++)
    if (r[i] >= 0 && r[i] < F->rank())
      trans[r[i]] = i;

  vecterm head;
  vecterm *result = &head;
  for ( ; v != NULL; v = v->next)
    if (trans[v->comp] != -1)
      {
	result->next = copy_term(v);
	result = result->next;
	result->comp = trans[v->comp];
	if (M != NULL)
	  {
	    M->divide(result->monom, F->base_monom(v->comp), result->monom);
	    M->mult(result->monom, base_monom(result->comp), result->monom);
	  }
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
	if (M != NULL)
	  {
	    M->divide(q->monom, m.rows()->base_monom(p->comp), q->monom);
	    M->mult(q->monom, base_monom(q->comp), q->monom);
	  }
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
	if (M != NULL)
	  {
	    M->divide(q->monom, m.rows()->base_monom(p->comp), q->monom);
	    M->mult(q->monom, base_monom(q->comp), q->monom);
	  }
	result.elem(r) = q;
      }
  for (c=0; c<result.n_cols(); c++)
    sort(result.elem(c));
}

vec FreeModule::mult_by_matrix(const Matrix &m,
				 const FreeModule *F, 
				 vec v) const
{
  // Each loop below should read
  // result = 0
  // for each non-zero term f e[component] of the vector v
  //    result += f M[v]
  vec result = NULL, f;
  switch (F->ty)
    {
    case FREE:
      for ( ; v != NULL; v = v->next)
	{
	  f = mult(v->coeff, m.elem(v->comp));
	  add_to(result, f);
	}
      break;

    case FREE_POLY:
      for ( ; v != NULL; v = v->next)
	{
	  f = mult_by_term(v->coeff, v->monom, m.elem(v->comp));
	  add_to(result, f);
	}
      break;

    case FREE_SCHREYER:
      for ( ; v != NULL; v = v->next)
	{
	  F->M->divide(v->monom, F->base_monom(v->comp), mon_1);
	  f = mult_by_term(v->coeff, mon_1, m.elem(v->comp));
	  add_to(result, f);
	}
    }
  return result;
}
vec FreeModule::tensor(const FreeModule *F, vec v, 
			 const FreeModule *G, vec w) const
{
  vec result = NULL;
  for ( ; v != NULL; v = v->next)
    for (vecterm *p = w; p != NULL; p = p->next)
      {
	ring_elem a = K->mult(v->coeff, p->coeff);
	if (K->is_zero(a)) 
	  {
	    K->remove(a);
	    continue;
	  }

	vecterm *t = new_term();
	t->comp = G->rank() * v->comp + p->comp; // MES: is this right??!
	t->coeff = a;
	if (M != NULL)
	  {
	    // MES: this might overflow allowable degrees!!
	    M->divide(v->monom, F->base_monom(v->comp), t->monom);
	    M->mult(t->monom, p->monom, t->monom);
	    M->divide(t->monom, G->base_monom(p->comp), t->monom);
	    M->mult(t->monom, base_monom(t->comp), t->monom);
	  }
	t->next = result;
	result = t;
      }
  sort(result);
  return result;
}

void FreeModule::auto_reduce(array<vec> & vecs) const
{
  // (a) Sort into increasing monomial order
  // (b) For each element: reduce w.r.t. the previous elements
  //     and then insert into the appropriate monomial ideal.
  intarray indices, vp;
  intarray degs; // Not used.
  array<MonomialIdeal> mis;
  sort(vecs, degs, 0, 1, indices);
  for (int x=0; x<rank(); x++)
    mis.append(MonomialIdeal(Ring_of()));
  for (int i=0; i<vecs.length(); i++)
    {
      // Reduce each one in turn, and replace.
      vec v = vecs[indices[i]];
      normal_form(v, mis, vecs);
      vp.shrink(0);
      lead_varpower(v, vp);
      Bag *b = new Bag(indices[i], vp);
      int isnew = mis[v->comp].insert(b);
      vecs[indices[i]] = v;
      if (!isnew)
	gError << "bad boy!";
    }
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

void FreeModule::sort(const array<vec> &vecs, 
		      const intarray &degrees, 
		      int degorder, // -1=descending, 0=don't use, 1=ascending
		      int monorder, // -1=descending, 1=ascending.
		      intarray &result) const
{
  result.shrink(0);
  if (vecs.length() == 0) return;

  monorder_ascending = monorder;
  deg_ascending = degorder;

  sort_vals = result.alloc(vecs.length());
  for (int i=0; i<vecs.length(); i++)
    sort_vals[i] = i;

  sort_vecs = &vecs;
  sort_degs = NULL;
  if (deg_ascending && degrees.length() != vecs.length())
    {
      gError << "sort: specified degree dort, without giving degrees";
      return;
    }
  else
    sort_degs = degrees.raw();

  sort_range(0,vecs.length()-1);
  sort_vals = NULL;
  sort_degs = NULL;
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

ring_elem FreeModule::diff_term(const int *m, const int *n, 
				  int *resultmon,
				  int use_coeff) const
{
  int sign = 0;
  if (!M->divides(m, n)) return K->from_int(0);
  if (M->is_skew() && use_coeff)
    sign = M->skew_divide(n, m, resultmon);
  else
    M->divide(n, m, resultmon);
  ring_elem result = K->from_int(1);
  if (!use_coeff) return result;
  intarray e1, e2;
  int *exp1 = e1.alloc(R->n_vars());
  int *exp2 = e2.alloc(R->n_vars());
  M->to_expvector(m, exp1);
  M->to_expvector(n, exp2);

  if (M->is_skew() && sign < 0)
    K->negate_to(result);

  for (int i=0; i<R->n_vars(); i++)
    {
      for (int j=exp1[i]-1; j>=0; j--)
	{
	  ring_elem g = K->from_int(exp2[i]-j);
	  K->mult_to(result, g);
	  K->remove(g);
	  if (K->is_zero(result)) return result;
	}
    }
  return result;
}

vec FreeModule::diff_by_term(const int *exp, vec v, bool use_coeff) const
{
  // The result terms will be in the same order as those of f.
  // NOT valid for skew commutative rings, although currently
  // this routine is only used by Weyl algebra stuff.
  vecterm head;
  vec result = &head;
  int nvars = M->n_vars();
  int *exp2 = new int[nvars];
  for (vec t = v; t != NULL; t = t->next)
    {
      M->to_expvector(t->monom, exp2);
      if (ntuple::divides(nvars,exp,exp2))
	{
	  // Now determine the coefficient.
	  ring_elem c = K->copy(t->coeff);
	  if (use_coeff)
	    {
	      if (ty == FREE_SCHREYER)
		{
		  emit("can't handle Schreyer order yet!!");
		}
	      for (int i=0; i<nvars; i++)
		for (int j=exp[i]-1; j>=0; j--)
		  {
		    ring_elem g = K->from_int(exp2[i]-j);
		    K->mult_to(c,g);
		    K->remove(g);
		    if (K->is_zero(c))
		      {
			K->remove(c);
			c = g;
			// break out of these two loops
			j = -1;
			i = nvars;
		      }
		  }
	    }
	  ntuple::divide(nvars,exp2,exp,exp2);
	  result->next = new_term();
	  result = result->next;
	  result->coeff = c;
	  result->comp = t->comp;
	  M->from_expvector(exp2, result->monom);
	}
    }
  delete [] exp2;
  result->next = NULL;
  return head.next;
}

vec FreeModule::diff(const FreeModule * F, vec v, 
		       const FreeModule * G, vec w,
		       int use_coeff) const
{
  int *mon1 = NULL, *mon2 = NULL;
  if (M != NULL)
    {
      mon1 = M->make_one();
      mon2 = M->make_one();
    }
  vec result = NULL;
  for ( ; v != NULL; v = v->next)
    for (vecterm *p = w; p != NULL; p = p->next)
      {
	ring_elem a = K->mult(v->coeff, p->coeff);
	if (K->is_zero(a)) 
	  {
	    K->remove(a);
	    continue;
	  }

	vecterm *t = new_term();
	t->comp = G->rank() * v->comp + p->comp; // MES: is this right??!
	t->coeff = a;
	if (M != NULL)
	  {
	    M->divide(v->monom, F->base_monom(v->comp), mon1);
	    M->divide(p->monom, G->base_monom(p->comp), mon2);
	    ring_elem g = diff_term(mon1, mon2, t->monom, use_coeff);
	    M->mult(t->monom, base_monom(t->comp), t->monom);
	    K->mult_to(t->coeff, g);
	    K->remove(g);
	    if (K->is_zero(t->coeff))
	      {
		t->next = NULL;
		remove(t);
		continue;
	      }
	  }
	t->next = result;
	result = t;
      }
  if (M != NULL)
    {
      M->remove(mon1);
      M->remove(mon2);
    }
  sort(result);
  return result;
}

int FreeModule::in_subring(int n, const vec v) const
{
  if (v == NULL) return 1;
  // MES BUG!!
  return M->in_subring(n, v->monom);
}

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

int FreeModule::degree_of_var(int n, const vec v) const
{
  if (M == NULL) return 0;
  if (v == NULL)
    {
      gError << "attempting to find degree of zero vector";
      return 0;
    }
  M->divide(v->monom, base_monom(v->comp), nf_1);
  M->to_expvector(nf_1, nf_exp);
  int mindegree = nf_exp[n];
  for (vecterm *t = v->next; t != NULL; t = t->next)
    {
      M->divide(t->monom, base_monom(t->comp), nf_1);
      M->to_expvector(nf_1, nf_exp);
      if (nf_exp[n] < mindegree)
	mindegree = nf_exp[n];
    }
  return mindegree;
}
vec FreeModule::divide_by_var(int n, int d, const vec v) const
{
  if (M == NULL) return copy(v);
  vecterm head;
  vecterm *result = &head;
  for (vecterm *t = v; t != NULL; t = t->next)
    {
      result->next = new_term();
      result = result->next;
      M->to_expvector(t->monom, nf_exp);
      if (nf_exp[n] >= d)
	nf_exp[n] -= d;
      else 
	nf_exp[n] = 0;
      result->comp = t->comp;
      result->coeff = K->copy(t->coeff);
      M->from_expvector(nf_exp, result->monom);
    }
  result->next = NULL;
  return head.next;
}

vec FreeModule::divide_by_expvector(const int *exp, const vec v) const
{
  if (M == NULL) return copy(v);
  vecterm head;
  vecterm *result = &head;
  for (vecterm *t = v; t != NULL; t = t->next)
    {
      result->next = new_term();
      result = result->next;
      M->to_expvector(t->monom, nf_exp);
      ntuple::quotient(R->n_vars(), nf_exp, exp, nf_exp);
      result->comp = t->comp;
      result->coeff = K->copy(t->coeff);
      M->from_expvector(nf_exp, result->monom);
    }
  result->next = NULL;
  return head.next;
}

//////////////////////////////////////////////
//  Homogeniety and the grading //////////////
//////////////////////////////////////////////

void FreeModule::term_degree(const vecterm *t, int *degt) const
{
  assert(t != NULL);

  switch (ty) {
  case FREE:
    degree_monoid()->one(degt);
    break;

  case FREE_POLY:
    M->multi_degree(t->monom, degt);
    break;

  case FREE_SCHREYER:
    int *m = M->make_new(t->monom);
    M->divide(t->monom, base_monom(t->comp), m);
    M->multi_degree(m, degt);
    M->remove(m);
    break;
  }

  degree_monoid()->mult(degt, degree(t->comp), degt);
}

void FreeModule::degree(const vec f, int *degf) const
{
  vecterm *t = f;
  assert(t != NULL);
  // MES: throw an error if t==NULL
  int *e = degree_monoid()->make_one();
  term_degree(t, degf);
  for (t = t->next ; t != NULL; t = t->next)
    {
      term_degree(t, e);
      degree_monoid()->lcm(degf, e, degf);
    }
  degree_monoid()->remove(e);
}

bool FreeModule::is_homogeneous(const vec f) const
{
  if (f == NULL) return 1;
  bool result = 1;
  int *d = degree_monoid()->make_one();
  int *e = degree_monoid()->make_one();
  degree(f, d);
  for (vecterm *t = f->next; t != NULL; t = t->next)
    {
      term_degree(t, e);
      if (0 != degree_monoid()->compare(d, e))
	{ result = 0; break; }
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
  degree_monoid()->copy(deg, components[i]->deg);
}

int FreeModule::primary_degree(const vec f) const
{
  if (f == NULL) return 0;
  if (M == NULL) return 0;
  return primary_degree(f->comp) + M->primary_degree(f->monom);
}

void FreeModule::degree_weights(const vec f, const int *wts, int &lo, int &hi) const
{
  vecterm *t = f;
  assert(t != NULL);
  int e = M->degree_weights(t->monom, wts) + primary_degree(t->comp);
  lo = hi = e;
  for (t = t->next; t != NULL; t = t->next)
    {
      e = M->degree_weights(t->monom, wts) + primary_degree(t->comp);
      if (e > hi) hi = e;
      else if (e < lo) lo = e;
    }
}

vec FreeModule::homogenize(const vec f, 
				int v, int d, const int *wts) const
{
  assert(wts[v] != 0);
  // If an error occurs, then return 0, and set gError.

  intarray expa;
  int *exp = expa.alloc(R->n_vars());

  vecterm head;
  vecterm *result = &head;
  for (vecterm *a = f ; a != NULL; a = a->next)
    {
      M->to_expvector(a->monom, exp);
      int e = 0;
      for (int i=0; i<R->n_vars(); i++) e += wts[i] * exp[i];
      e += primary_degree(a->comp);
      if (((d-e) % wts[v]) != 0)
	{
	  // We cannot homogenize, so clean up and exit.
	  result->next = NULL;
	  remove(head.next);
	  gError << "homogenization impossible";
	  result = NULL;
	  return result;
	}
      exp[v] += (d - e) / wts[v];

      result->next = new_term();
      result = result->next;
      result->coeff = K->copy(a->coeff);
      result->comp = a->comp;
      M->from_expvector(exp, result->monom);
    }
  result->next = NULL;
  sort(head.next);			// The monomial order, etc. might all have changed.
				// Some terms might even drop out
  if (is_quotient_ring) normal_form(head.next);
  return head.next;
}

vec FreeModule::homogenize(const vec f, int v, const int *wts) const
{
  vecterm *result = NULL;
  if (f == NULL) return result;
  int lo, hi;
  degree_weights(f, wts, lo, hi);
  assert(wts[v] != 0);
  int d = (wts[v] > 0 ? hi : lo);
  return homogenize(f, v, d, wts);
}

vec FreeModule::random() const
{
  vec result = NULL;
  int *m = NULL;
  if (M != NULL)
    m = M->make_one();
  for (int i=0; i<rank(); i++)
    {
      vec v = term(i,K->random(),m);
      if (v != NULL)
	{
	  v->next = result;
	  result = v;
	}
    }
  if (M != NULL)
    M->remove(m);
  return result;
}
//////////////////////////////////////////////
//  Translation and sorting routines /////////
//////////////////////////////////////////////

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
      int isone = (M == NULL || M->is_one(t->monom));
      //int isone = M->is_one(t->monom);
      K->elem_text_out(o,t->coeff);
      if (!isone)
	M->elem_text_out(o, t->monom);
      o << "<" << t->comp << ">";
      p_plus = 1;
    }

  p_one = old_one;
  p_parens = old_parens;
  p_plus = old_plus;
}

void FreeModule::elem_bin_out(buffer &o, const vec v) const
{
  const vecterm *t;

  int n = n_terms(v);
  bin_int_out(o,n);

  switch (ty) {
  case FREE:
    for (t=v; t != NULL; t = t->next)
      {
	bin_int_out(o, t->comp);
	K->elem_bin_out(o, t->coeff);
      }
    break;
  case FREE_POLY:
    for (t=v; t != NULL; t = t->next)
      {
	bin_int_out(o, t->comp);
	M->elem_bin_out(o, t->monom);
	K->elem_bin_out(o, t->coeff);
      }
    break;
  case FREE_SCHREYER:
    int *m = M->make_one();
    for (t=v; t != NULL; t = t->next)
      {
	bin_int_out(o, t->comp);
	M->divide(t->monom, base_monom(t->comp), m);
	M->elem_bin_out(o, m);
	K->elem_bin_out(o, t->coeff);
      }
    M->remove(m);
    break;
  }
}

vec FreeModule::eval(const RingMap *map, const FreeModule *F,
			  const vec v) const
{
  ring_elem r;
  vec g;
  intarray vp;
  geobucket H(F);

  for (vecterm *t = v; t != NULL; t = t->next)
    {
      if (M != NULL)
	{
	  vp.shrink(0);
	  M->divide(t->monom, base_monom(t->comp), t->monom);
	  M->to_varpower(t->monom, vp);
	  M->mult(t->monom, base_monom(t->comp), t->monom);
	  r = map->eval_term(K, t->coeff, vp.raw());
	}
      else
	{
	  r = K->eval(map, t->coeff);
	}
      g = F->term(t->comp, r);
      F->Ring_of()->remove(r);
      H.add(g);
    }
  return H.value();
}
#if 0
vec FreeModule::eval(const RingMap *map, const FreeModule *F,
			  const vec v) const
{
  ring_elem r;
  vec g;
  vec result = NULL;
  intarray vp;

  for (vecterm *t = v; t != NULL; t = t->next)
    {
      if (M != NULL)
	{
	  vp.shrink(0);
	  M->divide(t->monom, base_monom(t->comp), t->monom);
	  M->to_varpower(t->monom, vp);
	  M->mult(t->monom, base_monom(t->comp), t->monom);
	  r = map->eval_term(K, t->coeff, vp.raw());
	}
      else
	{
	  r = K->eval(map, t->coeff);
	}
      g = F->term(t->comp, r);
      F->Ring_of()->remove(r);
      F->add_to(result, g);
    }
  return result;
}

#endif
