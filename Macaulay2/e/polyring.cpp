// Copyright 1996 Michael E. Stillman

#include "polyring.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "matrix.hpp"
#include "Z.hpp"
#include "ntuple.hpp"
#include "termideal.hpp"

#include "geopoly.hpp"
#include "serial.hpp"

#include "gb_comp.hpp"

#define POLY(q) ((q).poly_val)

buffer debugo;

PolynomialRing::PolynomialRing(const Ring *K, const Monoid *MF)
: Ring(K->charac(), MF->n_vars(), MF->n_vars() + K->total_n_vars(), 
	K, MF, MF->degree_monoid()),
  base_ring(NULL)
{
  isgraded = MF->degree_monoid()->n_vars() > 0;
  normal_exp = normal_exp_a.alloc(nvars);
  normal_m = normal_m_a.alloc(M->monomial_size());

  // Set up the polynomial stash
  pstash = new stash("polys", 
		     sizeof(Nterm) +
		     sizeof(int) * (M->monomial_size() - 1));


  coefficients_are_ZZ = (K->is_Z());
  Rsyz = NULL;
  RidealZ = NULL;

  bool isfield = (nvars == 0 && K->is_field());
  if (isfield) declare_field();
}

PolynomialRing::PolynomialRing(const PolynomialRing *R, const array<ring_elem> &)
: Ring(*R),
  base_ring(R)
{
  bump_up((Ring *) base_ring);

  normal_exp = normal_exp_a.alloc(nvars);
  normal_m = normal_m_a.alloc(M->monomial_size());

  // Use the stash for R
  pstash = base_ring->pstash;

  coefficients_are_ZZ = (K->is_Z());
  Rsyz = NULL;
  RidealZ = NULL;

  bool isfield = (nvars == 0 && K->is_field());
  if (isfield) declare_field();
  // isgraded: is set in PolynomialRing::create.
}

PolynomialRing::~PolynomialRing()
{
  if (base_ring == NULL)
    delete pstash;
  else
    {
      // MES: remove Rideal
      bump_down((Ring *) base_ring);
    }
}

PolynomialRing *PolynomialRing::create(const Ring *K, const Monoid *MF)
{
  PolynomialRing *obj = new PolynomialRing(K,MF);
  obj->Rsyz = obj->make_FreeModule();
  return (PolynomialRing *) intern(obj);
}

PolynomialRing *PolynomialRing::create(
    const PolynomialRing *R, const array<ring_elem> &I)
{
  PolynomialRing *obj = new PolynomialRing(R,I);
  if (obj->coefficients_are_ZZ)
    {
      obj->make_RidealZ(I);
      obj->make_Rideal(I);
    }
  else
    obj->make_Rideal(I);

  obj->isgraded = obj->base_ring->isgraded;
  if (obj->isgraded)
    for (int i=0; i<obj->quotient_ideal.length(); i++)
      if (!obj->base_ring->is_homogeneous(obj->quotient_ideal[i]))
	{
	  obj->isgraded = false;
	  break;
	}

  return (PolynomialRing *) intern(obj);
}

Matrix PolynomialRing::get_ideal() const
{
  const PolynomialRing *R = this;
  while (R->base_ring != NULL) R = R->base_ring;
  Matrix result(R->make_FreeModule(1));
  for (int i=0; i<quotient_ideal.length(); i++)
    result.append(result.rows()->term(0, quotient_ideal[i]));
  return result;
}

#if 0
bool PolynomialRing::equals(const object_element *o) const
{
  if (o->class_id() != class_id())
    return false;

  const PolynomialRing *R2 = (PolynomialRing *)o;
  if (R2->K != K) return false;
  if (R2->M != M) return false;
  if (R2->base_ring != base_ring) return false;
  if (R2->quotient_ideal.length() != quotient_ideal.length())
    return false;
  // MESXX: this is not necessarily correct!!  It assumes that
  // the elements are in the same order...
  for (int i=0; i<quotient_ideal.length(); i++)
    if (!is_equal(R2->quotient_ideal[i], quotient_ideal[i]))
      return false;
  return true;
}
#endif

void PolynomialRing::write_object(object_writer &o) const
{
  o << class_id() << K << M << quotient_ideal.length();
  if (base_ring != NULL)
    {
      o << base_ring;
      for (int i=0; i<quotient_ideal.length(); i++)
	write_element(o, quotient_ideal[i]);
    }
}

PolynomialRing *PolynomialRing::read_object(object_reader &i)
{
  object_element *obj1, *obj2, *obj3;
  int nquotients;
  i >> obj1 >> obj2;
  const Ring *K = obj1->cast_to_Ring();
  const Monoid *M = obj2->cast_to_Monoid();
  i >> nquotients;
  if (nquotients > 0)
    {
      array<ring_elem> quots;
      i >> obj3;
      const PolynomialRing *R = obj3->cast_to_Ring()->cast_to_PolynomialRing();
      for (int j=0; j<nquotients; j++)
	{
	  ring_elem f;
	  R->read_element(i, f);
	  quots.append(f);
	}
      return new PolynomialRing(R,quots);
    }
  else
    {
      return new PolynomialRing(K,M);
    }
  
}

void PolynomialRing::text_out(buffer &o) const
{
  K->text_out(o);
  M->text_out(o);
  if (base_ring != NULL)
    {
      o << "/(";
      int n = quotient_ideal.length();
      for (int i=0; i<n; i++)
	{
	  if (i != 0) o << ", ";
	  base_ring->elem_text_out(o, quotient_ideal[i]);
	}
      o << ')';
    }
}

Nterm *PolynomialRing::new_term() const
{
  Nterm *result = (Nterm *)((PolynomialRing *) this)->pstash->new_elem();
  result->next = NULL;
  result->coeff = 0;  // This value is never used, one hopes...
  // In fact, it gets used in the line below:       K->remove(tmp->coeff);
  // which is called from the line below:           remove(idiotic);
  // and it crashes there, because this assignment only sets the integer
  // part of the union, so on a machine with 4 byte ints and 8 byte pointers, the
  // pointer part is not NULL!
  result->coeff.poly_val = NULL;  // so I added this line
  return result;
}

Nterm *PolynomialRing::copy_term(const Nterm *t) const
{
  Nterm *result = new_term();
  result->coeff = K->copy(t->coeff);
  M->copy(t->monom, result->monom);
  return result;
}

void PolynomialRing::make_RidealZ(const array<ring_elem> &polys)
{
  // If coefficients_are_ZZ, then
  // this routine sets the fields:
  // quotient_ideal, RidealZ, Rsyz,

  const PolynomialRing *S = base_ring;
  while (S->base_ring != NULL) S = S->base_ring;

  RidealZ = TermIdeal::make_ring_termideal(S,
			   base_ring->quotient_ideal, 
			   polys,
			   Rsyz,
			   quotient_ideal);
}
void PolynomialRing::make_Rideal(const array<ring_elem> &polys)
{
  int i, top;
  queue<Bag *> elems;
  intarray vp;

  top = base_ring->quotient_ideal.length();
  for (i=0; i<top; i++)
    {
      ring_elem q = copy(base_ring->quotient_ideal[i]);
      if (is_zero(q)) continue;
      M->to_varpower(POLY(q)->monom, vp);
      elems.insert(new Bag((void *)q, vp));
      vp.shrink(0);
    }

  top = polys.length();
  for (i=0; i<top; i++)
    {
      ring_elem q = copy(polys[i]);
      if (is_zero(q)) continue;
      M->to_varpower(POLY(q)->monom, vp);
      elems.insert(new Bag((void *)q, vp));
      vp.shrink(0);
    }

  Rideal = MonomialIdeal(this, elems);

  for (Index<MonomialIdeal> j = Rideal.first(); j.valid(); j++)
    {
      ring_elem f = (Nterm *) Rideal[j]->basis_ptr();

      // The following line adds the element to f, it is not already there.
      // Over ZZ, RidealZ has been set, and at the same time, quotient_ideal
      // was set too.
      if (RidealZ == 0)
	quotient_ideal.append(f);
    }
}

ring_elem PolynomialRing::from_int(int n) const
{
  ring_elem a = K->from_int(n);
  if (K->is_zero(a)) return (Nterm *)NULL;
  Nterm *result = new_term();
  result->coeff = a;
  M->one(result->monom);
  if (base_ring != NULL) normal_form(result);
  return result;
}
ring_elem PolynomialRing::from_int(mpz_t n) const
{
  ring_elem a = K->from_int(n);
  if (K->is_zero(a)) 
    {
      K->remove(a);
      return (Nterm *)NULL;
    }
  Nterm *result = new_term();
  result->coeff = a;
  M->one(result->monom);
  if (base_ring != NULL) normal_form(result);
  return result;
}

ring_elem PolynomialRing::var(int v, int n) const
{
  if (M->is_skew() && n > 1 && v >= 0 && M->is_skew_var(v))
    return ((Nterm *)NULL);

  Nterm *result = new_term();
  result->coeff = K->from_int(1);

  intarray ma;
  int *m = ma.alloc(nvars);
  for (int i=0; i<nvars; i++) m[i] = 0;
  if (v >= 0 && v < nvars) m[v] = n;

  M->from_expvector(m, result->monom);
  if (base_ring != NULL) normal_form(result);
  return result;
}
bool PolynomialRing::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // case 1:  Rf = A[x]/J ---> A[x]/I  is one of the 'base_ring's of 'this'.
  // case 2:  Rf = A      ---> A[x]/I  is the ring of scalars

  for (const PolynomialRing *base = base_ring; base != NULL; base = base->base_ring)
    if (base == Rf)
      {
	Nterm *g = copy(f);
	normal_form(g);
	result = g;
	return true;
      }
  if (K == Rf)
    {
      int *m = M->make_one();
      result = term(f,m);
      M->remove(m);
      return true;
    }
  return false;
}

bool PolynomialRing::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // case 1:  Rf = A[x]/J ---> A[x]/I  is one of the 'base_ring's of 'this'.
  // case 2:  Rf = A      ---> A[x]/I  is the ring of scalars

  for (const PolynomialRing *base = base_ring; base != NULL; base = base->base_ring)
    if (base == Rg)
      {
	result = copy(f);	// We are using the same representation
	return true;
      }
  if (K == Rg)
    {
      Nterm *g = f;
      if (g == NULL)
	{
	  result = K->from_int(0);
	  return true;
	}
      if (g->next != 0) return false;
      if (!M->is_one(g->monom)) return false;
      result = K->copy(g->coeff);
      return true;
    }
  return false;
}

bool PolynomialRing::is_unit(const ring_elem ff) const
{
  Nterm *f = ff;
  if (f == NULL) return false;
  if (f->next == NULL && M->is_one(f->monom)
      && K->is_unit(f->coeff))
    return true;

  if (base_ring == NULL)
    return false;

  if (is_field())
    return true;

  return false;
}

bool PolynomialRing::is_zero(const ring_elem f) const
{
  Nterm *a = f;
  return a == NULL;
}

bool PolynomialRing::is_equal(const ring_elem f, const ring_elem g) const
{
  Nterm *a = f;
  Nterm *b = g;
  for ( ;; a = a->next, b = b->next)
    {
      if (a == NULL)
	{
	  if (b == NULL) return true;
	  return false;
	}
      if (b == NULL) return false;
      if (!K->is_equal(a->coeff, b->coeff)) return false;
      if (nvars > 0 && (M->compare(a->monom, b->monom) != 0))
	return false;
    }
}

bool PolynomialRing::is_homogeneous(const ring_elem f) const
{
  if (is_zero(f)) return true;
  int *d = degree_monoid()->make_one();
  int *e = degree_monoid()->make_one();
  degree(f, d);
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      term_degree(t, e);
      if (0 != degree_monoid()->compare(d, e))
	return false;
    }
  degree_monoid()->remove(d);
  degree_monoid()->remove(e);
  return true;
}
void PolynomialRing::term_degree(const Nterm *t, int *degt) const
{
  assert(t != NULL); // MES: raise an exception instead...
  M->multi_degree(t->monom, degt);
}

void PolynomialRing::degree(const ring_elem f, int *degf) const
{
  Nterm *t = f;
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

int PolynomialRing::primary_degree(const ring_elem f) const
{
  Nterm *t = f;
  if (t == NULL) return 0;
  return M->primary_degree(t->monom);
}

void PolynomialRing::degree_weights(const ring_elem f, const int *wts, int &lo, int &hi) const
{
  Nterm *t = f;
  assert(t != NULL);
  int e = M->degree_weights(t->monom, wts);
  lo = hi = e;
  for (t = t->next; t != NULL; t = t->next)
    {
      e = M->degree_weights(t->monom, wts);
      if (e > hi) hi = e;
      else if (e < lo) lo = e;
    }
}

ring_elem PolynomialRing::homogenize(const ring_elem f, 
			     int v, int d, const int *wts) const
{
  assert(wts[v] != 0);
  // If an error occurs, then return 0, and set gError.

  intarray expa;
  int *exp = expa.alloc(nvars);

  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f ; a != NULL; a = a->next)
    {
      M->to_expvector(a->monom, exp);
      int e = 0;
      for (int i=0; i<nvars; i++) e += wts[i] * exp[i];
      if (((d-e) % wts[v]) != 0)
	{
	  // We cannot homogenize, so clean up and exit.
	  result->next = NULL;
	  ring_elem g = head.next;
	  remove(g);
	  gError << "homogenization impossible";
	  result = NULL;
	  return result;
	}
      exp[v] += (d - e) / wts[v];
      if (M->is_skew() && M->is_skew_var(v) && exp[v] > 1)
	continue;
      result->next = new_term();
      result = result->next;
      result->coeff = K->copy(a->coeff);
      M->from_expvector(exp, result->monom);
    }
  result->next = NULL;
  sort(head.next);			// The monomial order, etc. might all have changed.
				// Some terms might even drop out
  if (base_ring != NULL) normal_form(head.next);
  return head.next;
}

ring_elem PolynomialRing::homogenize(const ring_elem f, int v, const int *wts) const
{
  Nterm *result = NULL;
  if (POLY(f) == NULL) return result;
  int lo, hi;
  degree_weights(f, wts, lo, hi);
  assert(wts[v] != 0);
  int d = (wts[v] > 0 ? hi : lo);
  return homogenize(f, v, d, wts);
}

ring_elem PolynomialRing::copy(const ring_elem f) const
{
  Nterm *a = f;
  Nterm head;
  Nterm *result = &head;
  for ( ; a != NULL; a = a->next, result = result->next)
      result->next = copy_term(a);
  result->next = NULL;
  return head.next;
}

void PolynomialRing::remove(ring_elem &f) const
{
  Nterm *a = f;
  while (a != NULL)
    {
      Nterm *tmp = a;
      a = a->next;
      K->remove(tmp->coeff);
      pstash->delete_elem(tmp);
    }
}

void PolynomialRing::negate_to(ring_elem &f) const
{
  Nterm *v = f;
  while (v != NULL)
    {
      K->negate_to(v->coeff);
      v = v->next;
    }
}

void PolynomialRing::add_to(ring_elem &ff, ring_elem &gg) const
{
  Nterm *f = ff;
  Nterm *g = gg;
  gg = (Nterm *)NULL;
  if (g == NULL) return;
  if (f == NULL) { ff = g; return; }
  Nterm head;
  Nterm *result = &head;
  while (1)
    switch (M->compare(f->monom, g->monom))
      {
      case -1:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f; 
	    ff = head.next;
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
	    ff = head.next;
	    return;
	  }
	break;
      case 0:
	Nterm *tmf = f;
	Nterm *tmg = g;
	f = f->next;
	g = g->next;
	K->add_to(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  {
	    K->remove(tmf->coeff);
	    pstash->delete_elem(tmf);
	  }
	else
	  {
	    result->next = tmf;
	    result = result->next;
	  }
	K->remove(tmg->coeff);
	pstash->delete_elem(tmg);
	if (g == NULL) 
	  {
	    result->next = f; 
	    ff = head.next;
	    return;
	  }
	if (f == NULL) 
	  {
	    result->next = g; 
	    ff = head.next;
	    return;
	  }
	break;
      }
}

void PolynomialRing::subtract_to(ring_elem &f, ring_elem &g) const
{
  negate_to(g);
  add_to(f,g);
}

ring_elem PolynomialRing::negate(const ring_elem f) const
{
  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K->negate(a->coeff);
      M->copy(a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

ring_elem PolynomialRing::add(const ring_elem f, const ring_elem g) const
{
  ring_elem a = copy(f);
  ring_elem b = copy(g);
  add_to(a, b);
  return a;
}

ring_elem PolynomialRing::subtract(const ring_elem f, const ring_elem g) const
{
  ring_elem a = copy(f);
  ring_elem b = negate(g);
  add_to(a, b);
  return a;
}

ring_elem PolynomialRing::imp_skew_mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
   // return f*c*m
{
  Nterm head;
  Nterm *result = &head;
  ring_elem minus_c = K->negate(c);
  Nterm *nextterm = new_term();
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      int sign = M->skew_mult(m, a->monom, nextterm->monom);
      if (sign == 0) 
	continue;
      else if (sign > 0)
	nextterm->coeff = K->mult(a->coeff, c);
      else
	nextterm->coeff = K->mult(a->coeff, minus_c);

      result->next = nextterm;
      result = result->next;
      nextterm = new_term();
    }
  ring_elem idiotic = nextterm;
  remove(idiotic);
  result->next = NULL;
  return head.next;
}

ring_elem PolynomialRing::imp_mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
   // return f*c*m
{
  if (M->is_skew()) return imp_skew_mult_by_term(f,c,m);
  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K->mult(a->coeff, c);
      M->mult(m, a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

void PolynomialRing::imp_subtract_multiple_to(ring_elem &f, 
				 ring_elem a, const int *m, const ring_elem g) const
{
  ring_elem b = K->negate(a);
  ring_elem h = imp_mult_by_term(g, b, m);
  add_to(f, h);
  K->remove(b);
}

ring_elem PolynomialRing::mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
   // return f*c*m
{
  Nterm *result = imp_mult_by_term(f, c, m);
  if (base_ring != NULL) normal_form(result);
  return result;
}

void PolynomialRing::subtract_multiple_to(ring_elem &f, 
				 ring_elem a, const int *m, const ring_elem g) const
{
  ring_elem b = K->negate(a);
  ring_elem h = mult_by_term(g, b, m);
  add_to(f, h);
  K->remove(b);
}

void PolynomialRing::auto_reduce_to(ring_elem &f, ring_elem g) const
    // f -= c g, where c = coeff of in(g) in f
{
  Nterm *t = g;
  ring_elem a = coeff_of(f, t->monom);
  if (K->is_zero(a)) return;

  intarray ma;
  int *m = ma.alloc(M->monomial_size());
  M->one(m);
  imp_subtract_multiple_to(f, a, m, g);
}

void PolynomialRing::make_monic(ring_elem &f) const
{
  Nterm *t = f;
  if (t == NULL) return;
  ring_elem a = K->invert(t->coeff);
  for ( ; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K->mult(a, tmp);
      K->remove(tmp);
    }
}
void PolynomialRing::mult_coeff_to(ring_elem a, ring_elem &f) const
{
  Nterm *t = f;
  if (t == NULL) return;
  for ( ; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K->mult(a, tmp);
      K->remove(tmp);
    }
}
#if 0
ring_elem PolynomialRing::mult(const ring_elem f, const ring_elem g) const
{
  ring_elem result = NULL;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      ring_elem h = mult_by_term(g, a->coeff, a->monom);
      add_to(result, h);
    }
  return result;
}
#endif
ring_elem PolynomialRing::mult(const ring_elem f, const ring_elem g) const
{
  polyheap H(this);
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      ring_elem h = mult_by_term(g, a->coeff, a->monom);
      H.add(h);
    }
  return H.value();
}

ring_elem PolynomialRing::power2(const ring_elem ff, mpz_t m) const
{
  // The exponent 'm' should be > 0 here.
  mpz_t n;
  mpz_init_set(n, m);
  ring_elem prod = from_int(1);
  ring_elem base = copy(ff);
  ring_elem tmp;

  for (;;)
    {
      if (Z::mod_ui(n,2) == 1)
	{
	  tmp = mult(prod, base);
	  remove(prod);
	  prod = tmp;
	}
      mpz_tdiv_q_2exp(n, n, 1);
      if (mpz_sgn(n) == 0)
	{
	  remove(base);
	  mpz_clear(n);
	  return prod;
	}
      else
	{
	  tmp = mult(base, base);
	  remove(base);
	  base = tmp;
	}
    }
}

ring_elem PolynomialRing::power2(const ring_elem ff, int n) const
{
  // The exponent 'n' should be > 0 here.
  ring_elem prod = from_int(1);
  ring_elem base = copy(ff);
  ring_elem tmp;

  for (;;)
    {
      if ((n % 2) != 0)
	{
	  tmp = mult(prod, base);
	  remove(prod);
	  prod = tmp;
	}
      n >>= 1;
      if (n == 0)
	{
	  remove(base);
	  return prod;
	}
      else
	{
	  tmp = mult(base, base);
	  remove(base);
	  base = tmp;
	}
    }
}

ring_elem PolynomialRing::power(const ring_elem f0, mpz_t n) const
{
  ring_elem ff, result;
  bool isinverted = false;

  if (mpz_sgn(n) == 0) return from_int(1);
  if (is_zero(f0)) return (Nterm*)NULL;

  if (mpz_sgn(n) > 0)
    ff = f0;
  else
    {
      isinverted = true;
      ff = invert(f0);
      mpz_neg(n,n);
    }

  Nterm *f = ff;
  if (base_ring == NULL)
    {
      int n1;
      // In this case, the computation may only be formed in two
      // cases: (1) f is a constant, or (2) n is small enough
      if (Z::get_si(n1,n))
	{
	  result = power(f,n1);
	}
      else if (is_unit(f))  // really want a routine 'is_scalar'...
	{
	  ring_elem a = K->power(f->coeff, n);
	  result = term(a, f->monom);
	  K->remove(a);
	}
      else 
	{
	  gError << "exponent too large";
	  result = (Nterm *)NULL;
	}
    }
  else
    {
      // At this point, we are in a quotient ring, so we will try
      // to perform the exponentiation
      result = power2(ff, n);
    }

  if (isinverted) mpz_neg(n,n);
  return result;
}

ring_elem PolynomialRing::power(const ring_elem f0, int n) const
{
  ring_elem ff;

  if (n > 0)
    ff = f0;
  else if (n < 0)
    {
      ff = invert(f0);
      n = -n;
    }
  else 
    return from_int(1);

  ring_elem result, g, rest, h, tmp;
  ring_elem coef1, coef2, coef3;

  Nterm *lead = ff;
  if (lead == NULL) return (Nterm *)NULL;
  if (base_ring != NULL)
      return power2(ff,n);

  if (M->is_skew())
    {
      // We can also be smarter, but I'm not sure it is worth it here.
      result = from_int(1);
      for (int i=0; i<n; i++)
	{
	  ring_elem result1 = mult(result, ff);
	  remove(result);
	  result = result1;
	}
      return result;
    }
  unsigned int bin_coeff = 1;
  intarray ma;

  rest = lead->next;
  g = from_int(1);

  // Start the result with the n th power of the lead term
  Nterm *t = new_term();
  t->coeff = K->power(lead->coeff, n);
  M->power(lead->monom, n, t->monom);
  t->next = NULL;
  if (base_ring != NULL) normal_form(t);
  result = t;

  if (rest == (Nterm *)NULL) return result;
  int *m = ma.alloc(M->monomial_size());
  M->one(m);

  mpz_t bin_c;

  mpz_init_set_ui(bin_c, 1);

  for(int i=1; i<=n ; i++)
    {
      tmp = mult(g, rest);
      remove(g);
      g = tmp;

      bin_coeff *= (n-i+1);
      bin_coeff /= i;

      mpz_mul_ui(bin_c, bin_c, n-i+1);
      mpz_div_ui(bin_c, bin_c, i);

      //coef1 = K->from_int(bin_coeff);
      coef1 = K->from_int(bin_c);

      if (!K->is_zero(coef1))
	{
	  coef2 = K->power(lead->coeff, n-i);
	  coef3 = K->mult(coef1, coef2);
	  M->power(lead->monom, n-i, m);

	  h = mult_by_term(g, coef3, m);
	  add_to(result, h);

	  K->remove(coef2);
	  K->remove(coef3);
	}
      K->remove(coef1);
    }
  remove(g);
  return result;
}

ring_elem PolynomialRing::invert(const ring_elem f) const
{
  Nterm *ft = f;
  if (is_zero(f))
    {
      gError << "cannot divide by zero";
      return (Nterm *)NULL;
    }
  if (ft->next == NULL)
    if (M->is_one(ft->monom))
      {
	Nterm *t = new_term();
	t->coeff = K->invert(ft->coeff);
	M->one(t->monom);
	return t;
      }
    else if (M->is_group())
      {
	Nterm *t = new_term();
	t->coeff = K->invert(ft->coeff);
	M->power(ft->monom, -1, t->monom);
	return t;
      }
  if (nvars == 1 && quotient_ideal.length() == 1 && K->is_field())
    {
      ring_elem u,v;
      ring_elem F = quotient_ideal[0];
      ring_elem g = base_ring->gcd_extended(F, f, u, v);
      if (!base_ring->is_unit(g))
	{
	  gError << "element is not invertible";
	  // MES: what about setting some global error ring element
	  // which contains this 'certificate' g of non-field-ness?
	}
      remove(g); remove(u);
      return v;
    }
  else
    {
      gError << "division is not defined in this ring";
      return (Nterm *)NULL;
    }
}

ring_elem PolynomialRing::divide(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem d = divide(f,g,rem);
  if (is_zero(rem)) return d;
  remove(d);  // Alternatively, we can return d + rem*ginv
  remove(rem);
  ring_elem ginv = invert(g);
  ring_elem result = mult(f, ginv);
  remove(ginv);
  return result;
}

void PolynomialRing::imp_cancel_lead_term(ring_elem &f, 
					  ring_elem g, 
					  ring_elem &coeff, 
					  int *monom) const
{
  Nterm *t = f;
  Nterm *s = g;
  if (t == NULL || s == NULL) return;
  coeff = K->divide(t->coeff, s->coeff);
  if (M->is_skew())
    {
      int sign = M->skew_divide(t->monom, s->monom, monom);
      if (sign < 0) K->negate_to(coeff);
      imp_subtract_multiple_to(f, coeff, monom, g);
    }
  else
    {
      M->divide(t->monom, s->monom, monom);
      imp_subtract_multiple_to(f, coeff, monom, g);
    }
}
bool PolynomialRing::imp_attempt_to_cancel_lead_term(ring_elem &f, 
						     ring_elem g, 
						     ring_elem &coeff, 
						     int *monom) const
{
  Nterm *t = f;
  Nterm *s = g;
  if (t == NULL || s == NULL) return true;
  ring_elem r = K->remainderAndQuotient(t->coeff, s->coeff, coeff);
  bool result = (K->is_zero(r));  // true means lead term will be cancelled.
  K->remove(r);
  if (!K->is_zero(coeff))
    {
      if (M->is_skew())
	{
	  int sign = M->skew_divide(t->monom, s->monom, monom);
	  if (sign < 0) K->negate_to(coeff);
	  imp_subtract_multiple_to(f, coeff, monom, g);
	}
      else
	{
	  M->divide(t->monom, s->monom, monom);
	  imp_subtract_multiple_to(f, coeff, monom, g);
	}
    }
  return result;
}
void PolynomialRing::cancel_lead_term(ring_elem &f, 
				       ring_elem g, 
				       ring_elem &coeff, 
				       int *monom) const
{
  Nterm *t = f;
  Nterm *s = g;
  if (t == NULL || s == NULL) return;
  coeff = K->divide(t->coeff, s->coeff);
  if (M->is_skew())
    {
      int sign = M->skew_divide(t->monom, s->monom, monom);
      if (sign < 0) K->negate_to(coeff);
      subtract_multiple_to(f, coeff, monom, g);
    }
  else
    {
      M->divide(t->monom, s->monom, monom);
      subtract_multiple_to(f, coeff, monom, g);
    }
}

ring_elem PolynomialRing::divide(const ring_elem f, const ring_elem g, ring_elem &rem) const
{
  Nterm *quot;
  Nterm *r;
  r = division_algorithm(f,g,quot);
  rem = r;
  return quot;

#if 0
  ring_elem a = copy(f);
  Nterm *t = a;
  Nterm *b = g;
  Nterm divhead;
  Nterm remhead;
  Nterm *divt = &divhead;
  Nterm *remt = &remhead;
  while (t != NULL)
    if (M->divides(b->monom, t->monom))
      {
	divt->next = new_term();
	divt = divt->next;
	a = t;
	cancel_lead_term(a, g, divt->coeff, divt->monom);
	t = a;
      }
    else
      {
	a = t;
	remt->next = copy_term(a);
	remt = remt->next;
	t = t->next;
      }

  remt->next = NULL;
  divt->next = NULL;
  rem = remhead.next;
  return divhead.next;
#endif
}

ring_elem PolynomialRing::gcd(const ring_elem ff, const ring_elem gg) const
{
  if (nvars != 1)
    {
      gError << "multivariate gcd not yet implemented";
      return (Nterm *)NULL;
    }
  ring_elem f = copy(ff);
  ring_elem g = copy(gg);
  ring_elem s, rem;
  while (!is_zero(g))
    {
      s = divide(f, g, rem);
      remove(s);
      remove(f);
      f = g;
      g = rem;
    }
  make_monic(f);
  return f;
}

ring_elem PolynomialRing::gcd_extended(const ring_elem f, const ring_elem g, 
			       ring_elem &u, ring_elem &v) const
  // result == gcd(f,g) = u f + v g
{
  if (!has_gcd())
    {
      gError << "cannot use gcd_extended in this ring";
      return (Nterm *) NULL;
    }
  u = from_int(1);
  ring_elem result = copy(f);

  if (is_zero(g))
    {
      v = from_int(0);
      return result;
    }
  ring_elem v1 = (Nterm *)NULL;
  ring_elem v3 = copy(g);
  ring_elem t1, t3;
  ring_elem temp1, temp2, temp3;
  while (!is_zero(v3))
    {
      ring_elem q = divide(result, v3, t3);
      remove(result);

      // The following is: t1 = u - q*v1
      temp1 = mult(q,v1);
      subtract_to(u, temp1);
      t1 = u;

      u = v1;
      result = v3;
      v1 = t1;
      v3 = t3;
    }

  // make 'result' monic. (and divde 'u' by this as well)
  if (!is_zero(result))
    {
      Nterm *t = result;
      ring_elem c = K->invert(t->coeff);
      mult_coeff_to(c, result);
      mult_coeff_to(c, u);
      K->remove(c);
    }

  // The following is v = (result - f*u)/g
  temp1 = mult(f,u);
  temp2 = subtract(result, temp1);
  v = divide(temp2, g, temp3);
  remove(temp1); remove(temp2); remove(temp3);

  return result;
}

void PolynomialRing::minimal_monomial(ring_elem f, int * &monom) const
{
  // Determines the minimal monomial which divides each term of f.
  // This monomial is placed into 'monom'.
  
  Nterm *t = f;
  if (t == NULL) return;
  M->copy(t->monom, monom);
  for (t = t->next; t!=NULL; t=t->next)
    M->gcd(t->monom,monom,monom);
}

ring_elem PolynomialRing::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem;
  rem = remainderAndQuotient(f,g,quot);
  remove(quot);
  return rem;
}

ring_elem PolynomialRing::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem;
  rem = remainderAndQuotient(f,g,quot);
  remove(rem);
  return quot;
}

ring_elem PolynomialRing::remainderAndQuotient(const ring_elem f, const ring_elem g, 
					       ring_elem &quot) const
{
  Nterm *q, *r;
  ring_elem rem;
  if (is_zero(g))
    {
      quot = from_int(0);
      return copy(f);
    }
  else
    {
      if (M->is_group())
	{
	  // First factor out powers of minimal monomial from f, g.
	  ring_elem one_kk = K->from_int(1);
	  int *one = M->make_one();
	  int *mf1 = M->make_one();
	  int *mg1 = M->make_one();
	  int *mf = M->make_one();
	  int *mg = M->make_one();
	  PolynomialRing::minimal_monomial(f,mf);
	  PolynomialRing::minimal_monomial(g,mg);
#if 0
	  buffer o;
	  o << "Minimal monomial of ";
	  elem_text_out(o,f);
	  o << " is ";
	  M->elem_text_out(o,mf);
	  o << newline;

	  o << "Minimal monomial of ";
	  elem_text_out(o,g);
	  o << " is ";
	  M->elem_text_out(o,mg);
	  o << newline;
	  emit(o.str());
#endif
	  M->divide(one,mf,mf1);
	  M->divide(one,mg,mg1);
	  ring_elem f1 = mult_by_term(f,one_kk,mf1);
	  ring_elem g1 = mult_by_term(g,one_kk,mg1);
	  r = division_algorithm(f1,g1,q);
	  M->mult(mf,mg1,mg);
	  rem = mult_by_term(r,one_kk,mf);
	  quot = mult_by_term(q,one_kk,mg);
	  M->remove(mf);
	  M->remove(mg);
	  M->remove(mf1);
	  M->remove(mg1);
	  M->remove(one);
	  K->remove(one_kk);
	  return rem;
	}
      else if (!is_quotient_poly_ring())
	{
	  rem = division_algorithm(f,g,q);
	  quot = q;
	  return rem;
	}
      else if (false) //(n_vars() == 1 && K->is_field())
	{
	  // Case 2: There is a quotient ideal, but we have one variable, over
	  //         a field.  In this case, we can use gcd in k[x].
	  //         The ring must be commutative here: skew in one variable isn't
	  //         handled here.
	}
      else if (K->is_field() || K->is_Z())
	{
	  // Case 3: There is a quotient ideal.  Here we do a GB computation
	  //         of the ideal (g), and reduce f wrt this ideal.

	  // Create a GB of (g).
	  intarray syzygy_stop_conditions;
	  syzygy_stop_conditions.append(0); // ngb
	  syzygy_stop_conditions.append(0); // nsyz
	  syzygy_stop_conditions.append(0); // npairs
	  syzygy_stop_conditions.append(0);
	  syzygy_stop_conditions.append(0); 
	  syzygy_stop_conditions.append(0); // subring limit
	  syzygy_stop_conditions.append(0);
	  
	  const FreeModule *F = make_FreeModule(1);
	  Matrix m(F);
	  m.append(F->term(0,g));
	  gb_comp *g = gb_comp::make(m,false,-1,0);
	  bump_up(g);
	  g->calc(0, syzygy_stop_conditions);

	  // Reduce f wrt this GB.
	  Vector v(F,F->term(0,f));
	  Vector lifted;
	  Vector red = g->reduce(v,lifted);
	  // Now grab the two polynomials of interest:
	  ring_elem result = F->get_coefficient(red.get_value(),0); // Rermainder
	  quot = lifted.free_of()->get_coefficient(lifted.get_value(),0); // Quotient

	  // Remove the GB.
	  delete g;
	  return result;
	}
      else
	{
	  // Case 4: The coefficients are not ZZ, or a field.  Currently we say:
	  //         not implemented.

	  gError << "remainder not defined and/or implemented for this ring";
	}
    }
  quot = from_int(0);
  return from_int(0);
}


void PolynomialRing::syzygy(const ring_elem a, const ring_elem b,
			    ring_elem &x, ring_elem &y) const
{
  // Do some special cases first.  After that: compute a GB

  // For the GB, we need to make a 1 by 2 matrix, and compute until one syzygy is found.
  // create the matrix
  // create the gb comp
  // compute until one syz is found
  // grab the answer from the syz matrix.

  // Special situations:
  ring_elem one = from_int(1);
  ring_elem minus_one = from_int(-1);
  if (PolynomialRing::is_equal(b, one))
    {
      x = PolynomialRing::copy(b);
      y = PolynomialRing::negate(a);
    }
  else if (PolynomialRing::is_equal(b, minus_one))
    {
      x = PolynomialRing::from_int(1);
      y = PolynomialRing::copy(a);
    }
  else
    {
      intarray syzygy_stop_conditions;
      syzygy_stop_conditions.append(0); // ngb
      syzygy_stop_conditions.append(1); // nsyz
      syzygy_stop_conditions.append(0); // npairs
      syzygy_stop_conditions.append(0);
      syzygy_stop_conditions.append(0); 
      syzygy_stop_conditions.append(0); // subring limit
      syzygy_stop_conditions.append(0);
      
      const FreeModule *F = make_FreeModule(1);
      Matrix m(F);
      m.append(F->term(0,a));
      m.append(F->term(0,b));
      
#if 0  
  buffer o;
  o << "constructing syzygy on ";
  elem_text_out(o,a);
  o << " and ";
  elem_text_out(o,b);
  emit_line(o.str());
  o.reset();
  o << "matrix is" << newline;
  m.text_out(o);
  emit_line(o.str());
  o.reset();
#endif

      gb_comp *g = gb_comp::make(m,true,-1,0);
      bump_up(g);
      g->calc(0, syzygy_stop_conditions);
      Matrix s = g->syz_matrix();
#if 0
  if (s.n_cols() != 1)
    {
      o << "found " << s.n_cols() << " syzygies";
      emit_line(o.str());
    }
#endif
      x = s.elem(0,0);
      y = s.elem(1,0);

#if 0
  o << "result: x = ";
  elem_text_out(o,x);
  o << " and y = ";
  elem_text_out(o,y);
  emit_line(o.str());
#endif
      delete g;
    }
  remove(one);
  remove(minus_one);
}

ring_elem PolynomialRing::random() const
{
  gError << "not yet implemented";
  return 0;
}
ring_elem PolynomialRing::random(int /*homog*/, const int * /*deg*/) const
{
  gError << "not yet implemented";
  return 0;
}

void PolynomialRing::debug_out(const ring_elem f) const
{
  buffer o;
  elem_text_out(o, f);
  emit_line(o.str());
}

void PolynomialRing::debug_outt(const Nterm *f) const
{
  buffer o;
  ring_elem g = (Nterm *) f;
  elem_text_out(o, g);
  emit_line(o.str());
}

void PolynomialRing::elem_text_out(buffer &o, const ring_elem f) const
{
  Nterm *t = f;
  if (t == NULL)
    {
      o << '0';
      return;
    }

  ring_elem one = K->from_int(1);
  ring_elem minus_one = K->from_int(-1);
  int old_one = p_one;
  int old_parens = p_parens;
  int old_plus = p_plus;
  
  int two_terms = (t->next != NULL);

  int needs_parens = p_parens && two_terms;
  if (needs_parens) 
    {
      if (old_plus) o << '+';
      o << '(';
      p_plus = 0;
    }

  for (t = f; t != NULL; t = t->next)
    {
      int isone = M->is_one(t->monom);
      p_parens = !isone;
      p_one = (isone && needs_parens) || (isone && old_one);
      K->elem_text_out(o,t->coeff);
      if (!isone)
	{
//	  if (!K->is_equal(t->coeff, one) && !K->is_equal(t->coeff, minus_one))
//	    o << "*";
	  M->elem_text_out(o, t->monom);
	}
      p_plus = 1;
    }
  if (needs_parens) o << ')';

  K->remove(minus_one);
  K->remove(one);
  p_one = old_one;
  p_parens = old_parens;
  p_plus = old_plus;

}

void PolynomialRing::write_element(object_writer &o, const ring_elem f) const
{
  int n = n_terms(f);
  o << n;

  for (Nterm *t = f; t != NULL; t = t->next)
    {
      M->write_element(o, t->monom);
      K->write_element(o, t->coeff);
    }
}

void PolynomialRing::read_element(object_reader &i, ring_elem &result) const
{
  int n;
  i >> n;
  Nterm head;
  Nterm *f = &head;
  for (int j=0; j<n; j++)
    {
      Nterm *t = new_term();
      f->next = t;
      f = t;
     // M->read_element(i, t->monom);
      K->read_element(i, t->coeff);
    }
  f->next = NULL;
  result = head.next;
}

void PolynomialRing::elem_bin_out(buffer &o, const ring_elem f) const
{
  int n = n_terms(f);
  bin_int_out(o,n);

  for (Nterm *t = f; t != NULL; t = t->next)
    {
      M->elem_bin_out(o, t->monom);
      K->elem_bin_out(o, t->coeff);
    }
}

ring_elem PolynomialRing::eval(const RingMap *map, const ring_elem f) const
{
  // The way we collect the result depends on whether the target ring
  // is a polynomial ring: if so, use a heap structure.  If not, just add to the result.

  const Ring *target = map->get_ring();
  if (target->is_poly_ring())
    {
      intarray vp;
      polyheap H(target);
      
      for (Nterm *t = f; t != NULL; t = t->next)
	{
	  vp.shrink(0);
	  M->to_varpower(t->monom, vp);
	  ring_elem g = map->eval_term(K, t->coeff, vp.raw());
	  H.add(g);
	}
      return H.value();
    }
  else 
    {
      ring_elem result = target->from_int(0);
      intarray vp;
      
      for (Nterm *t = f; t != NULL; t = t->next)
	{
	  vp.shrink(0);
	  M->to_varpower(t->monom, vp);
	  ring_elem g = map->eval_term(K, t->coeff, vp.raw());
	  target->add_to(result, g);
	}
      return result;
    }
}


int PolynomialRing::n_terms(const ring_elem f) const
{
  int result = 0;
  for (Nterm *a = f; a != NULL; a = a->next)
    result++;
  return result;
}

ring_elem PolynomialRing::get_terms(const ring_elem f, int lo, int hi) const
{
  Nterm *v = f;
  Nterm head;
  Nterm *result = &head;
  int nmons = n_terms(f);
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

void PolynomialRing::apply_ring_elements(Nterm * &f, vec rsyz, const array<ring_elem> &elems) const
{
  // f in ring
  // rsyz in Rsyz
  // Modify f using the ring elements in rsyz.
  ring_elem ff = f;
  for (vec t = rsyz; t != NULL; t = t->next)
    {
      ring_elem r = elems[t->comp];
      ring_elem f1 = imp_mult_by_term(r, t->coeff, t->monom);
      add_to(ff, f1);
    }
  f = ff;
}
void PolynomialRing::normal_form_ZZ(Nterm *&f) const
{
  Nterm head;
  Nterm *result = &head;

  while (f != NULL)
    {
      vec gsyz, rsyz;
      bool reduces = RidealZ->search(f->coeff, f->monom,
				     gsyz, rsyz);
      if (rsyz != NULL)	
	{
	  apply_ring_elements(f,rsyz,quotient_ideal);
	  Rsyz->remove(rsyz);
	}
      if (!reduces)
	{
	  result->next = f;
	  f = f->next;
	  result = result->next;
	}
    }
  result->next = NULL;
  f = head.next;
}

void PolynomialRing::normal_form(Nterm *&f) const
{
  if (coefficients_are_ZZ)
    {
      normal_form_ZZ(f);
      return;
    }
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, normal_exp);
      int_bag *b;
      if (Rideal.search_expvector(normal_exp, b))
	{
	  Nterm *s = (Nterm *) (b->basis_ptr());
	  ring_elem tf = t;
	  ring_elem coeff;
	  imp_cancel_lead_term(tf, s, coeff, normal_m);
	  K->remove(coeff);
	  t = tf;
	}
      else
	{
	  result->next = t;
	  t = t->next;
	  result = result->next;
	}
    }
  result->next = NULL;
  f = head.next;
}

/////////////////////////////////////////
// Useful division algorithm routines ///
/////////////////////////////////////////
// These are private routines, called from remainder
// or remainderAndQuotient or quotient.
/////////////////////////////////////////
Nterm * PolynomialRing::division_algorithm(Nterm *f, Nterm *g, Nterm *&quot) const
{
  // This returns the remainder, and sets quot to be the quotient.

  // This does standard division by one polynomial.
  // However, it does work for Weyl algebra, skew commutative algebra,
  // This works if the coefficient ring is a field, or ZZ.

  ring_elem a = copy(f);
  Nterm *t = a;
  Nterm *b = g;
  Nterm divhead;
  Nterm remhead;
  Nterm *divt = &divhead;
  Nterm *remt = &remhead;
  Nterm *nextterm = new_term();

  //  buffer o;
  while (t != NULL)
    if (M->divides(b->monom, t->monom))
      {
	//o << "t = "; elem_text_out(o,t); o << newline;
	a = t;
	bool cancelled = imp_attempt_to_cancel_lead_term(a, g, nextterm->coeff, nextterm->monom);
	t = a;
	//	o << "  new t = "; elem_text_out(o,t); o << newline;
	//      o << "  cancelled = " << (cancelled ? "true" : "false") << newline;
	//	o << "  coeff = "; K->elem_text_out(o,nextterm->coeff); o << newline;
	//	emit(o.str());
	if (!K->is_zero(nextterm->coeff))
	  {
	    divt->next = nextterm;
	    divt = divt->next;
	    nextterm = new_term();
	  }
	if (!cancelled)
	  {
	    remt->next = t;
	    remt = remt->next;
	    t = t->next;
	  }
      }
    else
      {
	remt->next = t;
	remt = remt->next;
	t = t->next;
      }

  nextterm = NULL;
  ring_elem nt = nextterm;
  remove(nt);
  remt->next = NULL;
  divt->next = NULL;
  quot = divhead.next;
  return remhead.next;
}

Nterm * PolynomialRing::division_algorithm(Nterm *f, Nterm *g) const
{
  // This does standard division by one polynomial, returning the remainder.
  // However, it does work for Weyl algebra, skew commutative algebra,
  // as long as the coefficient ring is a field.

  ring_elem a = copy(f);
  Nterm *t = a;
  Nterm *b = g;
  Nterm remhead;
  Nterm *remt = &remhead;
  ring_elem c;
  int *m = M->make_one();
  while (t != NULL)
    if (M->divides(b->monom, t->monom))
      {
	a = t;
	bool cancelled = imp_attempt_to_cancel_lead_term(a, g, c, m);
	t = a;
	if (!cancelled)
	  {
	    remt->next = t;
	    remt = remt->next;
	    t = t->next;
	  }
	K->remove(c);
      }
    else
      {
	remt->next = t;
	remt = remt->next;
	t = t->next;
      }

  M->remove(m);
  remt->next = NULL;
  return remhead.next;
}

ring_elem PolynomialRing::term(const ring_elem a, const int *m) const
{
  if (K->is_zero(a)) return (Nterm *)NULL;
  Nterm *t = new_term();
  t->coeff = K->copy(a);
  M->copy(m, t->monom);
  t->next = NULL;
  if (base_ring != NULL) normal_form(t);
  return t;
}

ring_elem PolynomialRing::lead_coeff(const ring_elem f) const
{
  Nterm *t = f;
  if (t == NULL) return K->from_int(0);
  return K->copy(t->coeff);
}

ring_elem PolynomialRing::coeff_of(const ring_elem f, const int *m) const
{
  // m is a packed monomial
  for (Nterm *t = f; t != NULL; t = t->next)
    if (M->compare(m, t->monom) == 0)
      return K->copy(t->coeff);

  return K->from_int(0);
}

ring_elem PolynomialRing::get_coeff(const ring_elem f, const int *vp) const
{
  // note: vp is a varpower monomial.

  intarray ma;
  int *m = ma.alloc(M->monomial_size());
  M->from_varpower(vp, m);
  return coeff_of(f, m);
}

ring_elem PolynomialRing::diff_by_term(const int *exp, const ring_elem f,
				       int use_coeff) const
{
  // The result terms will be in the same order as those of f.
  // NOT valid for skew commutative rings, although currently
  // this routine is only used by Weyl algebra stuff.
  Nterm head;
  Nterm *result = &head;
  int *exp2 = new int[nvars];
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      M->to_expvector(t->monom, exp2);
      if (ntuple::divides(nvars,exp,exp2))
	{
	  // Now determine the coefficient.
	  ring_elem c = K->copy(t->coeff);
	  if (use_coeff)
	    {
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
	  M->from_expvector(exp2, result->monom);
	}
    }
  delete [] exp2;
  result->next = NULL;
  return head.next;
}

Nterm *PolynomialRing::resize(const PolynomialRing *R, Nterm *f) const
    // assumptions: (1) f is a polynomial in the ring R.
    // (2) the current ring is the same as 'R', except for
    // the size of monomials (i.e: same base ring, same 
    // monomial order, same degree info)
{
  Nterm head;
  Nterm *result = &head;
  intarray expa;
  int *exp = expa.alloc(nvars);
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K->copy(t->coeff);
      R->M->to_expvector(t->monom, exp);
      M->from_expvector(exp, result->monom);
    }
  result->next = NULL;
  return head.next;
}

void PolynomialRing::sort(Nterm *&f) const
{
  // Divide f into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.
  
  if (f == NULL || f->next == NULL) return;
  Nterm *f1 = NULL;
  Nterm *f2 = NULL;
  while (f != NULL)
    {
      Nterm *t = f;
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
  ring_elem g = f1;
  ring_elem h = f2;
  add_to(g, h);
  f = g;
}

