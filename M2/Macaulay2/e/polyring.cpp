// Copyright 1996 Michael E. Stillman

#include "polyring.hpp"
#include "text_io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "matrix.hpp"
#include "ZZ.hpp"
#include "ntuple.hpp"
#include "termideal.hpp"
#include "gbring.hpp"
#include "frac.hpp"
#include "geopoly.hpp"

#include "gb_comp.hpp"
#include "vector.hpp"

#define POLY(q) ((q).poly_val)

PolynomialRing::~PolynomialRing()
{
  // Nothing to do
}

void PolynomialRing::initialize_poly_ring(const Ring *K, const Monoid *M)
{
  initialize_ring(K->charac(),
		  M->n_vars(),
		  M->n_vars() + K->total_n_vars(),
		  K,
		  M,
		  M->degree_monoid());

  _poly_size = 
    sizeof(Nterm) + sizeof(int) * (M_->monomial_size() - 1);

  _gb_ring = 0;
  _base_ring = 0;
#if 0
  // _quotient_ideal is initialized to 0 already
  _Rideal = 0;
  _RidealZZ = 0;
#endif

  _quotient_gb = 0;

  _coefficients_are_ZZ = (K_->is_ZZ());
  _isgraded = M_->degree_monoid()->n_vars() > 0;
  _is_skew = 0;
  _EXP1 = new int[_nvars];
  _EXP2 = new int[_nvars];
  _EXP3 = new int[_nvars];

  trans_MONOM1 = M->make_one(); // Used in gbvector <--> vec translation
  trans_EXP1 = new int[_nvars];

  flattened_ring = make_flattened_ring();
}

PolynomialRing *PolynomialRing::create(const Ring *K, const Monoid *MF)
{
  PolynomialRing *result = new PolynomialRing;
  result->initialize_poly_ring(K,MF);
  const PolynomialRing *flatR = result->get_flattened_ring();
  result->_gb_ring = GBRing::create_PolynomialRing(flatR->Ncoeffs(), flatR->Nmonoms());
  return result;
}

const PolynomialRing *PolynomialRing::make_flattened_ring()
{
  // This is called only from PolynomialRing::create.  Thus, K[M]
  // is not a quotient ring.
  if (K_->is_basic_ring())
    return this;
  else {
    // If K is not basic, then it can be one of:
    //    QQ 
    //    frac(R) (possibly has flattened ring)
    //    poly ring (has flattened ring)
    //    skew poly ring (has flattened ring)
    //    Weyl algebra (has flattened ring)
    //    quotient ring  (has flattened ring)
    const Ring *Q = (const Ring *) globalQQ;
    if (K_ == Q) return PolynomialRing::create(globalZZ,Nmonoms());
    
    const PolynomialRing *R1;
    const FractionField *F;
    if ((F=K_->cast_to_FractionField()) != 0)
      {
	R1 = F->get_ring()->cast_to_PolynomialRing();
	if (R1 == 0) { ERROR("internal error: cannot create flattened ring"); exit(1);}
      }
    else
      {
	R1 = K_->cast_to_PolynomialRing();
	if (R1 == 0) { ERROR("internal error: cannot create flattened ring"); exit(1);}
	R1 = R1->get_flattened_ring();
	// Now we must make the poly ring R1[M], where M is the current monoid.
	const Monoid *M1 = R1->Nmonoms();
	const Monoid *newM = Monoid::tensor_product(Nmonoms(), M1);
	const PolynomialRing *P1 = PolynomialRing::create(R1->Ncoeffs(), newM);

	// P1 is the result, unless R1 is not commutative, or this is non-commutative.
#warning "make this ring non-commutative, if needed"
	return P1;
      }
  }
  return 0;
}

PolynomialRing *PolynomialRing::create_quotient_ring(Computation *G)
{
  const PolynomialRing *R = G->get_ring();
  PolynomialRing *result = new PolynomialRing;
  *result = *R;
  result->_quotient_gb = G;
  result->_base_ring = R;
  return result;
}

#if 0
PolynomialRing *PolynomialRing::create_quotient_ring(
    const PolynomialRing *R, const array<ring_elem> &I)
{
  PolynomialRing *obj = new PolynomialRing(R);
  if (obj->coefficients_are_ZZ)
    {
      obj->_is_ZZ_quotient = false;
      obj->_ZZ_quotient_value = (Nterm*)0;
      for (int i=0; i<I.length(); i++)
	{
	  Nterm *f = I[i];
	  if (f->next == 0 && R->M_->is_one(f->monom))
	    {
	      obj->_is_ZZ_quotient = true;
	      obj->_ZZ_quotient_value = R->K_->copy(f->coeff);
	      break;
	    }
	}
      obj->make_RidealZZ(I);
      obj->make_Rideal(I);
    }
  else
    obj->make_Rideal(I);

  obj->isgraded = obj->_base_ring->isgraded;
  if (obj->isgraded)
    for (int i=0; i<obj->_quotient_ideal.length(); i++)
      if (!obj->_base_ring->is_homogeneous(obj->_quotient_ideal[i]))
	{
	  obj->isgraded = false;
	  break;
	}

  obj->_gb_ring = GBRing::create_PolynomialRing(obj);
  obj->_grtype = R->get_GRType();
  return obj;
}
#endif

#if 0
const FreeModule *PolynomialRing::get_Rsyz() const 
{
#warning "implement PolynomialRing::get_Rsyz()"
#if 0
  // MES Aug 2002
  if (_RidealZZ == NULL) return NULL;
  return _RidealZZ->get_Rsyz();
#endif
  return 0;
}

Matrix PolynomialRing::get_ideal() const
{
  const PolynomialRing *R = this;
  while (R->_base_ring != NULL) R = R->_base_ring;
  Matrix result(R->make_FreeModule(1));
  for (int i=0; i<_quotient_ideal.length(); i++)
    result.append(result.rows()->raw_term(copy(_quotient_ideal[i]),0));
  return result;
}
#endif

#if 0
bool PolynomialRing::equals(const object_element *o) const
{
  if (o->class_id() != class_id())
    return false;

  const PolynomialRing *R2 = (PolynomialRing *)o;
  if (R2->K != K) return false;
  if (R2->M != M) return false;
  if (R2->_base_ring != _base_ring) return false;
  if (R2->_quotient_ideal.length() != _quotient_ideal.length())
    return false;
  // MESXX: this is not necessarily correct!!  It assumes that
  // the elements are in the same order...
  for (int i=0; i<_quotient_ideal.length(); i++)
    if (!is_equal(R2->_quotient_ideal[i], _quotient_ideal[i]))
      return false;
  return true;
}
#endif

void PolynomialRing::text_out(buffer &o) const
{
  K_->text_out(o);
  M_->text_out(o);
  if (_base_ring != NULL)
    {
#warning "text_out doesn't handle quotient rings"
#if 0
      o << "/(";
      int n = _quotient_ideal.length();
      for (int i=0; i<n; i++)
	{
	  if (i != 0) o << ", ";
	  _base_ring->elem_text_out(o, _quotient_ideal[i]);
	}
      o << ')';
#endif
    }
}

Nterm *PolynomialRing::new_term() const
{
  Nterm *result = (Nterm *)getmem(_poly_size);
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
  result->coeff = K_->copy(t->coeff);
  M_->copy(t->monom, result->monom);
  return result;
}

#if 0
void PolynomialRing::make_RidealZZ(const array<ring_elem> &polys)
{
  // If coefficients_are_ZZ, then
  // this routine sets the fields:
  // _quotient_ideal, RidealZ.
#if 0
  const PolynomialRing *S = _base_ring;
  while (S->_base_ring != NULL) S = S->_base_ring;

  _RidealZZ = TermIdeal::make_ring_termideal(S,
			   _base_ring->_quotient_ideal, 
			   polys,
			   _quotient_ideal);
#endif
}
void PolynomialRing::make_Rideal(const array<ring_elem> &polys)
{
  int i, top;
  queue<Bag *> elems;
  intarray vp;

  top = _base_ring->_quotient_ideal.length();
  for (i=0; i<top; i++)
    {
      ring_elem q = copy(_base_ring->_quotient_ideal[i]);
      if (is_zero(q)) continue;
      M_->to_varpower(POLY(q)->monom, vp);
      elems.insert(new Bag((void *)q, vp));
      vp.shrink(0);
    }

  top = polys.length();
  for (i=0; i<top; i++)
    {
      ring_elem q = copy(polys[i]);
      if (is_zero(q)) continue;
      M_->to_varpower(POLY(q)->monom, vp);
      elems.insert(new Bag((void *)q, vp));
      vp.shrink(0);
    }

  //  _Rideal = MonomialIdeal(this, elems);
  _Rideal = new MonomialIdeal(_base_ring, elems);

  for (Index<MonomialIdeal> j = _Rideal->first(); j.valid(); j++)
    {
#if 0
      ring_elem f = (Nterm *) (*_Rideal)[j]->basis_ptr();

      // The following line adds the element to f, it is not already there.
      // Over ZZ, RidealZ has been set, and at the same time, _quotient_ideal
      // was set too.

      // MES Aug 2002
      if (_RidealZZ == 0)
	_quotient_ideal.append(f);
#endif
    }
}
#endif

ring_elem PolynomialRing::from_int(int n) const
{
  ring_elem a = K_->from_int(n);
  if (K_->is_zero(a)) 
    {
      return (Nterm *)NULL;
    }
  Nterm *result = new_term();
  result->coeff = a;
  M_->one(result->monom);
  if (_base_ring != NULL) normal_form(result);
  return result;
}
ring_elem PolynomialRing::from_int(mpz_t n) const
{
  ring_elem a = K_->from_int(n);
  if (K_->is_zero(a)) 
    {
      return (Nterm *)NULL;
    }
  Nterm *result = new_term();
  result->coeff = a;
  M_->one(result->monom);
  if (_base_ring != NULL) normal_form(result);
  return result;
}
ring_elem PolynomialRing::from_double(double n) const
{
  ring_elem a = K_->from_double(n);
  if (K_->is_zero(a)) 
    {
      return (Nterm *)NULL;
    }
  Nterm *result = new_term();
  result->coeff = a;
  M_->one(result->monom);
  if (_base_ring != NULL) normal_form(result);
  return result;
}

ring_elem PolynomialRing::var(int v, int n) const
{
  if (_is_skew && v >= 0 && v < _nvars && n > 1 && _skew.is_skew_var(v))
    return (Nterm *)NULL;

  for (int i=0; i<_nvars; i++) _EXP1[i] = 0;
  if (v >= 0 && v < _nvars) _EXP1[v] = n;

  Nterm *result = new_term();
  result->coeff = K_->from_int(1);
  M_->from_expvector(_EXP1, result->monom);
  if (_base_ring != NULL) normal_form(result);
  return result;
}

int PolynomialRing::index_of_var(const ring_elem a) const
{
  Nterm *f = a;
  if (f->next != 0) return -1;
  if (!K_->is_equal(f->coeff, K_->from_int(1))) return -1;
  M_->to_expvector(f->monom, _EXP1);
  int result = -1;
  for (int i=0; i<n_vars(); i++)
    if (_EXP1[i] > 1) return -1;
    else if (_EXP1[i] == 1)
      {
	if (result >= 0) return -1;
	result = i;
      }
  return result;
}

M2_arrayint PolynomialRing::support(const ring_elem a) const
{
  for (int i=0; i<n_vars(); i++) _EXP1[i] = 0;
  for (const Nterm *f = a; f != 0; f = f->next)
    {
      M_->to_expvector(f->monom, _EXP2);
      for (int j=0; j<n_vars(); j++)
	if (_EXP2[j] != 0) _EXP1[j] = 1;
    }
  int nelems = 0;
  for (int i=0; i<n_vars(); i++)
    if (_EXP1[i] > 0) nelems++;
  M2_arrayint result = makearrayint(nelems);
  int next = 0;
  for (int i=0; i<n_vars(); i++)
    if (_EXP1[i] > 0)
	result->array[next++] = i;
  return result;
}

bool PolynomialRing::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // case 1:  Rf = A[x]/J ---> A[x]/I  is one of the 'base_ring's of 'this'.
  // case 2:  Rf = A      ---> A[x]/I  is the ring of scalars

  for (const PolynomialRing *base = _base_ring; base != NULL; base = base->_base_ring)
    if (base == Rf)
      {
	Nterm *g = copy(f);
	normal_form(g);
	result = g;
	return true;
      }
  if (K_ == Rf)
    {
      int *m = M_->make_one();
      result = term(f,m);
      //      M_->remove(m);
      return true;
    }
  return false;
}

bool PolynomialRing::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // case 1:  Rf = A[x]/J ---> A[x]/I  is one of the 'base_ring's of 'this'.
  // case 2:  Rf = A      ---> A[x]/I  is the ring of scalars

  if (K_ == Rg)
    {
      Nterm *g = f;
      if (g == NULL)
	{
	  result = K_->from_int(0);
	  return true;
	}
      if (g->next != 0) return false;
      if (!M_->is_one(g->monom)) return false;
      result = K_->copy(g->coeff);
      return true;
    }
  for (const PolynomialRing *base = _base_ring; base != NULL; base = base->_base_ring)
    if (base == Rg)
      {
	result = copy(f);	// We are using the same representation
	return true;
      }
  return false;
}

ring_elem PolynomialRing::preferred_associate(ring_elem ff) const
{
  Nterm *f = ff;
  if (f == NULL) return from_int(1);
  ring_elem c = K_->preferred_associate(f->coeff);
  Nterm *t = new_term();
  t->coeff = c;
  M_->one(t->monom);
  t->next = 0;
  return t;
}

bool PolynomialRing::is_unit(const ring_elem ff) const
{
  Nterm *f = ff;
  if (f == NULL) return false;
  if (f->next == NULL && M_->is_one(f->monom)
      && K_->is_unit(f->coeff))
    return true;

  if (_base_ring == NULL)
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
      if (!K_->is_equal(a->coeff, b->coeff)) return false;
      if (_nvars > 0 && (M_->compare(a->monom, b->monom) != 0))
	return false;
    }
}

bool PolynomialRing::is_homogeneous(const ring_elem f) const
{
  Nterm *t = f;
  if (t == 0) return true;
  bool result = true;
  int *e = degree_monoid()->make_one();
  int *degf = degree_monoid()->make_one();
  M_->multi_degree(t->monom, degf);
  for (t = t->next ; t != NULL; t = t->next)
    {
      M_->multi_degree(t->monom, e);
      if (0 != degree_monoid()->compare(degf, e))
	{
	  result = false;
	  break;
	}
    }
  return result;
}

void PolynomialRing::degree(const ring_elem f, int *degf) const
{
  multi_degree(f, degf);
}

bool PolynomialRing::multi_degree(const ring_elem f, int *degf) const
{
  Nterm *t = f;
  int *e = degree_monoid()->make_one();
  if (t == 0) 
    {
      degree_monoid()->one(degf);
      return true;
    }
  M_->multi_degree(t->monom, degf);
  bool result = true;
  for (t = t->next ; t != NULL; t = t->next)
    {
      M_->multi_degree(t->monom, e);
      if (0 != degree_monoid()->compare(degf, e))
	{
	  result = false;;
	  degree_monoid()->lcm(degf, e, degf);
	}
    }
  return result;
}

int PolynomialRing::primary_degree(const ring_elem f) const
{
  Nterm *t = f;
  if (t == NULL) return 0;
  return M_->primary_degree(t->monom);
}

void PolynomialRing::degree_weights(const ring_elem f, const M2_arrayint wts, 
				    int &lo, int &hi) const
{
  Nterm *t = f;
  if (t == NULL)
    {
      lo = hi = 0;
      return;
    }
  int e = M_->degree_weights(t->monom, wts);
  lo = hi = e;
  for (t = t->next; t != NULL; t = t->next)
    {
      e = M_->degree_weights(t->monom, wts);
      if (e > hi) hi = e;
      else if (e < lo) lo = e;
    }
}

ring_elem PolynomialRing::homogenize(const ring_elem f, 
			     int v, int d, const M2_arrayint wts) const
{
  // assert(wts[v] != 0);
  // If an error occurs, then return 0, and set gError.

  intarray expa;
  int *exp = expa.alloc(_nvars);

  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f ; a != NULL; a = a->next)
    {
      M_->to_expvector(a->monom, exp);
      int e = 0;
      for (int i=0; i<_nvars; i++) e += wts->array[i] * exp[i];
      if (((d-e) % wts->array[v]) != 0)
	{
	  // We cannot homogenize, so clean up and exit.
	  result->next = NULL;
	  ERROR("homogenization impossible");
	  result = NULL;
	  return result;
	}
      exp[v] += (d - e) / wts->array[v];
      if (_is_skew && _skew.is_skew_var(v) && exp[v] > 1)
	continue;
      result->next = new_term();
      result = result->next;
      result->coeff = K_->copy(a->coeff);
      M_->from_expvector(exp, result->monom);
    }
  result->next = NULL;
  sort(head.next);		// The monomial order, etc. might all have changed.
				// Some terms might even drop out
  if (_base_ring != NULL) normal_form(head.next);
  return head.next;
}

ring_elem PolynomialRing::homogenize(const ring_elem f, int v, M2_arrayint wts) const
{
  Nterm *result = NULL;
  if (POLY(f) == NULL) return result;
  int lo, hi;
  degree_weights(f, wts, lo, hi);
  assert(wts->array[v] != 0);
  int d = (wts->array[v] > 0 ? hi : lo);
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
}

void PolynomialRing::negate_to(ring_elem &f) const
{
  Nterm *v = f;
  while (v != NULL)
    {
      K_->negate_to(v->coeff);
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
    switch (M_->compare(f->monom, g->monom))
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
	K_->add_to(tmf->coeff, tmg->coeff);
	if (_is_ZZ_quotient)
	  {
	    ring_elem t = K_->remainder(tmf->coeff, _ZZ_quotient_value);
	    tmf->coeff = t;
	  }
	if (!K_->is_zero(tmf->coeff))
	  {
	    result->next = tmf;
	    result = result->next;
	  }
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
      result->coeff = K_->negate(a->coeff);
      M_->copy(a->monom, result->monom);
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

ring_elem PolynomialRing::imp_mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
   // return f*c*m
{
  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K_->mult(a->coeff, c);
      M_->mult(m, a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

void PolynomialRing::imp_subtract_multiple_to(ring_elem &f, 
				 ring_elem a, const int *m, const ring_elem g) const
{
  ring_elem b = K_->negate(a);
  ring_elem h = imp_mult_by_term(g, b, m);
  add_to(f, h);
}

ring_elem PolynomialRing::mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
   // return f*c*m
{
  Nterm *result = imp_mult_by_term(f, c, m);
  if (_base_ring != NULL) normal_form(result);
  return result;
}

void PolynomialRing::subtract_multiple_to(ring_elem &f, 
				 ring_elem a, const int *m, const ring_elem g) const
{
  ring_elem b = K_->negate(a);
  ring_elem h = mult_by_term(g, b, m);
  add_to(f, h);
}

void PolynomialRing::auto_reduce_to(ring_elem &f, ring_elem g) const
    // f -= c g, where c = coeff of in(g) in f
{
  Nterm *t = g;
  ring_elem a = coeff_of(f, t->monom);
  if (K_->is_zero(a)) return;

  intarray ma;
  int *m = ma.alloc(M_->monomial_size());
  M_->one(m);
  imp_subtract_multiple_to(f, a, m, g);
}

void PolynomialRing::make_monic(ring_elem &f) const
{
  Nterm *t = f;
  if (t == NULL) return;
  ring_elem a = K_->invert(t->coeff);
  for ( ; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K_->mult(a, tmp);
    }
}
void PolynomialRing::mult_coeff_to(ring_elem a, ring_elem &f) const
{
  Nterm *t = f;
  if (t == NULL) return;
  for ( ; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K_->mult(a, tmp);
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
      if (ZZ::mod_ui(n,2) == 1)
	{
	  tmp = mult(prod, base);
	  prod = tmp;
	}
      mpz_tdiv_q_2exp(n, n, 1);
      if (mpz_sgn(n) == 0)
	{
	  mpz_clear(n);
	  return prod;
	}
      else
	{
	  tmp = mult(base, base);
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
	  prod = tmp;
	}
      n >>= 1;
      if (n == 0)
	{
	  return prod;
	}
      else
	{
	  tmp = mult(base, base);
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
  if (_base_ring == NULL)
    {
      int n1;
      // In this case, the computation may only be formed in two
      // cases: (1) f is a constant, or (2) n is small enough
      if (ZZ::get_si(n1,n))
	{
	  result = power(f,n1);
	}
      else if (is_unit(f))  // really want a routine 'is_scalar'...
	{
	  ring_elem a = K_->power(f->coeff, n);
	  result = term(a, f->monom);
	}
      else 
	{
	  ERROR("exponent too large");
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
  if (_base_ring != NULL)
      return power2(ff,n);

  intarray ma;

  rest = lead->next;
  g = from_int(1);

  // Start the result with the n th power of the lead term
  Nterm *t = new_term();
  t->coeff = K_->power(lead->coeff, n);
  M_->power(lead->monom, n, t->monom);
  t->next = NULL;
  //  if (_base_ring != NULL) normal_form(t);  NOT NEEDED
  result = t;

  if (rest == (Nterm *)NULL) return result;
  int *m = ma.alloc(M_->monomial_size());
  M_->one(m);

  mpz_t bin_c;

  mpz_init_set_ui(bin_c, 1);

  for(int i=1; i<=n ; i++)
    {
      tmp = mult(g, rest);
      g = tmp;

      mpz_mul_ui(bin_c, bin_c, n-i+1);
      mpz_div_ui(bin_c, bin_c, i);

      coef1 = K_->from_int(bin_c);

      if (!K_->is_zero(coef1))
	{
	  coef2 = K_->power(lead->coeff, n-i);
	  coef3 = K_->mult(coef1, coef2);
	  M_->power(lead->monom, n-i, m);

	  h = mult_by_term(g, coef3, m);
	  add_to(result, h);
	}
    }
  return result;
}

ring_elem PolynomialRing::invert(const ring_elem f) const
{
  Nterm *ft = f;
  if (is_zero(f))
    {
      ERROR("cannot divide by zero");
      return (Nterm *)NULL;
    }
  if (ft->next == NULL)
    if (M_->is_one(ft->monom))
      {
	Nterm *t = new_term();
	t->coeff = K_->invert(ft->coeff);
	M_->one(t->monom);
	return t;
      }
    else if (M_->is_group())
      {
	Nterm *t = new_term();
	t->coeff = K_->invert(ft->coeff);
	M_->power(ft->monom, -1, t->monom);
	return t;
      }
#warning "invert doesn't handle quotient rings yet"
#if 0
  if (_nvars == 1 && _quotient_ideal.length() == 1 && K_->is_field())
    {
      ring_elem u,v;
      ring_elem F = _quotient_ideal[0];
      ring_elem g = _base_ring->gcd_extended(F, f, u, v);
      if (!_base_ring->is_unit(g))
	{
	  ERROR("element is not invertible");
	  // MES: what about setting some global error ring element
	  // which contains this 'certificate' g of non-field-ness?
	}
      return v;
    }
  else
#endif
    {
      ERROR("division is not defined in this ring");
      return (Nterm *)NULL;
    }
}

ring_elem PolynomialRing::divide(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem d = divide(f,g,rem);
  if (is_zero(rem)) return d;
  ring_elem ginv = invert(g);
  ring_elem result = mult(f, ginv);
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
  coeff = K_->divide(t->coeff, s->coeff); // exact division
  if (_is_skew)
    {
      M_->to_expvector(t->monom, _EXP1);
      M_->to_expvector(s->monom, _EXP2);
      int sign = _skew.divide(_EXP1, _EXP2, _EXP3);
      M_->from_expvector(_EXP3, monom);
      if (sign < 0) K_->negate_to(coeff);
      imp_subtract_multiple_to(f, coeff, monom, g);
    }
  else
    {
      M_->divide(t->monom, s->monom, monom);
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
  ring_elem r = K_->remainderAndQuotient(t->coeff, s->coeff, coeff);
  bool result = (K_->is_zero(r));  // true means lead term will be cancelled.
  if (!K_->is_zero(coeff))
    {
      if (_is_skew)
	{
	  M_->to_expvector(t->monom, _EXP1);
	  M_->to_expvector(s->monom, _EXP2);
	  int sign = _skew.divide(_EXP1, _EXP2, _EXP3);
	  M_->from_expvector(_EXP3, monom);
	  if (sign < 0) K_->negate_to(coeff);
	  imp_subtract_multiple_to(f, coeff, monom, g);
	}
      else
	{
	  M_->divide(t->monom, s->monom, monom);
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
  coeff = K_->divide(t->coeff, s->coeff); // exact division in a field
  if (_is_skew)
    {
	  M_->to_expvector(t->monom, _EXP1);
	  M_->to_expvector(s->monom, _EXP2);
	  int sign = _skew.divide(_EXP1, _EXP2, _EXP3);
	  M_->from_expvector(_EXP3, monom);
	  if (sign < 0) K_->negate_to(coeff);
	  subtract_multiple_to(f, coeff, monom, g);
    }
  else
    {
      M_->divide(t->monom, s->monom, monom);
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
    if (M_->divides(b->monom, t->monom))
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
  if (_nvars != 1)
    {
      ERROR("multivariate gcd not yet implemented");
      return (Nterm *)NULL;
    }
  ring_elem f = copy(ff);
  ring_elem g = copy(gg);
  ring_elem s, rem;
  while (!is_zero(g))
    {
      s = divide(f, g, rem);
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
      ERROR("cannot use gcd_extended in this ring");
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
      ring_elem c = K_->invert(t->coeff);
      mult_coeff_to(c, result);
      mult_coeff_to(c, u);
    }

  // The following is v = (result - f*u)/g
  temp1 = mult(f,u);
  temp2 = subtract(result, temp1);
  v = divide(temp2, g, temp3);

  return result;
}

void PolynomialRing::minimal_monomial(ring_elem f, int * &monom) const
{
  // Determines the minimal monomial which divides each term of f.
  // This monomial is placed into 'monom'.
  
  Nterm *t = f;
  if (t == NULL) return;
  M_->copy(t->monom, monom);
  for (t = t->next; t!=NULL; t=t->next)
    M_->gcd(t->monom,monom,monom);
}

ring_elem PolynomialRing::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem;
  rem = remainderAndQuotient(f,g,quot);
  return rem;
}

ring_elem PolynomialRing::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem;
  rem = remainderAndQuotient(f,g,quot);
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
      if (M_->is_group())
	{
	  Nterm *f1 = f;
	  Nterm *g1 = g;
	  r = powerseries_division_algorithm(f1,g1,q);
	  quot = q;
	  return r;
#if 0
	  // First factor out powers of minimal monomial from f, g.
	  ring_elem one_kk = K_->from_int(1);
	  int *one = M_->make_one();
	  int *mf1 = M_->make_one();
	  int *mg1 = M_->make_one();
	  int *mf = M_->make_one();
	  int *mg = M_->make_one();
	  PolynomialRing::minimal_monomial(f,mf);
	  PolynomialRing::minimal_monomial(g,mg);
#if 0
	  buffer o;
	  o << "Minimal monomial of ";
	  elem_text_out(o,f);
	  o << " is ";
	  M_->elem_text_out(o,mf);
	  o << newline;

	  o << "Minimal monomial of ";
	  elem_text_out(o,g);
	  o << " is ";
	  M_->elem_text_out(o,mg);
	  o << newline;
	  emit(o.str());
#endif
	  M_->divide(one,mf,mf1);
	  M_->divide(one,mg,mg1);
	  ring_elem f1 = mult_by_term(f,one_kk,mf1);
	  ring_elem g1 = mult_by_term(g,one_kk,mg1);
	  r = division_algorithm(f1,g1,q);
	  M_->mult(mf,mg1,mg);
	  rem = mult_by_term(r,one_kk,mf);
	  quot = mult_by_term(q,one_kk,mg);
	  return rem;
#endif
	}
      else if (!is_quotient_poly_ring())
	{
	  rem = division_algorithm(f,g,q);
	  quot = q;
	  return rem;
	}
      else if (false) //(n_vars() == 1 && K_->is_field())
	{
	  // Case 2: There is a quotient ideal, but we have one variable, over
	  //         a field.  In this case, we can use gcd in k[x].
	  //         The ring must be commutative here: skew in one variable isn't
	  //         handled here.
	}
      else if (K_->is_field() || K_->is_ZZ())
	{
#if 0
// MES Aug 2002: ifdef'ed this section because gb_comp is not back yet
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
	  Matrix *m = new Matrix(F);
	  m->append(F->raw_term(copy(g),0));
	  gb_comp *g = gb_comp::make(m,false,-1,0);
	  g->calc(0, syzygy_stop_conditions);

	  // Reduce f wrt this GB.
	  Vector *v = Vector::make_raw(F,F->raw_term(copy(f),0));
	  Vector *lifted;
	  Vector *red = g->reduce(v,lifted);
	  // Now grab the two polynomials of interest:
	  ring_elem result = F->get_coefficient(red->get_value(),0); // Rermainder
	  quot = lifted->free_of()->get_coefficient(lifted->get_value(),0); // Quotient

	  // Remove the GB.
	  delete g;
	  return result;
#endif
	}
      else
	{
	  // Case 4: The coefficients are not ZZ, or a field.  Currently we say:
	  //         not implemented.

	  ERROR("remainder not defined and/or implemented for this ring");
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
#if 0
// MES Aug 2002 ifdef'ed because gb_comp is not back yet
      intarray syzygy_stop_conditions;
      syzygy_stop_conditions.append(0); // ngb
      syzygy_stop_conditions.append(1); // nsyz
      syzygy_stop_conditions.append(0); // npairs
      syzygy_stop_conditions.append(0);
      syzygy_stop_conditions.append(0); 
      syzygy_stop_conditions.append(0); // subring limit
      syzygy_stop_conditions.append(0);
      
      const FreeModule *F = make_FreeModule(1);
      Matrix *m = new Matrix(F);
      m->append(F->raw_term(a,0));
      m->append(F->raw_term(b,0));
#if 0  
  buffer o;
  o << "constructing syzygy on ";
  elem_text_out(o,a);
  o << " and ";
  elem_text_out(o,b);
  emit_line(o.str());
  o.reset();
  o << "matrix is" << newline;
  m->text_out(o);
  emit_line(o.str());
  o.reset();
#endif

      gb_comp *g = gb_comp::make(m,true,-1,0);
      g->calc(0, syzygy_stop_conditions);
      Matrix *s = g->syz_matrix();

#if 0
  if (s.n_cols() != 1)
    {
      o << "found " << s.n_cols() << " syzygies";
      emit_line(o.str());
    }
#endif
      x = s->elem(0,0);
      y = s->elem(1,0);
      ring_elem c = preferred_associate(x);
      ring_elem x1 = mult(c,x);
      ring_elem y1 = mult(c,y);
      x = x1;
      y = y1;
#if 0
  o << "result: x = ";
  elem_text_out(o,x);
  o << " and y = ";
  elem_text_out(o,y);
  emit_line(o.str());
#endif
      delete g;
#endif      
    }
}

ring_elem PolynomialRing::random() const
{
  ERROR("not yet implemented");
  return 0;
}
ring_elem PolynomialRing::random(int /*homog*/, const int * /*deg*/) const
{
  ERROR("not yet implemented");
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
  ring_elem g = const_cast<Nterm *>(f);
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
      int isone = M_->is_one(t->monom);
      p_parens = !isone;
      p_one = (isone && needs_parens) || (isone && old_one);
      K_->elem_text_out(o,t->coeff);
      if (!isone)
	{
//	  if (!K_->is_equal(t->coeff, one) && !K_->is_equal(t->coeff, minus_one))
//	    o << "*";
	  M_->elem_text_out(o, t->monom);
	}
      p_plus = 1;
    }
  if (needs_parens) o << ')';

  p_one = old_one;
  p_parens = old_parens;
  p_plus = old_plus;

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
	  M_->to_varpower(t->monom, vp);
	  ring_elem g = map->eval_term(K_, t->coeff, vp.raw());
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
	  M_->to_varpower(t->monom, vp);
	  ring_elem g = map->eval_term(K_, t->coeff, vp.raw());
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

#if 0
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
#endif

void PolynomialRing::normal_form_ZZ(Nterm *&f) const
{
#if 0
  const FreeModule *Rsyz = get_Rsyz();
  Nterm head;
  Nterm *result = &head;

  while (f != NULL)
    {
      vec gsyz, rsyz;
      int reduces = _RidealZZ->search(f->coeff, f->monom,
				     gsyz, rsyz);
      if (rsyz != NULL)	
	{
	  apply_ring_elements(f,rsyz,_quotient_ideal);
	}
      if (reduces != TI_TERM)
	{
	  result->next = f;
	  f = f->next;
	  result = result->next;
	}
    }
  result->next = NULL;
  f = head.next;
#endif
}
void PolynomialRing::normal_form(Nterm *&f) const
{
#warning "normal_form is commented out for now: rewrite"
#if 0
  if (_coefficients_are_ZZ)
    {
      normal_form_ZZ(f);
      return;
    }
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  int *normal_m = M_->make_one();
  int *normal_exp = new int[_nvars];
  while (t != NULL)
    {
      M_->to_expvector(t->monom, normal_exp);
      int_bag *b;
      if (_Rideal->search_expvector(normal_exp, b))
	{
	  Nterm *s = (Nterm *) (b->basis_ptr());
	  ring_elem tf = t;
	  ring_elem coeff;
	  imp_cancel_lead_term(tf, s, coeff, normal_m);
	  t = tf;
	}
      else
	{
	  result->next = t;
	  t = t->next;
	  result = result->next;
	}
    }
  delete [] normal_exp;
  result->next = NULL;
  f = head.next;
#endif
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
    if (M_->divides(b->monom, t->monom))
      {
	//o << "t = "; elem_text_out(o,t); o << newline;
	a = t;
	bool cancelled = imp_attempt_to_cancel_lead_term(a, g, nextterm->coeff, nextterm->monom);
	t = a;
	//	o << "  new t = "; elem_text_out(o,t); o << newline;
	//      o << "  cancelled = " << (cancelled ? "true" : "false") << newline;
	//	o << "  coeff = "; K_->elem_text_out(o,nextterm->coeff); o << newline;
	//	emit(o.str());
	if (!K_->is_zero(nextterm->coeff))
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
  int *m = M_->make_one();
  while (t != NULL)
    if (M_->divides(b->monom, t->monom))
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
      }
    else
      {
	remt->next = t;
	remt = remt->next;
	t = t->next;
      }

  remt->next = NULL;
  return remhead.next;
}

Nterm * PolynomialRing::powerseries_division_algorithm(Nterm *f, Nterm *g, Nterm *&quot) const
{
  // This is intended for use when there is one variable, inverses are present,
  // and the coefficient ring is a field, or is ZZ.
  // The algorithm used is as follows.
  // Given a non-zero polynomial f = f(t,t^-1), let v(f) = top - bottom
  //   where top is the largest exponent in f, and bottom is the smallest.
  //   So if f is a monomial, v(f) = 0.  Also, v(fg) = v(f)v(g) (at least if the
  //   coefficient ring is a domain), and v(f) >= 0 always.
  // The algorithm is as follows:
  //   Reduce f = f0 by lt(g) to obtain f1, then again to obtain f2, etc.
  //   So v(f1) >= v(f2) >= ... >= v(fi),
  //   and either fi = 0, v(fi) < v(g), or v(f(i+1)) > v(fi).
  //   In this case, the remainder returned is fi.
  //   (Note: the last case won't happen if the coefficients are a field, or the
  //   lead coefficient of g is a unit).


  // This returns the remainder, and sets quot to be the quotient.

  ring_elem a = copy(f);
  Nterm *t = a;
  Nterm *b = g;
  Nterm divhead;
  Nterm remhead;
  Nterm *divt = &divhead;
  Nterm *remt = &remhead;
  Nterm *nextterm = new_term();
  int gval = 0, flast = 0;

  if (a != 0)
    {
      Nterm *z = a;
      for ( ; z->next != 0; z = z->next);

      if (degree_monoid()->n_vars() != 0) flast = M_->primary_degree(z->monom);
      else {
	M_->to_expvector(z->monom, _EXP1);
	flast = ntuple::degree(_nvars, _EXP1);
      }

    }

  if (g != 0)
    {
      int gfirst, glast;
      Nterm *z = b;

      if (degree_monoid()->n_vars() != 0) gfirst = M_->primary_degree(z->monom);
      else {
	M_->to_expvector(z->monom, _EXP1);
	gfirst = ntuple::degree(_nvars, _EXP1);
      }

      for ( ; z->next != 0; z = z->next);

      if (degree_monoid()->n_vars() != 0) glast = M_->primary_degree(z->monom);
      else {
	M_->to_expvector(z->monom, _EXP1);
	glast = ntuple::degree(_nvars, _EXP1);
      }

      gval = abs(gfirst-glast);
    }

  
  //  buffer o;
  while (t != NULL)
    {
      int ffirst;
      
      if (degree_monoid()->n_vars() != 0) ffirst = M_->primary_degree(t->monom);
      else {
	M_->to_expvector(t->monom, _EXP1);
	ffirst = ntuple::degree(_nvars, _EXP1);
      }

      int fval = abs(ffirst-flast);
      if (fval >= gval)
	{
	  //o << "t = "; elem_text_out(o,t); o << newline;
	  a = t;
	  bool cancelled = imp_attempt_to_cancel_lead_term(a, g, nextterm->coeff, nextterm->monom);
	  t = a;
	  //	o << "  new t = "; elem_text_out(o,t); o << newline;
	  //      o << "  cancelled = " << (cancelled ? "true" : "false") << newline;
	  //	o << "  coeff = "; K_->elem_text_out(o,nextterm->coeff); o << newline;
	  //	emit(o.str());
	  if (!K_->is_zero(nextterm->coeff))
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
    }

  nextterm = NULL;
  remt->next = NULL;
  divt->next = NULL;
  quot = divhead.next;
  return remhead.next;
}

ring_elem PolynomialRing::term(const ring_elem a, const int *m) const
{
  if (K_->is_zero(a)) return (Nterm *)NULL;
  Nterm *t = new_term();
  t->coeff = K_->copy(a);
  M_->copy(m, t->monom);
  t->next = NULL;
  if (_base_ring != NULL) normal_form(t);
  return t;
}

ring_elem PolynomialRing::lead_coeff(const ring_elem f) const
{
  Nterm *t = f;
  if (t == NULL) return K_->from_int(0);
  return K_->copy(t->coeff);
}

const int * PolynomialRing::lead_monomial(const ring_elem f) const
{
  Nterm *t = f;
  assert(t != NULL);
  return t->monom;
}

ring_elem PolynomialRing::coeff_of(const ring_elem f, const int *m) const
{
  // m is a packed monomial
  for (Nterm *t = f; t != NULL; t = t->next)
    if (M_->compare(m, t->monom) == 0)
      return K_->copy(t->coeff);

  return K_->from_int(0);
}

ring_elem PolynomialRing::get_coeff(const ring_elem f, const int *vp) const
{
  // note: vp is a varpower monomial.

  intarray ma;
  int *m = ma.alloc(M_->monomial_size());
  M_->from_varpower(vp, m);
  return coeff_of(f, m);
}

ring_elem PolynomialRing::diff(ring_elem a, ring_elem b, int use_coeff) const
{
  polyheap H(this);
  Nterm *d = new_term();
  for (Nterm *s = a; s != 0; s = s->next)
    {
      for (Nterm *t = b; t != 0; t = t->next)
	{
	  d->coeff = diff_term(s->monom, t->monom, d->monom, use_coeff);
	  if (!is_zero(d->coeff))
	    {
	      K_->mult_to(d->coeff, s->coeff);
	      K_->mult_to(d->coeff, t->coeff);
	      d->next = 0;
	      H.add(d);
	      d = new_term();
	    }
	}
    }
  return H.value();
}

ring_elem PolynomialRing::diff_term(const int *m, const int *n, 
				    int *resultmon,
				    int use_coeff) const
  // GRABBED FROM FREEMODULE CODE
{
  int sign = 0;
  if (!M_->divides(m, n)) return K_->from_int(0);
  if (_is_skew && use_coeff)
    {
      M_->to_expvector(m, _EXP1);
      M_->to_expvector(n, _EXP2);
      sign = _skew.diff(_EXP1, _EXP2, _EXP3);
      M_->from_expvector(_EXP3, resultmon);
    }
  else
    M_->divide(n, m, resultmon);
  ring_elem result = K_->from_int(1);
  if (!use_coeff) return result;
  intarray e1, e2;
  int *exp1 = e1.alloc(n_vars());
  int *exp2 = e2.alloc(n_vars());
  M_->to_expvector(m, exp1);
  M_->to_expvector(n, exp2);

  if (_is_skew && sign < 0)
    K_->negate_to(result);

  for (int i=0; i<n_vars(); i++)
    {
      for (int j=exp1[i]-1; j>=0; j--)
	{
	  ring_elem g = K_->from_int(exp2[i]-j);
	  K_->mult_to(result, g);
	  if (K_->is_zero(result)) return result;
	}
    }
  return result;
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
  int *exp = expa.alloc(_nvars);
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K_->copy(t->coeff);
      R->M_->to_expvector(t->monom, exp);
      M_->from_expvector(exp, result->monom);
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

bool PolynomialRing::in_subring(int n, const ring_elem a) const
{
  for (Nterm *t = a; t != 0; t = t->next)
    if (!M_->in_subring(n,t->monom)) return false;
  return true;
}

void PolynomialRing::degree_of_var(int n, const ring_elem a, int &lo, int &hi) const
{
  Nterm *t = a;
  if (t == NULL)
    {
      ERROR("attempting to find degree of a zero element");
      return;
    }
  int *exp = new int[n_vars()];
  M_->to_expvector(t->monom, exp);
  lo = hi = exp[n];
  for (t = t->next; t!=0; t=t->next)
    {
      M_->to_expvector(t->monom, exp);
      if (exp[n] < lo) 
	lo = exp[n];
      else if (exp[n] > hi)
	hi = exp[n];
    }
  delete [] exp;
}

ring_elem PolynomialRing::divide_by_var(int n, int d, const ring_elem a) const
  // Divide each monomial of 'a' by x^d, where x is the n th variable.
  // If a monomial is not divisible by x^d, then that monomial is not put
  // into the result.
{
  if (d == 0) return a;
  Nterm head;
  Nterm *result = &head;
  int *exp = new int[n_vars()];
  for (Nterm *t = a; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp);
      if (exp[n] >= d)
	exp[n] -= d;
      else
	continue;
      result->next = new_term();
      result = result->next;
      result->coeff = t->coeff;
      M_->from_expvector(exp, result->monom);
    }
  delete [] exp;
  result->next = 0;
  return head.next;
}

ring_elem PolynomialRing::divide_by_expvector(const int *exp, const ring_elem a) const
{
  Nterm * result = 0;
  int *exp0 = new int[n_vars()];
  for (Nterm *t = a; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp0);
      ntuple::quotient(n_vars(), exp0, exp, exp0);
      Nterm *u = new_term();
      u->coeff = t->coeff;
      M_->from_expvector(exp0, t->monom);
      u->next = result;
      result = u;
    }
  delete [] exp0;
  sort(result);
  return result;
}

///////////////////////////////////
// translation gbvector <--> vec //
///////////////////////////////////
#include "geovec.hpp"

ring_elem PolynomialRing::trans_to_ringelem(ring_elem coeff, 
					   const int *exp) const
{
  ring_elem a = Ncoeffs()->trans_to_ringelem(coeff, exp + n_vars());
  Nmonoms()->from_expvector(exp, trans_MONOM1);
  Nterm *t = term(a, trans_MONOM1);
  return t;
}

ring_elem PolynomialRing::trans_to_ringelem_denom(ring_elem coeff, 
						 ring_elem denom, 
						 int *exp) const
{
  ring_elem a = Ncoeffs()->trans_to_ringelem_denom(coeff, denom, exp + n_vars());
  Nmonoms()->from_expvector(exp, trans_MONOM1);
  Nterm *t = term(a, trans_MONOM1);
  return t;
}

void PolynomialRing::trans_from_ringelem(gbvectorHeap &H, 
			     ring_elem coeff, 
			     int comp, 
			     int *exp,
			     int firstvar) const
{
  const Monoid *M = Nmonoms();
  Nterm *t = coeff;
  for ( ; t != 0; t=t->next)
    {
      M->to_expvector(t->monom, exp + firstvar);
      Ncoeffs()->trans_from_ringelem(H, t->coeff, comp, exp, firstvar + n_vars());
    }
}

vec PolynomialRing::translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const
{
  vecHeap H(F);
  const Monoid *M = Nmonoms();

  for (const gbvector *t = v; t != 0; t=t->next)
    {
      M->to_expvector(t->monom, trans_EXP1);
      ring_elem a = trans_to_ringelem(t->coeff, trans_EXP1);
      vec w = F->raw_term(a, t->comp-1);
      H.add(w);
    }

  return H.value();
}

vec PolynomialRing::translate_gbvector_to_vec_denom(const FreeModule *F, 
					  const gbvector *v,
					  const ring_elem denom) const
{
  vecHeap H(F);
  const Monoid *M = Nmonoms();
  
  for (const gbvector *t = v; t != 0; t=t->next)
    {
      M->to_expvector(t->monom, trans_EXP1);
      ring_elem a = trans_to_ringelem_denom(t->coeff, denom, trans_EXP1);
      vec w = F->raw_term(a, t->comp-1);
      H.add(w);
    }

  return H.value();
}

gbvector * PolynomialRing::translate_gbvector_from_vec(const FreeModule *F, 
					     const vec v,
					     ring_elem &result_denominator) const
{
  gbvectorHeap H(get_gb_ring(),F);
  // TODO: remove common denominator!!
  // result_denominator = _one;
  // F->common_denominator(v, result_denominator);
  // now mult through by this element...
  // SERIOUS QUESTION: what ring is this element in????
  for (vec w = v; w != 0; w=w->next)
    {
      trans_from_ringelem(H, w->coeff, w->comp+1, trans_EXP1, 0);
    }

#warning "denominator should be allowed to be in larger ring"
  const Ring *K = get_flattened_ring()->Ncoeffs();
  result_denominator = K->from_int(1);
  return H.value();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:

