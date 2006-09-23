// Copyright 1995 Michael E. Stillman

#include "relem.hpp"
#include "monoid.hpp"
#include "monomial.hpp"
#include "frac.hpp"
#include "QQ.hpp"
#include "polyring.hpp"

RingZZ *globalZZ;
QQ *globalQQ;
RingRR *globalRR;
CC *globalCC;
RRR *globalRRR;
CCC *globalCCC;

RingElement * RingElement::make_raw(const Ring *R, ring_elem f)
{
  return new RingElement(R,f);
}

int RingElement::n_terms(int nvars) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  return P->n_logical_terms(nvars,val);
}

RingElement *RingElement::operator-() const
{
  return new RingElement(R, R->negate(val));
}

RingElement *RingElement::invert() const
{
  if (is_zero())
    {
      ERROR("ring division: attempt to divide by zero");
      return 0;
    }
  return new RingElement(R,R->invert(val));
}

RingElementOrNull *RingElement::operator+(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("ring addition requires both elements to have the same base ring");
      return 0;
    }
  ring_elem result = R->add(get_value(), b.get_value());
  if (error()) return 0;
  return new RingElement(R, result);
}

RingElementOrNull *RingElement::operator-(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("ring subtraction requires both elements to have the same base ring");
      return 0;
    }
  ring_elem result = R->subtract(get_value(), b.get_value());
  if (error()) return 0;
  return new RingElement(R, result);
}

RingElementOrNull *RingElement::operator*(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("ring multiplication requires both elements to have the same base ring");
      return 0;
    }
  ring_elem result = R->mult(get_value(), b.get_value());
  if (error()) return 0;
  return new RingElement(R, result);
}

RingElement *RingElement::operator*(int n) const
{
  ring_elem nR = R->from_int(n);
  if (is_zero() || (n == 0))
    return new RingElement(R, ZERO_RINGELEM);
  else
    return new RingElement(R, R->mult(nR,get_value()));
}

RingElementOrNull *RingElement::operator/(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("ring division requires both elements to have the same base ring");
      return 0;
    }
  if (b.is_zero())
    {
      ERROR("ring division: attempt to divide by zero");
      return 0;
    }
  ring_elem result = R->divide(get_value(), b.get_value());
  if (error()) return 0;
  return new RingElement(R, result);
}

RingElementOrNull *RingElement::power(int n) const
{
  // n negative is handled.
  ring_elem f = R->power(val,n);
  if (error()) return 0;
  return new RingElement(R, f);
}

RingElementOrNull *RingElement::power(mpz_t n) const
{
  ring_elem f = R->power(val,n);
  if (error()) return 0;
  return new RingElement(R, f);
}

RingElement *RingElement::random(const Ring *R)
{
  return new RingElement(R,R->random());
}

void RingElement::text_out(buffer &o) const
{
  R->elem_text_out(o, val);
}

RingElementOrNull *RingElement::get_terms(int nvars, int lo, int hi) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  return new RingElement(P, P->get_terms(nvars, val, lo, hi));
}

RingElementOrNull *RingElement::lead_coeff(const Ring *coeffR) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (is_zero())
    {
      return new RingElement(coeffR, coeffR->zero());
    }
  return new RingElement(coeffR, P->lead_logical_coeff(coeffR,val));
}

RingElementOrNull *RingElement::get_coeff(const Ring *coeffR, const Monomial *m) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  return new RingElement(coeffR, P->get_coeff(coeffR, get_value(), m->ints()));
}

Monomial *RingElement::lead_monom(int nvars) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (is_zero()) 
    {
      ERROR("zero polynomial has no lead monomial");
      return 0;
    }

  intarray resultvp;
  Nterm *t = get_value();

  int *exp = newarray(int,nvars);
  P->lead_logical_exponents(nvars,t,exp);
  varpower::from_ntuple(nvars,exp,resultvp);
  return Monomial::make(resultvp.raw());
}

int RingElement::is_homogeneous() const
{
  return R->is_homogeneous(val);
}

#if 0
intarray RingElement::degree() const
{
  // This should return an M2_arrayint?
  intarray result;

  int *mon = newarray(int,R->degree_monoid()->monomial_size());
  int *d = result.alloc(R->degree_monoid()->n_vars());

  if (is_zero()) 
    ERROR("the zero element has no degree");
  else
    {
      R->degree(get_value(), mon);
      R->degree_monoid()->to_expvector(mon, d);
    }

  deletearray(mon);
  return result;
}
#endif

void RingElement::degree_weights(M2_arrayint wts, int &lo, int &hi) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (is_zero())
    {
      ERROR("zero element has no degree");
      return;
    }
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return;
    }
  P->degree_weights(val, wts, lo, hi);
}

M2_arrayint RingElement::multi_degree() const
{
  if (is_zero()) 
    {
      ERROR("the zero element has no degree");
      return 0;
    }

  int *mon = newarray(int,R->degree_monoid()->monomial_size());
  R->degree(get_value(), mon);
  M2_arrayint result = R->degree_monoid()->to_arrayint(mon);

  deletearray(mon);
  return result;
}

RingElement *RingElement::homogenize(int v, M2_arrayint wts) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (v < 0 || v >= P->n_vars())
    {
      ERROR("homogenization: improper ring variable");
      return 0;
    }
  if (wts == NULL || wts->len != static_cast<unsigned int>(P->n_vars()))
    {
      ERROR("homogenization: improper weight function");
      return 0;
    }
  if (wts->array[v] == 0)
    {
      ERROR("homogenization: variable weight is zero");
      return 0;
    }
  
  RingElement *result = new RingElement(P, P->homogenize(val, v, wts));
  if (error()) return 0;
  return result;
}

RingElement *RingElement::homogenize(int v, int deg, M2_arrayint wts) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (v < 0 || v >= P->n_vars())
    {
      ERROR("homogenization: improper ring variable");
      return 0;
    }
  if (wts == NULL || wts->len != static_cast<unsigned int>(P->n_vars()))
    {
      ERROR("homogenization: improper weight function");
      return 0;
    }
  if (wts->array[v] == 0)
    {
      ERROR("homogenization: variable weight is zero");
      return 0;
    }
  
  RingElement *result = new RingElement(R, P->homogenize(val, v, deg, wts));
  if (error()) return 0;
  return result;
}

bool RingElement::promote(const Ring *S, const RingElement * &result) const
{
  if (S == R)
    {
      result = this;
      return true;
    }
  ring_elem g;
  if (S->promote(R, val, g))
    {
      result = new RingElement(S,g);
      return true;
    }
  return false;
}

bool RingElement::lift(const Ring *S, const RingElement * &result) const
{
  if (S == R)
    {
      result = this;
      return true;
    }
  ring_elem g;
  if (R->lift(S, val, g))
    {
      result = new RingElement(S,g);
      return true;
    }
  return false;
}

RingElement *RingElement::numerator() const
{
  if (R == globalQQ)
    return new RingElement(globalZZ, globalQQ->numerator(val));      
  const FractionField *K = R->cast_to_FractionField();
  if (K == NULL) 
    {
      ERROR("fraction field required");
      return 0;
    }
  return new RingElement(K->get_ring(), K->numerator(val));
}

RingElement *RingElement::denominator() const
{
  if (R == globalQQ)
    return new RingElement(globalZZ, globalQQ->denominator(val));      
  const FractionField *K = R->cast_to_FractionField();
  if (K == NULL) 
    {
      ERROR("fraction field required");
      return 0;
    }
  return new RingElement(K->get_ring(), K->denominator(val));
}

RingElement *RingElement::fraction(const Ring *K, const RingElement *bottom) const
{
  if (K == globalQQ)
    return new RingElement(globalQQ, globalQQ->fraction(val, bottom->get_value()));
  const FractionField *K1 = K->cast_to_FractionField();
  if (K1 == NULL || K1->get_ring() != R) 
    {
      ERROR("fraction field required");
      return 0;
    }
  return new RingElement(K1,K1->fraction(val, bottom->get_value()));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
