// Copyright 1995 Michael E. Stillman

#include "relem.hpp"
#include "monoid.hpp"
#include "monomial.hpp"
#include "frac.hpp"
#include "QQ.hpp"

Z *ZZ;
QQ *globalQQ;
RR *RRR;

RingElement * RingElement::make_raw(const Ring *R, ring_elem f)
{
  return new RingElement(R,f);
}

int RingElement::n_terms() const
{
  return R->n_terms(val);
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

RingElement *RingElement::operator+(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("ring addition requires both elements to have the same base ring");
      return 0;
    }
  return new RingElement(R, R->add(get_value(), b.get_value()));
}

RingElement *RingElement::operator-(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("ring subtraction requires both elements to have the same base ring");
      return 0;
    }
  return new RingElement(R, R->subtract(get_value(), b.get_value()));
}

RingElement *RingElement::operator*(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("ring multiplication requires both elements to have the same base ring");
      return 0;
    }
  return new RingElement(R, R->mult(get_value(), b.get_value()));
}

RingElement *RingElement::operator*(int n) const
{
  ring_elem nR = R->from_int(n);
  if (is_zero() || (n == 0))
    return new RingElement(R, (Nterm*)0);
  else
    return new RingElement(R, R->mult(nR,get_value()));
}

RingElement *RingElement::operator/(const RingElement &b) const
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
  return new RingElement(R, R->quotient(val, b.get_value()));
}

RingElement *RingElement::operator%(const RingElement &b) const
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
  return new RingElement(R, R->remainder(val, b.get_value()));
}

RingElement *RingElement::divide(const RingElement &b, RingElement * &rem) const
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
  ring_elem fquot;
  ring_elem frem = R->remainderAndQuotient(get_value(), b.get_value(), fquot);
  rem = new RingElement(R, frem);
  return new RingElement(R, fquot);
}

RingElement *RingElement::gcd(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR("gcd requires both elements to have the same base ring");
      return 0;
    }
  // MES: check that gcd is well-defined for this ring
  return new RingElement(R, R->gcd(get_value(), b.get_value()));
}

RingElement *RingElement::gcd_extended(const RingElement &b,
					RingElement * &u,
					RingElement * &v) const
{
  if (R != b.get_ring())
    {
      ERROR("gcd requires both elements to have the same base ring");
      return 0;
    }
  // MES: check that gcd is well-defined for this ring
  ring_elem u1,v1;
  ring_elem g = R->gcd_extended(get_value(), b.get_value(),
				u1,v1);
  u = new RingElement(R,u1);
  v = new RingElement(R,v1);
  return new RingElement(R, g);
}

RingElement *RingElement::power(int n) const
{
  // What if n is negative?  Does R->power handle that correctly?
  ring_elem f = R->power(val,n);
  if (error()) return 0;
  return new RingElement(R, f);
}

RingElement *RingElement::power(mpz_t n) const
{
  ring_elem f = R->power(val,n);
  if (error()) return 0;
  return new RingElement(R, f);
}

RingElement *RingElement::random(const Ring *R)
{
  return new RingElement(R,R->random());
}

RingElement *RingElement::random(const Ring *R,
				int homog,
				const intarray &deg)
{
  return new RingElement(R,R->random(homog,deg.raw()));
}

void RingElement::text_out(buffer &o) const
{
  R->elem_text_out(o, val);
}

RingElement *RingElement::get_terms(int lo, int hi) const
{
  return new RingElement(R, R->get_terms(val, lo, hi));
}

RingElement *RingElement::lead_coeff() const
{
  const Ring *K = R->Ncoeffs();
  if (is_zero())
    {
      ERROR("zero polynomial has no lead coefficient");
      return 0;
    }
  else
    return new RingElement(K, R->lead_coeff(val));
}

RingElement *RingElement::get_coeff(const Monomial *m) const
{
  const Ring *K = R->Ncoeffs();
  return new RingElement(K, R->get_coeff(get_value(), m->ints()));
}

Monomial *RingElement::lead_monom() const
{
  if (is_zero()) 
    {
      ERROR("zero polynomial has no lead monomial");
      return 0;
    }
  intarray resultvp;

  Nterm *t = get_value();
  R->Nmonoms()->to_varpower(t->monom, resultvp);
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

  int *mon = new int[R->degree_monoid()->monomial_size()];
  int *d = result.alloc(R->degree_monoid()->n_vars());

  if (is_zero()) 
    ERROR("the zero element has no degree");
  else
    {
      R->degree(get_value(), mon);
      R->degree_monoid()->to_expvector(mon, d);
    }

  delete [] mon;
  return result;
}
#endif

void RingElement::degree_weights(M2_arrayint wts, int &lo, int &hi) const
{
  // Check that wts has same length as nvars
  if (is_zero())
    {
      ERROR("zero element has no degree");
      return;
    }
  if (wts->len != (unsigned)(R->n_vars()))
    {
      ERROR("weight vector has incorrect length");
      return;
    }
  R->degree_weights(val, wts, lo, hi);
}

M2_arrayint RingElement::multi_degree() const
{
  if (is_zero()) 
    {
      ERROR("the zero element has no degree");
      return 0;
    }

  int *mon = new int[R->degree_monoid()->monomial_size()];
  R->degree(get_value(), mon);
  M2_arrayint result = R->degree_monoid()->to_arrayint(mon);

  delete [] mon;
  return result;
}

RingElement *RingElement::homogenize(int v, M2_arrayint wts) const
{
  if (v < 0 || v >= R->n_vars())
    {
      ERROR("homogenization: improper ring variable");
      return 0;
    }
  if (wts == NULL || wts->len != (unsigned)(R->n_vars()))
    {
      ERROR("homogenization: improper weight function");
      return 0;
    }
  if (wts->array[v] == 0)
    {
      ERROR("homogenization: variable weight is zero");
      return 0;
    }
  
  RingElement *result = new RingElement(R, R->homogenize(val, v, wts));
  if (error()) return 0;
  return result;
}

RingElement *RingElement::homogenize(int v, int deg, M2_arrayint wts) const
{
  if (v < 0 || v >= R->n_vars())
    {
      ERROR("homogenization: improper ring variable");
      return 0;
    }
  if (wts == NULL || wts->len != (unsigned)(R->n_vars()))
    {
      ERROR("homogenization: improper weight function");
      return 0;
    }
  if (wts->array[v] == 0)
    {
      ERROR("homogenization: variable weight is zero");
      return 0;
    }
  
  RingElement *result = new RingElement(R, R->homogenize(val, v, deg, wts));
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
    return new RingElement(ZZ, globalQQ->numerator(val));      
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
    return new RingElement(ZZ, globalQQ->denominator(val));      
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
