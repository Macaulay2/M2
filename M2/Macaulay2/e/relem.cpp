// Copyright 1995 Michael E. Stillman

#include "relem.hpp"
#include "monoid.hpp"
#include "monomial.hpp"
#include "frac.hpp"

stash *RingElement_rec::mystash;


RingElement::RingElement(const Ring *R, RingElement a, Monomial m) : 
  obj(new RingElement_rec(R))
{
  intarray ma;
  int *mon = ma.alloc(R->n_vars());
  R->Nmonoms()->from_varpower(m.ints(), mon);
  set_value( R->term(R->Ncoeffs()->copy(a.get_value()), mon) );
}

int RingElement_rec::length_of() const
{
  return R->n_terms(val);
}

RingElement RingElement::operator-() const
{
  const Ring *R = Ring_of();
  RingElement result(R, R->negate(obj->val));
  return result;
}

RingElement RingElement::invert() const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (is_zero())
    {
      *gError << "ring division: attempt to divide by zero";
      // MES: raise an exception
    }
  else
    result.set_value( R->invert(get_value()) );

  return result;
}

RingElement RingElement::operator+(const RingElement &b) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (R != b.Ring_of())
    *gError << "ring addition requires both elements to have the same "
      << "base ring";
  else 
    result.set_value( R->add(get_value(), b.get_value()) );

  return result;
}

RingElement RingElement::operator-(const RingElement &b) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (R != b.Ring_of())
    *gError << "ring subtraction requires both elements to have the same "
      << "base ring";
  else 
    result.set_value( R->subtract(get_value(), b.get_value()) );

  return result;
}

RingElement RingElement::operator*(const RingElement &b) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (R != b.Ring_of())
    *gError << "ring multiplication requires both elements to have the same "
      << "base ring";
  else 
    result.set_value( R->mult(get_value(), b.get_value()) );

  return result;
}

RingElement RingElement::operator*(int n) const
{
  const Ring *R = Ring_of();
  RingElement nR(R, n);
  RingElement result(R);
  if (!is_zero() && (n != 0))
    result.set_value( R->mult(nR.get_value(), get_value()) );

  return result;
}

RingElement RingElement::operator/(const RingElement &b) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (R != b.Ring_of())
    *gError << "ring division requires both elements to have the same "
      << "base ring";
  else if (b.is_zero())
    *gError << "ring division: attempt to divide by zero";
  else
    result.set_value( R->divide(get_value(), b.get_value()) );

  return result;
}
RingElement RingElement::operator%(const RingElement &b) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (R != b.Ring_of())
    *gError << "ring division requires both elements to have the same "
      << "base ring";
  else if (b.is_zero())
    *gError << "ring division: attempt to divide by zero";
  else
    {
      ring_elem frem;
      ring_elem fdiv = R->divide(get_value(), b.get_value(), frem);
      result.set_value(frem);
      R->remove(fdiv);
    }

  return result;
}
RingElement RingElement::divide(const RingElement &b, RingElement &rem) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (R != b.Ring_of())
    *gError << "ring division requires both elements to have the same "
      << "base ring";
  else if (b.is_zero())
    *gError << "ring division: attempt to divide by zero";
  else
    {
      ring_elem frem;
      ring_elem fdiv = R->divide(get_value(), b.get_value(), frem);
      result.set_value(fdiv);
      rem = RingElement(R, frem);
    }

  return result;
}
RingElement RingElement::gcd(const RingElement &b) const
{
  // MES: check that gcd is well-defined for this ring
  const Ring *R = Ring_of();
  if (R != b.Ring_of())
    {
      *gError << "gcd requires both elements to have the same "
	<< "base ring";
      return NULL;
    }
  else 
    return RingElement(R, R->gcd(get_value(), b.get_value()));
}
RingElement RingElement::gcd_extended(const RingElement &b,
					RingElement &u,
					RingElement &v) const
{
  // MES: check that gcd is well-defined for this ring
  const Ring *R = Ring_of();
  if (R != b.Ring_of())
    {
      *gError << "gcd requires both elements to have the same "
	<< "base ring";
      return *this;
    }
  else 
    {
      ring_elem u1,v1;
      ring_elem g = R->gcd_extended(get_value(), b.get_value(),
				     u1,v1);
      u = RingElement(R,u1);
      v = RingElement(R,v1);
      return RingElement(R, g);
    }
}

RingElement RingElement::power(int n) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  result.set_value( R->power(get_value(),n) );

  return result;
}
RingElement RingElement::power(mpz_t n) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  result.set_value( R->power(get_value(),n) );

  return result;
}

RingElement RingElement::random(const Ring *R)
{
  return RingElement(R,R->random());
}

RingElement RingElement::random(const Ring *R,
				int homog,
				const intarray &deg)
{
  return RingElement(R,R->random(homog,deg.raw()));
}

void RingElement_rec::text_out(ostream &o) const
{
  R->elem_text_out(o, val);
}

void RingElement::text_out(ostream &o) const
{
  obj->text_out(o);
}

void RingElement_rec::bin_out(ostream &o) const
{
  R->elem_bin_out(o, val);
}

RingElement RingElement::get_terms(int lo, int hi) const
{
  const Ring *R = Ring_of();
  RingElement result(R);
  if (!is_zero())
    result.set_value( R->get_terms(get_value(), lo, hi) );

  return result;
}

RingElement RingElement::lead_coeff() const
{
  const Ring *R = Ring_of();
  const Ring *K = R->Ncoeffs();
  RingElement result(K);
  if (!is_zero())
    result.set_value( R->lead_coeff(get_value()) );

  return result;
}

RingElement RingElement::get_coeff(const Monomial &m) const
{
  const Ring *R = Ring_of();
  const Ring *K = R->Ncoeffs();
  RingElement result(K);
  result.set_value( R->get_coeff(get_value(), m.ints()) );
  return result;
}

Monomial RingElement::lead_monom() const
{
  const Ring *R = Ring_of();
  if (is_zero()) 
    {
      *gError << "zero polynomial has no lead monomial";
      return Monomial();
    }
  intarray resultvp;

  Nterm *t = get_value();
  R->Nmonoms()->to_varpower(t->monom, resultvp);
  return Monomial(resultvp.raw());
}

int RingElement::is_homogeneous() const
{
  return Ring_of()->is_homogeneous(get_value());
}

intarray RingElement::degree() const
{
  const Ring *R = Ring_of();
  intarray result;

  int *mon = new int[R->degree_monoid()->monomial_size()];
  int *d = result.alloc(R->degree_monoid()->n_vars());

  if (is_zero()) 
    *gError << "the zero element has no degree";
  else
    {
      R->degree(get_value(), mon);
      R->degree_monoid()->to_expvector(mon, d);
    }

  delete [] mon;
  return result;
}

RingElement RingElement::homogenize(int v, const int *wts) const
{
  const Ring *R = Ring_of();
  RingElement result(R, R->homogenize(get_value(), v, wts));
  return result;
}

RingElement RingElement::homogenize(int v, int deg, const int *wts) const
{
  const Ring *R = Ring_of();
  RingElement result(R, R->homogenize(get_value(), v, deg, wts));
  return result;
}

int RingElement::promote(const Ring *S, RingElement &result) const
{
  if (S == Ring_of())
    {
      result = *this;
      return 1;
    }
  ring_elem g;
  if (S->promote(Ring_of(), get_value(), g))
    {
      result = RingElement(S,g);
      return 1;
    }
  return 0;
}

int RingElement::lift(const Ring *S, RingElement &result) const
{
  if (S == Ring_of())
    {
      result = *this;
      return 1;;
    }
  ring_elem g;
  if (Ring_of()->lift(S, get_value(), g))
    {
      result = RingElement(S,g);
      return 1;
    }
  return 0;
}

RingElement RingElement::numerator() const
{
  const FractionField *K = Ring_of()->cast_to_FractionField();
  if (K == NULL) 
    {
      *gError << "fraction field required";
      return *this;
    }
  RingElement g(K->Ring_of(), K->numerator(get_value()));
  return g;
}

RingElement RingElement::denominator() const
{
  const FractionField *K = Ring_of()->cast_to_FractionField();
  if (K == NULL) 
    {
      *gError << "fraction field required";
      return *this;
    }
  RingElement g(K->Ring_of(), K->denominator(get_value()));
  return g;
}
