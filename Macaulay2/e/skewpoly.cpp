// Copyright 2003 Michael E. Stillman

#include "skewpoly.hpp"
#include "gbring.hpp"

SkewPolynomialRing::~SkewPolynomialRing()
{
}

bool SkewPolynomialRing::initialize_skew(M2_arrayint skewvars)
{
  _is_skew = true;
  _skew = SkewMultiplication(_nvars, skewvars->len, skewvars->array);
  return true;
}

SkewPolynomialRing *SkewPolynomialRing::create(const PolynomialRing *R,
					       M2_arrayint skewvars)
{
  // CHECK: R is a polynomial ring, and is commutative.
  SkewPolynomialRing *result = new SkewPolynomialRing;

  result->initialize_poly_ring(R->Ncoeffs(), R->Nmonoms());
  if (!result->initialize_skew(skewvars)) return 0;
  const PolynomialRing *flatR = R->get_flattened_ring()->cast_to_PolynomialRing();
  result->_gb_ring = GBRing::create_SkewPolynomialRing(flatR->Ncoeffs(), flatR->Nmonoms(),result->_skew);
  return result;
}


ring_elem SkewPolynomialRing::imp_mult_by_term(const ring_elem f, 
					       const ring_elem c, 
					       const int *m) const
  // Computes c*m*f, BUT NOT doing normal form wrt a quotient ideal..
{
  Nterm head;
  Nterm *inresult = &head;

  M_->to_expvector(m, _EXP1);

  for (Nterm *s = f; s != NULL; s = s->next)
    {
      M_->to_expvector(s->monom, _EXP2);
      int sign = _skew.mult_sign(_EXP1, _EXP2);
      if (sign == 0) continue;

      Nterm *t = new_term();
      t->next = 0;
      t->coeff = K_->mult(c, s->coeff);
      if (sign < 0)
	K_->negate_to(t->coeff);

      M_->mult(m, s->monom, t->monom);
      inresult->next = t;
      inresult = inresult->next;
    }
  inresult->next = 0;
  return head.next;
}


ring_elem SkewPolynomialRing::power(const ring_elem f, mpz_t n) const
{
  int n1;
  if (ZZ::get_si(n1,n))
    return power(f,n1);
  else 
    {
      ERROR("exponent too large");
      return (Nterm *)NULL;
    }
}

ring_elem SkewPolynomialRing::power(const ring_elem f, int n) const
{
  return PolynomialRing::power2(f,n);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
