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

SkewPolynomialRing *SkewPolynomialRing::create(const Ring *K,
					       const Monoid *M,
					       const Ring *originalK,
					       const Monoid *originalM,
					       M2_arrayint skewvars)
{
  SkewPolynomialRing *result = new SkewPolynomialRing;

  result->initialize_poly_ring(K,M,originalK,originalM);
  if (!result->initialize_skew(skewvars)) return 0;
  result->_gb_ring = GBRing::create_SkewPolynomialRing(K,M,result->_skew);
  return result;
}

SkewPolynomialRing *SkewPolynomialRing::create(const PolynomialRing *R,
					       M2_arrayint skewvars)
{
  return create(R->getCoefficients(),
		R->getMonoid(),
		R->getLogicalCoefficients(),
		R->getLogicalMonoid(),
		skewvars);
}

const SkewPolynomialRing *SkewPolynomialRing::createPolyRing(const Monoid *M) const
  // creates this[M], which is commutative in M variables, but skew commutative in
  // (some of) the variables of this
{
  const Monoid *newM = Monoid::tensor_product(M, getMonoid());
  if (newM == 0) return 0;
  
  int nskew = n_skew_commutative_vars();
  int nvars = M->n_vars();
  M2_arrayint newskewvars = makearrayint(nskew);
  for (int i=0; i<nskew; i++)
    newskewvars->array[i] = nvars + skew_variable(i);

  return create(getCoefficients(),
		newM,
		this,
		M,
		newskewvars);
}


ring_elem SkewPolynomialRing::mult_by_term(const ring_elem f, 
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
      return ZERO_RINGELEM;
    }
}

ring_elem SkewPolynomialRing::power(const ring_elem f, int n) const
{
  return Ring::power(f,n);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
