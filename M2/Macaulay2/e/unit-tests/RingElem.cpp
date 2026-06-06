// Copyright 2026, The Macaulay2 Authors.

#include "unit-tests/RingElem.hpp"
#include "BasicPoly.hpp"
#include "rings/polyring.hpp"
#include "monoid.hpp"

// Convert a BasicPoly (parsed polynomial with mpz_class coefficients and
// varpower monomials) into a ring_elem in the given PolynomialRing.
static ring_elem basicPolyToRingElem(const PolynomialRing *P, const BasicPoly &bp)
{
  const Ring *K = P->getCoefficients();
  const Monoid *M = P->getMonoid();
  int nvars = M->n_vars();

  ring_elem result = P->from_long(0);
  std::vector<int> exp(nvars, 0);
  monomial monom = M->make_one();

  int monomStart = 0;
  for (size_t i = 0; i < bp.mCoefficients.size(); ++i)
    {
      // Convert coefficient: mpz_class -> ring_elem in coefficient ring
      ring_elem coeff = K->from_int(bp.mCoefficients[i].get_mpz_t());

      // Convert monomial: varpower format -> exponent vector -> encoded monomial
      int monomLen = bp.mMonomials[monomStart];
      std::fill(exp.begin(), exp.end(), 0);
      for (int j = monomStart + 1; j < monomStart + monomLen; j += 2)
        {
          int var = bp.mMonomials[j];
          int e = bp.mMonomials[j + 1];
          if (var >= 0 && var < nvars)
            exp[var] = e;
        }
      M->from_expvector(exp.data(), monom);

      // Create the term and add it to the result
      ring_elem term = P->make_flat_term(coeff, monom);
      result = P->add(result, term);

      monomStart += monomLen;
    }

  M->remove(monom);
  return result;
}

RingElem RingElem::fromString(const Ring *R, const std::string &s)
{
  // Polynomial rings (including WeylAlgebra, skew, quotient rings, etc.)
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P != nullptr)
    {
      const Monoid *M = P->getMonoid();
      std::vector<std::string> varnames = M->variableNames();
      BasicPoly bp = parseBasicPoly(s, varnames);
      ring_elem result = basicPolyToRingElem(P, bp);
      return RingElem(R, result);
    }

  // Base rings: parse as an integer
  // Works for ZZ, ZZ/p, QQ (integers only), and other rings with from_int
  mpz_class val(s);
  return RingElem(R, R->from_int(val.get_mpz_t()));
}
