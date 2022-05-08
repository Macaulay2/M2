// Copyright 1995 Michael E. Stillman

#include "relem.hpp"
#include "monoid.hpp"
#include "monomial.hpp"
#include "frac.hpp"
#include "localring.hpp"
#include "polyring.hpp"
#include "M2FreeAlgebra.hpp"

#include "aring-glue.hpp"

RingZZ *globalZZ;

unsigned int RingElement::computeHashValue() const
{
  return get_ring()->computeHashValue(get_value());
}

RingElement *RingElement::make_raw(const Ring *R, ring_elem f)
{
  return new RingElement(R, f);
}

int RingElement::n_terms(int nvars) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (is_zero()) return 0;
  if (P != nullptr)
    {
      return P->n_logical_terms(nvars, val);
    }
  auto Q = dynamic_cast<const M2FreeAlgebra *>(R);
  if (Q != nullptr)
    {
      return Q->n_terms(val);
    }
  
  return 1;
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
  return new RingElement(R, R->invert(val));
}

RingElement /* or null */ *RingElement::operator+(const RingElement &b) const
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

RingElement /* or null */ *RingElement::operator-(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR(
          "ring subtraction requires both elements to have the same base ring");
      return 0;
    }
  ring_elem result = R->subtract(get_value(), b.get_value());
  if (error()) return 0;
  return new RingElement(R, result);
}

RingElement /* or null */ *RingElement::operator*(const RingElement &b) const
{
  if (R != b.get_ring())
    {
      ERROR(
          "ring multiplication requires both elements to have the same base "
          "ring");
      return 0;
    }
  ring_elem result = R->mult(get_value(), b.get_value());
  if (error()) return 0;
  return new RingElement(R, result);
}

RingElement *RingElement::operator*(int n) const
{
  ring_elem nR = R->from_long(n);
  if (is_zero() || (n == 0))
    return new RingElement(R, ZERO_RINGELEM);
  else
    return new RingElement(R, R->mult(nR, get_value()));
}

RingElement /* or null */ *RingElement::operator/(const RingElement &b) const
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

RingElement /* or null */ *RingElement::power(int n) const
{
  // n negative is handled.
  ring_elem f = R->power(val, n);
  if (error()) return 0;
  return new RingElement(R, f);
}

RingElement /* or null */ *RingElement::power(mpz_srcptr n) const
{
  ring_elem f = R->power(val, n);
  if (error()) return 0;
  return new RingElement(R, f);
}

RingElement *RingElement::random(const Ring *R)
{
  return new RingElement(R, R->random());
}

void RingElement::text_out(buffer &o) const
{
  R->elem_text_out(o, val);
}

RingElement /* or null */ *RingElement::get_terms(int nvars,
                                                  int lo,
                                                  int hi) const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P != nullptr)
    {
      return new RingElement(P, P->get_terms(nvars, val, lo, hi));
    }
  const M2FreeAlgebra* A = dynamic_cast<const M2FreeAlgebra*>(R);
  if (A != nullptr)
    {
      return new RingElement(A, A->get_terms(val, lo, hi));
    }
  ERROR("expected polynomial ring");
  return nullptr;
}

RingElement /* or null */ *RingElement::lead_coeff(const Ring *coeffR) const
{
  if (is_zero())
    {
      return new RingElement(coeffR, coeffR->zero());
    }
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P != nullptr)
    {
      return new RingElement(coeffR, P->lead_logical_coeff(coeffR, val));
    }
  const M2FreeAlgebra* A = dynamic_cast<const M2FreeAlgebra*>(R);
  if (A != nullptr)
    {
      return new RingElement(coeffR, A->lead_coefficient(coeffR, val));
    }
  ERROR("expected polynomial ring");
  return nullptr;
}

RingElement /* or null */ *RingElement::get_coeff(const Ring *coeffR,
                                                  const Monomial *m) const
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
  if (is_zero())
    {
      ERROR("the zero element has no lead monomial");
      return nullptr;
    }
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P != nullptr)
    {
      intarray resultvp;
      Nterm *t = get_value();

      int *exp = newarray_atomic(int, nvars);
      P->lead_logical_exponents(nvars, t, exp);
      varpower::from_ntuple(nvars, exp, resultvp);
      return Monomial::make(resultvp.raw());
    }
  const M2FreeAlgebraOrQuotient* Q = dynamic_cast<const M2FreeAlgebraOrQuotient*>(R);
  if (Q != nullptr)
    {
      ERROR("not implemented yet");
      return nullptr;
    }
  ERROR("expected polynomial ring");
  return nullptr;
}

bool RingElement::is_homogeneous() const { return R->is_homogeneous(val); }
#if 0
// intarray RingElement::degree() const
// {
//   // This should return an M2_arrayint?
//   intarray result;
//
//   int *mon = newarray_atomic(int,R->degree_monoid()->monomial_size());
//   int *d = result.alloc(R->degree_monoid()->n_vars());
//
//   if (is_zero())
//     ERROR("the zero element has no degree");
//   else
//     {
//       R->degree(get_value(), mon);
//       R->degree_monoid()->to_expvector(mon, d);
//     }
//
//   freemem(mon);
//   return result;
// }
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

  int *mon = newarray_atomic(int, R->degree_monoid()->monomial_size());
  R->degree(get_value(), mon);
  M2_arrayint result = R->degree_monoid()->to_arrayint(mon);

  freemem(mon);
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

bool RingElement::promote(const Ring *S, const RingElement *&result) const
{
  if (S == R)
    {
      result = this;
      return true;
    }
  ring_elem g;
  if (S->promote(R, val, g))
    {
      result = new RingElement(S, g);
      return true;
    }
  return false;
}

bool RingElement::lift(const Ring *S, const RingElement *&result) const
{
  if (S == R)
    {
      result = this;
      return true;
    }
  ring_elem g;
  if (R->lift(S, val, g))
    {
      result = new RingElement(S, g);
      return true;
    }
  return false;
}

const RingElement /* or null */ *RingElement::content() const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  const Ring *targetR = (P == 0 ? R : P->getCoefficients());

  return new RingElement(targetR, R->content(val));
}

const RingElement /* or null */ *RingElement::remove_content() const
{
  ring_elem g = R->divide_by_content(val);
  return new RingElement(R, g);
}

const RingElement /* or null */ *RingElement::split_off_content(
    const RingElement /* or null */ *&result) const
{
  const RingElement *c = content();
  ring_elem g = R->divide_by_given_content(val, c->val);
  result = new RingElement(R, g);
  return c;
}

RingElement *RingElement::numerator() const
{
  if (R == globalQQ) return new RingElement(globalZZ, globalQQ->numerator(val));
  const FractionField *K = R->cast_to_FractionField();
  if (K != nullptr)
    return new RingElement(K->get_ring(), K->numerator(val));

  const LocalRing *L = R->cast_to_LocalRing();
  if (L != nullptr)
    return new RingElement(L->get_ring(), L->numerator(val));
  ERROR("fraction field or local ring required");
  return nullptr;
}

RingElement *RingElement::denominator() const
{
  if (R == globalQQ)
    return new RingElement(globalZZ, globalQQ->denominator(val));
  const FractionField *K = R->cast_to_FractionField();
  if (K != nullptr)
    return new RingElement(K->get_ring(), K->denominator(val));
  const LocalRing *L = R->cast_to_LocalRing();
  if (L != nullptr)
    return new RingElement(L->get_ring(), L->denominator(val));
  ERROR("fraction field or local rings required");
  return nullptr;
}

RingElement *RingElement::fraction(const Ring *K,
                                   const RingElement *bottom) const
{
  if (K == globalQQ)
    return new RingElement(globalQQ,
                           globalQQ->fraction(val, bottom->get_value()));
  const FractionField *K1 = K->cast_to_FractionField();
  if (K1 != nullptr)
    {
      if (K1->get_ring() != R)
        {
          ERROR("fraction field required");
          return nullptr;
        }
      return new RingElement(K1, K1->fraction(val, bottom->get_value()));
    }
  const LocalRing *L1 = K->cast_to_LocalRing();
  if (L1 != nullptr)
    {
      if (L1->get_ring() != R)
        {
          ERROR("local ring required");
          return nullptr;
        }
      return new RingElement(L1, L1->fraction(val, bottom->get_value()));
    }
  ERROR("fraction field or local ring required");
  return nullptr;
}

bool RingElement::getSmallIntegerCoefficients(
    std::vector<long> &result_coeffs) const
{
  const PolynomialRing *R = get_ring()->cast_to_PolynomialRing();
  if (R == 0 || R->n_vars() != 1)
    {
      throw exc::engine_error(
          "Expected a polynomial in a univariate polynomial ring");
      return false;  // Should not be needed
    }

  if (is_zero())
    {
      result_coeffs.resize(0);
      return true;
    }
  int lo, deg;  // ignore lo, and deg == degree of the univariate polynomial f.
  R->degree_of_var(0, get_value(), lo, deg);
  result_coeffs.resize(deg + 1);
  for (int i = 0; i <= deg; i++) result_coeffs[i] = 0;
  int exp[1];
  for (Nterm *t = get_value(); t != NULL; t = t->next)
    {
      std::pair<bool, long> res =
          R->getCoefficientRing()->coerceToLongInteger(t->coeff);
      if (not res.first)
        {
          // At this point, the answer is meaningless
          result_coeffs.resize(0);
          return false;
        }
      long coeff = res.second;

      R->getMonoid()->to_expvector(t->monom, exp);
      assert(exp[0] >= 0);
      assert(exp[0] <= deg);
      result_coeffs[exp[0]] = coeff;
    }
  return true;
}

M2_arrayintOrNull RingElement::getSmallIntegerCoefficients() const
{
  std::vector<long> coeffs;
  if (!getSmallIntegerCoefficients(coeffs)) return 0;
  return stdvector_to_M2_arrayint(coeffs);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
