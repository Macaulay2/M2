// Copyright 2003 Michael E. Stillman

#include "skewpoly.hpp"
#include "gbring.hpp"
#include "skew.hpp"

SkewPolynomialRing::~SkewPolynomialRing() {}
bool SkewPolynomialRing::initialize_skew(M2_arrayint skewvars)
{
  is_skew_ = true;
  skew_ = SkewMultiplication(nvars_, skewvars->len, skewvars->array);
  return true;
}

SkewPolynomialRing *SkewPolynomialRing::create(const Ring *K,
                                               const Monoid *M,
                                               M2_arrayint skewvars)
{
  SkewPolynomialRing *result = new SkewPolynomialRing;

  result->initialize_poly_ring(K, M);
  if (!result->initialize_skew(skewvars)) return nullptr;
  result->gb_ring_ = GBRing::create_SkewPolynomialRing(K, M, result->skew_);
  return result;
}

void SkewPolynomialRing::text_out(buffer &o) const
{
  o << "SkewPolynomialRing(";
  K_->text_out(o);
  M_->text_out(o);
  o << ")";
}

static int sign4[4] = {1, 1, -1, -1};

ring_elem SkewPolynomialRing::antipode(const ring_elem f) const
{
  Nterm head;
  Nterm *inresult = &head;

  exponents_t EXP = ALLOCATE_EXPONENTS(exp_size);

  for (Nterm& s : f)
    {
      M_->to_expvector(s.monom, EXP);
      int deg = skew_.skew_degree(EXP);
      // sign is (-1)^ (binomial(deg,2))
      // deg = 0: sign is 1
      // deg = 1: sign is 1
      // deg = 2: sign is -1
      // deg = 3: sign is -1
      // deg = 4: sign is 1, and so on
      int mod4 = deg % 4;
      int sign = sign4[mod4];
      Nterm *t = new_term();
      t->next = nullptr;
      t->coeff = (sign == 1 ? s.coeff : K_->negate(s.coeff));
      M_->copy(s.monom, t->monom);
      inresult->next = t;
      inresult = inresult->next;
    }
  inresult->next = nullptr;
  return head.next;
}

ring_elem SkewPolynomialRing::mult_by_term(const ring_elem f,
                                           const ring_elem c,
                                           const int *m) const
// Computes c*m*f, BUT NOT doing normal form wrt a quotient ideal..
{
  Nterm head;
  Nterm *inresult = &head;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents_t EXP2 = ALLOCATE_EXPONENTS(exp_size);
  M_->to_expvector(m, EXP1);

  for (Nterm& s : f)
    {
      M_->to_expvector(s.monom, EXP2);
      int sign = skew_.mult_sign(EXP1, EXP2);
      if (sign == 0) continue;

      Nterm *t = new_term();
      t->next = nullptr;
      t->coeff = K_->mult(c, s.coeff);
      if (sign < 0) K_->negate_to(t->coeff);

      M_->mult(m, s.monom, t->monom);
      inresult->next = t;
      inresult = inresult->next;
    }
  inresult->next = nullptr;
  return head.next;
}

ring_elem SkewPolynomialRing::power(const ring_elem f, mpz_srcptr n) const
{
  std::pair<bool, int> n1 = RingZZ::get_si(n);
  if (n1.first)
    return power(f, n1.second);
  else
    throw exc::engine_error("exponent too large");
}

ring_elem SkewPolynomialRing::power(const ring_elem f, int n) const
{
  return Ring::power(f, n);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
