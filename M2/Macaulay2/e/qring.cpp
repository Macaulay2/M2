// Copyright 2005, Michael E. Stillman

#include "qring.hpp"
#include "monideal.hpp"
#include "montable.hpp"
#include "montableZZ.hpp"
#include "gbring.hpp"
#include "poly.hpp"

#include "aring-glue.hpp"  // for globalQQ??
void QRingInfo::appendQuotientElement(Nterm *f, gbvector *g)
{
  quotient_ideal.push_back(f);
  quotient_gbvectors.push_back(g);
}

QRingInfo::QRingInfo(const PolyRing *ambientR) : R(ambientR)
{
  exp_size = EXPONENT_BYTE_SIZE(R->n_vars());
  monom_size = MONOMIAL_BYTE_SIZE(R->getMonoid()->monomial_size());
}

void QRingInfo::destroy(GBRing *GR)
{
  // remove the gbvector's as they are stashed in gbrings.
  // WARNING: these need to be deleted only if the gbring is non-NULL.

  if (GR == 0) return;
  for (int i = 0; i < quotient_gbvectors.size(); i++)
    GR->gbvector_remove(quotient_gbvectors[i]);
}

QRingInfo::~QRingInfo() {}
void QRingInfo_field::destroy(GBRing *GR)
{
  delete Rideal;
  delete ringtable;
  QRingInfo::destroy(GR);
}

QRingInfo_field::~QRingInfo_field() {}
QRingInfo_field::QRingInfo_field(const PolyRing *ambientR,
                                 const VECTOR(Nterm *) & quotients)
    : QRingInfo(ambientR)
// This constructs the quotient ideal, and sets the MonomialIdeal.
// A subset of 'quotients' must be a minimal GB, and any non-minimal elements
// should be writable in terms of previous ones.
// IE, in constructing (R/I)/J, the GB elements of J (mod I), together with the
// GB elements of I, form a GB, but the GB elements of I might not be minimal.
// In this case, 'quotients' should be the list of GB elements of J followed by
// those
// of I.
//
// The ideal may be in a tower of polynomial rings, in which case it needs to be
// a GB over the flattened ring.
{
  Rideal = new MonomialIdeal(R);
  ringtable = MonomialTable::make(R->n_vars());
  intarray vp;
  int *exp = newarray_atomic(int, R->n_vars());
  for (int i = 0; i < quotients.size(); i++)
    {
      // Make a varpower element.  See if it is in Rideal.
      // If not, place it into quotient_elements_.

      Nterm *f = quotients[i];
      R->getMonoid()->to_expvector(f->monom, exp);

      Bag *not_used;

      if (!Rideal->search_expvector(exp, not_used))
        {
          // The element is part of a minimal GB
          int index = n_quotients();
          gbvector *g = R->translate_gbvector_from_ringelem(f);
          appendQuotientElement(f, g);
          vp.shrink(0);
          R->getMonoid()->to_varpower(f->monom, vp);
          Bag *b = new Bag(index, vp);
          Rideal->insert(b);
          ringtable->insert(exp, 1, index);  // consumes exp
          exp = newarray_atomic(int, R->n_vars());
        }
    }
  freemem(exp);
}

QRingInfo_field_basic::QRingInfo_field_basic(const PolyRing *ambientR,
                                             const VECTOR(Nterm *) & quotients)
    : QRingInfo_field(ambientR, quotients)
{
}

QRingInfo_field_basic::~QRingInfo_field_basic() {}
void QRingInfo_field_basic::reduce_lead_term_basic_field(Nterm *&f,
                                                         const Nterm *g) const
{
  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);
  const Monoid *M = R->getMonoid();
  M->divide(f->monom, g->monom, MONOM1);
  ring_elem c = R->getCoefficients()->negate(f->coeff);
  if (R->is_skew_commutative())
    {
      exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
      // We need to determine the sign
      M->to_expvector(g->monom, EXP2);
      M->to_expvector(MONOM1, EXP1);
      if (R->getSkewInfo().mult_sign(EXP1, EXP2) < 0)
        R->getCoefficients()->negate_to(c);
    }
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R->mult_by_term(g1, c, MONOM1);
  ring_elem f1 = f;
  R->internal_add_to(f1, g1);
  f = f1;
}

void QRingInfo_field_basic::normal_form(ring_elem &f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  const Monoid *M = R->getMonoid();
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1);
      int_bag *b;
      if (Rideal->search_expvector(EXP1, b))
        {
          Nterm *s = quotient_element(b->basis_elem());
          // Now we must replace t with
          // t + c*m*s, where in(t) = in(c*m*s), and c is 1 or -1.
          reduce_lead_term_basic_field(t, s);
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

void QRingInfo_field_basic::gbvector_normal_form(const FreeModule *F,
                                                 gbvector *&f) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  GBRing *GR = R->get_gb_ring();
  gbvector head;
  gbvector *result = &head;
  gbvector *t = f;
  while (t != NULL)
    {
      GR->gbvector_get_lead_exponents(F, t, EXP1);
      int x = ringtable->find_divisor(EXP1, 1);
      if (x >= 0)
        {
          const gbvector *r = quotient_gbvector(x);
          gbvector *zero = 0;
          GR->gbvector_reduce_lead_term(F, F, zero, t, zero, r, zero);
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

QRingInfo_field_QQ::QRingInfo_field_QQ(const PolyRing *ambientR,
                                       const VECTOR(Nterm *) & quotients)
    : QRingInfo_field(ambientR, quotients)
{
}

QRingInfo_field_QQ::~QRingInfo_field_QQ() {}
void QRingInfo_field_QQ::reduce_lead_term_QQ(Nterm *&f, const Nterm *g) const
{
  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);
  const Monoid *M = R->getMonoid();
  M->divide(f->monom, g->monom, MONOM1);
  ring_elem c = globalQQ->RingQQ::negate(f->coeff);
  c = globalQQ->RingQQ::divide(c, g->coeff);
  if (R->is_skew_commutative())
    {
      exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
      // We need to determine the sign
      M->to_expvector(g->monom, EXP2);
      M->to_expvector(MONOM1, EXP1);
      if (R->getSkewInfo().mult_sign(EXP1, EXP2) < 0)
        globalQQ->RingQQ::negate_to(c);
    }
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R->mult_by_term(g1, c, MONOM1);
  ring_elem f1 = f;
  R->internal_add_to(f1, g1);
  f = f1;
}

void QRingInfo_field_QQ::normal_form(ring_elem &f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  const Monoid *M = R->getMonoid();
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1);
      int_bag *b;
      if (Rideal->search_expvector(EXP1, b))
        {
          Nterm *s = quotient_element(b->basis_elem());
          // Now we must replace t with
          // t + c*m*s, where in(t) = in(c*m*s), and c is 1 or -1.
          reduce_lead_term_QQ(t, s);
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

void QRingInfo_field_QQ::gbvector_normal_form(const FreeModule *F,
                                              gbvector *&f) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  GBRing *GR = R->get_gb_ring();
  gbvector head;
  gbvector *result = &head;
  result->next = NULL;
  gbvector *t = f;
  while (t != NULL)
    {
      GR->gbvector_get_lead_exponents(F, t, EXP1);
      int x = ringtable->find_divisor(EXP1, 1);
      if (x >= 0)
        {
          const gbvector *r = quotient_gbvector(x);
          gbvector *zero = 0;
          GR->gbvector_reduce_lead_term(F, F, head.next, t, zero, r, zero);
        }
      else
        {
          result->next = t;
          t = t->next;
          result = result->next;
          result->next = NULL;
        }
    }
  f = head.next;
}

void QRingInfo_field_QQ::gbvector_normal_form(const FreeModule *F,
                                              gbvector *&f,
                                              bool use_denom,
                                              ring_elem &denom) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  GBRing *GR = R->get_gb_ring();
  gbvector head;
  gbvector *result = &head;
  result->next = NULL;
  gbvector *t = f;
  while (t != NULL)
    {
      GR->gbvector_get_lead_exponents(F, t, EXP1);
      int x = ringtable->find_divisor(EXP1, 1);
      if (x >= 0)
        {
          const gbvector *r = quotient_gbvector(x);
          gbvector *zero = 0;
          GR->gbvector_reduce_lead_term(
              F, F, head.next, t, zero, r, zero, use_denom, denom);
        }
      else
        {
          result->next = t;
          t = t->next;
          result = result->next;
          result->next = NULL;
        }
    }
  f = head.next;
}

void QRingInfo_ZZ::destroy(GBRing *GR)
{
  delete ringtableZZ;
  QRingInfo::destroy(GR);
}

QRingInfo_ZZ::~QRingInfo_ZZ() {}
QRingInfo_ZZ::QRingInfo_ZZ(const PolyRing *ambientR,
                           const VECTOR(Nterm *) & quotients)
    : QRingInfo(ambientR)
{
  ringtableZZ = MonomialTableZZ::make(R->n_vars());
  int *exp = newarray_atomic(int, R->n_vars());
  for (int i = 0; i < quotients.size(); i++)
    {
      // Make a varpower element.  See if it is in Rideal.
      // If not, place it into quotient_elements.

      Nterm *f = quotients[i];
      R->getMonoid()->to_expvector(f->monom, exp);

      if (!ringtableZZ->is_strong_member(f->coeff.get_mpz(), exp, 1))
        {
          // The element is part of a minimal GB
          // Also, this grabs exp.
          int index = n_quotients();
          ringtableZZ->insert(f->coeff.get_mpz(), exp, 1, index);
          gbvector *g = R->translate_gbvector_from_ringelem(f);
          appendQuotientElement(f, g);
          exp = newarray_atomic(int, R->n_vars());

          if (f->next == 0 && R->getMonoid()->is_one(f->monom))
            {
              is_ZZ_quotient_ = true;
              ZZ_quotient_value_ = f->coeff;
            }
        }
    }
  freemem(exp);
}

bool QRingInfo_ZZ::reduce_lead_term_ZZ(Nterm *&f, const Nterm *g) const
// Never multiplies f by anything.  IE before(f), after(f) are equiv. mod g.
// this should ONLY be used if K is globalZZ.
{
  const Monoid *M = R->getMonoid();
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  ring_elem u, v, rem;
  rem = globalZZ->remainderAndQuotient(a, b, v);
  if (globalZZ->is_zero(v)) return false;
  v = globalZZ->negate(v);
  bool result = globalZZ->is_zero(rem);
  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);
  M->divide(f->monom, g->monom, MONOM1);
  if (R->is_skew_commutative())
    {
      exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
      // We need to determine the sign
      M->to_expvector(g->monom, EXP2);
      M->to_expvector(MONOM1, EXP1);
      if (R->getSkewInfo().mult_sign(EXP1, EXP2) < 0)
        R->getCoefficients()->negate_to(v);
    }

  // now mult g to cancel
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R->mult_by_term(g1, v, MONOM1);
  ring_elem f1 = f;
  R->internal_add_to(f1, g1);
  f = f1;
  return result;
}

void QRingInfo_ZZ::normal_form(ring_elem &f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  const Monoid *M = R->getMonoid();
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1);
      int w = ringtableZZ->find_smallest_coeff_divisor(EXP1, 1);
      if (w >= 0)
        {
          // reduce lead term as much as possible
          // If the lead monomial reduces away, continue,
          //   else tack the monomial onto the result
          Nterm *g = quotient_element(w);
          if (reduce_lead_term_ZZ(t, g)) continue;
        }
      result->next = t;
      t = t->next;
      result = result->next;
    }
  result->next = NULL;
  f = head.next;
}

void QRingInfo_ZZ::gbvector_normal_form(const FreeModule *F, gbvector *&f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  GBRing *GR = R->get_gb_ring();
  gbvector head;
  gbvector *result = &head;
  gbvector *t = f;
  while (t != NULL)
    {
      GR->gbvector_get_lead_exponents(F, t, EXP1);
      int w = ringtableZZ->find_smallest_coeff_divisor(EXP1, 1);
      if (w >= 0)
        {
          // reduce lead term as much as possible
          // If the lead monomial reduces away, continue,
          //   else tack the monomial onto the result
          const gbvector *g = quotient_gbvector(w);
          gbvector *zero = 0;
          if (GR->gbvector_reduce_lead_term_ZZ(F, F, t, zero, g, zero))
            continue;
        }
      result->next = t;
      t = t->next;
      result = result->next;
    }
  result->next = NULL;
  f = head.next;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
