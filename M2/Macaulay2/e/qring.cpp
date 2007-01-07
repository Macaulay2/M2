// Copyright 2005, Michael E. Stillman

#include "qring.hpp"
#include "monideal.hpp"
#include "montable.hpp"
#include "montableZZ.hpp"
#include "gbring.hpp"
#include "QQ.hpp"

void QRingInfo::appendQuotientElement(Nterm *f, gbvector *g)
{
  quotient_ideal.push_back(f);
  quotient_gbvectors.push_back(g);
}

QRingInfo::QRingInfo(const PolyRing *ambientR)
  : R(ambientR)
{
  EXP1_ = newarray_atomic(int, R->n_vars());
  EXP2_ = newarray_atomic(int, R->n_vars());
  MONOM1_ = R->getMonoid()->make_one();
}

QRingInfo_field::~QRingInfo_field()
{
}

QRingInfo_field::QRingInfo_field(const PolyRing *ambientR,
				 const VECTOR(Nterm *) &quotients)
  : QRingInfo(ambientR)
// This constructs the quotient ideal, and sets the MonomialIdeal.
// A subset of 'quotients' must be a minimal GB, and any non-minimal elements
// should be writable in terms of previous ones.
// IE, in constructing (R/I)/J, the GB elements of J (mod I), together with the
// GB elements of I, form a GB, but the GB elements of I might not be minimal.
// In this case, 'quotients' should be the list of GB elements of J followed by those
// of I.
//
// The ideal may be in a tower of polynomial rings, in which case it needs to be
// a GB over the flattened ring.
{
  Rideal = new MonomialIdeal(R);
  ringtable = MonomialTable::make(R->n_vars());
  intarray vp;
  int *exp = newarray_atomic(int,R->n_vars());
  for (int i=0; i<quotients.size(); i++)
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
	  ringtable->insert(exp, 1, index); // consumes exp
	  exp = newarray_atomic(int,R->n_vars());
	}
    }
  deletearray(exp);
}

QRingInfo_field_basic::QRingInfo_field_basic(
      const PolyRing *ambientR,
      const VECTOR(Nterm *) &quotients)
  : QRingInfo_field(ambientR, quotients)
{
}

QRingInfo_field_basic::~QRingInfo_field_basic()
{
}

void QRingInfo_field_basic::reduce_lead_term_basic_field(Nterm * &f, const Nterm * g) const
{
  const Monoid *M = R->getMonoid();
  M->divide(f->monom, g->monom, MONOM1_);
  ring_elem c = R->getCoefficients()->negate(f->coeff);
  if (R->is_skew_commutative())
    {
      // We need to determine the sign
      M->to_expvector(g->monom, EXP2_);
      M->to_expvector(MONOM1_, EXP1_);
      if (R->getSkewInfo().mult_sign(EXP1_, EXP2_) < 0)
	R->getCoefficients()->negate_to(c);
    }
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R->mult_by_term(g1,c,MONOM1_);
  ring_elem f1 = f;
  R->internal_add_to(f1, g1);
  f = f1;
}

void QRingInfo_field_basic::normal_form(ring_elem& f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  const Monoid *M = R->getMonoid();
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1_);
      int_bag *b;
      if (Rideal->search_expvector(EXP1_, b))
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

void QRingInfo_field_basic::gbvector_normal_form(const FreeModule *F, gbvector *&f) const
{
  GBRing *GR = R->get_gb_ring();
  const Monoid *M = R->getMonoid();
  gbvector head;
  gbvector *result = &head;
  gbvector *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1_);
      int x = ringtable->find_divisor(EXP1_, 1);
      if (x >= 0)
	{
	  const gbvector *r = quotient_gbvector(x);
	  gbvector *zero = 0;
	  GR->gbvector_reduce_lead_term(F,F,zero,t,zero,r,zero);
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
		   const VECTOR(Nterm *) &quotients)
  : QRingInfo_field(ambientR,quotients)
{
}


QRingInfo_field_QQ::~QRingInfo_field_QQ()
{
}

void QRingInfo_field_QQ::reduce_lead_term_QQ(Nterm * &f, const Nterm * g) const
{
  const Monoid *M = R->getMonoid();
  M->divide(f->monom, g->monom, MONOM1_);
  ring_elem c = globalQQ->QQ::negate(f->coeff);
  c = globalQQ->QQ::divide(c,g->coeff);
  if (R->is_skew_commutative())
    {
      // We need to determine the sign
      M->to_expvector(g->monom, EXP2_);
      M->to_expvector(MONOM1_, EXP1_);
      if (R->getSkewInfo().mult_sign(EXP1_, EXP2_) < 0)
	globalQQ->QQ::negate_to(c);
    }
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R->mult_by_term(g1,c,MONOM1_);
  ring_elem f1 = f;
  R->internal_add_to(f1, g1);
  f = f1;
}

void QRingInfo_field_QQ::normal_form(ring_elem& f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  const Monoid *M = R->getMonoid();
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1_);
      int_bag *b;
      if (Rideal->search_expvector(EXP1_, b))
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

void QRingInfo_field_QQ::gbvector_normal_form(const FreeModule *F, gbvector *&f) const
{
  GBRing *GR = R->get_gb_ring();
  const Monoid *M = R->getMonoid();
  gbvector head;
  gbvector *result = &head;
  result->next = NULL;
  gbvector *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1_);
      int x = ringtable->find_divisor(EXP1_, 1);
      if (x >= 0)
	{
	  const gbvector *r = quotient_gbvector(x);
	  gbvector *zero = 0;
	  GR->gbvector_reduce_lead_term(F,F,head.next,t,zero,r,zero);
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

QRingInfo_ZZ::~QRingInfo_ZZ()
{
}

QRingInfo_ZZ::QRingInfo_ZZ(const PolyRing *ambientR,
			   const VECTOR(Nterm *) &quotients)
  : QRingInfo(ambientR)
{
  ringtableZZ = MonomialTableZZ::make(R->n_vars());
  int *exp = newarray_atomic(int,R->n_vars());
  for (int i=0; i<quotients.size(); i++)
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
	  exp = newarray_atomic(int,R->n_vars());

	  if (f->next == 0 && R->getMonoid()->is_one(f->monom))
	    {
	      is_ZZ_quotient_ = true;
	      ZZ_quotient_value_ = f->coeff;
	    }
	}
    }
  deletearray(exp);
}

bool QRingInfo_ZZ::reduce_lead_term_ZZ(Nterm * &f, const Nterm * g) const
  // Never multiplies f by anything.  IE before(f), after(f) are equiv. mod g.
  // this should ONLY be used if K is globalZZ.
{
  const Monoid *M = R->getMonoid();
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  ring_elem u,v,rem;
  rem = globalZZ->remainderAndQuotient(a,b,v);
  if (globalZZ->is_zero(v)) return false;
  v = globalZZ->negate(v);
  bool result = globalZZ->is_zero(rem);
  M->divide(f->monom, g->monom, MONOM1_);
  if (R->is_skew_commutative())
    {
      // We need to determine the sign
      M->to_expvector(g->monom, EXP2_);
      M->to_expvector(MONOM1_, EXP1_);
      if (R->getSkewInfo().mult_sign(EXP1_, EXP2_) < 0)
	R->getCoefficients()->negate_to(v);
    }

  // now mult g to cancel
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R->mult_by_term(g1, v, MONOM1_);
  ring_elem f1 = f;
  R->internal_add_to(f1,g1);
  f = f1;
  return result;
}

void QRingInfo_ZZ::normal_form(ring_elem& f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  const Monoid *M = R->getMonoid();
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1_);
      int w = ringtableZZ->find_smallest_coeff_divisor(EXP1_, 1);
      if (w >= 0)
	{
	  // reduce lead term as much as possible
	  // If the lead monomial reduces away, continue,
	  //   else tack the monomial onto the result
	  Nterm *g = quotient_element(w);
	  if (reduce_lead_term_ZZ(t,g))
	    continue;
	}
      result->next = t;
      t = t->next;
      result = result->next;
    }
  result->next = NULL;
  f = head.next;
}

void QRingInfo_ZZ::gbvector_normal_form(const FreeModule *F, gbvector * &f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  GBRing *GR = R->get_gb_ring();
  const Monoid *M = R->getMonoid();
  gbvector head;
  gbvector *result = &head;
  gbvector *t = f;
  while (t != NULL)
    {
      M->to_expvector(t->monom, EXP1_);
      int w = ringtableZZ->find_smallest_coeff_divisor(EXP1_, 1);
      if (w >= 0)
	{
	  // reduce lead term as much as possible
	  // If the lead monomial reduces away, continue,
	  //   else tack the monomial onto the result
	  const gbvector *g = quotient_gbvector(w);
	  gbvector *zero = 0;
	  if (GR->gbvector_reduce_lead_term_ZZ(F,F,t,zero,g,zero))
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
// End:
