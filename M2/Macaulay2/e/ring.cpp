// Copyright 1995 Michael E. Stillman

#include "ring.hpp"
#include "monoid.hpp"
#include "monideal.hpp"
#include "respoly.hpp"
#include "polyring.hpp"

#include "freemod.hpp"

Ring::Ring(int P, 
	     int n, 
	     int totaln,
	     const Ring *KK,
	     const Monoid *MM,
	     const Monoid *DD)
: type(), P(P), nvars(n), totalvars(totaln), K(KK), M(MM), D(DD),
HRing(NULL)
{
  if (K != NULL) bump_up((Ring *) K);
  bump_up(M);
  bump_up(D);

  if (D->n_vars() > 0)
    {
      HRing = PolynomialRing::create(ZZ, D);
      bump_up(HRing);
    }
  else
    HRing = NULL;

  int msize = M->monomial_size();
  vecstash = new stash("vectors",
		       sizeof(vecterm) +
		       (msize-1) * sizeof(int));
  // Set up the resterm stash
  resstash = new stash("respoly", sizeof(resterm *) + sizeof(res_pair *)
		     + sizeof(ring_elem)
		     + sizeof(int) * M->monomial_size());

  isfield = false;
  isquotientring = false;
  zero_divisor = (Nterm *)0;

  is_ZZ_quotient = false;
  ZZ_quotient_value = (Nterm *)0;
}

Ring::Ring(const Ring &R)
: type(),
  P(R.P),
  nvars(R.nvars),
  totalvars(R.totalvars),
  K(R.K),
  M(R.M),
  D(R.D),
  HRing(R.HRing),
  zero_divisor((Nterm *)0),
  isfield(false),
  isquotientring(true),
  is_ZZ_quotient(false), // Needs to be set in polynomial ring creation
  ZZ_quotient_value((Nterm *)0), // Ditto.
  vecstash(R.vecstash),
  resstash(R.resstash)
{
  if (K != NULL) bump_up((Ring *) K);
}

Ring::~Ring()
{
  // PROBLEM: zero_divisor needs to be freed: this MUST be done by the specific ring.
  // PROBLEM: resstash, vecstash need to be freed, if this is not a quotient.
  if (!isquotientring)
    {
      delete vecstash;
      delete resstash;
      bump_down(M);
      bump_down(D);
      if (HRing != NULL) bump_down(HRing);
    }
  if (K != NULL) bump_down((Ring *) K);
}

FreeModule *Ring::make_FreeModule() const
{ 
  return new FreeModule(this); 
}

FreeModule *Ring::make_FreeModule(int n) const
{ 
  return new FreeModule(this,n);
}

bool Ring::is_field() const 
{ 
  return isfield; 
}
void Ring::declare_field() 
{ 
  isfield = true; 
}
ring_elem Ring::get_zero_divisor() const 
{ 
  return copy(zero_divisor); 
}


void Ring::remove_vector(vec &v) const
{
  while (v != NULL)
    {
      vecterm *tmp = v;
      v = v->next;
      K->remove(tmp->coeff);
      vecstash->delete_elem(tmp);
    }
}

void Ring::mult_to(ring_elem &f, const ring_elem g) const
{
  ring_elem h = mult(f,g);
  remove(f);
  f = h;
}

int Ring::coerce_to_int(ring_elem) const
{
  gError << "cannot coerce given ring element to an integer";
  return 0;
}

ring_elem Ring::random() const
{
  gError << "random scalar elements for this ring are not implemented";
  return 0;
}
ring_elem Ring::random(int /*homog*/, const int * /*deg*/) const
{
  gError << "random non-scalar elements for this ring are not implemented";
  return 0;
}

ring_elem Ring::preferred_associate(ring_elem f) const
{
  // Here we assume that 'this' is a field:
  if (is_zero(f)) return from_int(1);
  return invert(f);
}
