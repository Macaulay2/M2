// Copyright 2002 Michael E. Stillman

#include "monomial.hpp"
#include "monideal.hpp"
#include "matrix.hpp"
#include "engine.h"
#include "hilb.hpp"
#include "assprime.hpp"
#include "monideal_minprimes.hpp"

const MonomialIdeal *IM2_MonomialIdeal_make(const Matrix *m, int n)
{
  return m->make_monideal(n);
}

const Matrix *IM2_MonomialIdeal_to_matrix(const MonomialIdeal *I)
{
  return Matrix::make(I);
}

M2_string MonomialIdeal_to_string(const MonomialIdeal *I)
{
  buffer o;
  I->text_out(o);
  return o.to_string();
}

int IM2_MonomialIdeal_n_gens(const MonomialIdeal *I)
{
  return I->length();
}

M2_bool IM2_MonomialIdeal_is_equal(const MonomialIdeal *I, const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    return false;
  return I->is_equal(*J);
}

const MonomialIdeal *rawRadicalMonomialIdeal(const MonomialIdeal *I)
{
  return I->radical();
}

const MonomialIdeal *IM2_MonomialIdeal_add(const MonomialIdeal *I, 
					   const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  return (*I) + (*J);
}

const MonomialIdeal *IM2_MonomialIdeal_product(const MonomialIdeal *I, 
					       const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  return (*I) * (*J);
}

const MonomialIdeal *IM2_MonomialIdeal_intersect(const MonomialIdeal *I, 
						 const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  return I->intersect(*J);
}

const MonomialIdeal *rawColonMonomialIdeal1(const MonomialIdeal *I, 
						 const Monomial *a)
{
  return I->quotient(a->ints());
}

const MonomialIdeal *rawColonMonomialIdeal2(const MonomialIdeal *I, 
						const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  return I->quotient(*J);
}

const MonomialIdeal *rawSaturateMonomialIdeal1(const MonomialIdeal *I, 
					    const Monomial *a)
{
  return I->erase(a->ints());
}

const MonomialIdeal *rawSaturateMonomialIdeal2(const MonomialIdeal *I, 
					   const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  return I->sat(*J);
}

const MonomialIdeal *IM2_MonomialIdeal_borel(const MonomialIdeal *I)
{
  return I->borel();
}

M2_bool IM2_MonomialIdeal_is_borel(const MonomialIdeal *I)
{
  return I->is_borel();
}

int IM2_MonomialIdeal_codim(const MonomialIdeal *I)
{
  MinimalPrimes ap(I);
  return ap.codimension();
}

const MonomialIdeal *rawMonomialMinimalPrimes(const MonomialIdeal *I,
					      int codim_limit,
					      int count)
{
  MinimalPrimes ap(I);
  return ap.alg1_min_primes(codim_limit, count);
}

const MonomialIdeal *rawMaximalIndependentSets(const MonomialIdeal *I,
					       int count)
{
  AssociatedPrimes ap(I);
  return ap.associated_primes(count);
}

const RingElementOrNull * IM2_MonomialIdeal_Hilbert(const MonomialIdeal *I)
/* This routine computes the numerator of the Hilbert series
   for coker I.  NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */
{
  return hilb_comp::hilbertNumerator(I);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
