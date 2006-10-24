// Copyright 2002 Michael E. Stillman

#include "monomial.hpp"
#include "monideal.hpp"
#include "matrix.hpp"
#include "engine.h"
#include "hilb.hpp"
#include "assprime.hpp"
#include "monideal_minprimes.hpp"

static int monideals_nfinalized = 0;
static int monideals_nremoved = 0;

void remove_monideal(void *p, void *cd)
{
  MonomialIdeal *G = static_cast<MonomialIdeal *>(p);
  monideals_nremoved++;
  if (gbTrace>=3)
    fprintf(stderr, "\nremoving monomial ideal %d at %p\n",monideals_nremoved, G);
  G->remove_MonomialIdeal();
}

void intern_monideal(MonomialIdeal *G)
{
  GC_REGISTER_FINALIZER(G,remove_monideal,0,0,0);
  monideals_nfinalized++;
  if (gbTrace>=3)
    fprintf(stderr, "\n   -- registering monomial ideal %d at %p\n", monideals_nfinalized, (void *)G);
}

const MonomialIdeal *IM2_MonomialIdeal_make(const Matrix *m, int n)
{
  MonomialIdeal *result = m->make_monideal(n);
  intern_monideal(result);
  return result;
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
  MonomialIdeal *result = I->radical();
  intern_monideal(result);
  return result;

}

const MonomialIdeal *IM2_MonomialIdeal_add(const MonomialIdeal *I, 
					   const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  MonomialIdeal *result = (*I) + (*J);
  intern_monideal(result);
  return result;
}

const MonomialIdeal *IM2_MonomialIdeal_setminus(const MonomialIdeal *I, 
						const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  MonomialIdeal *result = (*I) - (*J);
  intern_monideal(result);
  return result;
}

const MonomialIdeal *IM2_MonomialIdeal_product(const MonomialIdeal *I, 
					       const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  MonomialIdeal *result = (*I) * (*J);
  intern_monideal(result);
  return result;
}

const MonomialIdeal *IM2_MonomialIdeal_intersect(const MonomialIdeal *I, 
						 const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  MonomialIdeal *result = I->intersect(*J);
  intern_monideal(result);
  return result;
}

const MonomialIdeal *rawColonMonomialIdeal1(const MonomialIdeal *I, 
						 const Monomial *a)
{
  MonomialIdeal *result = I->quotient(a->ints());
  intern_monideal(result);
  return result;
}

const MonomialIdeal *rawColonMonomialIdeal2(const MonomialIdeal *I, 
						const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  MonomialIdeal *result = I->quotient(*J);
  intern_monideal(result);
  return result;

}

const MonomialIdeal *rawSaturateMonomialIdeal1(const MonomialIdeal *I, 
					    const Monomial *a)
{
  MonomialIdeal *result = I->erase(a->ints());
  intern_monideal(result);
  return result;
}

const MonomialIdeal *rawSaturateMonomialIdeal2(const MonomialIdeal *I, 
					   const MonomialIdeal *J)
{
  if (I->get_ring() != J->get_ring())
    {
      ERROR("expected ideals in the same ring");
      return 0;
    }
  MonomialIdeal *result = I->sat(*J);
  intern_monideal(result);
  return result;
}

const MonomialIdeal *IM2_MonomialIdeal_borel(const MonomialIdeal *I)
{
  MonomialIdeal *result =  I->borel();
  intern_monideal(result);
  return result;
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
  MonomialIdeal *result = ap.alg1_min_primes(codim_limit, count);
  intern_monideal(result);
  return result;
}

const MonomialIdeal *rawMaximalIndependentSets(const MonomialIdeal *I,
					       int count)
{
  AssociatedPrimes ap(I);
  MonomialIdeal *result = ap.associated_primes(count);
  intern_monideal(result);
  return result;
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
