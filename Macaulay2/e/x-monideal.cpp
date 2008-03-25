// Copyright 2002 Michael E. Stillman

#include "monomial.hpp"
#include "monideal.hpp"
#include "matrix.hpp"
#include "engine.h"
#include "hilb.hpp"
#include "assprime.hpp"
#include "monideal-minprimes.hpp"
#include "exceptions.hpp"

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

const MonomialIdealOrNull *IM2_MonomialIdeal_make(const Matrix *m, int n)
{
     try {
	  MonomialIdeal *result = m->make_monideal(n);
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *IM2_MonomialIdeal_to_matrix(const MonomialIdeal *I)
{
     try {
	  return Matrix::make(I);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

M2_string MonomialIdeal_to_string(const MonomialIdeal *I)
{
     buffer o;
     try {
	  I->text_out(o);
	  return o.to_string();
     }
     catch (exc::engine_error e) {
	  o << "[unprintable monomial ideal]";
	  return o.to_string();
     }
}

int IM2_MonomialIdeal_n_gens(const MonomialIdeal *I)
{
  return I->length();
}

int IM2_MonomialIdeal_is_equal(const MonomialIdeal *I, const MonomialIdeal *J)
	// 1 = true, 0 = false, -1 = error
{
     try {
	  if (I->get_ring() != J->get_ring())
	    return false;
	  return I->is_equal(*J);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return -1;
     }
}

const MonomialIdealOrNull *rawRadicalMonomialIdeal(const MonomialIdeal *I)
{
     try {
	  MonomialIdeal *result = I->radical();
	  intern_monideal(result);
	  return result;

     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
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

const MonomialIdealOrNull *IM2_MonomialIdeal_product(const MonomialIdeal *I, 
					       const MonomialIdeal *J)
{
     try {
	  if (I->get_ring() != J->get_ring())
	    {
	      ERROR("expected ideals in the same ring");
	      return 0;
	    }
	  MonomialIdeal *result = (*I) * (*J);
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialIdealOrNull *IM2_MonomialIdeal_intersect(const MonomialIdeal *I, 
						 const MonomialIdeal *J)
{
     try {
	  if (I->get_ring() != J->get_ring())
	    {
	      ERROR("expected ideals in the same ring");
	      return 0;
	    }
	  MonomialIdeal *result = I->intersect(*J);
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialIdealOrNull *rawColonMonomialIdeal1(const MonomialIdeal *I, 
						 const Monomial *a)
{
     try {
	  MonomialIdeal *result = I->quotient(a->ints());
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialIdealOrNull *rawColonMonomialIdeal2(const MonomialIdeal *I, 
						const MonomialIdeal *J)
{
     try {
	  if (I->get_ring() != J->get_ring())
	    {
	      ERROR("expected ideals in the same ring");
	      return 0;
	    }
	  MonomialIdeal *result = I->quotient(*J);
	  intern_monideal(result);
	  return result;

     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialIdealOrNull *rawSaturateMonomialIdeal1(const MonomialIdeal *I, 
					    const Monomial *a)
{
     try {
	  MonomialIdeal *result = I->erase(a->ints());
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialIdealOrNull *rawSaturateMonomialIdeal2(const MonomialIdeal *I, 
					   const MonomialIdeal *J)
{
     try {
	  if (I->get_ring() != J->get_ring())
	    {
	      ERROR("expected ideals in the same ring");
	      return 0;
	    }
	  MonomialIdeal *result = I->sat(*J);
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialIdealOrNull *IM2_MonomialIdeal_borel(const MonomialIdeal *I)
{
     try {
	  MonomialIdeal *result =  I->borel();
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

M2_bool IM2_MonomialIdeal_is_borel(const MonomialIdeal *I)
{
  return I->is_borel();
}

int IM2_MonomialIdeal_codim(const MonomialIdeal *I)
{
     try {
	  MinimalPrimes ap(I);
	  return ap.codimension();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return -1;		// -1 is not a valid codimension, and interface.d knows it
     }
}

const MonomialIdealOrNull *rawMonomialMinimalPrimes(const MonomialIdeal *I,
					      int codim_limit,
					      int count)
{
     try {
	  MinimalPrimes ap(I);
	  MonomialIdeal *result = ap.alg1_min_primes(codim_limit, count);
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialIdealOrNull *rawMaximalIndependentSets(const MonomialIdeal *I,
					       int count)
{
     try {
	  AssociatedPrimes ap(I);
	  MonomialIdeal *result = ap.associated_primes(count);
	  intern_monideal(result);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull * IM2_MonomialIdeal_Hilbert(const MonomialIdeal *I)
/* This routine computes the numerator of the Hilbert series
   for coker I.  NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */
{
     try {
	  return hilb_comp::hilbertNumerator(I);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

#if 0
#include "/Users/mike/src/M2/Macaulay2/bugs/mike/monideals/frobby_vmike2/src/frobby.h"

class MyTermConsumer : public Frobby::TermConsumer {
  MonomialIdeal *J;
public:
  MyTermConsumer(MonomialIdeal *J0) : J(J0) {}
  virtual void consume(mpz_ptr* exponentVector) { }
  MonomialIdeal *result() { return J; }
};

const MonomialIdeal *FrobbyAlexanderDual(MonomialIdeal *I)
{
  MonomialIdeal *J;
  MyTermConsumer M(J);
  Frobby::ExternalIdeal F(5);
  Frobby::alexanderDual(&F, 0, &M);
  return M.result();
}
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
