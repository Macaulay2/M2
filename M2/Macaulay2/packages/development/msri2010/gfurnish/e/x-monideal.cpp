// Copyright 2002 Michael E. Stillman

#include "monomial.hpp"
#include "monideal.hpp"
#include "matrix.hpp"
#include "engine.h"
#include "hilb.hpp"
#include "assprime.hpp"
#include "monideal-minprimes.hpp"
#include "exceptions.hpp"
#include "text-io.hpp"
#include "../d/M2inits.h"

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

#include "debug.hpp"

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
	  if (gbTrace >= 1)
	    dstash();
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

M2_arrayint rawMonomialIdealLCM(const MonomialIdeal *I)
{
  return I->lcm();
}


#if HAVE_FROBBY
#include "frobby.h"

class MyIdealConsumer : public Frobby::IdealConsumer, our_new_delete {
  int nv; // The size of exponentVector coming from frobby
  int *exp;
  MonomialIdeal *J;
public:
  MyIdealConsumer(const PolynomialRing *R, int nv0) : nv(nv0)
  {
    J = new MonomialIdeal(R);
    exp = newarray_atomic(int,nv);
  }
  ~MyIdealConsumer()
  {
    deletearray(exp);
    J = 0;
  }
  virtual void consume(mpz_ptr* exponentVector) 
  { 
    // insert into J.  This is a minimal generator of J
    for (int i=0; i<nv; i++)
      exp[i] = mpz_get_si(exponentVector[i]); // overflow should not occur, as input fit

    if (gbTrace >= 5)
      {
	fprintf(stderr, "got ");
	for (int j=0; j<nv; j++)
	  fprintf(stderr, "%d ", exp[j]);
	fprintf(stderr, "\n");
      }
      
    Bag *b = new Bag();
    varpower::from_ntuple(nv, exp, b->monom());
    J->insert_minimal(b);
  }
  MonomialIdeal *result() { return J; }
};

static MonomialIdeal *FrobbyAlexanderDual(const MonomialIdeal *I, const mpz_t *topvec)
{
  int nv = I->topvar() + 1;
  int *exp = newarray_atomic(int, nv);
  Frobby::Ideal F(nv);
  for (Index<MonomialIdeal> i = I->first(); i.valid(); i++)
    {
      Bag *b = I->operator[](i);
      varpower::to_ntuple(nv, b->monom().raw(), exp);

      if (gbTrace >= 4) fprintf(stderr, "adding ");
      for (int j=0; j<nv; j++)
	{
	  if (gbTrace >= 4) fprintf(stderr, "%d ", exp[j]);
	  F.addExponent(exp[j]);
	}
      if (gbTrace >= 4) fprintf(stderr, "\n");
    }

  // Now create the consumer object, and call Frobby
  MyIdealConsumer M(I->get_ring(), nv);
  Frobby::alexanderDual(F, topvec, M);
  deletearray(exp);
  // Extract the answer as a MonomialIdeal
  return M.result();
}

static MonomialIdeal *wrapperFrobbyAlexanderDual(const MonomialIdeal *I, const M2_arrayint top)
// Assumption: top is an array of at least the number of variables of I
//   whose v-th entry is at least as large as the v-th exp of any mingen of I
{
  // Create a Frobby Ideal containing I.
  int nv = I->topvar() + 1;
  if (nv == 0)
    {
      INTERNAL_ERROR("attempting to use frobby with zero variables");
      return 0;
    }

  mpz_t *topvec = 0;
  if (top->len > 0)
    {
      topvec = newarray(mpz_t, top->len);
      for (int i=0; i<top->len; i++)
	mpz_init_set_si(topvec[i], top->array[i]);
    }

  enterMalloc();  // WARNING: the consumer should not create or destroy M2 gmp ints
  MonomialIdeal *result = FrobbyAlexanderDual(I, topvec);
  enterM2(); // resets memory functions

  // Clean up
  if (topvec != 0)
    {
      for (int i=0; i<top->len; i++)
	mpz_clear(topvec[i]);
      deletearray(topvec);
    }

  return result;
}
#endif

static MonomialIdealOrNull *alexDual(const MonomialIdeal *I, const M2_arrayint top, int strategy)
{
  if (I->topvar() < 0)
    strategy = 1; // i.e. don't use frobby if there are no generators and/or variables
  switch (strategy) {
#if HAVE_FROBBY
  case 0:
    if (gbTrace >= 1) emit_line(" -- [Alexander dual: frobby]");
    return wrapperFrobbyAlexanderDual(I,top);
#else
#warning "frobby not enabled"
#endif
  default:
    if (gbTrace >= 1) emit_line(" -- [Alexander dual: M2 monideal]");
    return I->alexander_dual(top);
  }
}

const MonomialIdealOrNull *rawAlexanderDual(const MonomialIdeal *I, const M2_arrayint top, int strategy)
{
  try {
    MonomialIdeal *result = alexDual(I,top,strategy);
    intern_monideal(result);
    return result;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
