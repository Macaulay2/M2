// Copyright 2002 Michael E. Stillman

#include "interface/monomial-ideal.h"

#include <frobby.h> // TODO: move Frobby routines elsewhere?

#include "assprime.hpp"
#include "buffer.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "finalize.hpp"
#include "hilb.hpp"
#include "index.hpp"
#include "int-bag.hpp"
#include "intarray.hpp"
#include "matrix.hpp"
#include "monideal-minprimes.hpp"
#include "monideal.hpp"
#include "monomial.hpp"
#include "newdelete.hpp"
#include "text-io.hpp"
#include "varpower.hpp"

class PolynomialRing;
class RingElement;

engine_RawMonomialIdealOrNull IM2_MonomialIdeal_make(const Matrix *m, int n)
{
  try
    {
      MonomialIdeal *result = m->make_monideal(n);
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

unsigned int rawMonomialIdealHash(const MonomialIdeal *F) { return F->hash(); }
const Matrix /* or null */ *IM2_MonomialIdeal_to_matrix(const MonomialIdeal *I)
{
  try
    {
      return Matrix::make(I);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

M2_string MonomialIdeal_to_string(const MonomialIdeal *I)
{
  buffer o;
  try
    {
      I->text_out(o);
      return o.to_string();
  } catch (const exc::engine_error& e)
    {
      o << "[unprintable monomial ideal]";
      return o.to_string();
  }
}

int IM2_MonomialIdeal_n_gens(const MonomialIdeal *I) { return I->length(); }
int IM2_MonomialIdeal_is_equal(const MonomialIdeal *I, const MonomialIdeal *J)
// 1 = true, 0 = false, -1 = error
{
  try
    {
      if (I->get_ring() != J->get_ring()) return false;
      return I->is_equal(*J);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return -1;
  }
}

const MonomialIdeal /* or null */ *rawRadicalMonomialIdeal(
    const MonomialIdeal *I)
{
  try
    {
      MonomialIdeal *result = I->radical();
      intern_monideal(result);
      return result;

  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const MonomialIdeal /* or null */ *IM2_MonomialIdeal_intersect(
    const MonomialIdeal *I,
    const MonomialIdeal *J)
{
  try
    {
      if (I->get_ring() != J->get_ring())
        {
          ERROR("expected ideals in the same ring");
          return 0;
        }
      MonomialIdeal *result = I->intersect(*J);
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

#include "debug.hpp"

const MonomialIdeal /* or null */ *rawColonMonomialIdeal1(
    const MonomialIdeal *I,
    const Monomial *a)
{
  try
    {
      MonomialIdeal *result = I->quotient(a->ints());
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const MonomialIdeal /* or null */ *rawColonMonomialIdeal2(
    const MonomialIdeal *I,
    const MonomialIdeal *J)
{
  try
    {
      if (I->get_ring() != J->get_ring())
        {
          ERROR("expected ideals in the same ring");
          return 0;
        }
      MonomialIdeal *result = I->quotient(*J);
      intern_monideal(result);
      if (M2_gbTrace >= 1) dstash();
      return result;

  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const MonomialIdeal /* or null */ *rawSaturateMonomialIdeal1(
    const MonomialIdeal *I,
    const Monomial *a)
{
  try
    {
      MonomialIdeal *result = I->erase(a->ints());
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const MonomialIdeal /* or null */ *rawSaturateMonomialIdeal2(
    const MonomialIdeal *I,
    const MonomialIdeal *J)
{
  try
    {
      if (I->get_ring() != J->get_ring())
        {
          ERROR("expected ideals in the same ring");
          return 0;
        }
      MonomialIdeal *result = I->sat(*J);
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const MonomialIdeal /* or null */ *IM2_MonomialIdeal_borel(
    const MonomialIdeal *I)
{
  try
    {
      MonomialIdeal *result = I->borel();
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
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
  try
    {
      MinimalPrimes ap(I);
      return ap.codimension();
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return -1;  // -1 is not a valid codimension, and interface.d knows it
  }
}

const MonomialIdeal /* or null */ *
rawMonomialMinimalPrimes(const MonomialIdeal *I, int codim_limit, int count)
{
  try
    {
      MinimalPrimes ap(I);
      MonomialIdeal *result = ap.alg1_min_primes(codim_limit, count);
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const MonomialIdeal /* or null */ *rawMaximalIndependentSets(
    const MonomialIdeal *I,
    int count)
{
  try
    {
      AssociatedPrimes ap(I);
      MonomialIdeal *result = ap.associated_primes(count);
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *IM2_MonomialIdeal_Hilbert(
    const MonomialIdeal *I)
/* This routine computes the numerator of the Hilbert series
   for coker I.  NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */
{
  try
    {
      return hilb_comp::hilbertNumerator(I);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

M2_arrayint rawMonomialIdealLCM(const MonomialIdeal *I) { return I->lcm(); }

/***********************/
/*** Frobby routines ***/
/***********************/

class MyIdealConsumer : public Frobby::IdealConsumer, our_new_delete
{
  int nv;  // The size of exponentVector coming from frobby
  int *exp;
  MonomialIdeal *J;

 public:
  MyIdealConsumer(const PolynomialRing *R, int nv0) : nv(nv0)
  {
    J = new MonomialIdeal(R);
    exp = newarray_atomic(int, nv);
  }
  ~MyIdealConsumer()
  {
    freemem(exp);
    J = 0;
  }
  virtual void consume(mpz_ptr *exponentVector)
  {
    // insert into J.  This is a minimal generator of J
    for (int i = 0; i < nv; i++)
      exp[i] = static_cast<int>(mpz_get_si(
          exponentVector[i]));  // overflow should not occur, as input fit

    if (M2_gbTrace >= 5)
      {
        fprintf(stderr, "got ");
        for (int j = 0; j < nv; j++) fprintf(stderr, "%d ", exp[j]);
        fprintf(stderr, "\n");
      }

    Bag *b = new Bag();
    varpower::from_ntuple(nv, exp, b->monom());
    J->insert_minimal(b);
  }
  MonomialIdeal *result() { return J; }
};

static MonomialIdeal *FrobbyAlexanderDual(const MonomialIdeal *I,
                                          const mpz_t *topvec)
{
  int nv = I->topvar() + 1;
  int *exp = newarray_atomic(int, nv);
  Frobby::Ideal F(nv);
  for (Index<MonomialIdeal> i = I->first(); i.valid(); i++)
    {
      Bag *b = I->operator[](i);
      varpower::to_ntuple(nv, b->monom().raw(), exp);

      if (M2_gbTrace >= 4) fprintf(stderr, "adding ");
      for (int j = 0; j < nv; j++)
        {
          if (M2_gbTrace >= 4) fprintf(stderr, "%d ", exp[j]);
          F.addExponent(exp[j]);
        }
      if (M2_gbTrace >= 4) fprintf(stderr, "\n");
    }

  // Now create the consumer object, and call Frobby
  MyIdealConsumer M(I->get_ring(), nv);
  Frobby::alexanderDual(F, topvec, M);
  freemem(exp);
  // Extract the answer as a MonomialIdeal
  return M.result();
}

static MonomialIdeal *wrapperFrobbyAlexanderDual(const MonomialIdeal *I,
                                                 const M2_arrayint top)
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
      for (int i = 0; i < top->len; i++)
        mpz_init_set_si(topvec[i], top->array[i]);
    }

  MonomialIdeal *result = FrobbyAlexanderDual(I, topvec);

  // Clean up
  if (topvec != 0)
    {
      for (int i = 0; i < top->len; i++) mpz_clear(topvec[i]);
      freemem(topvec);
    }

  return result;
}

static MonomialIdeal /* or null */ *alexDual(const MonomialIdeal *I,
                                             const M2_arrayint top,
                                             int strategy)
{
  if (I->topvar() < 0)
    strategy =
        1;  // i.e. don't use frobby if there are no generators and/or variables
  switch (strategy)
    {
      case 0:
        if (M2_gbTrace >= 1) emit_line(" -- [Alexander dual: frobby]");
        return wrapperFrobbyAlexanderDual(I, top);
      default:
        if (M2_gbTrace >= 1) emit_line(" -- [Alexander dual: M2 monideal]");
        return I->alexander_dual(top);
    }
}

const MonomialIdeal /* or null */ *rawAlexanderDual(const MonomialIdeal *I,
                                                    const M2_arrayint top,
                                                    int strategy)
{
  try
    {
      MonomialIdeal *result = alexDual(I, top, strategy);
      intern_monideal(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
