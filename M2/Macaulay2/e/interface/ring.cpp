// Copyright 2002 Michael E. Stillman

#include "interface/ring.h"

#include "monoid.hpp"
#include "monomial.hpp"
#include "relem.hpp"
#include "ZZp.hpp"
#include "ZZ.hpp"
#include "GF.hpp"
#include "polyring.hpp"
#include "schur.hpp"
#include "schur2.hpp"
#include "schurSn.hpp"
#include "frac.hpp"
#include "localring.hpp"
#include "weylalg.hpp"
#include "skewpoly.hpp"
#include "solvable.hpp"
#include "polyquotient.hpp"

#include "matrix.hpp"
#include "exceptions.hpp"
#include "finalize.hpp"

#include "tower.hpp"

#include "aring.hpp"
#include "aring-glue.hpp"
#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fq_nmod.h>
#pragma GCC diagnostic pop

unsigned int rawRingHash(const Ring *R) { return R->hash(); }

M2_string IM2_Ring_to_string(const Ring *R)
{
  buffer o;
  R->text_out(o);
  return o.to_string();
}

long rawRingCharacteristic(const Ring *R) { return R->characteristic(); }

///////////////////
// Ring creation //
///////////////////

const Ring *IM2_Ring_ZZ(void) { return globalZZ; }
const Ring *IM2_Ring_QQ(void) { return globalQQ; }
const Ring /* or null */ *IM2_Ring_ZZp(int p)
/* p must be a prime number <= 32767 */
{
  if (p <= 1 || p >= 32750)
    {
      ERROR("ZZP: expected a prime number p in range 2 <= p <= 32749");
      return 0;
    }
  return Z_mod::create(p);
}

const Ring /* or null */ *rawGaloisField(const RingElement *f)
{
  // Check that the ring R of f is a polynomial ring in one var over a ZZ/p
  // Check that f has degree >= 2
  // Check that f is monic
  // If any of these fail, then return 0.
  try
    {
      return GF::create(f);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_RRR(unsigned long prec)
{
  if (prec <= 53)
    return M2::ConcreteRing<M2::ARingRR>::create(new M2::ARingRR());
  return M2::ConcreteRing<M2::ARingRRR>::create(new M2::ARingRRR(prec));
}

const Ring /* or null */ *IM2_Ring_CCC(unsigned long prec)
{
  if (prec <= 53)
    return M2::ConcreteRing<M2::ARingCC>::create(new M2::ARingCC());
  return M2::ConcreteRing<M2::ARingCCC>::create(new M2::ARingCCC(prec));
}

const Ring *IM2_Ring_trivial_polyring()
{
  return PolyRing::get_trivial_poly_ring();
}

const Ring /* or null */ *IM2_Ring_polyring(const Ring *K, const Monoid *M)
{
  try
    {
      const PolyRing *result = PolyRing::create(K, M);
      intern_polyring(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring * /* or null */ rawDividedPowerRing(const Ring *K, const Monoid *M)
{
#if 0
  //TODO: MES, this function has not yet been implemented, or even placed in ring.h
  try {
    const DividedPowerRing * result = 0;  // DividedPowerRing::create(K,M);
    intern_polyring(result);
    return result;
  }
  catch (const exc::engine_error& e) {
    ERROR(e.what());
    return NULL;
  }
#endif
  ERROR("not yet implemented");
  return 0;
}

const Ring /* or null */ *IM2_Ring_skew_polyring(const Ring *R,
                                                 M2_arrayint skewvars)
{
  try
    {
      const PolynomialRing *P = R->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected a polynomial ring");
          return 0;
        }
      SkewPolynomialRing *result = SkewPolynomialRing::create(
          P->getCoefficients(), P->getMonoid(), skewvars);
      intern_polyring(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_weyl_algebra(const Ring *R,
                                                M2_arrayint comm_vars,
                                                M2_arrayint diff_vars,
                                                int homog_var)
{
  try
    {
      const PolynomialRing *P = R->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected a polynomial ring");
          return 0;
        }
      WeylAlgebra *result = WeylAlgebra::create(P->getCoefficients(),
                                                P->getMonoid(),
                                                diff_vars,
                                                comm_vars,
                                                homog_var);
      intern_polyring(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_solvable_algebra(const Ring *R,
                                                    const Matrix *Q)
{
  try
    {
      const PolynomialRing *P = R->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected a polynomial ring");
          return 0;
        }
      SolvableAlgebra *result = SolvableAlgebra::create(P, Q);
      intern_polyring(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_frac(const Ring *R)
{
  try
    {
      if (R == globalZZ) return globalQQ;
      const PolyRingFlat *P = R->cast_to_PolyRingFlat();
      if (P == 0)
        {
          ERROR("expected polynomial ring");
          return 0;
        }
      if (P->getMonoid()->numNonTermOrderVariables() > 0)
        {
          ERROR(
              "cannot currently make fraction field over a polynomial ring "
              "with a non-global monomial order");
          return 0;
        }
      if (P->getMonoid()->numInvertibleVariables() > 0)
        {
          ERROR(
              "cannot currently make fraction field over a polynomial ring "
              "with Laurent variables, i.e. Inverses=>true set");
          return 0;
        }
      if (R->get_precision() > 0)
        {
          ERROR("cannot make fraction field over approximate field base");
          return 0;
        }
      if (P->getCoefficients()->cast_to_FractionField() != 0)
        {
          ERROR(
              "fraction fields over other fraction fields not yet implemented");
          return 0;
        }
      if (P->getCoefficients()->cast_to_LocalRing() != 0)
        {
          ERROR("fraction fields over other local rings not yet implemented");
          return 0;
        }
      return FractionField::create(P);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_localization(const Ring *R, Computation *C)
{
  try
    {
      const PolyRing *PR = R->cast_to_PolyRing(); // FIXME should this get a PolyRing or Ring?
      GBComputation *P = C->cast_to_GBComputation();
      if (PR == nullptr)
        {
          ERROR("expected a polynomial ring");
          return nullptr;
        }
      if (P == nullptr)
        {
          ERROR("expected a Grobner basis computation");
          return nullptr;
        }
      if (P->get_ring() != PR)
        {
          ERROR("expected matrix to be over the same ring");
          return nullptr;
        }
      return LocalRing::create(PR, P);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_quotient(const Ring *R, const Matrix *I)
{
  try
    {
      if (I->get_ring() != R)
        {
          ERROR("expected matrix to be over the same ring");
        }
      if (I->n_rows() != 1)
        {
          ERROR("expected a one row matrix of quotient elements");
          return 0;
        }
      const PolynomialRing *P = R->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected a polynomial ring");
          return 0;
        }
      PolynomialRing *result = PolynomialRing::create_quotient(P, I);
      intern_polyring(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_quotient1(const Ring *R, const Ring *B)
/* R is a poly ring of the form A[x], B = A/I, constructs A[x]/I */
/* if R is a polynomial ring of the form A[x]/J, and B = A/I (where A is a poly
   ring)
   then form the quotient ring B[x]/J. */
{
  try
    {
      const PolynomialRing *R1 = R->cast_to_PolynomialRing();
      const PolynomialRing *B1 = B->cast_to_PolynomialRing();
      if (R1 == 0 || B1 == 0)
        {
          ERROR("expected a polynomial ring");
          return 0;
        }
      if (R1->n_quotients() > 0)
        {
          ERROR("encountered quotient polynomial ring");
          return 0;
        }
      PolynomialRing *result = PolyRingQuotient::create_quotient(R1, B1);
      intern_polyring(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *IM2_Ring_schur(const Ring *R)
{
  try
    {
      const PolynomialRing *P = R->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("Schur ring construction: expected a polynomial ring");
          return 0;
        }
      SchurRing *result = SchurRing::create(P);
      intern_polyring(result);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring *rawSchurRing1(const Ring *A)
{
  try
    {
      SchurRing2 *result = SchurRing2::createInfinite(A);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring *rawSchurRing2(const Ring *A, int n)
{
  try
    {
      SchurRing2 *result = SchurRing2::create(A, n);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring *rawSchurSnRing(const Ring *A, int n)
{
  try
    {
      SchurSnRing *result = SchurSnRing::create(A, n);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *rawTowerRing1(long charac, M2_ArrayString names)
{
  return Tower::create(static_cast<int>(charac), names);
}

const Ring /* or null */ *rawTowerRing2(const Ring *R1,
                                        M2_ArrayString new_names)
{
  try
    {
      const Tower *R = R1->cast_to_Tower();
      if (R == 0)
        {
          ERROR("expected a tower coefficient ring");
          return 0;
        }
      return Tower::create(R, new_names);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return 0;
  }
}

const Ring /* or null */ *rawTowerRing3(const Ring *R1,
                                        engine_RawRingElementArray eqns)
{
  try
    {
      const Tower *R = R1->cast_to_Tower();
      if (R == 0)
        {
          ERROR("expected a tower coefficient ring");
          return 0;
        }
      VECTOR(ring_elem) extensions;
      for (int i = 0; i < eqns->len; i++)
        {
          const RingElement *f = eqns->array[i];
          if (f->get_ring() != R1)
            {
              ERROR("extension element has incorrect base ring");
              return 0;
            }
          extensions.push_back(f->get_value());
        }
      return Tower::create(R, extensions);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return 0;
  }
}

M2_bool IM2_Ring_is_field(const Ring *K)
/* Returns true if K is a field, or has been declared to be one.
   In the latter case, if an operation shows that K cannot be a field,
   then this function will thereafter return false, and
   rawGetNonUnit(K) can be used to obtain a non-unit, if one
   has been found. */
{
  return K->is_field();
}

M2_bool IM2_Ring_declare_field(const Ring *K)
/* Declare that K is a field.  The ring K can then be used as the coefficient
   ring for computing Groebner bases,etc.  */
{
  return const_cast<Ring *>(K)->declare_field();
}

const RingElement *rawGetNonUnit(const Ring *K)
{
  return RingElement::make_raw(K, K->get_non_unit());
}

const Ring /* or null */ *rawAmbientRing(const Ring *R)
/* If R is a quotient of a polynomial ring, or is a fraction ring, return the
   polynomial ring over a basic ring of which this is a quotient (or fraction
   ring) of.
   For example, if R = frac(ZZ[s,t]/(s^2-1))[x,y,z]/(s*x+t*y+z^2), then the
   returned
   ring is ZZ[s,t][x,y,z]. This routine is provided only for debugging the
   engine. */
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  return P->getAmbientRing();
}

const Ring /* or null */ *rawDenominatorRing(const Ring *R)
/* If elements of R may have denominators, then this routine returns true, and
   the ambient ring for denominators is placed into resultRing. Otherwise, false
   is returned. This routine is provided only for debugging the engine. */
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  return P->getDenominatorRing();
}

/////////////////////////////
// GaloisField routines /////
/////////////////////////////

bool findConwayPolynomial(long charac,
                          long deg,
                          bool find_random_if_no_conway_poly_available,
                          std::vector<long> &result_poly)
{
  // returns true if result_poly is actually set
  int ret = 1;
  fq_nmod_ctx_t ctx;
  fmpz_t p;
  fmpz_init(p);
  fmpz_set_si(p, charac);
  if (!find_random_if_no_conway_poly_available)
    ret = _fq_nmod_ctx_init_conway(ctx, p, deg, "a");
  else
    fq_nmod_ctx_init(ctx, p, deg, "a");

  if (ret == 0) return false;

  result_poly.resize(deg + 1);
  for (long i = 0; i <= deg; i++) result_poly[i] = 0;
  for (long i = 0; i < ctx->len; i++)
    {
      if (ctx->j[i] < 0 or ctx->j[i] > deg)
        printf("error: encountered bad degree\n");
      // power is ctx->j[i]
      // coeff is ctx->a[i]
      result_poly[ctx->j[i]] = ctx->a[i];
    }

  // printf("flint GF information:\n");
  // fq_nmod_ctx_print(ctx);

  fq_nmod_ctx_clear(ctx);
  return true;
}

M2_arrayint rawConwayPolynomial(long charac,
                                long deg,
                                M2_bool find_random_if_no_conway_poly_available)
{
  std::vector<long> poly;
  findConwayPolynomial(
      charac, deg, find_random_if_no_conway_poly_available, poly);
  return stdvector_to_M2_arrayint(poly);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
