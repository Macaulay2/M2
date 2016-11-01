// Copyright 2002 Michael E. Stillman

#include "engine.h"

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

unsigned int rawRingHash(const Ring *R)
{
  return R->hash();
}

M2_string IM2_Ring_to_string(const Ring *R)
{
  buffer o;
  R->text_out(o);
  return o.to_string();
}

long rawRingCharacteristic(const Ring* R)
{
  return R->characteristic();
}

///////////////////
// Ring creation //
///////////////////

const Ring *IM2_Ring_ZZ(void)
{
  return globalZZ;
}

const Ring *IM2_Ring_QQ(void)
{
  return globalQQ;
}

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
     try {
          return GF::create(f);
     }
     catch (exc::engine_error e) {
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
     try {
       const PolyRing *result = PolyRing::create(K,M);
       intern_polyring(result);
       return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring* /* or null */ rawDividedPowerRing(const Ring *K, const Monoid *M)
{
#if 0
  //TODO: MES, this function has not yet been implemented, or even placed in engine.h
  try {
    const DividedPowerRing * result = 0;  // DividedPowerRing::create(K,M);
    intern_polyring(result);
    return result;
  }
  catch (exc::engine_error e) {
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
     try {
          const PolynomialRing *P = R->cast_to_PolynomialRing();
          if (P == 0)
            {
              ERROR("expected a polynomial ring");
              return 0;
            }
          SkewPolynomialRing *result =
             SkewPolynomialRing::create(P->getCoefficients(),
                                        P->getMonoid(),
                                        skewvars);
          intern_polyring(result);
          return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring /* or null */ *IM2_Ring_weyl_algebra(const Ring *R,
                                        M2_arrayint comm_vars,
                                        M2_arrayint diff_vars,
                                        int homog_var)
{
     try {
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
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring /* or null */ *IM2_Ring_solvable_algebra(const Ring *R,
                                            const Matrix *Q)
{
     try {
          const PolynomialRing *P = R->cast_to_PolynomialRing();
          if (P == 0)
            {
              ERROR("expected a polynomial ring");
              return 0;
            }
          SolvableAlgebra *result = SolvableAlgebra::create(P, Q);
          intern_polyring(result);
          return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring /* or null */ *IM2_Ring_frac(const Ring *R)
{
     try {
          if (R == globalZZ) return globalQQ;
          const PolyRingFlat *P = R->cast_to_PolyRingFlat();
          if (P == 0)
            {
              ERROR("expected polynomial ring");
              return 0;
            }
          if (P->getMonoid()->numNonTermOrderVariables() > 0)
            {
              ERROR("cannot currently make fraction field over a polynomial ring with a non-global monomial order");
              return 0;
            }
          if (P->getMonoid()->numInvertibleVariables() > 0)
            {
              ERROR("cannot currently make fraction field over a polynomial ring with Laurent variables, i.e. Inverses=>true set");
              return 0;
            }
          if (R->get_precision() > 0)
            {
              ERROR("cannot make fraction field over approximate field base");
              return 0;
            }
          if (P->getCoefficients()->cast_to_FractionField() != 0)
            {
              ERROR("fraction fields over other fraction fields not yet implemented");
              return 0;
            }
          return FractionField::create(P);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring /* or null */ *IM2_Ring_localization(const Ring *R, const Matrix *Prime)
{
     try {
          const PolynomialRing *P = R->cast_to_PolynomialRing();
          if (P == 0)
            {
              ERROR("expected a polynomial ring");
              return 0;
            }
#if 0
          //   return P->create_FractionRing(Prime);
#endif
          /* TODO */
#ifdef DEVELOPMENT
#warning "implement IM2_Ring_localization"
#endif
          return 0;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring /* or null */ * IM2_Ring_quotient(const Ring *R,
                                     const Matrix *I)
{
     try {
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
          PolynomialRing *result = PolynomialRing::create_quotient(P,I);
          intern_polyring(result);
          return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring /* or null */ * IM2_Ring_quotient1(const Ring *R,
                                      const Ring *B)
  /* R is a poly ring of the form A[x], B = A/I, constructs A[x]/I */
/* if R is a polynomial ring of the form A[x]/J, and B = A/I (where A is a poly ring)
   then form the quotient ring B[x]/J. */
{
     try {
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
          PolynomialRing *result = PolyRingQuotient::create_quotient(R1,B1);
          intern_polyring(result);
          return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring /* or null */ *IM2_Ring_schur(const Ring *R)
{
     try {
          const PolynomialRing *P = R->cast_to_PolynomialRing();
          if (P == 0)
            {
              ERROR("Schur ring construction: expected a polynomial ring");
              return 0;
            }
          SchurRing *result = SchurRing::create(P);
          intern_polyring(result);
          return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Ring *rawSchurRing1(const Ring *A)
{
  try {
    SchurRing2 *result = SchurRing2::createInfinite(A);
    return result;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}


const Ring *rawSchurRing2(const Ring *A, int n)
{
  try {
    SchurRing2 *result = SchurRing2::create(A,n);
    return result;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const Ring *rawSchurSnRing(const Ring *A, int n)
{
  try {
    SchurSnRing *result = SchurSnRing::create(A,n);
    return result;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const Ring /* or null */ *rawTowerRing1(long charac, M2_ArrayString names)
{
  return Tower::create(static_cast<int>(charac), names);
}

const Ring /* or null */ *rawTowerRing2(const Ring *R1, M2_ArrayString new_names)
{
  try {
    const Tower *R = R1->cast_to_Tower();
    if (R == 0)
      {
        ERROR("expected a tower coefficient ring");
        return 0;
      }
    return Tower::create(R, new_names);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return 0;
  }
}

const Ring /* or null */ *rawTowerRing3(const Ring *R1, engine_RawRingElementArray eqns)
{
  try {
    const Tower *R = R1->cast_to_Tower();
    if (R == 0)
      {
        ERROR("expected a tower coefficient ring");
        return 0;
      }
    VECTOR(ring_elem) extensions;
    for (int i=0; i<eqns->len; i++)
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
  }
  catch (exc::engine_error e) {
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

const RingElement * rawGetNonUnit(const Ring *K)
{
  return RingElement::make_raw(K, K->get_non_unit());
}

const Ring /* or null */ *rawAmbientRing(const Ring *R)
/* If R is a quotient of a polynomial ring, or is a fraction ring, return the
   polynomial ring over a basic ring of which this is a quotient (or fraction ring) of.
   For example, if R = frac(ZZ[s,t]/(s^2-1))[x,y,z]/(s*x+t*y+z^2), then the returned
   ring is ZZ[s,t][x,y,z]. This routine is provided only for debugging the engine. */
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
/*********************************************************************/

unsigned int rawRingElementHash(const RingElement *a)
{
  return a->hash();
}

const Ring * IM2_RingElement_ring(const RingElement *a)
{
  return a->get_ring();
}

M2_string IM2_RingElement_to_string(const RingElement *f)
{
  buffer o;
  f->text_out(o);
  return o.to_string();
}

const RingElement *IM2_RingElement_from_Integer(const Ring *R, gmp_ZZ d)
{
  return RingElement::make_raw(R, R->from_int(d));
}

const RingElement *IM2_RingElement_from_rational(const Ring *R, gmp_QQ r)
{
  return RingElement::make_raw(R, R->from_rational(r));
}

const RingElement *IM2_RingElement_from_BigComplex(const Ring *R, gmp_CC z)
{
  ring_elem f;
  if (R->from_BigComplex(z,f))
    return RingElement::make_raw(R, f);
  ERROR("cannot create element of this ring from an element of CC");
  return 0;
}

const RingElement *IM2_RingElement_from_BigReal(const Ring *R, gmp_RR z)
{
  ring_elem f;
  if (R->from_BigReal(z,f))
    return RingElement::make_raw(R, f);
  ERROR("cannot create element of this ring from an element of RR");
  return 0;
}


gmp_ZZorNull IM2_RingElement_to_Integer(const RingElement *a)
  /* If the ring of a is ZZ, or ZZ/p, this returns the underlying representation.
     Otherwise, NULL is returned, and an error is given */
{
  const Ring *R = a->get_ring();
  if (R->is_ZZ())
    {
      void *f = a->get_value().poly_val;
      return static_cast<gmp_ZZ>(f);
    }
  if (R->isFinitePrimeField())
    {
      gmp_ZZ result = newitem(__mpz_struct);

      std::pair<bool,long> res = R->coerceToLongInteger(a->get_value());
      M2_ASSERT(res.first);

      mpz_init_set_si(result, static_cast<int>(res.second));
      return result;
    }
  ERROR("Expected ZZ or ZZ/p as base ring");
  return 0;
}

gmp_QQorNull IM2_RingElement_to_rational(const RingElement *a)
{
  if (!a->get_ring()->is_QQ())
    {
      ERROR("expected an element of QQ");
      return 0;
    }
  void *f = a->get_value().poly_val;
  return static_cast<gmp_QQ>(f);
}

gmp_RRorNull IM2_RingElement_to_BigReal(const RingElement *a)
{
  const Ring* R = a->get_ring();
  gmp_RR result;
  void* b;
  double* c;
  const M2::ConcreteRing<M2::ARingRRR> *R1;

  switch (R->ringID()) 
    {
    case M2::ring_RR:
      result = getmemstructtype(gmp_RR);
      mpfr_init2(result, 53);
      b = static_cast<void*>(a->get_value().poly_val);
      c = static_cast<double*>(b);
      mpfr_set_d(result, *c, GMP_RNDN);
      return result;
    case M2::ring_RRR:
      R1 = dynamic_cast< const M2::ConcreteRing<M2::ARingRRR> * >(a->get_ring());
      result = getmemstructtype(gmp_RR);
      mpfr_init2(result, R1->get_precision());
      b = a->get_value().poly_val;
      mpfr_set(result, static_cast<gmp_RR>(b), GMP_RNDN);
      return result;
    default:
      if (!a->get_ring()->is_RRR())
        {
          ERROR("expected an element of RRR");
          return 0;
        }
      return a->get_value().mpfr_val;
    }
}

gmp_CCorNull IM2_RingElement_to_BigComplex(const RingElement *a)
{
  const Ring* R = a->get_ring();
  auto RCCC = dynamic_cast<const M2::ConcreteRing<M2::ARingCCC>*>(R);
  if (RCCC != 0)
    {
      M2::ARingCCC::ElementType b;
      RCCC->ring().init(b);
      RCCC->ring().from_ring_elem(b, a->get_value());
      gmp_CC result = RCCC->ring().toBigComplex(b);
      RCCC->ring().clear(b);
      return result;
    }
  auto RCC = dynamic_cast<const M2::ConcreteRing<M2::ARingCC>*>(R);
  if (RCC != 0)
    {
      M2::ARingCC::ElementType b;
      RCC->ring().init(b);
      RCC->ring().from_ring_elem(b, a->get_value());
      gmp_CC result = RCC->ring().toBigComplex(b);
      RCC->ring().clear(b);
      return result;
    }
  ERROR("expected an element of CCC");
  return 0;
}

#if 0
int rawDiscreteLog(const RingElement *h)
{
  const Ring *R = h->get_ring();
  
  const Z_mod *RP = R->cast_to_Z_mod();
  if (RP != 0)
    return RP->discrete_log(h->get_value());

  const GF *P = R->cast_to_GF();
  if (P != 0)
    return P->discrete_log(h->get_value());

  // Returns -1 if either h is zero, or the ring of h doesn't have a discrete log algorithm
  return -1;
}
#endif

long rawDiscreteLog(const RingElement *h)
{
  try
    {
      const Ring *R = h->get_ring();
      return R->discreteLog(h->get_value());
    }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return -1;
  }
}

const RingElement* rawMultiplicativeGenerator(const Ring *R)
{
  return R->getGenerator();
}

const RingElement /* or null */ *IM2_RingElement_make_var(const Ring *R, int v)
{
     try {
          ring_elem a = R->var(v);
          if (error()) return 0;
          return RingElement::make_raw(R, a);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

M2_bool IM2_RingElement_is_zero(const RingElement *a)
{
  return a->get_ring()->is_zero(a->get_value());
}

M2_bool IM2_RingElement_is_equal(const RingElement *a,
                                 const RingElement *b)
{
  const Ring *R = a->get_ring();
  if (R != b->get_ring()) return 0;
  return R->is_equal(a->get_value(), b->get_value());
}

engine_RawRingElementPair IM2_RingElement_divmod(const RingElement *a,
                                               const RingElement *b)
{
     try {
          const Ring *R = a->get_ring();
          if (R != b->get_ring())
            {
              ERROR("ring remainder requires both elements to have the same base ring");
              return 0;
            }
          ring_elem fquot;
          ring_elem frem = R->remainderAndQuotient(a->get_value(), b->get_value(), fquot);
          if (error())  return NULL;

          engine_RawRingElementPair result = new engine_RawRingElementPair_struct;
          result->a = RingElement::make_raw(R, fquot);
          result->b = RingElement::make_raw(R, frem);
          return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

int rawRingElementCompare(const RingElement *a,
                          const RingElement *b)
{
     try {
          const Ring *R = a->get_ring();
          if (R != b->get_ring()) return 0;
          //  if (a->is_zero())
          //    {
          //      if (b->is_zero()) return 0;
          //      return 1;
          //    }
          //  if (b->is_zero()) return -1;
          return R->compare_elems(a->get_value(), b->get_value());
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return -2;
     }
}

const RingElement *IM2_RingElement_promote(const Ring *S,
                                           const RingElement *f)
{
     try {
          const RingElement *result;

          if (f->promote(S,result))
            return result;
          ERROR("cannot promote given ring element");
          return 0;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_lift(int *success_return, const Ring *S,
                                        const RingElement *f)
{
     try {
          const RingElement *result;

          if (f->lift(S,result)) {
            *success_return = 1;
            return result;
          }
          // ERROR("cannot lift given ring element");
          return 0;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

M2_bool IM2_RingElement_is_graded(const RingElement *a)
{
  return a->is_homogeneous();
}

M2_arrayint IM2_RingElement_multidegree(const RingElement *a)
{
     try {
          return a->multi_degree();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement* /* or null */ rawRingElementAntipode(const RingElement* f)
{
  try {
    const Ring* R = f->get_ring();
    return RingElement::make_raw(R, R->antipode(f->get_value()));
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

gmp_ZZpairOrNull rawWeightRange(M2_arrayint wts,
                                       const RingElement *a)
  /* The first component of the degree is used, unless the degree monoid is trivial,
     in which case the degree of each variable is taken to be 1.
     Returns lo,hi degree.  If the ring is not a graded ring or a polynomial ring
     then (0,0) is returned.
  */
{
     try {
          int lo,hi;
          a->degree_weights(wts,lo,hi);
          if (error()) return 0;
          gmp_ZZpair p = new gmp_ZZpair_struct;
          p->a = newitem(__mpz_struct);
          p->b = newitem(__mpz_struct);
          mpz_init_set_si(p->a, static_cast<long>(lo));
          mpz_init_set_si(p->b, static_cast<long>(hi));
          return p;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_homogenize_to_degree(const RingElement *a,
                                                              int v,
                                                              int deg,
                                                              M2_arrayint wts)
{
     try {
          return a->homogenize(v,deg,wts);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_homogenize(const RingElement *a,
                                                    int v,
                                                    M2_arrayint wts)
{
     try {
          return a->homogenize(v,wts);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_term(const Ring *R,
                                              const RingElement *a,
                                              const Monomial *m)
  /* R must be a polynomial ring, and 'a' an element of the
     coefficient ring of R.  Returns a*m, if this is a valid
     element of R.  Returns NULL if not (with an error message). */
{
     try {
          const PolynomialRing *P = R->cast_to_PolynomialRing();
          if (P == 0)
            {
              ERROR("requires a polynomial ring");
              return 0;
            }

          int nvars0 = P->n_vars();
          const PolynomialRing *K = a->get_ring()->cast_to_PolynomialRing();
          if (K != 0 && K != P->getCoefficients())
            nvars0 -= K->n_vars();
          int *exp = newarray_atomic(int,nvars0);
          varpower::to_ntuple(nvars0, m->ints(), exp);
          ring_elem val = P->make_logical_term(a->get_ring(), a->get_value(), exp);
          return RingElement::make_raw(R,val);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_get_terms(
            int nvars, /* n variables in an outermost monoid */
            const RingElement *a,
            int lo, int hi)
  /* Returns the sum of some monomials of 'a', starting at 'lo',
     going up to 'hi'.  If either of these are negative, they are indices
     from the back of the polynomial.
     'a' should be an element of a polynomial ring.
  */
{
  return a->get_terms(nvars,lo,hi);
}

const RingElement /* or null */ *IM2_RingElement_get_coeff(
           const Ring * coeffRing, /* ring of the result */
           const RingElement *a,
           const Monomial *m)
  /* Return (as an element of the coefficient ring) the coeff
       of the monomial 'm'.
    */
{
     try {
          return a->get_coeff(coeffRing,m);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_lead_coeff(
           const Ring * coeffRing, /* ring of the result */
           const RingElement *a)
{
     try {
          return a->lead_coeff(coeffRing);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Monomial /* or null */ *IM2_RingElement_lead_monomial(
           int nvars, /* number of variables in an outermost monoid */
           const RingElement *a)
{
     try {
          return a->lead_monom(nvars);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

int IM2_RingElement_n_terms(
          int nvars, /* number of variables in an outermost monoid */
          const RingElement *a)
{
  return a->n_terms(nvars);
}

engine_RawArrayPairOrNull IM2_RingElement_list_form(
          const Ring * coeffRing, /* ring of the result coefficients */
          const RingElement *f)
{
  try {
    const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
    if (P != 0)
      {
        return P->list_form(coeffRing, f->get_value());
      }
    const SchurRing2 *S = f->get_ring()->cast_to_SchurRing2();
    if (S != 0)
      {
        return S->list_form(coeffRing, f->get_value());
      }
    ERROR("expected a polynomial");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

engine_RawRingElementArray rawGetParts(const M2_arrayint wts,
                                     const RingElement *f)
/* Return an array of RingElement's, each having pure weight, and sorted by
   strictly increasing weight value.  The wt vector values must fit into
   a word length integer.  */
{
  try {
    const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
    if (P == 0)
      {
        ERROR("expected a polynomial");
        return 0;
      }
    long relems_len;
    ring_elem *relems = P->get_parts(wts, f->get_value(), relems_len);
    engine_RawRingElementArray result = getmemarraytype(engine_RawRingElementArray,relems_len);
    result->len = static_cast<int>(relems_len);
    for (int i=0; i<result->len; i++)
      result->array[i] = RingElement::make_raw(P,relems[i]);
    deletearray(relems);
    return result;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

#if 0
void convolve(const Ring *R,
              const VECTOR(ring_elem) &input_relems,
              VECTOR(ring_elem) &output_relems,
              int convolve_type)
{
  int n = input_relems.size() - 1; // array is 0..n, with the 0 part being ignored.
  // first set [1, e1] := [1, h1].
  output_relems[0] = input_relems[0];
  output_relems[1] = input_relems[1];
  for (int i=2; i<=n; i++)
    {
      ring_elem result = R->copy(input_relems[i]);
      for (int j=i-1; j>=1; --j)
        {
          ring_elem f = R->mult(input_relems[j], output_relems[i-j]);
          result = R->add(result,f);
        }
      output_relems[i] = result;
    }
}
#endif

void convolve(const PolyRing *R,
              const VECTOR(ring_elem) &input_relems,
              VECTOR(ring_elem) &output_relems,
              int convolve_type)
{
  const Ring *K = R->getCoefficientRing();
  ring_elem invn;
  size_t n = input_relems.size() - 1; // array is 0..n, with the 0 part being ignored.
  // first set [1, e1] := [1, h1].
  output_relems[0] = input_relems[0];
  for (int i=1; i<=n; i++)
    {
      // ASSUMPTION: input_relems[i] is either a variable or - of a variable
      ring_elem result = R->copy(input_relems[i]);
      if (convolve_type == 2)
        R->mult_coeff_to(K->from_long(-i), result);
      for (int j=i-1; j>=1; --j)
        {
          ring_elem hr;
          Nterm *g = input_relems[j];
          if (g != 0)
            {
              hr.poly_val = R->mult_by_term(output_relems[i-j], g->coeff, g->monom);
              R->internal_add_to(result, hr);
            }
        }
      if (convolve_type == 1)
        {
          invn = K->invert(K->from_long(i));
          R->mult_coeff_to(invn,result);
        }
      output_relems[i] = result;
    }
}

engine_RawRingElementArrayOrNull rawConvolve(engine_RawRingElementArray H,
                                       int convolve_type)
{
  // convolve_type == 1: E_n = E_(n-1) * H_1 - E(n-2) * H_2 + ... + (-1)^n * H_n
  // convolve_type == 2: n * E_n = E_(n-1) * H_1 - E(n-2) * H_2 + ... + (-1)^n * H_n
  // others of interest?  These are good for translating between symmetric functions of type E,H,P

  // H_0 is ignored... (OR: should we instead shift everything: first entry of H is denoted H_1...?)

  try {
    int len = H->len;
    if (len <= 1)
      {
        ERROR("expected ring element array of length at least 2");
        return 0;
      }
    const PolyRing *P = H->array[1]->get_ring()->cast_to_PolyRing();
    if (P == 0)
      {
        ERROR("expected a polynomial ring");
        return 0;
      }
    VECTOR(ring_elem) input_relems(len);
    VECTOR(ring_elem) output_relems(len);
    for (int i=0; i<len; i++)
      input_relems[i] = H->array[i]->get_value();
    convolve(P, input_relems, output_relems, convolve_type);

    engine_RawRingElementArray result = getmemarraytype(engine_RawRingElementArray,len);
    result->len = len;
    for (int i=0; i<result->len; i++)
      result->array[i] = RingElement::make_raw(P,output_relems[i]);

    return result;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const RingElement /* or null */ *rawGetPart(const M2_arrayint wts,
                                   const RingElement *f,
                                   M2_bool lobound_given,
                                   M2_bool hibound_given,
                                   long lobound,
                                   long hibound)
/* Return the sum of all of the terms t of f, which satisfy: lobound <= wt.t <= hibound,
   where, if lobound_given is false, then lobound is -infinity, and if hibound_given
   is false, then hibound is infinity. */
{
  try {
    const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
    if (P == 0)
      {
        ERROR("expected a polynomial");
        return 0;
      }
    ring_elem g = P->get_part(wts,
                              f->get_value(),
                              lobound_given,
                              hibound_given,
                              lobound,
                              hibound);
    return RingElement::make_raw(P,g);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

int IM2_RingElement_index_if_var(const RingElement *f)
  /* if f is a variable of its ring, then the index of that variable is returned.
     If f isnot a variable, then -1 is returned. */
{
  const Ring *R = f->get_ring();
  return R->index_of_var(f->get_value());
}

M2_arrayint rawRingElementIndices(const RingElement *f)
  /* The list of indices of variables which occur in f is returned. */
{
  const Ring *R = f->get_ring();
  return R->support(f->get_value());
}

const RingElement /* or null */ * rawAssociateDivisor(const RingElement *f)
{
     try {
          const PolyRing *P = f->get_ring()->cast_to_PolyRing();
          if (P == 0)
            {
              ERROR("expected an element of a polynomial ring");
              return 0;
            }
          if (!P->getCoefficients()->has_associate_divisors())
            {
              ERROR("cannot find preferred associates for this ring");
              return 0;
            }
          return RingElement::make_raw(P->getCoefficients(), P->preferred_associate_divisor(f->get_value()));
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *rawRingElementContent(const RingElement *f)
// returns the content of f (as a matrix over the base coefficient ring)
{
  return f->content();
}

const RingElement /* or null */ *rawRingElementRemoveContent(const RingElement *f)
// returns the polynomial which results after division by the content
{
  return f->remove_content();
}

const RingElement /* or null */ *rawRingElementSplitContent(const RingElement *f, const RingElement /* or null */ **result)
// returns the content of f (as a matrix over the base coefficient ring)
// result is set to the polynomial which results after division by the content
{
  return f->split_off_content(*result);
}

const RingElement /* or null */ *IM2_RingElement_numerator(const RingElement *a)
{
     try {
          return a->numerator();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_denominator(const RingElement *a)
{
     try {
          return a->denominator();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *IM2_RingElement_fraction(const Ring *R,
                                                  const RingElement *a,
                                                  const RingElement *b)
{
     try {
       const RingElement *f = a->fraction(R,b);
       if (error()) return NULL;
       return f;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

gmp_ZZorNull rawSchurDimension(const RingElement *f)
{
     try {
          const SchurRing *S = f->get_ring()->cast_to_SchurRing();
          if (S == 0)
            {
              ERROR("expected a polynomial over a Schur ring");
              return 0;
            }
          ring_elem result = S->dimension(f->get_value());
          return result.get_mpz();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const RingElement /* or null */ *rawSchurSnTensorMult(const RingElement *a, const RingElement *b)
  /* the tensor multiplication function in SchurSnRing */
{
  try {
    const SchurSnRing *R = a->get_ring()->cast_to_SchurSnRing();
    if (R == 0)
      {
        ERROR("expected a SchurSn ring element");
        return 0;
      }
    if (R != b->get_ring())
      {
        ERROR("expected SchurSn ring elements");
        return 0;
      }
    ring_elem result = R->tensor_mult(a->get_value(), b->get_value());
    return RingElement::make_raw(R,result);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const RingElement /* or null */ *rawSchurFromPartition(const Ring *R, M2_arrayint part)
{
  // R should be a SchurRing2
  // part should be a partition: a weakly descending list of integers (for now, non-negative)
  // if R has a limit on the size of partitions, then
  try {
    const SchurRing2 *S = R->cast_to_SchurRing2();
    if (S == 0)
      {
        ERROR("expected a Schur ring");
        return 0;
      }
    // Check that part is a partition, and that the number of parts is <= number allowed
    if (!S->is_valid_partition(part)) return 0;
    ring_elem result = S->from_partition(part);
    return RingElement::make_raw(S, result);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}
/* Special routines for tower rings */

int rawDegree(int v, const RingElement *f)
  /* Returns -1 if 0 or not implemented for a given ring.  For now, valid only for tower rings */
{
  const Tower *R = f->get_ring()->cast_to_Tower();
  if (R == 0) return -1;
  return R->degreeInVariable(v, f->get_value());
}

int rawExtensionDegree(int firstvar, const Ring *R1)
  /* Currently only valid for tower rings.  Others return 0.  */
{
  const Tower *R = R1->cast_to_Tower();
  if (R == 0) return 0;
  if (firstvar < 0) {
    ERROR("use rawCharacteristic to find the characteristic");
    return -1;
  }
  return R->extension_degree(firstvar);
}

const RingElement /* or null */ *rawDiff(int v, const RingElement *f)
{
  const Tower *R = f->get_ring()->cast_to_Tower();
  if (R == 0)
    {
      ERROR("not implemented for this ring");
      return 0;
    }
  return RingElement::make_raw(R,  R->differentiate(v, f->get_value()));
}

const RingElement /* or null */ *rawLowerP(const RingElement *f)
{
  const Tower *R = f->get_ring()->cast_to_Tower();
  if (R == 0)
    {
      ERROR("not implemented for this ring");
      return 0;
    }
  return RingElement::make_raw(R,  R->lowerP(f->get_value()));
}

const RingElement /* or null */ *rawPowerMod(const RingElement *f, mpz_ptr n, const RingElement *g)
{
  const Tower *R = f->get_ring()->cast_to_Tower();
  if (R == 0)
    {
      ERROR("not implemented for this ring");
      return 0;
    }
  if (g->get_ring() != R)
    {
      ERROR("expected elements in the same ring");
      return 0;
    }
  return RingElement::make_raw(R,  R->power_mod(f->get_value(), n, g->get_value()));
}

/////////////////////////////
// GaloisField routines /////
/////////////////////////////

bool findConwayPolynomial(long charac, 
                          long deg, 
                          bool find_random_if_no_conway_poly_available,
                          std::vector<long>& result_poly)
{
  // returns true if result_poly is actually set
  int ret = 1;
  fq_nmod_ctx_t ctx;
  fmpz_t p;
  fmpz_init(p);
  fmpz_set_si(p, charac);
  if (!find_random_if_no_conway_poly_available)
    ret = _fq_nmod_ctx_init_conway(ctx,p,deg,"a");
  else
    fq_nmod_ctx_init(ctx,p,deg,"a");

  if (ret == 0) return false;

  result_poly.resize(deg+1);
  for (long i=0; i<=deg; i++)
    result_poly[i] = 0;
  for (long i=0; i<ctx->len; i++)
    {
      if (ctx->j[i] < 0 or ctx->j[i] > deg)
        printf("error: encountered bad degree\n");
      // power is ctx->j[i]
      // coeff is ctx->a[i]
      result_poly[ctx->j[i]] = ctx->a[i];
    }

  //printf("flint GF information:\n");
  //fq_nmod_ctx_print(ctx);

  fq_nmod_ctx_clear(ctx);
  return true;
}

M2_arrayint rawConwayPolynomial(
    long charac, 
    long deg, 
    M2_bool find_random_if_no_conway_poly_available)
{
  std::vector<long> poly;
  findConwayPolynomial(charac, deg, find_random_if_no_conway_poly_available, poly);
  return stdvector_to_M2_arrayint(poly);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
