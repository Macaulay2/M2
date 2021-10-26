// Copyright 2002 Michael E. Stillman

#include "interface/ringelement.h"

#include <assert.h>
#include <memory>
#include <utility>
#include <vector>

#include "M2FreeAlgebra.hpp"
#include "M2FreeAlgebraQuotient.hpp"

#include "aring-CC.hpp"
#include "aring-CCC.hpp"
#include "aring-glue.hpp"
#include "aring.hpp"
#include "buffer.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "monomial.hpp"
#include "newdelete.hpp"
#include "poly.hpp"
#include "polyring.hpp"
#include "relem.hpp"
#include "ring.hpp"
#include "ringelem.hpp"
#include "schur.hpp"
#include "schur2.hpp"
#include "schurSn.hpp"
#include "tower.hpp"
#include "varpower.hpp"

namespace M2 { class ARingRRR; }

unsigned int rawRingElementHash(const RingElement *a) { return a->hash(); }
const Ring *IM2_RingElement_ring(const RingElement *a) { return a->get_ring(); }
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

const RingElement *IM2_RingElement_from_rational(const Ring *R, mpq_srcptr r)
{
  ring_elem result;
  bool ok = R->from_rational(r, result);
  if (not ok)
    {
      ERROR("unable to coerce rational into ring");
      return nullptr;
    }
  return RingElement::make_raw(R, result);
}

const RingElement *IM2_RingElement_from_BigComplex(const Ring *R, gmp_CC z)
{
  ring_elem f;
  if (R->from_BigComplex(z, f)) return RingElement::make_raw(R, f);
  ERROR("cannot create element of this ring from an element of CC");
  return 0;
}

const RingElement *IM2_RingElement_from_BigReal(const Ring *R, gmp_RR z)
{
  ring_elem f;
  if (R->from_BigReal(z, f)) return RingElement::make_raw(R, f);
  ERROR("cannot create element of this ring from an element of RR");
  return 0;
}

const RingElement *IM2_RingElement_from_Interval(const Ring *R, gmp_RRi z)
{
   ring_elem f;
   if (R->from_Interval(z, f)) return RingElement::make_raw(R, f);
   ERROR("cannot create element of this ring from an element of RRi");
   return 0;
}

gmp_ZZorNull IM2_RingElement_to_Integer(const RingElement *a)
/* If the ring of a is ZZ, or ZZ/p, this returns the underlying representation.
   Otherwise, NULL is returned, and an error is given */
{
  const Ring *R = a->get_ring();
  if (R->is_ZZ())
    {
      // Note: RingElement ZZ elements are on the "gc" side of the memory barrier, and
      // are read only.  Therefore we can just return the same value.
      return a->get_value().get_mpz();
    }
  if (R->isFinitePrimeField())
    {
      mpz_ptr result = newitem(__mpz_struct);

      std::pair<bool, long> res = R->coerceToLongInteger(a->get_value());
      assert(res.first);

      mpz_init_set_si(result, static_cast<int>(res.second));
      mpz_reallocate_limbs(result);
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
      return nullptr;
    }
  return a->get_value().get_mpq();
}

gmp_RRorNull IM2_RingElement_to_BigReal(const RingElement *a)
{
  const Ring *R = a->get_ring();
  gmp_RRmutable result;
  const M2::ConcreteRing<M2::ARingRRR> *R1;

  switch (R->ringID())
    {
      case M2::ring_RR:
        result = getmemstructtype(gmp_RRmutable);
        mpfr_init2(result, 53);
        mpfr_set_d(result, a->get_value().get_double(), GMP_RNDN);
        return moveTo_gmpRR(result);
      case M2::ring_RRR:
        R1 =
            dynamic_cast<const M2::ConcreteRing<M2::ARingRRR> *>(a->get_ring());
        result = getmemstructtype(gmp_RRmutable);
        mpfr_init2(result, R1->get_precision());
        mpfr_set(result, a->get_value().get_mpfr(), GMP_RNDN);
        return moveTo_gmpRR(result);
      default:
        ERROR("expected an element of RRR");
        return nullptr;
    }
}

gmp_RRiorNull IM2_RingElement_to_Interval(const RingElement *a)
{
    const Ring *R = a->get_ring();
    gmp_RRimutable result;
    const M2::ConcreteRing<M2::ARingRRi> *R1;
          
    switch (R->ringID())
    {
       case M2::ring_RR:
          result = getmemstructtype(gmp_RRimutable);
          mpfi_init2(result, 53);
          mpfi_set_d(result, a->get_value().get_double());
          return moveTo_gmpRRi(result);
       case M2::ring_RRi:
          R1 =
          dynamic_cast<const M2::ConcreteRing<M2::ARingRRi> *>(a->get_ring());
          result = getmemstructtype(gmp_RRimutable);
          mpfi_init2(result, R1->get_precision());
          mpfi_set(result, a->get_value().get_mpfi());
          return moveTo_gmpRRi(result);
       default:
          ERROR("expected an element of RRi");
          return nullptr;
    }
}

gmp_CCorNull IM2_RingElement_to_BigComplex(const RingElement *a)
{
  const Ring *R = a->get_ring();
  auto RCCC = dynamic_cast<const M2::ConcreteRing<M2::ARingCCC> *>(R);
  if (RCCC != 0)
    {
      M2::ARingCCC::ElementType b;
      RCCC->ring().init(b);
      RCCC->ring().from_ring_elem(b, a->get_value());
      gmp_CC result = RCCC->ring().toBigComplex(b);
      RCCC->ring().clear(b);
      return result;
    }
  auto RCC = dynamic_cast<const M2::ConcreteRing<M2::ARingCC> *>(R);
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
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return -1;
  }
}

const RingElement *rawMultiplicativeGenerator(const Ring *R)
{
  return R->getGenerator();
}

const RingElement /* or null */ *IM2_RingElement_make_var(const Ring *R, int v)
{
  try
    {
      ring_elem a = R->var(v);
      if (error()) return 0;
      return RingElement::make_raw(R, a);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

M2_bool IM2_RingElement_is_zero(const RingElement *a)
{
  return a->get_ring()->is_zero(a->get_value());
}

M2_bool IM2_RingElement_is_equal(const RingElement *a, const RingElement *b)
{
  const Ring *R = a->get_ring();
  if (R != b->get_ring()) return 0;
  return R->is_equal(a->get_value(), b->get_value());
}

engine_RawRingElementPair IM2_RingElement_divmod(const RingElement *a,
                                                 const RingElement *b)
{
  try
    {
      const Ring *R = a->get_ring();
      if (R != b->get_ring())
        {
          ERROR(
              "ring remainder requires both elements to have the same base "
              "ring");
          return 0;
        }
      ring_elem fquot;
      ring_elem frem =
          R->remainderAndQuotient(a->get_value(), b->get_value(), fquot);
      if (error()) return NULL;

      engine_RawRingElementPair result = new engine_RawRingElementPair_struct;
      result->a = RingElement::make_raw(R, fquot);
      result->b = RingElement::make_raw(R, frem);
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

int rawRingElementCompare(const RingElement *a, const RingElement *b)
{
  try
    {
      const Ring *R = a->get_ring();
      if (R != b->get_ring()) return 0;
      //  if (a->is_zero())
      //    {
      //      if (b->is_zero()) return 0;
      //      return 1;
      //    }
      //  if (b->is_zero()) return -1;
      return R->compare_elems(a->get_value(), b->get_value());
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return -2;
  }
}

const RingElement *IM2_RingElement_promote(const Ring *S, const RingElement *f)
{
  try
    {
      const RingElement *result;

      if (f->promote(S, result)) return result;
      ERROR("cannot promote given ring element");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *IM2_RingElement_lift(int *success_return,
                                                      const Ring *S,
                                                      const RingElement *f)
{
  try
    {
      const RingElement *result;

      if (f->lift(S, result))
        {
          *success_return = 1;
          return result;
        }
      // ERROR("cannot lift given ring element");
      return 0;
  } catch (const exc::engine_error& e)
    {
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
  try
    {
      return a->multi_degree();
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement * /* or null */ rawRingElementAntipode(const RingElement *f)
{
  try
    {
      const Ring *R = f->get_ring();
      return RingElement::make_raw(R, R->antipode(f->get_value()));
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

gmp_ZZpairOrNull rawWeightRange(M2_arrayint wts, const RingElement *a)
/* The first component of the degree is used, unless the degree monoid is
   trivial,
   in which case the degree of each variable is taken to be 1.
   Returns lo,hi degree.  If the ring is not a graded ring or a polynomial ring
   then (0,0) is returned.
*/
{
  try
    {
      int lo, hi;
      a->degree_weights(wts, lo, hi);
      if (error()) return 0;
      gmp_ZZpair p = new gmp_ZZpair_struct;
      p->a = newitem(__mpz_struct);
      p->b = newitem(__mpz_struct);
      mpz_init_set_si(const_cast<mpz_ptr>(p->a), static_cast<long>(lo));
      mpz_init_set_si(const_cast<mpz_ptr>(p->b), static_cast<long>(hi));
      return p;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *IM2_RingElement_homogenize_to_degree(
    const RingElement *a,
    int v,
    int deg,
    M2_arrayint wts)
{
  try
    {
      return a->homogenize(v, deg, wts);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *
IM2_RingElement_homogenize(const RingElement *a, int v, M2_arrayint wts)
{
  try
    {
      return a->homogenize(v, wts);
  } catch (const exc::engine_error& e)
    {
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
    if (P != nullptr)
      {
        int nvars0 = P->n_vars();
        const PolynomialRing *K = a->get_ring()->cast_to_PolynomialRing();
        if (K != nullptr && K != P->getCoefficients()) nvars0 -= K->n_vars();
        int *exp = newarray_atomic(int,nvars0);
        varpower::to_ntuple(nvars0, m->ints(), exp);
        ring_elem val = P->make_logical_term(a->get_ring(), a->get_value(), exp);
        return RingElement::make_raw(R,val);
      }
    auto Q = dynamic_cast<const M2FreeAlgebraOrQuotient *>(R);
    if (Q != nullptr)
      {
        if (Q->coefficientRing() != a->get_ring())
          {
            ERROR("wrong coefficient ring");
            return nullptr;
          }
        return RingElement::make_raw(Q,
                                     Q->makeTerm(a->get_value(),
                                                 m->ints()));
      }
    ERROR("requires a polynomial ring");
    return nullptr;
  }
  catch (exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

const RingElement /* or null */ *IM2_RingElement_get_terms(
    int nvars, /* n variables in an outermost monoid */
    const RingElement *a,
    int lo,
    int hi)
/* Returns the sum of some monomials of 'a', starting at 'lo',
   going up to 'hi'.  If either of these are negative, they are indices
   from the back of the polynomial.
   'a' should be an element of a polynomial ring.
*/
{
  return a->get_terms(nvars, lo, hi);
}

const RingElement /* or null */ *IM2_RingElement_get_coeff(
    const Ring *coeffRing, /* ring of the result */
    const RingElement *a,
    const Monomial *m)
/* Return (as an element of the coefficient ring) the coeff
     of the monomial 'm'.
  */
{
  try
    {
      return a->get_coeff(coeffRing, m);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *IM2_RingElement_lead_coeff(
    const Ring *coeffRing, /* ring of the result */
    const RingElement *a)
{
  try
    {
      return a->lead_coeff(coeffRing);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Monomial /* or null */ *IM2_RingElement_lead_monomial(
    int nvars, /* number of variables in an outermost monoid */
    const RingElement *a)
{
  try
    {
      return a->lead_monom(nvars);
  } catch (const exc::engine_error& e)
    {
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
    const Ring *coeffRing, /* ring of the result coefficients */
    const RingElement *f)
{
  try {
    const PolynomialRing* P = f->get_ring()->cast_to_PolynomialRing();
    if (P != nullptr)
      {
        return P->list_form(coeffRing, f->get_value());
      }
    const SchurRing2* S = f->get_ring()->cast_to_SchurRing2();
    if (S != nullptr)
      {
        return S->list_form(coeffRing, f->get_value());
      }
    /* added by Frank */
    const M2FreeAlgebra* ncP = dynamic_cast<const M2FreeAlgebra*>(f->get_ring());
    if (ncP != nullptr)
      {
        return ncP->list_form(coeffRing, f->get_value());
      }
    /* added by Frank */
    const M2FreeAlgebraQuotient* ncQ = dynamic_cast<const M2FreeAlgebraQuotient*>(f->get_ring());
    if (ncQ != nullptr)
      {
        return ncQ->m2FreeAlgebra().list_form(coeffRing, f->get_value());
      }
    ERROR("expected a polynomial");
    return nullptr;
  }
  catch (exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

engine_RawRingElementArray rawGetParts(const M2_arrayint wts,
                                       const RingElement *f)
/* Return an array of RingElement's, each having pure weight, and sorted by
   strictly increasing weight value.  The wt vector values must fit into
   a word length integer.  */
{
  try
    {
      const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected a polynomial");
          return 0;
        }
      long relems_len;
      ring_elem *relems = P->get_parts(wts, f->get_value(), relems_len);
      engine_RawRingElementArray result =
          getmemarraytype(engine_RawRingElementArray, relems_len);
      result->len = static_cast<int>(relems_len);
      for (int i = 0; i < result->len; i++)
        result->array[i] = RingElement::make_raw(P, relems[i]);
      freemem(relems);
      return result;
  } catch (const exc::engine_error& e)
    {
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
              const VECTOR(ring_elem) & input_relems,
              VECTOR(ring_elem) & output_relems,
              int convolve_type)
{
  const Ring *K = R->getCoefficientRing();
  ring_elem invn;
  size_t n =
      input_relems.size() - 1;  // array is 0..n, with the 0 part being ignored.
  // first set [1, e1] := [1, h1].
  output_relems[0] = input_relems[0];
  for (int i = 1; i <= n; i++)
    {
      // ASSUMPTION: input_relems[i] is either a variable or - of a variable
      ring_elem result = R->copy(input_relems[i]);
      if (convolve_type == 2) R->mult_coeff_to(K->from_long(-i), result);
      for (int j = i - 1; j >= 1; --j)
        {
          ring_elem hr;
          Nterm *g = input_relems[j];
          if (g != 0)
            {
              hr.poly_val =
                  R->mult_by_term(output_relems[i - j], g->coeff, g->monom);
              R->internal_add_to(result, hr);
            }
        }
      if (convolve_type == 1)
        {
          invn = K->invert(K->from_long(i));
          R->mult_coeff_to(invn, result);
        }
      output_relems[i] = result;
    }
}

engine_RawRingElementArrayOrNull rawConvolve(engine_RawRingElementArray H,
                                             int convolve_type)
{
  // convolve_type == 1: E_n = E_(n-1) * H_1 - E(n-2) * H_2 + ... + (-1)^n * H_n
  // convolve_type == 2: n * E_n = E_(n-1) * H_1 - E(n-2) * H_2 + ... + (-1)^n *
  // H_n
  // others of interest?  These are good for translating between symmetric
  // functions of type E,H,P

  // H_0 is ignored... (OR: should we instead shift everything: first entry of H
  // is denoted H_1...?)

  try
    {
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
      for (int i = 0; i < len; i++) input_relems[i] = H->array[i]->get_value();
      convolve(P, input_relems, output_relems, convolve_type);

      engine_RawRingElementArray result =
          getmemarraytype(engine_RawRingElementArray, len);
      result->len = len;
      for (int i = 0; i < result->len; i++)
        result->array[i] = RingElement::make_raw(P, output_relems[i]);

      return result;
  } catch (const exc::engine_error& e)
    {
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
/* Return the sum of all of the terms t of f, which satisfy: lobound <= wt.t <=
   hibound,
   where, if lobound_given is false, then lobound is -infinity, and if
   hibound_given
   is false, then hibound is infinity. */
{
  try
    {
      const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected a polynomial");
          return 0;
        }
      ring_elem g = P->get_part(
          wts, f->get_value(), lobound_given, hibound_given, lobound, hibound);
      return RingElement::make_raw(P, g);
  } catch (const exc::engine_error& e)
    {
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

const RingElement /* or null */ *rawAssociateDivisor(const RingElement *f)
{
  try
    {
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
      return RingElement::make_raw(
          P->getCoefficients(), P->preferred_associate_divisor(f->get_value()));
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *rawRingElementContent(const RingElement *f)
// returns the content of f (as a matrix over the base coefficient ring)
{
  return f->content();
}

const RingElement /* or null */ *rawRingElementRemoveContent(
    const RingElement *f)
// returns the polynomial which results after division by the content
{
  return f->remove_content();
}

const RingElement /* or null */ *rawRingElementSplitContent(
    const RingElement *f,
    const RingElement /* or null */ **result)
// returns the content of f (as a matrix over the base coefficient ring)
// result is set to the polynomial which results after division by the content
{
  return f->split_off_content(*result);
}

const RingElement /* or null */ *IM2_RingElement_numerator(const RingElement *a)
{
  try
    {
      return a->numerator();
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *IM2_RingElement_denominator(
    const RingElement *a)
{
  try
    {
      return a->denominator();
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *IM2_RingElement_fraction(const Ring *R,
                                                          const RingElement *a,
                                                          const RingElement *b)
{
  try
    {
      const RingElement *f = a->fraction(R, b);
      if (error()) return NULL;
      return f;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

gmp_ZZorNull rawSchurDimension(const RingElement *f)
{
  try
    {
      const SchurRing *S = f->get_ring()->cast_to_SchurRing();
      if (S == 0)
        {
          ERROR("expected a polynomial over a Schur ring");
          return 0;
        }
      ring_elem result = S->dimension(f->get_value());
      return result.get_mpz();
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *rawSchurSnTensorMult(const RingElement *a,
                                                      const RingElement *b)
/* the tensor multiplication function in SchurSnRing */
{
  try
    {
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
      return RingElement::make_raw(R, result);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const RingElement /* or null */ *rawSchurFromPartition(const Ring *R,
                                                       M2_arrayint part)
{
  // R should be a SchurRing2
  // part should be a partition: a weakly descending list of integers (for now,
  // non-negative)
  // if R has a limit on the size of partitions, then
  try
    {
      const SchurRing2 *S = R->cast_to_SchurRing2();
      if (S == 0)
        {
          ERROR("expected a Schur ring");
          return 0;
        }
      // Check that part is a partition, and that the number of parts is <=
      // number allowed
      if (!S->is_valid_partition(part)) return 0;
      ring_elem result = S->from_partition(part);
      return RingElement::make_raw(S, result);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}
/* Special routines for tower rings */

int rawDegree(int v, const RingElement *f)
/* Returns -1 if 0 or not implemented for a given ring.  For now, valid only for
 * tower rings */
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
  if (firstvar < 0)
    {
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
  return RingElement::make_raw(R, R->differentiate(v, f->get_value()));
}

const RingElement /* or null */ *rawLowerP(const RingElement *f)
{
  const Tower *R = f->get_ring()->cast_to_Tower();
  if (R == 0)
    {
      ERROR("not implemented for this ring");
      return 0;
    }
  return RingElement::make_raw(R, R->lowerP(f->get_value()));
}

const RingElement /* or null */ *rawPowerMod(const RingElement *f,
                                             mpz_srcptr n,
                                             const RingElement *g)
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
  return RingElement::make_raw(R,
                               R->power_mod(f->get_value(), n, g->get_value()));
}

// Local Variables:
// indent-tabs-mode: nil
// End:
