// Copyright 2002 Michael E. Stillman

#include "engine.h"

#include "monoid.hpp"
#include "monomial.hpp"
#include "relem.hpp"
#include "z_mod_p.hpp"
#include "ZZ.hpp"
#include "QQ.hpp"
#include "RR.hpp"
#include "CC.hpp"
#include "RRR.hpp"
#include "CCC.hpp"
#include "GF.hpp"
#include "polyring.hpp"
#include "schur.hpp"
#include "frac.hpp"
#include "weylalg.hpp"
#include "skewpoly.hpp"
#include "solvable.hpp"
#include "polyquotient.hpp"

#include "matrix.hpp"
#include "../d/M2mem.h"
#include "random.hpp"
#include "exceptions.hpp"

int32_t rawSetRandomSeed(M2_Integer seed)
{
  return Random::set_seed(seed);
}

void rawSetRandomMax(M2_Integer maxN)
{
  Random::set_max_int(maxN);
}

M2_Integer rawRandomInteger(M2_Integer maxN)
{
  return Random::get_random_integer_0(maxN);
}

unsigned long IM2_Ring_hash(const Ring *R)
{
  return R->get_hash_value();
}

M2_string IM2_Ring_to_string(const Ring *R)
{
  buffer o;
  R->text_out(o);
  return o.to_string();
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

const RingOrNull *IM2_Ring_ZZp(int p)
  /* p must be a prime number <= 32767 */
{
  if (p <= 1 || p >= 32750)
    {
      ERROR("ZZP: expected a prime number p in range 2 <= p <= 32749");
      return 0;
    }
  return Z_mod::create(p);
}

const RingOrNull *rawGaloisField(const RingElement *f)
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

const RingOrNull *IM2_Ring_RR(double precision)
{
  return globalRR;
  //  return RR::create(precision);
}

const RingOrNull *IM2_Ring_CC(double precision)
{
  return globalCC;
  //  return CC::create(precision);
}

const RingOrNull *IM2_Ring_RRR()
{
  return RRR::create();
}

const RingOrNull *IM2_Ring_CCC()
{
  return CCC::create();
}

const Ring *IM2_Ring_trivial_polyring()
{
  return PolyRing::get_trivial_poly_ring();
}

const RingOrNull *IM2_Ring_polyring(const Ring *K, const Monoid *M)
{
     try {
#if 0
//   if (K == globalQQ)
//     {
//       const PolyRing *P = PolyRing::create(globalZZ,M);
//       return PolyQQ::create(P);
//     }
#endif
	  return PolyRing::create(K,M);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingOrNull *IM2_Ring_skew_polyring(const Ring *R,
					 M2_arrayint skewvars)
{
     try {
#if 0
//   const PolyQQ *RQ = R->cast_to_PolyQQ();
//   if (RQ != 0)
//     {
//       const PolyRing *P = SkewPolynomialRing::create(globalZZ,
// 						     RQ->getMonoid(),
// 						     skewvars);
//       return PolyQQ::create(P);
//     }
#endif
	  const PolynomialRing *P = R->cast_to_PolynomialRing();
	  if (P == 0) 
	    {
	      ERROR("expected a polynomial ring");
	      return 0;
	    }
	  return SkewPolynomialRing::create(P->getCoefficients(),
					    P->getMonoid(),
					    skewvars);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingOrNull *IM2_Ring_weyl_algebra(const Ring *R,
					M2_arrayint comm_vars,
					M2_arrayint diff_vars,
					int homog_var)
{
     try {
#if 0
//   const PolyQQ *RQ = R->cast_to_PolyQQ();
//   if (RQ != 0)
//     {
//       const WeylAlgebra *P = WeylAlgebra::create(globalZZ,
// 						 RQ->getMonoid(),
// 						 diff_vars, 
// 						 comm_vars, 
// 						 homog_var);
//       return PolyQQ::create(P);
//     }
#endif
	  const PolynomialRing *P = R->cast_to_PolynomialRing();
	  if (P == 0) 
	    {
	      ERROR("expected a polynomial ring");
	      return 0;
	    }
	  return WeylAlgebra::create(P->getCoefficients(),
				     P->getMonoid(),
				     diff_vars, 
				     comm_vars, 
				     homog_var);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingOrNull *IM2_Ring_solvable_algebra(const Ring *R,
					    const Matrix *Q)
{
     try {
	  const PolynomialRing *P = R->cast_to_PolynomialRing();
	  if (P == 0) 
	    {
	      ERROR("expected a polynomial ring");
	      return 0;
	    }
	  return SolvableAlgebra::create(P, Q);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingOrNull *IM2_Ring_frac(const Ring *R)
{
     try {
	  if (R == globalZZ) return globalQQ;
	  const PolyRingFlat *P = R->cast_to_PolyRingFlat();
	  if (P == 0)
	    {
	      ERROR("expected polynomial ring");
	      return 0;
	    }
	  return FractionField::create(P);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingOrNull *IM2_Ring_localization(const Ring *R, const Matrix *Prime)
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

const RingOrNull * IM2_Ring_quotient(const Ring *R, 
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
	  return PolynomialRing::create_quotient(P,I);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingOrNull * IM2_Ring_quotient1(const Ring *R, 
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
	  return PolyRingQuotient::create_quotient(R1,B1);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingOrNull *IM2_Ring_schur(const Ring *R)
{
     try {
	  const PolynomialRing *P = R->cast_to_PolynomialRing();
	  if (P == 0) 
	    {
	      ERROR("Schur ring construction: expected a polynomial ring");
	      return 0;
	    }
	  return SchurRing::create(P);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

M2_bool IM2_Ring_is_field(const Ring *K)
  /* Returns true if K is a field, or has been declared to be one.
     In the latter case, if an operation shows that K cannot be a field,
     then this function will thereafter return false, and 
     IM2_Ring_get_zero_divisor(K) can be used to obtain a non-unit, if one
     has been found. */
{
  return K->is_field();
}

void IM2_Ring_declare_field(const Ring *K)
  /* Declare that K is a field.  The ring K can then be used as the coefficient
     ring for computing Groebner bases,etc.  */
{
  const_cast<Ring *>(K)->declare_field(); // Note: this modifies a const value...
}

const RingElement * IM2_Ring_get_zero_divisor(const Ring *K)
{
  return RingElement::make_raw(K, K->get_zero_divisor());
}

const RingOrNull *rawAmbientRing(const Ring *R)
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

const RingOrNull *rawDenominatorRing(const Ring *R)
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

#if 0
// const RingElement *IM2_RingElement_from_int(const Ring *R, int d)
// {
//   return new RingElement(R, R->from_int(d));
// }
#endif
const RingElement *IM2_RingElement_from_Integer(const Ring *R, M2_Integer d)
{
  return RingElement::make_raw(R, R->from_int(d));
}

const RingElement *IM2_RingElement_from_double(const Ring *R, double d)
{
  return RingElement::make_raw(R, R->from_double(d));
}

const RingElement *IM2_RingElement_from_rational(const Ring *R, M2_Rational r)
{
  return RingElement::make_raw(R, R->from_rational(r));
}

const RingElement *IM2_RingElement_from_complex(const Ring *R, M2_CC z)
{
  return RingElement::make_raw(R, R->from_complex(z));
}

const RingElement *IM2_RingElement_from_BigReal(const Ring *R, M2_RRR z)
{
  return RingElement::make_raw(R, R->from_BigReal(z));
}

M2_IntegerOrNull IM2_RingElement_to_Integer(const RingElement *a)
  /* If the ring of a is ZZ, or ZZ/p, this returns the underlying representation.
     Otherwise, NULL is returned, and an error is given */
{
  const Ring *R = a->get_ring();
  if (R->is_ZZ())
    {
      void *f = a->get_value().poly_val;
      return static_cast<M2_Integer>(f);
    }
  if (R->cast_to_Z_mod() != 0)
    {
      M2_Integer result = newitem(__mpz_struct);
      mpz_init_set_si(result, R->coerce_to_int(a->get_value()));
      return result;
    }
  ERROR("Expected ZZ or ZZ/p as base ring");
  return 0;
}

M2_RationalOrNull IM2_RingElement_to_rational(const RingElement *a)
{
  if (!a->get_ring()->is_QQ())
    {
      ERROR("expected an element of QQ");
      return 0;
    }
  void *f = a->get_value().poly_val;
  return static_cast<M2_Rational>(f);
}

M2_RRRorNull IM2_RingElement_to_BigReal(const RingElement *a)
{
  if (!a->get_ring()->is_RRR())
    {
      ERROR("expected an element of RRR");
      return 0;
    }
  void *f = a->get_value().poly_val;
  return static_cast<M2_RRR>(f);
}

double IM2_RingElement_to_double(const RingElement *a)
/* If the ring of a is RR, this returns the underlying representation of 'a'.
   Otherwise 0.0 is returned. */
{
  if (!a->get_ring()->cast_to_RingRR())
    {
      ERROR("expected an element of RR");
      return 0;
    }
  return globalRR->to_double(a->get_value());
    
}

M2_CCOrNull IM2_RingElement_to_complex(const RingElement *a)
/* If the ring of a is RR, this returns the underlying representation of 'a'.
   Otherwise 0.0 is returned. */
{
  if (!a->get_ring()->cast_to_CC())
    {
      ERROR("expected an element of CC");
      return 0;
    }
  void *f = a->get_value().poly_val;
  return static_cast<M2_CC>(f);
}

const RingElementOrNull *rawRRRFromString(M2_string s)
{
  ring_elem f;
  if (globalRRR->from_string(s,f))
    return RingElement::make_raw(globalRRR, f);
  return 0;
}

const RingElementOrNull *IM2_RingElement_make_var(const Ring *R, int v)
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

const RingElement *IM2_RingElement_negate(const RingElement *a)
{
     try {
	  return -(*a);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull *IM2_RingElement_add(const RingElement *a, 
					     const RingElement *b)
{
     try {
	  return (*a) + (*b);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull *IM2_RingElement_subtract(const RingElement *a, 
						  const RingElement *b)
{
     try {
	  return (*a) - (*b);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull *IM2_RingElement_mult(const RingElement *a, 
					      const RingElement *b)
{
     try {
	  return (*a) * (*b);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

#if 0
// const RingElement *IM2_RingElement_mult_int(const RingElement *a, int b)
// {
//   return (*a) * b;
// }
#endif
const RingElementOrNull *IM2_RingElement_div(const RingElement *a, 
					     const RingElement *b)
{
     try {
	  const Ring *R = a->get_ring();
	  if (R != b->get_ring())
	    {
	      ERROR("ring division requires both elements to have the same base ring");
	      return 0;
	    }
	  ring_elem result = R->quotient(a->get_value(), b->get_value());
	  if (error()) return 0;
	  return RingElement::make_raw(R, result);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull *IM2_RingElement_mod(const RingElement *a, 
					     const RingElement *b)
{
     try {
	  const Ring *R = a->get_ring();
	  if (R != b->get_ring())
	    {
	      ERROR("ring remainder requires both elements to have the same base ring");
	      return 0;
	    }
	  ring_elem result = R->remainder(a->get_value(), b->get_value());
	  if (error()) return 0;
	  return RingElement::make_raw(R, result);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElement_pair *IM2_RingElement_divmod(const RingElement *a, 
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

	  RingElement_pair *result = new RingElement_pair;
	  result->a = RingElement::make_raw(R, fquot);
	  result->b = RingElement::make_raw(R, frem);
	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull *IM2_RingElement_power(const RingElement *a, 
					       M2_Integer n)
{
     try {
	  return a->power(n);
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

const RingElement *IM2_RingElement_lift(const Ring *S, 
					const RingElement *f)
{
     try {
	  const RingElement *result;
	  if (f->lift(S,result))
	    return result;
	  ERROR("cannot lift given ring element");
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

M2_Integer_pair_OrNull *rawWeightRange(M2_arrayint wts, 
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
	  M2_Integer_pair *p = new M2_Integer_pair;
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

const RingElementOrNull *IM2_RingElement_homogenize_to_degree(const RingElement *a,
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

const RingElementOrNull *IM2_RingElement_homogenize(const RingElement *a,
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
						
const RingElementOrNull *IM2_RingElement_term(const Ring *R,
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
	  if (K != 0)
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

const RingElement *IM2_RingElement_get_terms(
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

const RingElementOrNull *IM2_RingElement_get_coeff(
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

const RingElementOrNull *IM2_RingElement_lead_coeff(
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

const MonomialOrNull *IM2_RingElement_lead_monomial(
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
     try {
	  return a->n_terms(nvars);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return 0;
     }
}

ArrayPairOrNull IM2_RingElement_list_form(
          const Ring * coeffRing, /* ring of the result coefficients */
          const RingElement *f)
{
     try {
	  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
	  if (P == 0)
	    {
	      ERROR("expected a polynomial");
	      return 0;
	    }
	  return P->list_form(coeffRing, f->get_value());
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

M2_arrayint IM2_RingElement_indices(const RingElement *f)
  /* The list of indices of variables which occur in f is returned. */
{
  const Ring *R = f->get_ring();
  return R->support(f->get_value());
}

const RingElementOrNull * rawAssociateDivisor(const RingElement *f)
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

const RingElementOrNull *IM2_RingElement_numerator(const RingElement *a)
{
     try {
	  return a->numerator();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull *IM2_RingElement_denominator(const RingElement *a)
{
     try {
	  return a->denominator();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const RingElementOrNull *IM2_RingElement_fraction(const Ring *R,
						  const RingElement *a,
						  const RingElement *b)
{
     try {
	  return a->fraction(R,b);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

M2_IntegerOrNull rawSchurDimension(const RingElement *f)
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
