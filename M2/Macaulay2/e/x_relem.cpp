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


int32 rawSetRandomSeed(M2_Integer seed)
{
  return Random::set_seed(seed);
}

void rawSetRandomMax(M2_Integer maxN)
{
  Random::set_max_int(maxN);
}

M2_Integer rawRandomInteger(M2_Integer maxN)
{
  return Random::get_random_integer(maxN);
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
  return GF::create(f);
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
  return PolyRing::create(K,M);
}

const RingOrNull *IM2_Ring_skew_polyring(const Ring *R,
					 M2_arrayint skewvars)
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  return SkewPolynomialRing::create(P,skewvars); // returns 0 if P is not commutative
}

const RingOrNull *IM2_Ring_weyl_algebra(const Ring *R,
					M2_arrayint comm_vars,
					M2_arrayint diff_vars,
					int homog_var)
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  // returns 0 if P is not commutative:
  return WeylAlgebra::create(P,diff_vars, comm_vars, homog_var);
}

const RingOrNull *IM2_Ring_solvable_algebra(const Ring *R,
					    const Matrix *Q)
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  // returns 0 if P is not commutative:
  return SolvableAlgebra::create(P,Q);
}

const RingOrNull *IM2_Ring_frac(const Ring *R)
{
  if (R == globalZZ) return globalQQ;
  return FractionField::create(R);
}

const RingOrNull *IM2_Ring_localization(const Ring *R, const Matrix *Prime)
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
#if 0
  return P->create_FractionRing(Prime);
#endif
  /* TODO */
#warning "implement IM2_Ring_localization"
  return 0;
}

const RingOrNull * IM2_Ring_quotient(const Ring *R, 
				     const Matrix *I)
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  if (I->get_ring() != P)
    {
      ERROR("expected matrix to be over the same ring");
    }
  if (I->n_rows() != 1)
    {
      ERROR("expected a one row matrix of quotient elements");
      return 0;
    }
  return PolyRingQuotient::create(P,I);
}

const RingOrNull * IM2_Ring_quotient1(const Ring *R, 
				      const Ring *B)
/* if R is a polynomial ring of the form A[x]/J, and B = A/I (where A is a poly ring)
   then form the quotient ring B[x]/J. */
{
  const PolyRing *R1 = R->cast_to_PolyRing();
  const PolynomialRing *B1 = B->cast_to_PolynomialRing();
  if (R1 == 0 || B1 == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  return PolyRingQuotient::create(R1,B1);
}

const RingOrNull *IM2_Ring_schur(const Ring *R)
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("Schur ring construction: expected a polynomial ring");
      return 0;
    }
  return SchurRing::create(P);
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

const M2_string IM2_RingElement_to_string(const RingElement *f)
{
  buffer o;
  f->text_out(o);
  return o.to_string();
}

#if 0
const RingElement *IM2_RingElement_from_int(const Ring *R, int d)
{
  return new RingElement(R, R->from_int(d));
}
#endif
const RingElement *IM2_RingElement_from_Integer(const Ring *R, const M2_Integer d)
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

const RingElement *IM2_RingElement_from_BigReal(const Ring *R, const M2_BigReal z)
{
  return RingElement::make_raw(R, R->from_BigReal(z));
}

const M2_IntegerOrNull IM2_RingElement_to_Integer(const RingElement *a)
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

const M2_RationalOrNull IM2_RingElement_to_rational(const RingElement *a)
{
  if (!a->get_ring()->is_QQ())
    {
      ERROR("expected an element of QQ");
      return 0;
    }
  void *f = a->get_value().poly_val;
  return static_cast<M2_Rational>(f);
}

double IM2_RingElement_to_double(const RingElement *a)
/* If the ring of a is RR, this returns the underlying representation of 'a'.
   Otherwise 0.0 is returned. */
{
  if (!a->get_ring()->cast_to_RR())
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

const RingElementOrNull *rawRRRFromString(const M2_string s)
{
  ring_elem f;
  if (globalRRR->from_string(s,f))
    return RingElement::make_raw(globalRRR, f);
  return 0;
}


const RingElementOrNull *IM2_RingElement_make_var(const Ring *R, int v, int e)
{
  ring_elem a = R->var(v,e);
  if (error()) return 0;
  return RingElement::make_raw(R, a);
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
  return -(*a);
}

const RingElementOrNull *IM2_RingElement_add(const RingElement *a, 
					     const RingElement *b)
{
  return (*a) + (*b);
}

const RingElementOrNull *IM2_RingElement_subtract(const RingElement *a, 
						  const RingElement *b)
{
  return (*a) - (*b);
}

const RingElementOrNull *IM2_RingElement_mult(const RingElement *a, 
					      const RingElement *b)
{
  return (*a) * (*b);
}

#if 0
const RingElement *IM2_RingElement_mult_int(const RingElement *a, int b)
{
  return (*a) * b;
}
#endif
const RingElementOrNull *IM2_RingElement_div(const RingElement *a, 
					     const RingElement *b)
{
  // What is this exactly??
  return (*a) / (*b);
}

const RingElementOrNull *IM2_RingElement_mod(const RingElement *a, 
					     const RingElement *b)
{
  // What is this exactly??
  return (*a) % (*b);
}

const RingElement_pair *IM2_RingElement_divmod(const RingElement *a, 
					       const RingElement *b)
{
  RingElement *quot, *rem;
  quot = a->divide(*b, rem);
  if (error()) return 0;
  RingElement_pair *result = new RingElement_pair;
  result->a = quot;
  result->b = rem;
  return result;
}

const RingElementOrNull *IM2_RingElement_power(const RingElement *a, 
					       const M2_Integer n)
{
  return a->power(n);
}

int rawRingElementCompare(const RingElement *a,
			  const RingElement *b)
{
  const Ring *R = a->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (R != b->get_ring() || P == 0)
    return 0;
  
  const int *m = P->lead_logical_monomial(a->get_value());
  const int *n = P->lead_logical_monomial(b->get_value());
  return P->getLogicalMonoid()->compare(m,n);
}

const RingElement *IM2_RingElement_promote(const Ring *S, 
					   const RingElement *f)
{
  const RingElement *result;
  if (f->promote(S,result))
    return result;
  ERROR("cannot promote given ring element");
  return 0;
}

const RingElement *IM2_RingElement_lift(const Ring *S, 
					const RingElement *f)
{
  const RingElement *result;
  if (f->lift(S,result))
    return result;
  ERROR("cannot lift given ring element");
  return 0;
}

M2_bool IM2_RingElement_is_graded(const RingElement *a)
{
  return a->is_homogeneous();
}

M2_arrayint IM2_RingElement_multidegree(const RingElement *a)
{
  return a->multi_degree();
}

M2_Integer_pair_OrNull *IM2_RingElement_degree(const RingElement *a, 
					       const M2_arrayint wts)
  /* The first component of the degree is used, unless the degree monoid is trivial,
     in which case the degree of each variable is taken to be 1. 
     Returns lo,hi degree.  If the ring is not a graded ring or a polynomial ring
     then (0,0) is returned.
  */
{
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

const RingElementOrNull *IM2_RingElement_homogenize_to_degree(const RingElement *a,
							      int v,
							      int deg,
							      const M2_arrayint wts)
{
  return a->homogenize(v,deg,wts);
}

const RingElementOrNull *IM2_RingElement_homogenize(const RingElement *a,
						    int v,
						    const M2_arrayint wts)
{
  return a->homogenize(v,wts);
}
						
const RingElementOrNull *IM2_RingElement_term(const Ring *R,
					      const RingElement *a,
					      const Monomial *m)
  /* R must be a polynomial ring, and 'a' an element of the
     coefficient ring of R.  Returns a*m, if this is a valid
     element of R.  Returns NULL if not (with an error message). */
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("requires a polynomial ring");
      return 0;
    }
  if (P->getLogicalCoefficients() != a->get_ring())
    {
      ERROR("term: expected same ring");
      return 0;
    }

  int *mon = P->getLogicalMonoid()->make_one();
  P->getLogicalMonoid()->from_varpower(m->ints(), mon);
  ring_elem val = P->make_logical_term(a->get_value(), mon);
  
  return RingElement::make_raw(R,val);
}

const RingElement *IM2_RingElement_get_terms(const RingElement *a,
					     int lo, int hi)
{
  return a->get_terms(lo,hi);
}

const RingElementOrNull *IM2_RingElement_get_coeff(const RingElement *a,
						   const Monomial *m)
  /* Return (as an element of the coefficient ring) the coeff
     of the monomial 'm'. */
{
  return a->get_coeff(m);
}

const RingElement *IM2_RingElement_lead_coeff(const RingElement *a)
{
  return a->lead_coeff();
}

const MonomialOrNull *IM2_RingElement_lead_monomial(const RingElement *a)
{
  return a->lead_monom();
}

int IM2_RingElement_n_terms(const RingElement *a)
{
  return a->n_terms();
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

ArrayPairOrNull IM2_RingElement_list_form(const RingElement *f)
{
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a polynomial");
      return 0;
    }
  return P->list_form(f->get_value());
}

#if 0
  int n = f->n_terms();
  Monomial_array *monoms = GETMEM(Monomial_array *, sizeofarray(monoms,n));
  RingElement_array *coeffs = GETMEM(RingElement_array *, sizeofarray(coeffs,n));
  monoms->len = n;
  coeffs->len = n;
  ArrayPairOrNull result = newitem(ArrayPair);
  result->monoms = monoms;
  result->coeffs = coeffs;

  const Ring *K = P->getLogicalCoefficients();
  intarray resultvp;
  Nterm *t = f->get_value();
  int next = 0;
  while (t != 0)
    {
      ring_elem c = P->get_logical_coeff(K, t); // increments t to the next term of f.
      P->getMonoid()->to_expvector(t->monom, exp);
      varpower::from_ntuple(nvars, exp, resultvp);
      monoms->array[next] = Monomial::make(resultvp.raw());
      assert( monoms->array[next] != NULL );
      coeffs->array[next] = RingElement::make_raw(K, c);
      assert( coeffs->array[next] != NULL );
      next++;
      resultvp.shrink(0);
    }
  return result;

  intarray resultvp;
  Nterm *t = f->get_value();
  int next = 0;
  for ( ; t != NULL; t = t->next)
    {
      P->Nmonoms()->to_varpower(t->monom, resultvp);
      monoms->array[next] = Monomial::make(resultvp.raw());
      assert( monoms->array[next] != NULL );
      coeffs->array[next] = RingElement::make_raw(P->Ncoeffs(), P->Ncoeffs()->copy(t->coeff));
      assert( coeffs->array[next] != NULL );
      next++;
      resultvp.shrink(0);
    }
  return result;
#endif

const RingElementOrNull *IM2_RingElement_numerator(const RingElement *a)
{
  return a->numerator();
}

const RingElementOrNull *IM2_RingElement_denominator(const RingElement *a)
{
  return a->denominator();
}

const RingElementOrNull *IM2_RingElement_fraction(const Ring *R,
						  const RingElement *a,
						  const RingElement *b)
{
  return a->fraction(R,b);
}

const M2_IntegerOrNull rawSchurDimension(const RingElement *f)
{
  const SchurRing *S = f->get_ring()->cast_to_SchurRing();
  if (S == 0)
    {
      ERROR("expected a polynomial over a Schur ring");
      return 0;
    }
  ring_elem result = S->dimension(f->get_value());
  return MPZ_VAL(result);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
