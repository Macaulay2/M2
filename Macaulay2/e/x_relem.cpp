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
#include "bigRR.hpp"
#include "bigCC.hpp"
#include "GF.hpp"
#include "polyring.hpp"
#include "schur.hpp"
#include "frac.hpp"
#include "weylalg.hpp"
#include "skewpoly.hpp"
#include "solvable.hpp"

#include "matrix.hpp"
#include "../d/M2mem.h"

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

const RingOrNull *IM2_Ring_GF(const RingElement *f)
{
  // Check that the ring R of f is a polynomial ring in one var over a ZZ/p
  // Check that f has degree >= 2
  // Check that f is monic
  // If any of these fail, then return 0.
  return GF::create(f);
}

const RingOrNull *IM2_Ring_RR(double precision)
{
  return RR::create(precision);
}

const RingOrNull *IM2_Ring_CC(double precision)
{
  return CC::create(precision);
}

const RingOrNull *IM2_Ring_bigRR()
{
  return bigRR::create();
}

const RingOrNull *IM2_Ring_bigCC()
{
  return bigCC::create();
}

const RingOrNull *IM2_Ring_polyring(const Ring *K, const Monoid *M)
{
  return PolynomialRing::create(K,M);
}

const RingOrNull *IM2_Ring_skew_polyring(const Ring *R,
					 M2_arrayint skewvars)
{
  /* TODO */
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  return SkewPolynomialRing::create(P,skewvars);
}

const RingOrNull *IM2_Ring_weyl_algebra(const Ring *R,
					M2_arrayint comm_vars,
					M2_arrayint diff_vars,
					int homog_var)
{
  /* TODO */
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) 
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
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
  return SolvableAlgebra::create(P,Q);
}

const RingOrNull *IM2_Ring_frac(const Ring *R)
{
  if (R == globalZZ) return globalQQ;
  return FractionField::create(R);
}

const RingOrNull *IM2_Ring_localization(const Ring *R, const Matrix *P)
{
  /* TODO */
  return 0;
}

const RingOrNull * IM2_Ring_quotient(const Ring *R, 
				     const Matrix *I)
{
#warning "implement IM2_Ring_quotient"
#if 0
  // TODO
  return R->create_quotient(I);
#endif
  return 0;
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

#if 0
const RingElement *IM2_RingElement_from_double(const Ring *R, const M2_BigReal d)
{
  return new RingElement(R, R->from_BigReal(d));
}
#endif

const M2_IntegerOrNull IM2_RingElement_to_Integer(const RingElement *a)
  /* If the ring of a is ZZ, or ZZ/p, this returns the underlying representation.
     Otherwise, NULL is returned, and an error is given */
{
  const Ring *R = a->get_ring();
  if (R == globalZZ)
    {
      ring_elem f = a->get_value();
      return MPZ_VAL(f);
#if 0
      Nterm *t = f;
      return (M2_Integer)(t);
#endif
    }
  else if (R->cast_to_Z_mod() != 0)
    {
      // Translate the value to an integer
      M2_Integer result = newitem(__mpz_struct);
      mpz_init_set_si(result, R->coerce_to_int(a->get_value()));
      return result;
    }
  else
    {
      ERROR("Expected ZZ or ZZ/p as base ring");
      return 0;
    }

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
  if (P->Ncoeffs() != a->get_ring())
    {
      ERROR("term: expected same ring");
      return 0;
    }

  int *mon = P->Nmonoms()->make_one();
  P->Nmonoms()->from_varpower(m->ints(), mon);
  /* Caution: I am considering ringelem's which live in RingElement's to
     be immutable, and so to not need copying */
  ring_elem val = P->term(a->get_value(), mon);
  
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
  int n = f->n_terms();
  Monomial_array *monoms = GETMEM(Monomial_array *, sizeofarray(monoms,n));
  RingElement_array *coeffs = GETMEM(RingElement_array *, sizeofarray(coeffs,n));
  monoms->len = n;
  coeffs->len = n;
  ArrayPairOrNull result = newitem(ArrayPair);
  result->monoms = monoms;
  result->coeffs = coeffs;
  
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
}

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



#if 0

static int inline isequal2(const RingElement x, const RingElement y) {
  // Amazingly inserting this little routine fixes a bug in Microsoft CL
  // which causes cmd_Ring_isequal below to return true when it shouldn't.
  // And it shouldn't slow us down with gcc because it's inline.
  return x.is_equal(y);
}

void cmd_Ring_isequal(object &oa, object &ob)
{
  const RingElement a = oa->cast_to_RingElement();
  const Ring *Ra = a.get_ring();
  int result;
  const RingElement b = ob->cast_to_RingElement();
  const Ring *Rb = b.get_ring();
  if (Ra == Rb)
    result = a.is_equal(b);
  else if (Ra == ZZ)
    {
      const RingElement aa(Rb, MPZ_VAL(a.get_value()));
      result = isequal2(b,aa);
    }
  else if (Rb == ZZ)
    {
      const RingElement bb(Ra, MPZ_VAL(b.get_value()));
      result = isequal2(a,bb);
    }
  else
    {
      gError << "equality check: ring elements have different base rings";
      result = 0;
    }
  gStack.insert(make_object_int(result));
}

void cmd_Ring_isunit(object &oa)
{
  RingElement a = oa->cast_to_RingElement();
  gStack.insert(make_object_int(a.is_unit()));
}

void cmd_Ring_gcd(object &oa, object &ob)
{
  RingElement a = oa->cast_to_RingElement();
  RingElement b = ob->cast_to_RingElement();
  gStack.insert(a.gcd(b));
}
void cmd_Ring_gcdextended(object &oa, object &ob)
{
  RingElement a = oa->cast_to_RingElement();
  RingElement b = ob->cast_to_RingElement();
  RingElement u;
  RingElement v;
  RingElement g = a.gcd_extended(b, u, v);
  
  gStack.insert(g);
  gStack.insert(u);
  gStack.insert(v);
}

void cmd_Ring_power(object &oa, object &on)
{
  RingElement a = oa->cast_to_RingElement();
  RingElement b = on->cast_to_RingElement();

  if (b.get_ring() != ZZ)
    {
      gError << "must use integers for exponentiation";
      return;
    }
  gStack.insert(a.power(MPZ_VAL(b.get_value())));
}

// Polynomial routines
void cmd_PolynomialRing_term(object &oR, object &oa, object &om)
{
  // MES: this has several problems: (1) needs error checking
  // (2) the RingElement construction is incorrect
  const Ring *R = oR->cast_to_Ring();
  RingElement a = oa->cast_to_RingElement();
  Monomial m = om->cast_to_Monomial();
  if (R->Ncoeffs() != a.get_ring())
    { gError << "term: incorrect arguments"; return; }
  if (R->n_vars() == 0)
    { gError << "term: need a polynomial ring"; return; }

  gStack.insert(RingElement(R, a, m));
}
void cmd_PolynomialRing_getterms(object &oelem, object &om, object &on)
{
  RingElement r = oelem->cast_to_RingElement();
  int m = om->int_of();
  int n = on->int_of();
  gStack.insert(r.get_terms(m,n));
}
void cmd_PolynomialRing_getcoeff(object &oelem, object &om)
{
  RingElement r = oelem->cast_to_RingElement();
  Monomial m = om->cast_to_Monomial();
  gStack.insert(r.get_coeff(m));
}
void cmd_PolynomialRing_leadcoeff(object &oelem)
{
  RingElement r = oelem->cast_to_RingElement();
  gStack.insert(r.lead_coeff());
}
void cmd_PolynomialRing_leadmonom(object &oelem)
{
  RingElement r = oelem->cast_to_RingElement();
  gStack.insert(r.lead_monom());
}
void cmd_PolynomialRing_ishomogeneous(object &oelem)
{
  RingElement r = oelem->cast_to_RingElement();
  gStack.insert(make_object_int(r.is_homogeneous()));
}
void cmd_PolynomialRing_degree(object &oelem)
{
  RingElement r = oelem->cast_to_RingElement();
  
  gStack.insert(new object_intarray(r.degree()));
}
void cmd_PolynomialRing_homogenize(object &oelem, object &ov, object &owts)
{
  RingElement r = oelem->cast_to_RingElement();
  int v = ov->int_of();
  intarray *wts = owts->intarray_of();
  const Ring *R = r.get_ring();
  if (v < 0 || v >= R->n_vars())
    {
      gError << "homogenization: improper ring variable";
      return;
    }
  if (wts == NULL || wts->length() != R->n_vars())
    {
      gError << "homogenization: improper weight function";
      return;
    }
  if ((*wts)[v] == 0)
    {
      gError << "homogenization: variable weight is zero";
      return;
    }

  RingElement rh = r.homogenize(v, wts->raw());
  if (!error_exists())
    gStack.insert(rh);
}
void cmd_PolynomialRing_homogenize1(object &oelem, object &ov, object &odeg, object &owts)
{
  RingElement r = oelem->cast_to_RingElement();
  int v = ov->int_of();
  int deg = odeg->int_of();
  intarray *wts = owts->intarray_of();

  const Ring *R = r.get_ring();
  if (v < 0 || v >= R->n_vars())
    {
      gError << "homogenization: improper ring variable";
      return;
    }
  if (wts == NULL || wts->length() != R->n_vars())
    {
      gError << "homogenization: improper weight function";
      return;
    }
  if ((*wts)[v] == 0)
    {
      gError << "homogenization: variable weight is zero";
      return;
    }

  RingElement rh = r.homogenize(v, deg, wts->raw());
  if (!error_exists())
    gStack.insert(rh);
}

//// Fraction field routines ////
void cmd_FractionField_numerator(object &of)
{
  RingElement f = of->cast_to_RingElement();
  const Ring *R = f.get_ring();
  const FractionField *K = R->cast_to_FractionField();
  if (K == NULL)
    {
      gError << "fraction field required";
      return;
    }
  gStack.insert(f.numerator());
}

void cmd_FractionField_denominator(object &of)
{
  RingElement f = of->cast_to_RingElement();
  const Ring *R = f.get_ring();
  const FractionField *K = R->cast_to_FractionField();
  if (K == NULL)
    {
      gError << "fraction field required";
      return;
    }
  gStack.insert(f.denominator());
}

void cmd_FractionField_fraction(object &oR, object &o1, object &o2)
{
  const Ring *R = oR->cast_to_Ring();
  const FractionField *K = R->cast_to_FractionField();
  if (K == NULL)
    {
      gError << "fraction field required";
      return;
    }
  RingElement a = o1->cast_to_RingElement();
  RingElement b = o2->cast_to_RingElement();
  gStack.insert(a.fraction(K,b));
}



void cmd_Z_mod(object &r, object &oM)
{
  int p = r->int_of();
  const Monoid *M = oM->cast_to_Monoid()->cast_to_Monoid();
  if (p > 0)
    gStack.insert(Z_mod::create(p,M));
  else if (p == 0)
    gStack.insert(ZZ::create(M));
  else
    gError << "Z/pZ: p must be non-negative";
}

void cmd_Z()
{
  gStack.insert(ZZ);
}

void cmd_RR()
{
  gStack.insert(globalRR);
}

void cmd_GF(object &oprim)
{
  
  RingElement prim = oprim->cast_to_RingElement();

  // MES: check that K is Z/p[x], for some p.
  // MES: check that f is a monic polynomial of degree >= 2
  // MES: later, check that f is irreducible

  gStack.insert(GF::create(prim));
}
#if 0
void cmd_GF2(object &oR)
{
  PolynomialRing *R = oR->cast_to_Ring()->cast_to_PolynomialRing();
  if (R == NULL)
    {
      gError << "GF: Polynomial ring expected";
      return;
    }
  if (R->n_vars() != 1)
    {
      gError << "GF: expected a polynomial ring with one variable";
      return;
    }

  gStack.insert(new GF2(R));
}
#endif
void cmd_PolynomialRing(object &oK, object &oMF)
{
  const Ring *K = oK->cast_to_Ring();
  const Monoid *MF = oMF->cast_to_Monoid();
  gStack.insert(PolynomialRing::create(K,MF));
}
static void cmd_WeylAlgebra1(object &o1, object &o2, object &o3, object &o4, object &o5)
{
  const Ring *K = o1->cast_to_Ring();
  const Monoid *M = o2->cast_to_Monoid();
  intarray *comms = o3->intarray_of();
  intarray *diffs = o4->intarray_of();
  if (diffs->length() != comms->length())
    {
      gError << "Weyl algebra: expected same length arrays";
      return;
    }
  int homog_var = o5->int_of();
  if (homog_var < 0) homog_var = -1;

  WeylAlgebra *W = WeylAlgebra::create(K,M,
			   diffs->length(),
			   diffs->raw(),
			   comms->raw(),
			   homog_var);
  gStack.insert(W);
}
void cmd_fraction_field(object &oR)
{
  const Ring *R = oR->cast_to_Ring();
  gStack.insert(FractionField::create(R));
}
void cmd_qring(object &om)
{
  Matrix m = om->cast_to_Matrix();
  array<ring_elem> I;
  if (m.n_rows() != 1)
    {
      gError << "quotient ring expects matrix with one row";
      return;
    }
  const PolynomialRing *P = m.get_ring()->cast_to_PolynomialRing();
  if (P == NULL)
    {
      gError << "qring: expected polynomial ring";
      return;
    }
  for (int i=0; i<m.n_cols(); i++)
    I.append(m.elem(0,i));
  gStack.insert(PolynomialRing::create(P, I));
}
void cmd_qring2(object &on, object &oR)
{
  array<ring_elem> I;
  int n = on->int_of();
  const Ring *R = oR->cast_to_Ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == NULL)
    {
      gError << "qring: expected polynomial ring";
      return;
    }
  // MES: for now, we create an array of ring elems
  // by grabbing them one by one off the stack.
  // 'n' is the number of polynomials on the stack.

  for (int i=n-1; i >= 0 ; i--) 
    {
      if (!gStack.in_bounds(i) || gStack[i]->type_id() != TY_RING_ELEM)
	{
	  gError << "qring: expected ring element on the stack";
	  break;
	}
      RingElement f = gStack[i]->cast_to_RingElement();
      if (f.get_ring() != P)
	{
	  gError << "qring: ring element has incorrect base ring";
	  break;
	}
      I.append(P->copy(f.get_value()));
    }
  gStack.poppem(n);

  gStack.insert(PolynomialRing::create(P, I));
}
void cmd_schur(object &oK, object &oMF)
{
  const Ring *K = oK->cast_to_Ring();
  const Monoid *MF = oMF->cast_to_Monoid();
  gStack.insert(new SchurRing(K, MF));
}
void cmd_schur_dim(object &of)
{
  RingElement f = of->cast_to_RingElement();
  const Ring *R = f.get_ring();
  const SchurRing *F = R->cast_to_SchurRing();
  if (F == NULL)
    {
      gError << "schur dimension: need an element of a schur ring";
      return;
    }
  RingElement result(F->Ncoeffs(), F->dimension(f.get_value()));
  gStack.insert(result);
}

void cmd_qring_ideal(object &oR)
{
  const PolynomialRing *R = oR->cast_to_Ring()->cast_to_PolynomialRing();
  if (R == NULL)
    {
      gError << "polynomial ring expected";
      return;
    }
  gStack.insert(R->get_ideal());
}
void cmd_declare_field(object &oR)
{
  Ring *R = oR->cast_to_Ring();
  R->declare_field();
}
void cmd_get_zero_divisor(object &oR)
{
  const Ring *R = oR->cast_to_Ring();
  RingElement result(R, R->get_zero_divisor());
  gStack.insert(result);
}
void i_ring_elem_cmds(void)
{
  assert(trivial_monoid != NULL);
  ZZ = ZZ::create(Monoid::get_trivial_monoid());
  double default_epsilon = .000000000001;
  RRR = RR::create(Monoid::get_trivial_monoid(),default_epsilon);

  // Ring Creation
  install(ggZ, cmd_Z);
  install(ggRR, cmd_RR);

#if !defined(MIKE_NEWENGINE)
  install(ggEZZ, cmd_Z);
#endif

  install(ggcharp, cmd_Z_mod, TY_INT, TY_MONOID);
  install(ggGF, cmd_GF, TY_RING_ELEM);
  //  install(ggGF, cmd_GF2, TY_RING);
  install(ggpolyring, cmd_PolynomialRing, TY_RING, TY_MONOID);

  install(ggweylalgebra, cmd_WeylAlgebra1, TY_RING, TY_MONOID, 
	  TY_INTARRAY, TY_INTARRAY, TY_INT);

  install(ggschur, cmd_schur, TY_RING, TY_MONOID);
  install(ggqring, cmd_qring, TY_MATRIX);
  install(ggqring, cmd_qring2, TY_INT, TY_RING);
  install(ggfractionfield, cmd_fraction_field, TY_RING);

  install(gggetideal, cmd_qring_ideal, TY_RING);

  install(ggdeclarefield, cmd_declare_field, TY_RING);
  install(gggetzerodivisor, cmd_get_zero_divisor, TY_RING);

  // Informational
  install(ggisequal, cmd_Ring_isequal, TY_RING_ELEM, TY_RING_ELEM);
  install(ggiszero, cmd_Ring_iszero, TY_RING_ELEM);
  install(ggisunit, cmd_Ring_isunit, TY_RING_ELEM);

  // ring element commands
  install(ggfromdouble, cmd_Ring_from_double, TY_RING, TY_RING_ELEM);
  install(ggfromint, cmd_Ring_from_int, TY_RING, TY_INT);
  install(ggvar, cmd_Ring_var, TY_INT, TY_INT, TY_RING);

  install(ggnegate, cmd_Ring_negate, TY_RING_ELEM);
  install(ggadd, cmd_Ring_add, TY_RING_ELEM, TY_RING_ELEM);
  install(ggsubtract, cmd_Ring_subtract, TY_RING_ELEM, TY_RING_ELEM);
  install(ggmult, cmd_Ring_mult, TY_RING_ELEM, TY_RING_ELEM);
  install(ggdiv, cmd_Ring_div, TY_RING_ELEM, TY_RING_ELEM);
  install(ggmod, cmd_Ring_mod, TY_RING_ELEM, TY_RING_ELEM);
  install(ggdivmod, cmd_Ring_divmod, TY_RING_ELEM, TY_RING_ELEM);
  install(gggcd, cmd_Ring_gcd, TY_RING_ELEM, TY_RING_ELEM);
  install(gggcdextended, cmd_Ring_gcdextended, TY_RING_ELEM, TY_RING_ELEM);
  install(ggpower, cmd_Ring_power, TY_RING_ELEM, TY_INT);

  // polynomial commands
  install(ggterm, cmd_PolynomialRing_term, TY_RING, TY_RING_ELEM, TY_MONOMIAL);
  install(gggetcoeff, cmd_PolynomialRing_getcoeff, TY_RING_ELEM, TY_MONOMIAL);
  install(gggetterms, cmd_PolynomialRing_getterms, TY_RING_ELEM, TY_INT, TY_INT);
  install(ggleadcoeff, cmd_PolynomialRing_leadcoeff, TY_RING_ELEM);
  install(ggleadmonom, cmd_PolynomialRing_leadmonom, TY_RING_ELEM);
  install(ggishomogeneous, cmd_PolynomialRing_ishomogeneous, TY_RING_ELEM);
  install(ggdegree, cmd_PolynomialRing_degree, TY_RING_ELEM);
  install(gghomogenize, cmd_PolynomialRing_homogenize, TY_RING_ELEM, TY_INT, TY_INTARRAY);
  install(gghomogenize1, cmd_PolynomialRing_homogenize1, 
	  TY_RING_ELEM, TY_INT, TY_INT, TY_INTARRAY);

  // schur ring functions
  install(ggdim, cmd_schur_dim, TY_RING_ELEM);

  // fraction field elements
  install(ggnumerator, cmd_FractionField_numerator, TY_RING_ELEM);
  install(ggdenominator, cmd_FractionField_denominator, TY_RING_ELEM);
  install(ggfraction, cmd_FractionField_fraction, TY_RING, TY_RING_ELEM, TY_RING_ELEM);
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
