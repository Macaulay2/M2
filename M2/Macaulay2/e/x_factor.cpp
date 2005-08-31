// copyright Daniel R. Grayson, 1995

#include "config.h"
#include <assert.h>
#include <iostream>

#include "../d/M2inits.h"

#ifdef FACTORY
#define Matrix MaTrIx
/* #define NOSTREAMIO */
#include <factor.h>		// from Messollen's libfac
#undef Matrix
//#include <templates/ftmpl_list.cc>
#endif

#include "matrix.hpp"
#include "z_mod_p.hpp"
#include "ZZ.hpp"
#include "frac.hpp"
#include "polyring.hpp"

#include "relem.hpp"
#include "../d/M2mem.h"

#include "text_io.hpp"
#include "buffer.hpp"

#ifdef FACTORY
#define REVERSE_VARIABLES 1	// did we have a good reason for reversing the variables before?  probably so the ideal reordering of Messollen would work...

// debugging display routines to be called from gdb
// needs factory to be configured without option --disable-streamio 
void showvar(Variable &t) { cout << t << endl; }
void showcf(CanonicalForm &t) { cout << t << endl; }
void showcfl(CFList &t) { cout << t << endl; }
void showcffl(CFFList &t) { cout << t << endl; }

extern "C" {
  extern void factory_setup_2();
};

struct enter_factory { 
  void *(*save_gmp_allocate_func  )(size_t);
  void *(*save_gmp_reallocate_func)(void *, size_t, size_t);
  void  (*save_gmp_free_func      )(void *, size_t);
  void enter() {
    save_gmp_allocate_func = __gmp_allocate_func;
    save_gmp_reallocate_func = __gmp_reallocate_func;
    save_gmp_free_func = __gmp_free_func;
    factory_setup_2();
  }
  void exit() {
    __gmp_allocate_func = save_gmp_allocate_func;
    __gmp_reallocate_func = save_gmp_reallocate_func;
    __gmp_free_func = save_gmp_free_func;
  }
  enter_factory:: enter_factory() { enter(); }
  enter_factory::~enter_factory() { exit(); }
};

struct enter_M2 { 
  void *(*save_gmp_allocate_func  )(size_t);
  void *(*save_gmp_reallocate_func)(void *, size_t, size_t);
  void  (*save_gmp_free_func      )(void *, size_t);
  void enter() {
    save_gmp_allocate_func = __gmp_allocate_func;
    save_gmp_reallocate_func = __gmp_reallocate_func;
    save_gmp_free_func = __gmp_free_func;
    M2_setup();
  }
  void exit() {
    __gmp_allocate_func = save_gmp_allocate_func;
    __gmp_reallocate_func = save_gmp_reallocate_func;
    __gmp_free_func = save_gmp_free_func;
  }
  enter_M2:: enter_M2() { enter(); }
  enter_M2::~enter_M2() { exit(); }
};

static MP_INT toInteger(CanonicalForm h) {
     // there has to be a faster way, since factory uses gmp inside
     enter_M2 b;
     static const unsigned int base = 1 << 16;
     intarray v;
     int sign;
     assert(h.inZ());
     {
       enter_factory a;
       if (h < 0) { sign = -1; h = -h; } else sign = 1;
       while ( h != 0 ) {
	    CanonicalForm k = h % base;
	    v.append(k.intval());
	    h /= base;
       }
     }
     mpz_t x;
     mpz_init(x);
     for (int i = v.length() - 1; i >= 0; i--) {
	  mpz_mul_ui(x,x,base); // x = x * base;
	  mpz_add_ui(x,x,static_cast<unsigned>(v[i]));
     }
     if (sign == -1) mpz_neg(x,x); // x = -x;
     return x[0];
}

static const RingElement * convert(const PolynomialRing *R, CanonicalForm h) {
     // this seems not to handle polynomials with rational coefficients at all!!
     enter_M2 b;
     const int n = R->n_vars();
     if (h.inCoeffDomain()) {
	  if (h.inZ()) {
	       mpz_t x = {toInteger(h)};
	       ring_elem ret = R->from_int(x);
	       mpz_clear(x);
	       return RingElement::make_raw(R, ret);
	  }
	  else if (h.inQ()) {
	       enter_factory c;
	       CanonicalForm hnum = h.num(), hden = h.den();
	       enter_M2 d;
	       MP_RAT z = {toInteger(h.num()), toInteger(h.den())};
	       RingElement *ret = RingElement::make_raw(R,R->from_rational(&z));
	       mpq_clear(&z);
	       return ret;
	  }
	  else if (h.inFF()) {
	       return RingElement::make_raw(R, R->from_int(h.intval()));
	  }
	  else {
	       ERROR("conversion from factory over unknown type");
	       return RingElement::make_raw(R,R->one());
	  }
     }
     ring_elem result = R->from_int(0);
     for (int j = 0; j <= h.degree(); j++) {
       const RingElement *r = convert(R, h[j]);
       ring_elem r1 = r->get_value();
       int var = 
#                   if REVERSE_VARIABLES
			    (n-1) - 
#                   endif
                            (h.level()-1);
       ring_elem v = R->var(var);
       v = R->power(v,j);
       r1 = R->mult(r1,v);
       R->add_to(result,r1);
     }
     return RingElement::make_raw(R,result);
}

static enter_M2 b1;
static enter_factory a1;
 static int base_set = 0;
 static CanonicalForm base;
static enter_factory a2;
static enter_M2 b2;

static CanonicalForm convert(const mpz_ptr p) {
     enter_factory a;
     int size = p -> _mp_size;
     int sign = size < 0 ? -1 : 1;
     if (size < 0) size = -size;
     if (!base_set) {
	  base_set = 1;
	  base = 1;
	  for (int i = 0; i < mp_bits_per_limb; i++) base *= 2;
     }
     CanonicalForm m = 0;
     for (int i = size - 1; i >= 0; i--) {
	  mp_limb_t digit = p -> _mp_d[i];
	  for (int j = mp_bits_per_limb; j > 0; ) {
	       int k = j - 16;
	       if (k < 0) k = 0;
	       int n = j - k;
	       int subbase = 1 << n;
	       m = subbase * m + ((digit >> k) & (subbase - 1));
	       j = k;
	  }
     }
     m = m * sign;
     return m;
}

//#define FRAC_VAL(f) ((frac_elem *) (f).poly_val)
#define MPQ_VAL(f) (M2_Rational ((f).poly_val))

enum coeff_type { FAC_ZZp, FAC_ZZ, FAC_QQ, FAC_BAD_RING };

static coeff_type get_ring_type(const Ring *R)
{
     const PolynomialRing *P = R->cast_to_PolynomialRing();
     if (P == 0) return FAC_BAD_RING;
     const Ring *F = P->Ncoeffs();
     if (F->cast_to_Z_mod() != 0) return FAC_ZZp;
     if (F->cast_to_RingZZ() != 0) return FAC_ZZ;
     if (F->cast_to_QQ() != 0) return FAC_QQ;
     ERROR("expected coefficient ring of the form ZZ/n, ZZ, or QQ");
     return FAC_BAD_RING;
}

static bool is_valid_factory_ring(const Ring *R)
{
  return get_ring_type(R) != FAC_BAD_RING;
}

static CanonicalForm convert(const RingElement &g) {
     const Ring *R = g.get_ring();
     const PolynomialRing *P = R->cast_to_PolynomialRing();
     if (P == 0)
       {
	 ERROR("expected a polynomial ring");
	 return 0;
       }
     const int n = P->n_vars();
     const Ring *F = P->Ncoeffs();
     const Z_mod *Zn = F->cast_to_Z_mod();
     const RingZZ *Z0 = F->cast_to_RingZZ();
     const QQ *Q = F->cast_to_QQ();
#if 0
     const FractionField *Q = (
			       NULL != F->cast_to_FractionField()
			       &&
			       NULL != F->cast_to_FractionField()->get_ring()->cast_to_ZZ()
			       ?
			       F->cast_to_FractionField()
			       :
			       (FractionField*) NULL
			       );
#endif
     if (Zn == NULL && Z0 == NULL && Q == NULL) {
	  ERROR("expected coefficient ring of the form ZZ/n, ZZ, or QQ");
	  return 0;
     }
     const Monoid *M = P->Nmonoms();
     intarray vp;
     enter_factory a;
     setCharacteristic(R->charac());
     if (Q != NULL) On( SW_RATIONAL );
     CanonicalForm f = 0;
     for (Nterm *t = g.get_value(); t != NULL; t = t->next) {
       vp.shrink(0);
       M->to_varpower(t->monom,vp);
       CanonicalForm m = (
			  Zn != NULL ? CanonicalForm(Zn->to_int(t->coeff)) :
			  Z0 != NULL ? convert(MPZ_VAL(t->coeff)) :
			  Q != NULL ? convert(mpq_numref(MPQ_VAL(t->coeff))) / convert(mpq_denref(MPQ_VAL(t->coeff)))
#if 0
			  convert(MPZ_VAL(FRAC_VAL(t->coeff)->numer)) / convert(MPZ_VAL(FRAC_VAL(t->coeff)->denom))
#endif
			  : CanonicalForm(0) // shouldn't happen
			  );
       for (index_varpower l = vp.raw(); l.valid(); ++l)
	 {
	   m *= power(
		      Variable(1 + (
#                       if REVERSE_VARIABLES
				    (n-1) - 
#                       endif
				    l.var()
				    )), 
		      l.exponent()
		      );
	 }
       f += m;
     }
     if (Q != NULL) Off( SW_RATIONAL );
     return f;
}

void displayCF(PolynomialRing *R, const CanonicalForm &h)
{
  buffer o;
  const RingElement *g = convert(R,h);
  o << IM2_RingElement_to_string(g);
  emit(o.str());
}
#endif

const RingElementOrNull *rawGCDRingElement(const RingElement *f, const RingElement *g)
{
#warning "check that the rings of f and g both polynomial rings"
#ifdef FACTORY
  const RingElement *ret;
  const PolyRing *P = f->get_ring()->cast_to_PolyRing();
  const PolyRing *P2 = g->get_ring()->cast_to_PolyRing();
  if (P == 0) {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (P != P2) {
      ERROR("encountered different rings");
      return 0;
    }
  if (!is_valid_factory_ring(P))
    {
      // Error message has already been issued.
      return 0;
    }

  {
    enter_factory a;
    CanonicalForm p = convert(*f);
    CanonicalForm q = convert(*g);
    //         std::cerr << "p = " << p << endl
    //              << "q = " << q << endl;
    p = p / icontent(p);
    q = q / icontent(q);
    //         std::cerr << "p = " << p << endl
    //              << "q = " << q << endl;
    CanonicalForm h = gcd(p,q);
    ret = convert(P,h);
  }
  ring_elem a = P->preferred_associate_divisor(ret->get_value()); // an element in the coeff ring
  ring_elem b = P->getCoefficients()->invert(a);
  ring_elem r = ret->get_value();
  P->mult_coeff_to(b, r);
  return RingElement::make_raw(P,r);
#else
  ERROR("'factory' library not installed");
  return NULL;
#endif
}

const RingElementOrNull *rawExtendedGCDRingElement(const RingElement *f, const RingElement *g, const RingElement **A, const RingElement **B)
{
#warning "check that the rings of f and g both polynomial rings"
#ifdef FACTORY
  const RingElement *ret;
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *P2 = g->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (P != P2)
    {
      ERROR("encountered different rings");
      return 0;
    }

  if (!is_valid_factory_ring(P))
    {
      // Error message has already been issued.
      return 0;
    }

  enter_factory here;
  CanonicalForm p = convert(*f), q = convert(*g), a, b;
  //     std::cerr << "p = " << p << endl
  //          << "q = " << q << endl;
  CanonicalForm h = extgcd(p,q,a,b);
  ret = convert(P,h);
  *A = convert(P,a);
  *B = convert(P,b);
  return ret;
#else
  ERROR("'factory' library not installed");
  return NULL;
#endif
}

const RingElementOrNull *rawPseudoRemainder(const RingElement *f, const RingElement *g)
{
#warning "check that the rings of f and g both polynomial rings"
#ifdef FACTORY
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *P2 = g->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (P != P2)
    {
      ERROR("encountered different rings");
      return 0;
    }

  if (!is_valid_factory_ring(P))
    {
      // Error message has already been issued.
      return 0;
    }

  enter_factory a;
  CanonicalForm p = convert(*f);
  CanonicalForm q = convert(*g);
  //     std::cerr << "p = " << p << endl
  //          << "q = " << q << endl;
  CanonicalForm h = Prem(p,q);
  const RingElement *r = convert(P,h);
  return r;
#else
  ERROR("'factory' library not installed");
  return NULL;
#endif
}

void rawFactor(const RingElement *g, 
	       RingElement_array_OrNull **result_factors, 
	       M2_arrayint_OrNull *result_powers)
{
#ifdef FACTORY
  const PolynomialRing *P = g->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      *result_factors = 0;
      *result_powers = 0;
      return;
    }

  if (!is_valid_factory_ring(P))
    {
      // Error message has already been issued.
      *result_factors = 0;
      *result_powers = 0;
      return;
    }

  enter_factory a;
  factoryseed(23984729);
  CanonicalForm h = convert(*g);
  // displayCF(P,h);
  CFFList q;
  if (P->charac() == 0) {
    q = factorize(h);		// suitable for k = QQ, comes from libcf (factory)
  }
  else {
    q = Factorize(h);		// suitable for k = ZZ/p, comes from libfac
  }
  int nfactors = q.length();

  *result_factors = reinterpret_cast<RingElement_array *>(getmem(sizeofarray((*result_factors),nfactors)));
  (*result_factors)->len = nfactors;

  *result_powers = makearrayint(nfactors);

  int next = 0;
  for (CFFListIterator i = q; i.hasItem(); i++) {
    (*result_factors)->array[next] = convert(P,i.getItem().factor());
    (*result_powers)->array[next++] = i.getItem().exp();
  }
#else
  ERROR("'factory' library not installed");
#endif
}

M2_arrayint_OrNull rawIdealReorder(const Matrix *M)
{
#ifdef FACTORY
  const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
     factoryseed(23984729);
     const int N = P->n_vars();

     if (get_ring_type(P) == FAC_BAD_RING) return 0; // error message already issued

     CFList I;
     int i;
     for (i = 0; i < M->n_rows(); i++) {
	  for (int j=0; j < M->n_cols(); j++) {
	    const RingElement *g = RingElement::make_raw(P, M->elem(i,j));
	    I.append(convert(*g));
	  }
     }

     enter_factory a;

     List<int> t = neworderint(I);

     int n = t.length();
     intarray u(N);
     ListIterator<int> ii(t);
     for (i=0; ii.hasItem(); ii++, i++) u.append(
					      (n-1)-(ii.getItem()-1) // REVERSE!
					      );
     if (n>0) for (i=(n-1)/2; i>=0; i--) { // REVERSE!
	  int tmp = u[n-1-i];
	  u[n-1-i] = u[i];
	  u[i] = tmp;
     }
     for (i=n; i<N; i++) u.append(i);

     M2_arrayint result = makearrayint(N);
     for (i=0; i<N; i++)
       result->array[i] = u[i];
     return result;
#else
  ERROR("'factory' library not installed");
  return NULL;
#endif
}    

Matrix_array_OrNull * rawCharSeries(const Matrix *M)
{
#ifdef FACTORY
  const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
     factoryseed(23984729);

     if (get_ring_type(P) == FAC_BAD_RING) return 0; // error message already issued

     CFList I;
     for (int i = 0; i < M->n_rows(); i++) {
	  for (int j=0; j < M->n_cols(); j++) {
	    const RingElement *g = RingElement::make_raw(P, M->elem(i,j));
	    I.append(convert(*g));
	  }
     }

     enter_factory a;

     List<CFList> t = IrrCharSeries(I);

     Matrix_array *result = reinterpret_cast<Matrix_array *>(getmem(sizeofarray(result,t.length())));
     result->len = t.length();
     
     int next = 0;
     for (ListIterator<List<CanonicalForm> > ii = t; ii.hasItem(); ii++) {
	  CFList u = ii.getItem();
	  RingElement_array *result1 = reinterpret_cast<RingElement_array *>(getmem(sizeofarray(result1,u.length())));
	  result1->len = u.length();
	  int next1 = 0;
	  for (ListIterator<CanonicalForm> j = u; j.hasItem(); j++) {
	    result1->array[next1++] = convert(P,j.getItem());
	  }
	  enter_M2 b;
	  result->array[next++] = IM2_Matrix_make1(M->rows(), u.length(), result1, false);
     }
     
     return result;
#else
  ERROR("'factory' library not installed");
  return NULL;
#endif
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
