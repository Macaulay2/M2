// copyright Daniel R. Grayson, 1995

#include "../d/M2inits.h"

#include "config.h"
#include <assert.h>
#include <iostream>
using std::cout;
using std::endl;
#include <cstdio>

#if FACTORY
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

#include <NTL/ZZ.h>

#if FACTORY
#define REVERSE_VARIABLES 1	// did we have a good reason for reversing the variables before?  probably so the ideal reordering of Messollen would work...

extern "C" {
  extern void factory_setup_2();
};

enum factoryCoeffMode { modeError = 0, modeQQ, modeZZ, modeZn, modeUnknown };
static enum factoryCoeffMode coeffMode(const PolynomialRing *P) {
     const Ring *F = P->Ncoeffs();
     if (F->cast_to_QQ()) return modeQQ;
     if (F->cast_to_RingZZ()) return modeZZ;
     if (F->cast_to_Z_mod()) return modeZn;
     ERROR("expected coefficient ring of the form ZZ/n, ZZ, or QQ");
     return modeError;
}

int debugging;

extern "C" {
     extern void *GC_malloc_function(size_t);
};

static void advertise () {
     static enum { none, withGC, withGCindirect, withFactory } lastMessage = none;
     if (debugging == 0) return;
     if (__gmp_allocate_func == (void *(*) (size_t))getmem) {
	  if (lastMessage == withGCindirect) return;
	  lastMessage = withGCindirect;
	  printf("--gmp allocating with gc (indirectly)\n");
     }
     else if (__gmp_allocate_func == GC_malloc) {
	  if (lastMessage == withGC) return;
	  lastMessage = withGC;
	  printf("--gmp allocating with gc\n");
     }
     else {
	  if (lastMessage == withFactory) return;
	  lastMessage = withFactory;
	  printf("--gmp allocating with functions provided by factory\n");
     }
}

static void init_seeds() {
     SetSeed(ZZ::zero());	// NTL
     factoryseed(0);		// factory (which uses NTL, as we've compiled it)
}

struct enter_factory { 
  enum factoryCoeffMode mode;
  int oldcharac;
  int newcharac;
  int oldRatlState;
  int newRatlState;
  const Z_mod *Zn;
  void *(*save_gmp_allocate_func  )(size_t);
  void *(*save_gmp_reallocate_func)(void *, size_t, size_t);
  void *(*save_gmp_allocate_atomic_func  )(size_t);
  void *(*save_gmp_reallocate_atomic_func)(void *, size_t, size_t);
  void  (*save_gmp_free_func      )(void *, size_t);
  void enter() {
      if (debugging) advertise();
      save_gmp_allocate_func = __gmp_allocate_func;
      save_gmp_reallocate_func = __gmp_reallocate_func;
      save_gmp_allocate_atomic_func = __gmp_allocate_atomic_func;
      save_gmp_reallocate_atomic_func = __gmp_reallocate_atomic_func;
      save_gmp_free_func = __gmp_free_func;
      enterFactory();
      if (debugging) advertise();
      oldcharac = getCharacteristic();
      oldRatlState = isOn(SW_RATIONAL);
      switch (mode) {
          case modeZZ:
	       newRatlState = 0;
	       Off( SW_RATIONAL );
	       newcharac =0;
	       setCharacteristic(0);
	       break;
	  case modeZn: 
	       newRatlState = 0;
	       Off( SW_RATIONAL );
	       setCharacteristic(newcharac); 
	       break;
	  case modeQQ: 
	       newRatlState = 1;
	       On ( SW_RATIONAL );
	       newcharac = 0;
	       setCharacteristic(0);
	       break;
          default: 
	       newRatlState = oldRatlState;
	       newcharac = oldcharac;
	       break;
      }
      if (debugging) {
	   if (oldRatlState != newRatlState) printf(newRatlState ? "--setting factory rational mode on\n" : "--setting factory rational mode off\n");
	   if (oldcharac != newcharac) printf("--changing factory characteristic from %d to %d\n",oldcharac,newcharac);
      }
  }
  void exit() {
       if (oldRatlState) On(SW_RATIONAL); else Off(SW_RATIONAL);
       setCharacteristic(oldcharac);
       if (debugging) advertise();
       __gmp_allocate_func = save_gmp_allocate_func;
       __gmp_reallocate_func = save_gmp_reallocate_func;
       __gmp_allocate_atomic_func = save_gmp_allocate_atomic_func;
       __gmp_reallocate_atomic_func = save_gmp_reallocate_atomic_func;
       __gmp_free_func = save_gmp_free_func;
       if (debugging) {
	    advertise();
	    if (oldcharac != newcharac) printf("--changing factory characteristic back from %d to %d\n",newcharac,oldcharac);
	    if (oldRatlState != newRatlState) printf(oldRatlState ? "--setting factory rational mode back on\n" : "--setting factory rational mode back off\n");
       }
  }

  enter_factory() : 
       mode(modeUnknown),
       Zn(NULL)
     { enter(); }

  enter_factory(const PolynomialRing *P) :
       mode(coeffMode(P)),
       newcharac(mode == modeZn ? P->charac() : 0),
       Zn(mode == modeZn ? P->Ncoeffs()->cast_to_Z_mod() : NULL)
     { enter(); }

  ~enter_factory() { exit(); }

};

struct enter_M2 { 
  void *(*save_gmp_allocate_func  )(size_t);
  void *(*save_gmp_reallocate_func)(void *, size_t, size_t);
  void *(*save_gmp_allocate_atomic_func  )(size_t);
  void *(*save_gmp_reallocate_atomic_func)(void *, size_t, size_t);
  void  (*save_gmp_free_func      )(void *, size_t);
  void enter() {
    if (debugging) advertise();
    save_gmp_allocate_func = __gmp_allocate_func;
    save_gmp_reallocate_func = __gmp_reallocate_func;
    save_gmp_allocate_atomic_func = __gmp_allocate_atomic_func;
    save_gmp_reallocate_atomic_func = __gmp_reallocate_atomic_func;
    save_gmp_free_func = __gmp_free_func;
    enterM2();
    if (debugging) advertise();
  }
  void exit() {
    if (debugging) advertise();
    __gmp_allocate_func = save_gmp_allocate_func;
    __gmp_reallocate_func = save_gmp_reallocate_func;
    __gmp_allocate_atomic_func = save_gmp_allocate_atomic_func;
    __gmp_reallocate_atomic_func = save_gmp_reallocate_atomic_func;
    __gmp_free_func = save_gmp_free_func;
    if (debugging) advertise();
  }
  enter_M2() { enter(); }
  ~enter_M2() { exit(); }
};

static MP_INT toInteger(CanonicalForm h) {
//// we don't have access to int_cf.h and int_int.h from factory, so the following commented-out code won't compile; but it might have worked.
//     struct enter_factory foo;
//     assert(h.inZ());
//     InternalCF *value = h.getval();
//     MP_INT y = value->MPI();
//     struct enter_M2 bar;
//     mpz_t x;
//     mpz_init(x);
//     mpz_set(x,y);
//     return *x;
     struct enter_M2 b;
     static const unsigned int base = 1 << 16;
     intarray v;
     int sign;
     {
       struct enter_factory foo;
       int RationalMode = isOn(SW_RATIONAL) ? (Off(SW_RATIONAL), 1) : 0;
       if (h < 0) { sign = -1; h = -h; } else sign = 1;
       while ( h != 0 ) {
	    CanonicalForm k = h % base;
	    v.append(k.intval());
	    h = h/base;
       }
       if (RationalMode) On(SW_RATIONAL);
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

static const RingElement * convertToM2(const PolynomialRing *R, CanonicalForm h) {
     // this seems not to handle polynomials with rational coefficients at all!!
     struct enter_M2 foo;
     const int n = R->n_vars();
     if (h.inCoeffDomain()) {
	  if (h.inZ()) {
	       mpz_t x = {toInteger(h)};
	       ring_elem ret = R->from_int(x);
	       mpz_clear(x);
	       return RingElement::make_raw(R, ret);
	  }
	  else if (h.inQ()) {
	       struct enter_factory c;
	       CanonicalForm hnum = h.num(), hden = h.den();
	       struct enter_M2 d;
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
       const RingElement *r = convertToM2(R, h[j]);
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

static struct enter_M2 b1;
static struct enter_factory foo1;
    static int base_set = 0;
    static CanonicalForm base;

    // debugging display routines to be called from gdb
    // needs factory to be configured without option --disable-streamio 
    void showvar(Variable &t) { cout << t << endl; }
    void showcf(CanonicalForm &t) { cout << t << endl; }
    void showcfl(CFList &t) { cout << t << endl; }
    void showcffl(CFFList &t) { cout << t << endl; }
static struct enter_factory foo2;
static struct enter_M2 b2;

static CanonicalForm convertToFactory(const mpz_ptr p) {
     struct enter_factory foo;
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
//#define MPQ_VAL(f) (M2_Rational ((f).poly_val))

static CanonicalForm convertToFactory(const RingElement &g) {
     const Ring *R = g.get_ring();
     const PolynomialRing *P = R->cast_to_PolynomialRing();
     if (P == 0)
       {
	 ERROR("expected a polynomial ring");
	 return 0;
       }
     const int n = P->n_vars();
     const Monoid *M = P->Nmonoms();
     intarray vp;
     struct enter_factory foo(P);
     if (foo.mode == modeError) return 0;
     CanonicalForm f = 0;
     for (Nterm *t = g.get_value(); t != NULL; t = t->next) {
       vp.shrink(0);
       M->to_varpower(t->monom,vp);
       CanonicalForm m = (
			  foo.mode == modeZn ? CanonicalForm(foo.Zn->to_int(t->coeff)) :
			  foo.mode == modeZZ ? convertToFactory(t->coeff.get_mpz()) :
			  foo.mode == modeQQ ? convertToFactory(mpq_numref(MPQ_VAL(t->coeff))) / convertToFactory(mpq_denref(MPQ_VAL(t->coeff)))
			  // old way : convertToFactory(FRAC_VAL(t->coeff)->numer.get_mpz()) / convertToFactory(FRAC_VAL(t->coeff)->denom.get_mpz())
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
     return f;
}

void displayCF(PolynomialRing *R, const CanonicalForm &h)
{
  buffer o;
  const RingElement *g = convertToM2(R,h);
  o << IM2_RingElement_to_string(g) << "\n";
  emit(o.str());
}
#endif

const RingElementOrNull *rawGCDRingElement(const RingElement *f, const RingElement *g)
{
#if FACTORY
  const RingElement *ret = NULL;
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
  {
    struct enter_factory foo(P);
    if (foo.mode == modeError) return 0;
    CanonicalForm p = convertToFactory(*f);
    CanonicalForm q = convertToFactory(*g);
    CanonicalForm h = gcd(p,q);
    ret = convertToM2(P,h);
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
#if FACTORY
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

  struct enter_factory foo(P);
  if (foo.mode == modeError) return 0;
  CanonicalForm p = convertToFactory(*f);
  CanonicalForm q = convertToFactory(*g);
  CanonicalForm a, b;
  CanonicalForm h = extgcd(p,q,a,b);
  ret = convertToM2(P,h);
  *A = convertToM2(P,a);
  *B = convertToM2(P,b);
  return ret;
#else
  ERROR("'factory' library not installed");
  return NULL;
#endif
}

const RingElementOrNull *rawPseudoRemainder(const RingElement *f, const RingElement *g)
{
#if FACTORY
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

  struct enter_factory foo(P);
  if (foo.mode == modeError) return 0;  
  CanonicalForm p = convertToFactory(*f);
  CanonicalForm q = convertToFactory(*g);
  CanonicalForm h = Prem(p,q);
  const RingElement *r = convertToM2(P,h);
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
#if FACTORY
  const PolynomialRing *P = g->get_ring()->cast_to_PolynomialRing();
  *result_factors = 0;
  *result_powers = 0;
  if (P == 0) {
       ERROR("expected polynomial ring");
       return;
  }
  struct enter_factory foo(P);
  if (foo.mode == modeError) return;
  CanonicalForm h = convertToFactory(*g);
  // displayCF(P,h);
  CFFList q;
  init_seeds();
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
    (*result_factors)->array[next] = convertToM2(P,i.getItem().factor());
    (*result_powers)->array[next++] = i.getItem().exp();
  }
#else
  ERROR("'factory' library not installed");
#endif
}

M2_arrayint_OrNull rawIdealReorder(const Matrix *M)
{
#if FACTORY
  init_seeds();
  const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
     const int N = P->n_vars();

     CFList I;
     int i;
     for (i = 0; i < M->n_rows(); i++) {
	  for (int j=0; j < M->n_cols(); j++) {
	    const RingElement *g = RingElement::make_raw(P, M->elem(i,j));
	    I.append(convertToFactory(*g));
	  }
     }

     struct enter_factory foo(P);
     if (foo.mode == modeError) return NULL;

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
#if FACTORY
     const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
     if (P == 0) {
	  ERROR("expected polynomial ring");
	  return 0;
     }

     CFList I;
     for (int i = 0; i < M->n_rows(); i++) {
	  for (int j=0; j < M->n_cols(); j++) {
	    const RingElement *g = RingElement::make_raw(P, M->elem(i,j));
	    I.append(convertToFactory(*g));
	  }
     }

     struct enter_factory foo(P);
     if (foo.mode == modeError) return NULL;
     init_seeds();

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
	    result1->array[next1++] = convertToM2(P,j.getItem());
	  }
	  struct enter_M2 b;
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
