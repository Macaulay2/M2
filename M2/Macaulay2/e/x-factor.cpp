// copyright Daniel R. Grayson, 1995

#include "../d/M2inits.h"
#include "exceptions.hpp"
#include "config.h"
#include <assert.h>
#include <iostream>
using std::cout;
using std::endl;
#include <cstdio>

#define Matrix MaTrIx
#include <factor.h>		// from Messollen's libfac
#undef Matrix
//#include <templates/ftmpl_list.cc>

#include "matrix.hpp"
#include "ZZp.hpp"
#include "ZZ.hpp"
#include "frac.hpp"
#include "polyring.hpp"

#include "relem.hpp"
#include "GF.hpp"
#include "../d/M2mem.h"

#include "text-io.hpp"
#include "buffer.hpp"

#include <NTL/ZZ.h>

#define REVERSE_VARIABLES 1	// did we have a good reason for reversing the variables before?  probably so the ideal reordering of Messollen would work...

extern "C" {
  extern void factory_setup_2();
};

enum factoryCoeffMode { modeError = 0, modeQQ, modeZZ, modeZn, modeGF, modeUnknown };
static enum factoryCoeffMode coeffMode(const PolynomialRing *P) {
     const Ring *F = P->Ncoeffs();
     if (F->cast_to_QQ()) return modeQQ;
     if (F->cast_to_RingZZ()) return modeZZ;
     if (F->cast_to_Z_mod()) return modeZn;
     if (F->cast_to_GF()) return modeGF;
     ERROR("expected coefficient ring of the form ZZ/n, ZZ, QQ, or GF");
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

CanonicalForm algebraicElement_Fac;
const RingElement *algebraicElement_M2;

struct enter_factory { 
  enum factoryCoeffMode mode;
  int oldcharac;
  int newcharac;
  int oldRatlState;
  int newRatlState;
  const Z_mod *Zn;
  const GF *gf;
  void *(*save_gmp_allocate_func  )(size_t);
  void *(*save_gmp_reallocate_func)(void *, size_t, size_t);
  void  (*save_gmp_free_func      )(void *, size_t);
  void enter();
  void exit();

  enter_factory() : 
       mode(modeUnknown),
       Zn(NULL),
       gf(NULL)
     { enter(); }

  enter_factory(const PolynomialRing *P) :
       mode(coeffMode(P)),
       newcharac(mode == modeZn || mode == modeGF ? P->charac() : 0),
       Zn(mode == modeZn ? P->Ncoeffs()->cast_to_Z_mod() : NULL),
       gf(mode == modeGF ? P->Ncoeffs()->cast_to_GF(): NULL)
     { enter(); }

  ~enter_factory() { exit(); }

};

void enter_factory::enter() {
      if (debugging) advertise();
      save_gmp_allocate_func = __gmp_allocate_func;
      save_gmp_reallocate_func = __gmp_reallocate_func;
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
          case modeGF:
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

void enter_factory::exit() {
       if (oldRatlState) On(SW_RATIONAL); else Off(SW_RATIONAL);
       setCharacteristic(oldcharac);
       if (debugging) advertise();
       __gmp_allocate_func = save_gmp_allocate_func;
       __gmp_reallocate_func = save_gmp_reallocate_func;
       __gmp_free_func = save_gmp_free_func;
       if (debugging) {
	    advertise();
	    if (oldcharac != newcharac) printf("--changing factory characteristic back from %d to %d\n",newcharac,oldcharac);
	    if (oldRatlState != newRatlState) printf(oldRatlState ? "--setting factory rational mode back on\n" : "--setting factory rational mode back off\n");
       }
  }

struct enter_M2 { 
  void *(*save_gmp_allocate_func  )(size_t);
  void *(*save_gmp_reallocate_func)(void *, size_t, size_t);
  void  (*save_gmp_free_func      )(void *, size_t);
  void enter();
  void exit();
  enter_M2() { enter(); }
  ~enter_M2() { exit(); }
};

void enter_M2::enter() {
    if (debugging) advertise();
    save_gmp_allocate_func = __gmp_allocate_func;
    save_gmp_reallocate_func = __gmp_reallocate_func;
    save_gmp_free_func = __gmp_free_func;
    enterM2();
    if (debugging) advertise();
  }

void enter_M2::exit() {
    if (debugging) advertise();
    __gmp_allocate_func = save_gmp_allocate_func;
    __gmp_reallocate_func = save_gmp_reallocate_func;
    __gmp_free_func = save_gmp_free_func;
    if (debugging) advertise();
  }

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
	  else if (h.inFF()) return RingElement::make_raw(R, R->from_int(h.intval()));
	  else if (h.inExtension()) {
	       assert( algebraicElement_M2 != NULL );
	       ring_elem result = R->from_int(0);
	       for (int j = h.taildegree(); j <= h.degree(); j++) {
		 const RingElement *r = convertToM2(R, h[j]);
		 if (error()) return RingElement::make_raw(R,R->one());
		 ring_elem r1 = r->get_value();
		 ring_elem v = algebraicElement_M2->get_value();
		 v = R->power(v,j);
		 r1 = R->mult(r1,v);
		 R->add_to(result,r1);
	       }
	       return RingElement::make_raw(R,result);
	  }
	  else {
	       ERROR("conversion from factory over unknown type");
	       return RingElement::make_raw(R,R->one());
	  }
     }
     ring_elem result = R->from_int(0);
     for (int j = h.taildegree(); j <= h.degree(); j++) {
       const RingElement *r = convertToM2(R, h[j]);
       if (error()) return RingElement::make_raw(R,R->one());
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
#if DEBUG
void showvar(Variable &t) { cout << t << endl; }
void showcf(CanonicalForm &t) { cout << t << endl; }
void showcfl(CFList &t) { cout << t << endl; }
void showcffl(CFFList &t) { cout << t << endl; }
#endif

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

static CanonicalForm convertToFactory(const RingElement &g, bool inExtension);

static CanonicalForm convertToFactory(const ring_elem &q, const GF *k) { // use algebraicElement_Fac for converting this galois field element
  const PolynomialRing *A = k->originalR();
  RingElement *g = RingElement::make_raw(A,k->get_rep(q));
  intarray vp;
  const Monoid *M = A->Nmonoms();
  const Z_mod *Zn = k->originalR()->Ncoeffs()->cast_to_Z_mod();
  CanonicalForm f = 0;
  for (Nterm *t = g->get_value(); t != NULL; t = t->next) {
    vp.shrink(0);
    M->to_varpower(t->monom,vp);
    CanonicalForm m = CanonicalForm(Zn->to_int(t->coeff));
    for (index_varpower l = vp.raw(); l.valid(); ++l)
      m *= power( algebraicElement_Fac, l.exponent() );
    f += m;
  }
  return f;
}

static CanonicalForm convertToFactory(const RingElement &g,bool inExtension) {
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
			  foo.mode == modeGF ? convertToFactory(t->coeff,foo.gf) :
			  foo.mode == modeZZ ? convertToFactory(t->coeff.get_mpz()) :
			  foo.mode == modeQQ ? (
						convertToFactory(mpq_numref(MPQ_VAL(t->coeff)))
						/
						convertToFactory(mpq_denref(MPQ_VAL(t->coeff)))
						) :
			  CanonicalForm(0) // shouldn't happen
			  );
       for (index_varpower l = vp.raw(); l.valid(); ++l)
	 {
	   int index = 1 + 
#                       if REVERSE_VARIABLES
				    (n-1) - 
#                       endif
				    l.var()
	     ;
	   m *= power( index == 1 && inExtension ? algebraicElement_Fac : Variable(index), l.exponent() );
	 }
       f += m;
     }
     return f;
}

void displayCF(PolynomialRing *R, const CanonicalForm &h) // for debugging
{
  buffer o;
  const RingElement *g = convertToM2(R,h);
  o << IM2_RingElement_to_string(g) << "\n";
  emit(o.str());
}

const RingElementOrNull *rawGCDRingElement(const RingElement *f, const RingElement *g,
					   const RingElement *mipo, const M2_bool inExtension)
{
  cerr << "--entering gcd()" << endl;
  const RingElement *ret = NULL;
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *P2 = g->get_ring()->cast_to_PolynomialRing();
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
    if (foo.mode == modeError) { algebraicElement_M2 = NULL; return 0; }
    if (foo.mode == modeGF) {
      assert( ! inExtension );
      algebraicElement_Fac = rootOf(convertToFactory(*foo.gf->get_minimal_poly(),inExtension),'a');
      {
	struct enter_M2 bar;
	(RingElement::make_raw(P->Ncoeffs()->cast_to_GF(), foo.gf->var(0)))->promote(P,algebraicElement_M2); // sets algebraicElement_M2
      }
    }
    if (inExtension) {
      CanonicalForm minp = convertToFactory(*mipo,false);
      cerr << "--mipo = " << minp << endl;
      algebraicElement_Fac = rootOf(minp,'a');
      cerr << "--a = " << algebraicElement_Fac << endl;
      cerr << "--a.level() = " << algebraicElement_Fac.level() << endl;
    }
    CanonicalForm p = convertToFactory(*f,inExtension);
    cerr << "--p = " << p << endl;
    CanonicalForm q = convertToFactory(*g,inExtension);
    cerr << "--q = " << q << endl;
    CanonicalForm h = gcd(p,q);
    cerr << "--gcd = " << h << endl;
    if (inExtension) {
      assert( foo.mode != modeGF );
      struct enter_M2 bar;
      algebraicElement_M2 = RingElement::make_raw(P,P->var(P->n_vars()-1)); // the algebraic generator is always the last variable in M2, the first one in factory
    }
    ret = convertToM2(P,h);
    if (error()) { algebraicElement_M2 = NULL ; return NULL; }
  }
  ring_elem a = P->getNumeratorRing()->preferred_associate_divisor(ret->get_value()); // an element in the coeff ring
  ring_elem b = P->getCoefficients()->invert(a);
  ring_elem r = ret->get_value();
  P->mult_coeff_to(b, r);
  algebraicElement_M2 = NULL ; 
  return RingElement::make_raw(P,r);
}

const RingElementOrNull *rawExtendedGCDRingElement(const RingElement *f, const RingElement *g, const RingElement **A, const RingElement **B)
{
  const bool inExtension = false;
  const RingElement *ret;
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *P2 = g->get_ring()->cast_to_PolynomialRing();
  *A = NULL;
  *B = NULL;
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
  CanonicalForm p = convertToFactory(*f,inExtension);
  CanonicalForm q = convertToFactory(*g,inExtension);
  CanonicalForm a, b;
  CanonicalForm h = extgcd(p,q,a,b);
  ret = convertToM2(P,h);
  if (error()) return NULL;
  *A = convertToM2(P,a);
  if (error()) return NULL;
  *B = convertToM2(P,b);
  if (error()) return NULL;
  return ret;
}

const RingElementOrNull *rawPseudoRemainder(const RingElement *f, const RingElement *g)
{
  const bool inExtension = false;
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
  CanonicalForm p = convertToFactory(*f,inExtension);
  CanonicalForm q = convertToFactory(*g,inExtension);
  CanonicalForm h = Prem(p,q);
  const RingElement *r = convertToM2(P,h);
  if (error()) return NULL;
  return r;
}

void rawFactor(const RingElement *g, 
	       RingElement_array_OrNull **result_factors, 
	       M2_arrayint_OrNull *result_powers)
{
  const bool inExtension = false;
     try {
	  const PolynomialRing *P = g->get_ring()->cast_to_PolynomialRing();
	  *result_factors = 0;
	  *result_powers = 0;
	  if (P == 0) {
	       ERROR("expected polynomial ring");
	       return;
	  }
	  struct enter_factory foo(P);
	  if (foo.mode == modeError) return;
	  CanonicalForm h = convertToFactory(*g,inExtension);
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
	  if (error()) *result_factors = NULL, *result_powers = NULL;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return;
     }
}

M2_arrayint_OrNull rawIdealReorder(const Matrix *M)
{
  const bool inExtension = false;
     try {
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
		    I.append(convertToFactory(*g,inExtension));
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
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}    

Matrix_array_OrNull * rawCharSeries(const Matrix *M)
{
  const bool inExtension = false;
     try {
	     const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
	     if (P == 0) {
		  ERROR("expected polynomial ring");
		  return 0;
	     }

	     CFList I;
	     for (int i = 0; i < M->n_rows(); i++) {
		  for (int j=0; j < M->n_cols(); j++) {
		    const RingElement *g = RingElement::make_raw(P, M->elem(i,j));
		    I.append(convertToFactory(*g,inExtension));
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
		    if (error()) return NULL;
		  }
		  struct enter_M2 b;
		  result->array[next++] = IM2_Matrix_make1(M->rows(), u.length(), result1, false);
	     }

	     return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

void rawDummy(void) {
  struct enter_factory foo;
  // we use this routine just for testing bits of stand-alone factory code
  On( SW_RATIONAL );
  setCharacteristic(101);
  Variable a('a');
  Variable x('x');
  CanonicalForm mipo = a*a - 2;
  cout << "a^2     = " << a*a << endl;
  CanonicalForm f = (x+a)*(x+a+1)*(x+a+1)*(x+a+34);
  cout << "f = " << f << endl;
  cout << "x.level() = " << x.level() << endl ;
  cout << "a.level() = " << a.level() << endl ;
  CFFList fac = Factorize(f,mipo);
  cout << "fac = " << fac << endl;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
