// copyright Daniel R. Grayson, 1995

#define Matrix MaTrIx
#include <factor.h>		// from Messollen's libfac
#undef Matrix

#include <templates/ftmpl_list.cc>

#include <assert.h>
#include <iostream>
#include "matrix.hpp"
#include "z_mod_p.hpp"
#include "ZZ.hpp"
#include "frac.hpp"

#include "relem.hpp"
#include "../d/M2mem.h"

#include "text_io.hpp"
#include "buffer.hpp"

#ifdef drg
// debugging display routines to be called from gdb
// needs factory to be configured without option --disable-streamio 
void showvar(Variable &t) { cout << t << endl; }
void showcf(CanonicalForm &t) { cout << t << endl; }
void showcfl(CFList &t) { cout << t << endl; }
void showcffl(CFFList &t) { cout << t << endl; }
#endif

static const RingElement * convert(const Ring *R, CanonicalForm h) {
     const int n = R->n_vars();
     if (h.inCoeffDomain()) {
	  if (R->charac() == 0) {
	       static const unsigned int base = 1 << 16;
	       intarray v;
	       int sign;
	       if (h < 0) {
		    sign = -1;
		    h = -h;
	       }
	       else sign = 1;
	       while ( h != 0 ) {
		    CanonicalForm k = h % base;
		    v.append(k.intval());
		    h /= base;
	       }
	       mpz_t x;
	       mpz_init(x);
	       for (int i = v.length() - 1; i >= 0; i--) {
		    mpz_mul_ui(x,x,base); // x = x * base;
		    mpz_add_ui(x,x,(unsigned)v[i]);
	       }
	       if (sign == -1) mpz_neg(x,x); // x = -x;
	       ring_elem ret = R->from_int(x);
	       mpz_clear(x);
	       return RingElement::make_raw(R, ret);
	  }
	  else {
	    return RingElement::make_raw(R, R->from_int(h.intval()));
	  }
     }
     ring_elem result = R->from_int(0);
     for (int j = 0; j <= h.degree(); j++) {
       const RingElement *r = convert(R, h[j]);
       ring_elem r1 = r->get_value();
       ring_elem v = R->var((n-1) - (h.level()-1), j);
       r1 = R->mult(r1,v);
       R->add_to(result,r1);
     }
     return RingElement::make_raw(R,result);
}

static int base_set = 0;
static CanonicalForm base;

static CanonicalForm convert(const mpz_ptr p) {
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
     const Ring *F = R->Ncoeffs();
     if (F->cast_to_Z_mod() != 0) return FAC_ZZp;
     if (F->cast_to_ZZ() != 0) return FAC_ZZ;
     if (F->cast_to_QQ() != 0) return FAC_QQ;
     ERROR("expected coefficient ring of the form ZZ/n, ZZ, or QQ");
     return FAC_BAD_RING;
}

static CanonicalForm convert(const RingElement &g) {
     const Ring *R = g.get_ring();
     const int n = R->n_vars();
     const Ring *F = R->Ncoeffs();
     const Z_mod *Zn = F->cast_to_Z_mod();
     const ZZ *Z0 = F->cast_to_ZZ();
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
     const Monoid *M = R->Nmonoms();
     intarray vp;
     setCharacteristic(R->charac());
     if (Q != NULL) On( SW_RATIONAL );
     CanonicalForm f = 0;
     for (Nterm *t = g.get_value(); t != NULL; t = t->next) {
       vp.shrink(0);
       M->to_varpower(t->monom,vp);
       CanonicalForm m = (
			  Zn != NULL 
			  ?
			  CanonicalForm(Zn->to_int(t->coeff)) 
			  :
			  Z0 != NULL 
			  ?
			  convert(MPZ_VAL(t->coeff)) 
			  :
			  Q != NULL 
			  ?
			  convert(mpq_numref(MPQ_VAL(t->coeff)))
			  / convert(mpq_denref(MPQ_VAL(t->coeff)))
#if 0
			  convert(MPZ_VAL(FRAC_VAL(t->coeff)->numer))
			  / convert(MPZ_VAL(FRAC_VAL(t->coeff)->denom))
#endif
			  :
			  CanonicalForm(0) // shouldn't happen
			  );
       for (int l = 1; l < vp[0] ; l++) {
	 m *= power(
		    Variable(1 + (
				  (n-1) - varpower::var(vp[l]) // REVERSE !
				  )), 
		    varpower::exponent(vp[l])
		    );
       }
       f += m;
     }
     if (Q != NULL) Off( SW_RATIONAL );
     return f;
}

void displayCF(Ring *R, const CanonicalForm &h)
{
  buffer o;
  const RingElement *g = convert(R,h);
  o << IM2_RingElement_to_string(g);
  emit(o.str());
}

const RingElementOrNull *rawGCDRingElement(const RingElement *f, const RingElement *g)
{
#warning "check that the rings of f and g both polynomial rings"
  CanonicalForm p = convert(*f);
  CanonicalForm q = convert(*g);
  //     cerr << "p = " << p << endl
  //          << "q = " << q << endl;
  CanonicalForm h = gcd(p,q);
  return convert(f->get_ring(),h);
}

const RingElementOrNull *rawPseudoRemainder(const RingElement *f, const RingElement *g)
{
#warning "check that the rings of f and g both polynomial rings"
  CanonicalForm p = convert(*f);
  CanonicalForm q = convert(*g);
  //     cerr << "p = " << p << endl
  //          << "q = " << q << endl;
  CanonicalForm h = Prem(p,q);
  return convert(f->get_ring(),h);
}

void rawFactor(const RingElement *g, 
	       RingElement_array_OrNull **result_factors, 
	       M2_arrayint_OrNull *result_powers)
{
  factoryseed(23984729);
  const Ring *R = g->get_ring();
  CanonicalForm h = convert(*g);
  // displayCF(R,h);
#ifdef drg
  GC_gcollect();			// debugging
#endif
  CFFList q = Factorize(h);
#ifdef drg
  GC_gcollect();			// debugging
#endif
  int nfactors = q.length();

  *result_factors = (RingElement_array *) getmem(sizeofarray((*result_factors),nfactors));
  (*result_factors)->len = nfactors;

  *result_powers = makearrayint(nfactors);

  int next = 0;
  for (CFFListIterator i = q; i.hasItem(); i++) {
    (*result_factors)->array[next] = convert(R,i.getItem().factor());
    (*result_powers)->array[next++] = i.getItem().exp();
  }
}

M2_arrayint_OrNull rawIdealReorder(const Matrix *M)
{
     factoryseed(23984729);
     const Ring *R = M->get_ring();
     const int N = R->n_vars();

     if (get_ring_type(R) == FAC_BAD_RING) return 0; // error message already issued

     CFList I;
     int i;
     for (i = 0; i < M->n_rows(); i++) {
	  for (int j=0; j < M->n_cols(); j++) {
	    const RingElement *g = RingElement::make_raw(R, M->elem(i,j));
	    I.append(convert(*g));
	  }
     }


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

Matrix_array_OrNull * rawCharSeries(const Matrix *M)
{
     factoryseed(23984729);
     const Ring *R = M->get_ring();

     if (get_ring_type(R) == FAC_BAD_RING) return 0; // error message already issued

     CFList I;
     for (int i = 0; i < M->n_rows(); i++) {
	  for (int j=0; j < M->n_cols(); j++) {
	    const RingElement *g = RingElement::make_raw(R, M->elem(i,j));
	    I.append(convert(*g));
	  }
     }

     List<CFList> t = IrrCharSeries(I);

     Matrix_array *result = (Matrix_array *) getmem(sizeofarray(result,t.length()));
     result->len = t.length();
     
     int next = 0;
     for (ListIterator<List<CanonicalForm> > ii = t; ii.hasItem(); ii++) {
	  CFList u = ii.getItem();
	  RingElement_array *result1 = (RingElement_array *) getmem(sizeofarray(result1,u.length()));
	  result1->len = u.length();
	  int next1 = 0;
	  for (ListIterator<CanonicalForm> j = u; j.hasItem(); j++) {
	    result1->array[next1++] = convert(R,j.getItem());
	  }
	  result->array[next++] = IM2_Matrix_make1(M->rows(), u.length(), result1, false, false);
     }
     
     return result;
}

#if 0

static void gcd_ring_elem(object &ff, object &gg) {
     const RingElement &f = ff -> cast_to_RingElement();
     const RingElement &g = gg -> cast_to_RingElement();
     const Ring *R = f.get_ring();
     CanonicalForm p = convert(f);
     CanonicalForm q = convert(g);
     //     cerr << "p = " << p << endl
     //          << "q = " << q << endl;
     CanonicalForm h = gcd(p,q);
     gStack.insert(convert(R,h));
}

#if 0
static void extgcd_ring_elem(object &ff, object &gg) {
     const RingElement &f = ff -> cast_to_RingElement();
     const RingElement &g = gg -> cast_to_RingElement();
     const Ring *R = f.get_ring();
     CanonicalForm p, q;
     CanonicalForm h = extgcd(convert(f),convert(g),p,q);
     gStack.insert(convert(R,p));
     gStack.insert(convert(R,q));
     gStack.insert(convert(R,h));
}
#endif

static void pseudo_remainder(object &ff, object &gg) {
     const RingElement &f = ff -> cast_to_RingElement();
     const RingElement &g = gg -> cast_to_RingElement();
     const Ring *R = f.get_ring();
     CanonicalForm h = Prem(convert(f),convert(g));
     gStack.insert(convert(R,h));
}

static void factor_ring_elem(object &gg) {
     factoryseed(23984729);
     const RingElement &g = gg -> cast_to_RingElement();
     const Ring *R = g.get_ring();
     CanonicalForm h = convert(g);
     CFFList q = Factorize(h);
     for (CFFListIterator i = q; i.hasItem(); i++) {
	  gStack.insert(convert(R,i.getItem().factor()));
	  gStack.insert(new object_int(i.getItem().exp()));
     }
     gStack.insert(new object_int(q.length()));
}

static void ideal_reorder(object &mm) {
     factoryseed(23984729);
     const Matrix &m = mm -> cast_to_Matrix();
     const Ring *R = m.get_ring();
     const int N = R->n_vars();
     const Ring *F = R->Ncoeffs();
     const Z_mod *Zn = F->cast_to_Z_mod();
     const Z *Z0 = F->cast_to_Z();
     const FractionField *Q = (
			       NULL != F->cast_to_FractionField()
			       &&
			       NULL != F->cast_to_FractionField()->get_ring()->cast_to_Z()
			       ?
			       F->cast_to_FractionField()
			       :
			       (FractionField*) NULL
			       );
     if (Zn == NULL && Z0 == NULL && Q == NULL) {
	  gError << "expected coefficient ring of the form ZZ/n, ZZ, or QQ";
	  return;
     }
     CFList I;
	 int i,j;
     for (i = 0; i < m.n_rows(); i++) {
	  for (j=0; j < m.n_cols(); j++) {
	       RingElement g(R, m.elem(i,j));
	       I.append(convert(g));
	  }
     }
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
     gStack.insert(new object_intarray(u));
}    

static void ideal_charset(object &mm) {
     factoryseed(23984729);
     const Matrix &m = mm -> cast_to_Matrix();
     const Ring *R = m.get_ring();
     const Ring *F = R->Ncoeffs();
     const Z_mod *Zn = F->cast_to_Z_mod();
     const Z *Z0 = F->cast_to_Z();
     const FractionField *Q = (
			       NULL != F->cast_to_FractionField()
			       &&
			       NULL != F->cast_to_FractionField()->get_ring()->cast_to_Z()
			       ?
			       F->cast_to_FractionField()
			       :
			       (FractionField*) NULL
			       );
     if (Zn == NULL && Z0 == NULL && Q == NULL) {
	  gError << "expected coefficient ring of the form ZZ/n, ZZ, or QQ";
	  return;
     }
     CFList I;
     int i;
     for (i = 0; i < m.n_rows(); i++) {
	  for (int j=0; j < m.n_cols(); j++) {
	       RingElement g(R, m.elem(i,j));
	       I.append(convert(g));
	  }
     }
     List<CFList> t = IrrCharSeries(I);
     for (ListIterator<List<CanonicalForm> > ii = t; ii.hasItem(); ii++) {
	  CFList u = ii.getItem();
	  for (ListIterator<CanonicalForm> j = u; j.hasItem(); j++) {
	       gStack.insert(convert(R,j.getItem()));
	  }
	  gStack.insert(new object_int(u.length()));
     }
     gStack.insert(new object_int(t.length()));
}    

void i_factor_cmds() {
     install(ggfactor, factor_ring_elem, TY_RING_ELEM);
     install(ggfactor1, gcd_ring_elem, TY_RING_ELEM, TY_RING_ELEM);
     // install(ggfactor, extgcd_ring_elem, TY_RING_ELEM, TY_RING_ELEM);
     install(ggfactor2, pseudo_remainder, TY_RING_ELEM, TY_RING_ELEM);
     install(ggfactor1, ideal_reorder, TY_MATRIX);
     install(ggfactor2, ideal_charset, TY_MATRIX);
}

#else

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
