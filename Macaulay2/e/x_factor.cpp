// copyright Daniel R. Grayson, 1995

//#if !defined(__MWERKS__)
#if 1
#include <assert.h>
#include <iostream.h>
#include "interp.hpp"
#include "matrix.hpp"
#include "z_mod_p.hpp"
#include "Z.hpp"
#include "frac.hpp"

ostream &operator<<(ostream &o,const intarray &w) {
     o << hex;
     for (int k = 0; ; ) {
	  o << w[k];
	  if (++k >= w.length()) break;
	  o << ' ';
     }
     o << dec;
     return o;
}

#define Matrix MaTrIx
/* #include <factory.h> */
#include <factor.h>		// from Messollen
#include <templates/functions.h>
#undef Matrix

static RingElement convert(const Ring *R, CanonicalForm h) {
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
	       RingElement ret(R,x);
	       mpz_clear(x);
	       return ret;
	  }
	  else {
	       return RingElement(R,h.intval());
	  }
     }
     RingElement q(R,0);
     for (int j = 0; j <= h.degree(); j++) {
	  q = q + (
		   convert(R,
			   h[j]		// coefficient
			   )
		   * 
		   RingElement(R,
			       (n-1)-(h.level()-1), // which variable, REVERSE!
			       j		    // exponent
			       )
		   );
     }
     return q;
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

#define FRAC_VAL(f) ((frac_elem *) (f).poly_val)

static CanonicalForm convert(const RingElement &g) {
     const Ring *R = g.Ring_of();
     const int n = R->n_vars();
     const Ring *F = R->Ncoeffs();
     const Z_mod *Zn = F->cast_to_Z_mod();
     const Z *Z0 = F->cast_to_Z();
     const FractionField *Q = (
			       NULL != F->cast_to_FractionField()
			       &&
			       NULL != F->cast_to_FractionField()->Ring_of()->cast_to_Z()
			       ?
			       F->cast_to_FractionField()
			       :
			       (FractionField*) NULL
			       );
     if (Zn == NULL && Z0 == NULL && Q == NULL) {
	  gError << "expected coefficient ring of the form ZZ/n, ZZ, or QQ";
	  return 0;
     }
     const Monoid *M = R->Nmonoms();
     intarray vp;
     setCharacteristic(R->charac());
     // if (Q != NULL) On( SW_RATIONAL );
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
			  convert(MPZ_VAL(FRAC_VAL(t->coeff)->numer))
			  /* / convert(MPZ_VAL(FRAC_VAL(t->coeff)->denom)) */
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
     // if (Q != NULL) Off( SW_RATIONAL );
     return f;
}

static void gcd_ring_elem(object &ff, object &gg) {
     const RingElement &f = ff -> cast_to_RingElement();
     const RingElement &g = gg -> cast_to_RingElement();
     const Ring *R = f.Ring_of();
     CanonicalForm h = gcd(convert(f),convert(g));
     gStack.insert(convert(R,h));
}

#if 0
static void extgcd_ring_elem(object &ff, object &gg) {
     const RingElement &f = ff -> cast_to_RingElement();
     const RingElement &g = gg -> cast_to_RingElement();
     const Ring *R = f.Ring_of();
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
     const Ring *R = f.Ring_of();
     CanonicalForm h = Prem(convert(f),convert(g));
     gStack.insert(convert(R,h));
}

static void factor_ring_elem(object &gg) {
     const RingElement &g = gg -> cast_to_RingElement();
     const Ring *R = g.Ring_of();
     CanonicalForm h = convert(g);
     CFFList q = Factorize(h);
     for (CFFListIterator i = q; i.hasItem(); i++) {
	  gStack.insert(convert(R,i.getItem().factor()));
	  gStack.insert(new object_int(i.getItem().exp()));
     }
     gStack.insert(new object_int(q.length()));
}

static void ideal_reorder(object &mm) {
     const Matrix &m = mm -> cast_to_Matrix();
     const Ring *R = m.Ring_of();
     const int N = R->n_vars();
     const Ring *F = R->Ncoeffs();
     const Z_mod *Zn = F->cast_to_Z_mod();
     const Z *Z0 = F->cast_to_Z();
     const FractionField *Q = (
			       NULL != F->cast_to_FractionField()
			       &&
			       NULL != F->cast_to_FractionField()->Ring_of()->cast_to_Z()
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
     for (i=(n-1)/2; i>=0; i--) { // REVERSE!
	  int tmp = u[n-1-i];
	  u[n-1-i] = u[i];
	  u[i] = tmp;
     }
     for (i=n; i<N; i++) u.append(i);
     gStack.insert(new object_intarray(u));
}    

static void ideal_charset(object &mm) {
     const Matrix &m = mm -> cast_to_Matrix();
     const Ring *R = m.Ring_of();
     const Ring *F = R->Ncoeffs();
     const Z_mod *Zn = F->cast_to_Z_mod();
     const Z *Z0 = F->cast_to_Z();
     const FractionField *Q = (
			       NULL != F->cast_to_FractionField()
			       &&
			       NULL != F->cast_to_FractionField()->Ring_of()->cast_to_Z()
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

void i_factor_cmds() {
}

#endif

//template int tmax(int const &, int const &);
//template int tmin(int const &, int const &);
//template CanonicalForm tmax(CanonicalForm const &, CanonicalForm const &);
//template CanonicalForm tmin(CanonicalForm const &, CanonicalForm const &);
