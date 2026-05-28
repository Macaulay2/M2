// Copyright 2012 Michael E. Stillman

#ifndef _aring_CCi_hpp_
#define _aring_CCi_hpp_

#include <iostream>

#include <mpfi.h>
#include "interface/random.h"
#include "interface/gmp-util.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"
#include "aring-RRR.hpp"
#include "aring-RRi.hpp"
#include "aring-CCC.hpp"

class CCi;
class RingMap;

namespace M2 {
/**
\ingroup rings
*/
class ARingCCi : public SimpleARing<ARingCCi>
{
  // Higher precision real intervals

 public:
  static const RingID ringID = ring_CCi;

  typedef cci_struct elem;
  typedef elem ElementType;

  ARingCCi(unsigned long precision) : mPrecision(precision) {}
  // ring informational
  size_t characteristic() const { return 0; }
  unsigned long get_precision() const { return mPrecision; }
  void text_out(buffer &o) const;

  unsigned int computeHashValue(const elem &a) const
  {
      double d1 = mpfr_get_d(&a.re.left, MPFR_RNDN);
      double d2 = mpfr_get_d(&(a.re.right), MPFR_RNDN);
      double d3 = mpfr_get_d(&(a.im.left), MPFR_RNDN);
      double d4 = mpfr_get_d(&(a.im.right), MPFR_RNDN);
      double d = 12347. * d1 + 865800. * d2 + 72158. * d3 + 86429. * d4;
    return static_cast<unsigned int>(d);
  }

  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

    bool is_unit(const ElementType &f) const { return !is_zero(f); }
    bool is_zero(const ElementType &f) const { return mpfr_cmp_si(&(f.im.left), 0) == 0 and mpfr_cmp_si(&(f.im.right), 0) == 0 and
        mpfr_cmp_si(&(f.re.left), 0) == 0 and
        mpfr_cmp_si(&(f.re.right), 0) == 0; }
    bool is_equal(const ElementType &f, const ElementType &g) const
    { return
        mpfr_cmp(&(f.re.left), &(g.re.left)) == 0 and mpfr_cmp(&(f.re.right), &(g.re.right)) == 0 and
            mpfr_cmp(&(f.im.left), &(g.im.left)) == 0 and mpfr_cmp(&(f.im.right), &(g.im.right)) == 0;
  }

  int compare_elems(const ElementType &f, const ElementType &g) const
  {
    int cmp = mpfi_cmp(&f.re, &g.re);
    if (cmp < 0) return -1;
    if (cmp > 0) return 1;
    cmp = mpfi_cmp(&f.im, &g.im);
    if (cmp < 0) return -1;
    if (cmp > 0) return 1;
    return 0;
  }

    bool is_empty(const ElementType &f) const { return mpfi_is_empty(&f.re)>0 || mpfi_is_empty(&f.im)>0; }
    bool is_member(const ARingCCC::ElementType &a, const ElementType &f) const { return mpfi_cmp_fr(&f.re,&a.re) == 0 && mpfi_cmp_fr(&f.im,&a.im) == 0; }
    bool is_member(const ARingRRi::ElementType &a, const ElementType &f) const { return mpfi_cmp(&f.re,&a) == 0 && mpfi_cmp_si(&f.im,0); }
    bool is_member(const ARingRRR::ElementType &a, const ElementType &f) const { return mpfi_cmp_fr(&f.re,&a) == 0 && mpfi_cmp_si(&f.im,0); }
    bool is_member(mpq_srcptr a, const ElementType &f) const { return mpfi_cmp_q(&f.re,a) == 0 && mpfi_cmp_si(&f.im,0); }
    bool is_member(mpz_srcptr a, const ElementType &f) const { return mpfi_cmp_z(&f.re,a) == 0 && mpfi_cmp_si(&f.im,0); }
    bool is_member(long a, const ElementType &f) const { return mpfi_cmp_si(&f.re,a) == 0 && mpfi_cmp_si(&f.im,0); }
    bool is_member(double a, const ElementType &f) const { return mpfi_cmp_d(&f.re,a) == 0 && mpfi_cmp_si(&f.im,0); }
    
    bool is_subset(const ElementType &g, const ElementType &f) const { return mpfi_cmp_fr(&f.re,&(g.re.left)) == 0 and mpfi_cmp_fr(&f.re,&(g.re.right)) == 0 and mpfi_cmp_fr(&f.im,&(g.im.left)) == 0 and mpfi_cmp_fr(&f.im,&(g.im.right)) == 0; }

  ////////////////////////////
  // to/from ringelem ////////
  ////////////////////////////
  // These simply repackage the element as either a ringelem or an
  // 'ElementType'.
  // No reinitialization is done.
  // Do not take the same element and store it as two different ring_elem's!!
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
      cci_ptr res = getmemstructtype(cci_ptr);
      mpfi_init2(&res->re,mPrecision);
      mpfi_init2(&res->im,mPrecision);
      mpfi_set(&res->re,&a.re);
      mpfi_set(&res->im,&a.im);
      mpfi_reallocate_limbs(&res->re);
      mpfi_reallocate_limbs(&res->im);
      result = ring_elem(res);
  }

  void from_ring_elem(ElementType &result, const ring_elem &a) const
  {
      mpfi_set(&result.re, &a.get_cci()->re);
      mpfi_set(&result.im, &a.get_cci()->im);
  }

  const ElementType &from_ring_elem_const(const ring_elem &a) const
  {
      return *a.get_cci();
  }
  // 'init', 'init_set' functions

  void init(ElementType &result) const {
      mpfi_init2(&result.re, mPrecision);
      mpfi_init2(&result.im, mPrecision);
  }
  void init_set(ElementType &result, const ElementType &a) const
  {
    init(result);
    mpfi_set(&result.re, &a.re);
    mpfi_set(&result.im, &a.im);
  }

  void set(ElementType &result, const ElementType &a) const
  {
    mpfi_set(&result.re, &a.re);
    mpfi_set(&result.im, &a.im);
  }

  void set(ElementType &result, const gmp_CCi a) const
  {
    mpfi_set(&result.re, a->re);
    mpfi_set(&result.im, a->im);
  }

  void set_zero(ElementType &result) const
  {
    mpfi_set_si(&result.re, 0);
    mpfi_set_si(&result.im, 0);
  }

  static void clear(ElementType &result) {
      mpfi_clear(&result.re);
      mpfi_clear(&result.im);
  }
  void copy(ElementType &result, const ElementType &a) const
  {
    mpfi_set(&result.re, &a.re);
    mpfi_set(&result.im, &a.im);
  }

  void set_from_long(ElementType &result, long a) const
  {
    mpfi_set_si(&result.re, a);
    mpfi_set_si(&result.im, 0);
  }

  void set_var(ElementType &result, int v) const
  {
    mpfi_set_si(&result.re, v);
    mpfi_set_si(&result.im, 0);
  }

  void set_from_mpz(ElementType &result, mpz_srcptr a) const
  {
    mpfi_set_z(&result.re, a);
    mpfi_set_si(&result.im, 0);
  }

  bool set_from_mpq(ElementType &result, mpq_srcptr a) const
  {
    mpfi_set_q(&result.re, a);
    mpfi_set_si(&result.im, 0);
    return true;
  }

  bool set_from_double(ElementType &result, double a) const
  {
    mpfi_set_d(&result.re, a);
    mpfi_set_si(&result.im, 0);
    return true;
  }
    
  bool set_from_BigReal(ElementType &result, gmp_RR a) const
  {
    mpfi_set_fr(&result.re, a);
    mpfi_set_si(&result.im, 0);
    return true;
  }
    
  bool set_from_Interval(ElementType &result, gmp_RRi a) const
  {
    mpfi_set(&result.re, a);
    mpfi_set_si(&result.im, 0);
    return true;
  }

  bool set_from_BigComplex(ElementType &result, gmp_CC a) const
  {
    mpfi_set_fr(&result.re, a->re);
    mpfi_set_fr(&result.im, a->im);
    return true;
  }

  bool set_from_BigComplex(ElementType &result, const cc_struct * a) const
  {
    mpfi_set_fr(&result.re, &a->re);
    mpfi_set_fr(&result.im, &a->im);
    return true;
  }

  bool set_from_complex_double(ElementType &result, double re, double im) const
  {
    mpfi_set_d(&result.re, re);
    mpfi_set_d(&result.im, im);
    return true;
  }

  bool set_from_ComplexInterval(ElementType &result, gmp_CCi a) const
  {
    mpfi_set(&result.re, a->re);
    mpfi_set(&result.im, a->im);
    return true;
  }

  bool set_from_ComplexInterval(ElementType &result, ElementType &a) const
  {
    mpfi_set(&result.re, &a.re);
    mpfi_set(&result.im, &a.im);
    return true;
  }

  void set_from_BigReals(ElementType& result, gmp_RR re, gmp_RR im) const
    {
      mpfi_set_fr(&result.re, re);
      mpfi_set_fr(&result.im, im);
    }
  void set_from_doubles(ElementType& result, double re, double im) const
    {
      mpfi_set_d(&result.re, re);
      mpfi_set_d(&result.im, im);
    }

  const ARingRRi::ElementType& realPartReference(const ElementType& a) const
    {
      return a.re;
    }
  const ARingRRi::ElementType& imaginaryPartReference(
        const ElementType& a) const
    {
      return a.im;
    }
  void set_real_part(ElementType& c, ARingRRi::ElementType& a) const
    {
      mpfi_set(&c.re, &a);
    }
  void set_imaginary_part(ElementType& c, ARingRRi::ElementType& a) const
    {
      mpfi_set(&c.im, &a);
    }

  // arithmetic
  void negate(ElementType &result, const ElementType &a) const
  {
    mpfi_mul_si(&result.re, &a.re, -1);
    mpfi_mul_si(&result.im, &a.im, -1);
  }

  void invert(ElementType &result, const ElementType &a) const
  // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
  {
    mpfi_t norm, temp;
    mpfi_init2(norm,get_precision());
    mpfi_init2(temp,get_precision());
    mpfi_set(norm,&a.re);
    mpfi_mul(norm,norm,norm);
    mpfi_set(temp,&a.im);
    mpfi_mul(temp,temp,temp);
    mpfi_add(norm,norm,temp);
    mpfi_set(&result.re,&a.re);
    mpfi_mul_si(&result.im,&a.im,-1);
    mpfi_div(&result.re,&result.re,norm);
    mpfi_div(&result.im,&result.im,norm);
  }

  void add(ElementType &result,
           const ElementType &a,
           const ElementType &b) const
  {
    mpfi_add(&result.re, &a.re, &b.re);
    mpfi_add(&result.im, &a.im, &b.im);
  }

  
   void addMultipleTo(ElementType &result,
                     const ElementType &a,
                     const ElementType &b) const
  {
      ElementType ab;
      init(ab);
      mult(ab,a,b);
      add(result,result,ab);
  }


  void subtract(ElementType &result,
                const ElementType &a,
                const ElementType &b) const
  {
    mpfi_sub(&result.re, &a.re, &b.re);
    mpfi_sub(&result.im, &a.im, &b.im);
  }

  void subtract_multiple(ElementType &result,
                         const ElementType &a,
                         const ElementType &b) const
  {
    // result -= a*b
    ElementType ab;
    init(ab);
    mult(ab, a, b);
    subtract(result, result, ab);
    clear(ab);
  }

  void mult(ElementType &result,
            const ElementType &a,
            const ElementType &b) const
  {
    mpfi_t temp, retemp, imtemp;
    mpfi_init2(temp,get_precision());
    mpfi_init2(retemp,get_precision());
    mpfi_init2(imtemp,get_precision());
    mpfi_mul(retemp,&a.re,&b.re);
    mpfi_mul(temp,&a.im,&b.im);
    mpfi_sub(retemp,retemp,temp);
    mpfi_mul(temp,&a.re,&b.im);
    mpfi_mul(imtemp,&a.im,&b.re);
    mpfi_add(&result.im,imtemp,temp);
    mpfi_set(&result.re,retemp);
  }

  void divide(ElementType &result,
              const ElementType &a,
              const ElementType &b) const
  {
    // result -= a*b
    ElementType b_inv;
    init(b_inv);
    invert(b_inv,b);
    mult(result,a,b_inv);
    clear(b_inv);
  }

  void power(ElementType &result, const ElementType &a, int n) const
  {
    if (n >= 2)
      {
          ElementType b;
          init(b);
          if (n%2 == 0)
          {
              power(b,a,n/2);
              mult(result,b,b);
          }
          else
          {
              power(b,a,n-1);
              mult(result,a,b);
          }
      }
    else if (n == 1)
    {
      mpfi_set(&result.re,&a.re);
      mpfi_set(&result.im,&a.im);
    }
    else if (n == 0)
    {
      mpfi_set_si(&result.re,1);
      mpfi_set_si(&result.im,0);
    }
    else if (n<0)
        throw 20;
  }

  /* Not entirely sure how to deal with this one. */
   void power_mpz(ElementType &result, const ElementType &a, mpz_srcptr n) const
  {
      if (mpz_cmp_si(n,2)>=0)
      {
          mpz_t r;
          mpz_init(r);
          mpz_fdiv_r_ui(r,n,2);
          
          ElementType b;
          init(b);
          
          mpz_t m;
          mpz_init(m);
          
          if (mpz_cmp_si(r,0) == 0)
          {
              mpz_cdiv_q_ui(m,n,2);
              
              power_mpz(b,a,m);
              mult(result,b,b);
          }
          else
          {
              mpz_sub_ui(m,n,1);
              
              power_mpz(b,a,m);
              mult(result,a,b);
          }
          mpz_clear(r);
          mpz_clear(m);
      }
      else if (mpz_cmp_si(n,1)==0)
      {
          mpfi_set(&result.re,&a.re);
          mpfi_set(&result.im,&a.im);
      }
      else if (mpz_cmp_si(n,0)==0)
      {
          mpfi_set_si(&result.re,1);
          mpfi_set_si(&result.im,0);
      }
      else if (mpz_cmp_si(n,0)<0)
          throw 20;
  }

  void swap(ElementType &a, ElementType &b) const {
      mpfi_swap(&a.re, &b.re);
      mpfi_swap(&a.im, &b.im);
  }
    
  void midpoint(ARingCCC::ElementType &a, const ElementType &b) const {
      mpfi_mid(&a.re,&b.re);
      mpfi_mid(&a.im,&b.im);
  }
    
  void diameter(ARingRRi::ElementType &a, const ElementType &b) const {
      mpfi_t temp;
      mpfi_set(&a,&b.re);
      mpfi_sqr(&a,&a);
      mpfi_set(temp,&b.im);
      mpfi_sqr(temp,temp);
      mpfi_add(&a,&a,temp);
      mpfi_sqrt(&a,&a);
  }
    
  void elem_text_out(buffer &o,
                     const ElementType &a,
                     bool p_one,
                     bool p_plus,
                     bool p_parens) const;

  void syzygy(const ElementType &a,
              const ElementType &b,
              ElementType &x,
              ElementType &y) const  // remove?
  // returns x,y s.y. x*a + y*b == 0.
  // if possible, x is set to 1.
  // no need to consider the case a==0 or b==0.
  {
    set_var(x, 0);  // set x=1
    if (!is_zero(b))
      {
        set(y, a);
        negate(y, y);
        divide(y, y, b);
      }
  }

    /* rewrite this (in rand.cpp or just copy over?) */
  void random(ElementType &result) const  // redo?
  {
      mpfr_t val;
      mpfr_init2(val, mPrecision);
      randomMpfr(val);
      mpfi_set_fr(&result.re,val);
      
      randomMpfr(val);
      mpfi_put_fr(&result.re,val);

      randomMpfr(val);
      mpfi_set_fr(&result.im,val);

      randomMpfr(val);
      mpfi_put_fr(&result.im,val);
      mpfr_clear(val);
  }

    /* Needs to be redone. */
  void eval(const RingMap *map,
            const ElementType &f,
            int first_var,
            ring_elem &result) const
  {
      gmp_CCi_struct g;
      g.re = &f.re;
      g.im = &f.im;
      if (!map->get_ring()->from_ComplexInterval(&g, result))
      {
           result = map->get_ring()->from_long(0);
           ERROR("cannot coerce RRi value to ring type");
      }
  }

/* Not ready */
  void zeroize_tiny(gmp_RR epsilon, ElementType &a) const
  {
      throw 20;
    //if (mpfr_cmpabs(&a, epsilon) < 0) set_zero(a);
  }
    /* Not ready */
  void increase_norm(gmp_RRmutable norm, const ElementType &a) const
  {
      throw 20;
   /* if (mpfr_cmpabs(&a, norm) > 0)
      {
        set(*norm, a);
        abs(*norm, *norm);
      }*/
  }
    
  void abs(ElementType &result, const ElementType &a) const
  {
      mpfi_t temp;
      mpfi_set(&result.re,&a.re);
      mpfi_sqr(&result.re,&result.re);
      mpfi_set(temp,&a.im);
      mpfi_sqr(temp,temp);
      mpfi_add(&result.re,&result.re,temp);
      mpfi_sqrt(&result.re,&result.re);
      mpfi_set_si(&result.im,0);
  }

  void abs_squared(ElementType &result, const ElementType &a) const
  {
      abs(result,a);
      mult(result, result, result);
  }

    gmp_CC toBigComplex(const ElementType &a) const
  {
    gmp_CCmutable result = getmemstructtype(gmp_CCmutable);
    result->re = getmemstructtype(gmp_RRmutable);
    result->im = getmemstructtype(gmp_RRmutable);
    mpfr_init2(result->re, get_precision());
    mpfr_init2(result->im, get_precision());
    mpfi_get_fr(result->re, &a.re);
    mpfi_get_fr(result->im, &a.im);
    return moveTo_gmpCC(result);
  }

 private:
  unsigned long mPrecision;
};

};  // end namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
