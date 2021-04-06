// Copyright 2012 Michael E. Stillman

#ifndef _aring_RRi_hpp_
#define _aring_RRi_hpp_

#include <iostream>

#include <mpfi.h>
#include "interface/random.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"
#include "aring-RRR.hpp"

class RRi;
class RingMap;

namespace M2 {
/**
\ingroup rings
*/
class ARingRRi : public RingInterface
{
  // Higher precision real intervals

 public:
  static const RingID ringID = ring_RRi;

  typedef __mpfi_struct elem;
  typedef elem ElementType;

  ARingRRi(unsigned long precision) : mPrecision(precision) {}
  // ring informational
  size_t characteristic() const { return 0; }
  unsigned long get_precision() const { return mPrecision; }
  void text_out(buffer &o) const;

  unsigned int computeHashValue(const elem &a) const
  {
    double d = 12347. * mpfr_get_d(&(a.left), GMP_RNDN) + 865800. * mpfr_get_d(&(a.right), GMP_RNDN);
    return static_cast<unsigned int>(d);
  }

  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

  bool is_unit(const ElementType &f) const { return !is_zero(f); }
  bool is_zero(const ElementType &f) const { return mpfr_cmp_si(&(f.left), 0) == 0 and mpfr_cmp_si(&(f.right), 0) == 0; }
  bool is_equal(const ElementType &f, const ElementType &g) const
  {
    return mpfr_cmp(&(f.left), &(g.left)) == 0 and mpfr_cmp(&(f.right), &(g.right)) == 0;
  }

  int compare_elems(const ElementType &f, const ElementType &g) const
  {
    int cmp = mpfi_cmp(&f, &g);
    if (cmp < 0) return -1;
    if (cmp > 0) return 1;
    return 0;
  }

    bool is_empty(const ElementType &f) const { return mpfi_is_empty(&f)>0; }
    
    bool is_member(const ARingRRR::ElementType &a, const ElementType &f) const { return mpfi_cmp_fr(&f,&a) == 0; }
    bool is_member(mpq_srcptr a, const ElementType &f) const { return mpfi_cmp_q(&f,a) == 0; }
    bool is_member(mpz_srcptr a, const ElementType &f) const { return mpfi_cmp_z(&f,a) == 0; }
    bool is_member(long a, const ElementType &f) const { return mpfi_cmp_si(&f,a) == 0; }
    bool is_member(double a, const ElementType &f) const { return mpfi_cmp_d(&f,a) == 0; }
    
    bool is_subset(const ElementType &g, const ElementType &f) const { return mpfi_cmp_fr(&f,&(g.left)) == 0 and mpfi_cmp_fr(&f,&(g.right)) == 0; }

  ////////////////////////////
  // to/from ringelem ////////
  ////////////////////////////
  // These simply repackage the element as either a ringelem or an
  // 'ElementType'.
  // No reinitialization is done.
  // Do not take the same element and store it as two different ring_elem's!!
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
      mpfi_ptr res = getmemstructtype(mpfi_ptr);
      mpfi_init2(res,mPrecision);
      mpfi_set(res, &a);
      result = ring_elem(moveTo_gmpRRi(res));
  }

  void from_ring_elem(ElementType &result, const ring_elem &a) const
  {
      mpfi_set(&result, a.get_mpfi());
  }

  // 'init', 'init_set' functions

  void init(ElementType &result) const { mpfi_init2(&result, mPrecision); }
  void init_set(ElementType &result, const ElementType &a) const
  {
    init(result);
    mpfi_set(&result, &a);
  }

  void set(ElementType &result, const ElementType &a) const
  {
    mpfi_set(&result, &a);
  }

  void set_zero(ElementType &result) const
  {
    mpfi_set_si(&result, 0);
  }

  void clear(ElementType &result) const { mpfi_clear(&result); }
  void copy(ElementType &result, const ElementType &a) const
  {
    mpfi_set(&result, &a);
  }

  void set_from_long(ElementType &result, long a) const
  {
    mpfi_set_si(&result, a);
  }

  void set_var(ElementType &result, int v) const
  {
    mpfi_set_si(&result, v);
  }

  void set_from_mpz(ElementType &result, mpz_srcptr a) const
  {
    mpfi_set_z(&result, a);
  }

  bool set_from_mpq(ElementType &result, mpq_srcptr a) const
  {
    mpfi_set_q(&result, a);
    return true;
  }

  bool set_from_double(ElementType &result, double a) const
  {
    mpfi_set_d(&result, a);
    return true;
  }
    
  bool set_from_BigReal(ElementType &result, gmp_RR a) const
  {
    mpfi_set_fr(&result, a);
    return true;
  }
    
  bool set_from_Interval(ElementType &result, gmp_RRi a) const
  {
    mpfi_set(&result, a);
    return true;
  }

  // arithmetic
  void negate(ElementType &result, const ElementType &a) const
  {
    mpfi_mul_si(&result, &a, -1);
  }

  void invert(ElementType &result, const ElementType &a) const
  // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
  {
    mpfi_si_div(&result, 1, &a);
  }

  void add(ElementType &result,
           const ElementType &a,
           const ElementType &b) const
  {
    mpfi_add(&result, &a, &b);
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
    mpfi_sub(&result, &a, &b);
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
    mpfi_mul(&result, &a, &b);
  }

  void divide(ElementType &result,
              const ElementType &a,
              const ElementType &b) const
  {
    mpfi_div(&result, &a, &b);
  }

  void power(ElementType &result, const ElementType &a, int n) const
  {
    if (n >= 2)
      {
          if (n%2 == 0)
          {
              ElementType b;
              init(b);
              power(b,a,n/2);
              mult(result,b,b);
          }
          else
          {
              ElementType b;
              init(b);
              power(b,a,n-1);
              mult(result,a,b);
          }
      }
    else if (n == 1)
      mpfi_set(&result,&a);
    else if (n == 0)
      mpfi_set_si(&result,1);
    else if (n<0)
        throw 20;
  }

  /* Not entirely sure how to deal with this one. */
   void power_mpz(ElementType &result, const ElementType &a, mpz_srcptr n) const
  {
      if (mpz_cmp_si(n,2)>=0)
      {
          mpz_ptr r = getmemstructtype(mpz_ptr);
          mpz_init(r);
          mpz_fdiv_r_ui(r,n,2);
          
          ElementType b;
          init(b);
          
          mpz_ptr m = getmemstructtype(mpz_ptr);
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
      }
      else if (mpz_cmp_si(n,1)==0)
          mpfi_set(&result,&a);
      else if (mpz_cmp_si(n,0)==0)
          mpfi_set_si(&result,1);
      else if (mpz_cmp_si(n,0)<0)
          throw 20;
  }

  void swap(ElementType &a, ElementType &b) const { mpfi_swap(&a, &b); }
    
  void midpoint(ARingRRR::ElementType &a, const ElementType &b) const { mpfi_mid(&a,&b); }
    
  void diameter(ARingRRR::ElementType &a, const ElementType &b) const { mpfi_diam_abs(&a,&b); }
    
  void left(ARingRRR::ElementType &a, const ElementType &b) const { mpfi_get_left(&a,&b); }
    
  void right(ARingRRR::ElementType &a, const ElementType &b) const { mpfi_get_right(&a,&b); }
    
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
      mpfi_set_fr(&result,val);
      
      randomMpfr(val);
      mpfi_put_fr(&result,val);
      mpfr_clear(val);
  }

    /* Needs to be redone. */
  void eval(const RingMap *map,
            ElementType &f,
            int first_var,
            ring_elem &result) const
  {
      if (!map->get_ring()->from_Interval(&f, result))
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
      mpfi_abs(&result,&a);
  }

  void abs_squared(ElementType &result, const ElementType &a) const
  {
      abs(result,a);
      mult(result, result, result);
  }

  double coerceToDouble(const ElementType &a) const
  {
    return mpfi_get_d(&a);
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
