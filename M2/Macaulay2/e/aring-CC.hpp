// Copyright 2012 Michael E. Stillman

#ifndef _aring_CC_hpp_
#define _aring_CC_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"

class RingMap;

namespace M2 {
/**
\ingroup rings
*/
  class ARingCC : public RingInterface
  {
    // approximate real numbers, implemented as doubles.
  public:
    static const RingID ringID = ring_CC;

    struct complex {
      double re;
      double im;
    };
    typedef complex* complex_struct_ptr;

    typedef complex elem;
    typedef elem ElementType;

    ARingCC() {}

    // ring informational
    size_t characteristic() const { return 0; }
    unsigned long get_precision() const { return 53; }
    void text_out(buffer &o) const;

    unsigned long computeHashValue(const elem& a) const 
    { 
      double v = 12347. * a.re + 865800. * a.im;
      return static_cast<unsigned long>(v);
    }

    /////////////////////////////////
    // ElementType informational ////
    /////////////////////////////////

    bool is_unit(const ElementType& f) const { 
      return !is_zero(f); 
    }

    bool is_zero(const ElementType& f) const {
      return f.re == 0.0 and f.im == 0.0;
    }
    
    bool is_equal(const ElementType& f, const ElementType& g) const { 
      return f.re == g.re and f.im == g.im;
    }

    int compare_elems(const ElementType& f, const ElementType& g) const {
      double cmp_re = f.re - g.re;
      if (cmp_re < 0) return -1;
      if (cmp_re > 0) return 1;
      double cmp_im = f.im - g.im;
      if (cmp_im < 0) return -1;
      if (cmp_im > 0) return 1;
      return 0;
    }

    ////////////////////////////
    // to/from ringelem ////////
    ////////////////////////////
    // These simply repackage the element as either a ringelem or an 'ElementType'.
    // No reinitialization is done.
    // Do not take the same element and store it as two different ring_elem's!!
    void to_ring_elem(ring_elem &result, const ElementType &a) const
    {
      complex* res = getmemstructtype(complex_struct_ptr);
      init(*res);
      set(*res, a);
      result.poly_val = reinterpret_cast<Nterm*>(res);
    }

    void from_ring_elem(ElementType &result, const ring_elem &a) const
    {
      result = * reinterpret_cast<complex*>(a.poly_val);
    }

    // 'init', 'init_set' functions

    void init(ElementType &result) const { 
      result.re = 0.0;
      result.im = 0.0;
    }

    void init_set(ElementType &result, const ElementType& a) const { 
      result = a;
    }

    void set(ElementType &result, const ElementType& a) const { 
      result = a;
    }

    void set_zero(ElementType &result) const { 
      result.re = 0.0;
      result.im = 0.0;
    }

    void clear(ElementType &result) const { 
      // do nothing
    }

    void copy(ElementType &result, const ElementType& a) const { 
      set(result,a); 
    }

    void set_from_int(ElementType &result, int a) const {
      result.re = a;
      result.im = 0.0;
    }

    void set_var(ElementType &result, int v) const { 
      set_from_int(result, 1);
    }

    void set_from_mpz(ElementType &result, mpz_ptr a) const {
      result.re = mpz_get_d(a);
      result.im = 0.0;
    }

    void set_from_mpq(ElementType &result, mpq_ptr a) const {
      result.re = mpq_get_d(a);
      result.im = 0.0;
    }

    bool set_from_BigReal(ElementType &result, gmp_RR a) const {
      result.re = mpfr_get_d(a, GMP_RNDN);
      result.im = 0.0;
      return true;
    }

    bool set_from_BigComplex(ElementType &result, gmp_CC a) const {
      result.re = mpfr_get_d(a->re, GMP_RNDN);
      result.im = mpfr_get_d(a->im, GMP_RNDN);
      return true;
    }

    // arithmetic
    void negate(ElementType &result, const ElementType& a) const
    {
      result.re = -a.re;
      result.im = -a.im;
    }

    void invert(ElementType &result, const ElementType& a) const
      // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
    {
      if (fabs(a.re) >= fabs(a.im))
        {
          double p = a.im/a.re;
          double denom = a.re + p * a.im;
          result.re = 1.0/denom;
          result.im = - p/denom;
        }
      else
        {
          double p = a.re/a.im;
          double denom = a.im + p * a.re;
          result.re = p/denom;
          result.im = -1.0/denom;
        }
    }

    void add(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result.re = a.re + b.re;
      result.im = a.im + b.im;
    }

    void subtract(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result.re = a.re - b.re;
      result.im = a.im - b.im;
    }

    void subtract_multiple(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result -= a*b;
    }

    void mult(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result = a*b;
    }

    void divide(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result = a/b;
    }

    void power(ElementType &result, const ElementType& a, int n) const
    {
      // TODO
    }

    void power_mpz(ElementType &result, const ElementType& a, mpz_ptr n) const
    {
      // TODO
    }

    void swap(ElementType &a, ElementType &b) const
    {
      std::swap(a,b);
    }

    void elem_text_out(buffer &o,
                       ElementType &a, //const???
                       bool p_one,
                       bool p_plus,
                       bool p_parens) const;

    void syzygy(const ElementType& a, const ElementType& b,
                ElementType &x, ElementType &y) const // remove?
    // returns x,y s.y. x*a + y*b == 0.
    // if possible, x is set to 1.
    // no need to consider the case a==0 or b==0.
    {
      // TODO: remove this?
      set_var(x,0); //set x=1
      if(!is_zero(b))
        {
          set(y,a);
          negate(y,y);
          divide(y,y,b);
        }
    }

    void random(ElementType &result) const // redo?
    {
      mpfr_t val;
      mpfr_init2(val, 53);
      rawRandomMpfr(val, 53);
      result = mpfr_get_d(val, GMP_RNDN);
      mpfr_clear(val);
    }

    void eval(const RingMap *map, ElementType &f, int first_var, ring_elem &result) const
    {
      // TODO
      // map->get_ring()->from_BigReal(&f, result);
    }

    void zeroize_tiny(gmp_RR epsilon, ElementType &a) const
    {
      if (fabs(a.re) < epsilon) a.re = 0.0;
      if (fabs(a.im) < epsilon) a.im = 0.0;
    }
  };

}; // end namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
