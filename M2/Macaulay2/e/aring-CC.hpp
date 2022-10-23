// Copyright 2012 Michael E. Stillman

#ifndef _aring_CC_hpp_
#define _aring_CC_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"

#include "aring-RR.hpp"

class RingMap;

namespace M2 {

/**
\ingroup rings
*/
class ARingCC : public RingInterface
{
  // approximate real numbers, implemented as doubles.

 private:
  const ARingRR mRR;  // reals with the same precision
 public:
  static const RingID ringID = ring_CC;

  typedef cc_doubles_struct elem;
  typedef elem ElementType;
  typedef ARingRR RealRingType;
  typedef RealRingType::ElementType RealElementType;

  ARingCC() {}
  // ring informational
  size_t characteristic() const { return 0; }
  unsigned long get_precision() const { return 53; }
  void text_out(buffer& o) const;

  const RealRingType& real_ring() const { return mRR; }
  unsigned int computeHashValue(const elem& a) const
  {
    double v = 12347. * a.re + 865800. * a.im;
    return static_cast<unsigned int>(v);
  }

  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

  bool is_unit(const ElementType& f) const { return !is_zero(f); }
  bool is_zero(const ElementType& f) const
  {
    return f.re == 0.0 and f.im == 0.0;
  }

  bool is_equal(const ElementType& f, const ElementType& g) const
  {
    return f.re == g.re and f.im == g.im;
  }

  int compare_elems(const ElementType& f, const ElementType& g) const
  {
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
  // These simply repackage the element as either a ringelem or an
  // 'ElementType'.
  // No reinitialization is done.
  // Do not take the same element and store it as two different ring_elem's!!
  void to_ring_elem(ring_elem& result, const ElementType& a) const
  {
    cc_doubles_ptr res = getmemstructtype(cc_doubles_ptr);
    *res = a;
    result = ring_elem(res);
  }

  void from_ring_elem(ElementType& result, const ring_elem& a) const
  {
    result = * a.get_cc_doubles();
  }

  // 'init', 'init_set' functions

  void init(ElementType& result) const
  {
    result.re = 0.0;
    result.im = 0.0;
  }

  void init_set(ElementType& result, const ElementType& a) const { result = a; }
  void set(ElementType& result, const ElementType& a) const { result = a; }
  void set_zero(ElementType& result) const
  {
    result.re = 0.0;
    result.im = 0.0;
  }

  void clear(ElementType& result) const
  {
    // do nothing
  }

  void copy(ElementType& result, const ElementType& a) const { set(result, a); }
  void set_from_long(ElementType& result, long a) const
  {
    result.re = static_cast<double>(a);
    result.im = 0.0;
  }

  void set_var(ElementType& result, int v) const { set_from_long(result, 1); }
  void set_from_mpz(ElementType& result, mpz_srcptr a) const
  {
    result.re = mpz_get_d(a);
    result.im = 0.0;
  }

  bool set_from_mpq(ElementType& result, mpq_srcptr a) const
  {
    result.re = mpq_get_d(a);
    result.im = 0.0;
    return true;
  }

  bool set_from_BigReal(ElementType& result, gmp_RR a) const
  {
    result.re = mpfr_get_d(a, MPFR_RNDN);
    result.im = 0.0;
    return true;
  }
  bool set_from_BigReals(ElementType& result, gmp_RR re, gmp_RR im) const
  {
    result.re = mpfr_get_d(re, MPFR_RNDN);
    result.im = mpfr_get_d(im, MPFR_RNDN);
    return true;
  }
  bool set_from_BigComplex(ElementType& result, gmp_CC a) const
  {
    result.re = mpfr_get_d(a->re, MPFR_RNDN);
    result.im = mpfr_get_d(a->im, MPFR_RNDN);
    return true;
  }
  bool set_from_double(ElementType& result, double a) const
  {
    result.re = a;
    result.im = 0;
    return true;
  }
  bool set_from_complex_double(ElementType& result, double re, double im) const
  {
    result.re = re;
    result.im = im;
    return true;
  }

  // arithmetic
  void negate(ElementType& result, const ElementType& a) const
  {
    result.re = -a.re;
    result.im = -a.im;
  }

  void invert(ElementType& res, const ElementType& a) const
  // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
  {
    ElementType result;
    if (fabs(a.re) >= fabs(a.im))
      {
        double p = a.im / a.re;
        double denom = a.re + p * a.im;
        result.re = 1.0 / denom;
        result.im = -p / denom;
      }
    else
      {
        double p = a.re / a.im;
        double denom = a.im + p * a.re;
        result.re = p / denom;
        result.im = -1.0 / denom;
      }
    set(res, result);
  }

  void add(ElementType& res, const ElementType& a, const ElementType& b) const
  {
    ElementType result;
    result.re = a.re + b.re;
    result.im = a.im + b.im;
    set(res, result);
  }

  void addMultipleTo(ElementType& res,
                     const RealElementType& a,
                     const ElementType& b) const
  {
    ElementType result;
    result.re += a * b.re;
    result.im += a * b.im;
    set(res, result);
  }

  void addMultipleTo(ElementType& res,
                     const ElementType& a,
                     const ElementType& b) const
  {
    ElementType result;
    result.re += a.re * b.re - a.im * b.im;
    result.im += a.im * b.re + a.re * b.im;
    set(res, result);
  }

  void subtract(ElementType& res,
                const ElementType& a,
                const ElementType& b) const
  {
    ElementType result;
    result.re = a.re - b.re;
    result.im = a.im - b.im;
    set(res, result);
  }

  void subtract_multiple(ElementType& result,
                         const ElementType& a,
                         const ElementType& b) const
  {
    // result -= a*b
    ElementType ab;
    mult(ab, a, b);
    subtract(result, result, ab);
  }

  void mult(ElementType& res,
            const ElementType& a,
            const RealElementType& b) const
  {
    res.re = a.re * b;
    res.im = a.im * b;
  }

  void mult(ElementType& res, const ElementType& a, const ElementType& b) const
  {
    RealElementType tmp;
    tmp = a.re * b.re - a.im * b.im;
    res.im = a.re * b.im + a.im * b.re;
    res.re = tmp;
  }

  void divide(ElementType& res,
              const ElementType& a,
              const RealElementType& b) const
  {
    res.re = a.re / b;
    res.im = a.im / b;
  }

  void divide(ElementType& res,
              const ElementType& a,
              const ElementType& b) const
  {
    RealElementType p, denom;  // double

    ElementType result;
    if (fabs(b.re) >= fabs(b.im))
      {
        p = b.im / b.re;
        denom = b.re + p * b.im;
        result.re = (a.re + p * a.im) / denom;
        result.im = (a.im - p * a.re) / denom;
      }
    else
      {
        p = b.re / b.im;
        denom = b.im + p * b.re;
        result.re = (a.im + p * a.re) / denom;
        result.im = (p * a.im - a.re) / denom;
      }
    set(res, result);
  }

  void abs_squared(ARingRR::ElementType& result, const ElementType& a) const
  {
    result = a.re * a.re + a.im * a.im;
  }

  void abs(ARingRR::ElementType& result, const ElementType& a) const
  {
    result = sqrt(a.re * a.re + a.im * a.im);
  }

  void power(ElementType& result, const ElementType& a, int n) const
  {
    ElementType curr_pow;
    init(curr_pow);
    set_from_long(result, 1);
    if (n == 0)
      {
      }
    else if (n < 0)
      {
        n = -n;
        invert(curr_pow, a);
      }
    else
      {
        set(curr_pow, a);
      }
    while (n > 0)
      {
        if (n % 2)
          {
            mult(result, result, curr_pow);
          }
        n = n / 2;
        mult(curr_pow, curr_pow, curr_pow);
      }
    clear(curr_pow);
  }

  void power_mpz(ElementType& result, const ElementType& a, mpz_srcptr n) const
  {
    std::pair<bool, int> n1 = RingZZ::get_si(n);
    if (n1.first)
      power(result, a, n1.second);
    else
      throw exc::engine_error("exponent too large");
  }

  void swap(ElementType& a, ElementType& b) const { std::swap(a, b); }
  void elem_text_out(buffer& o,
                     const ElementType& a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;

  void syzygy(const ElementType& a,
              const ElementType& b,
              ElementType& x,
              ElementType& y) const  // remove?
  // returns x,y s.y. x*a + y*b == 0.
  // if possible, x is set to 1.
  // no need to consider the case a==0 or b==0.
  {
    // TODO: remove this?
    set_var(x, 0);  // set x=1
    if (!is_zero(b))
      {
        set(y, a);
        negate(y, y);
        divide(y, y, b);
      }
  }

  void random(ElementType& result) const  // redo?
  {
    result.re = randomDouble();
    result.im = randomDouble();
  }

  void eval(const RingMap* map,
            ElementType& f,
            int first_var,
            ring_elem& result) const
  {
    if (!map->get_ring()->from_complex_double(f.re, f.im, result))
      {
        result = map->get_ring()->from_long(0);
        if (not error()) ERROR("cannot coerce CC value to ring type");
      }
  }

  gmp_CC toBigComplex(const ElementType& a) const
  {
    gmp_CCmutable result = getmemstructtype(gmp_CCmutable);
    result->re = getmemstructtype(mpfr_ptr);
    result->im = getmemstructtype(mpfr_ptr);
    mpfr_init2(result->re, get_precision());
    mpfr_init2(result->im, get_precision());
    mpfr_set_d(result->re, a.re, MPFR_RNDN);
    mpfr_set_d(result->im, a.im, MPFR_RNDN);
    return moveTo_gmpCC(result);
  }

  void set_from_doubles(ElementType& result, double re, double im) const
  {
    result.re = re;
    result.im = im;
  }

  void zeroize_tiny(gmp_RR epsilon, ElementType& a) const
  {
    if (mpfr_cmp_d(epsilon, fabs(a.re)) > 0) a.re = 0.0;
    if (mpfr_cmp_d(epsilon, fabs(a.im)) > 0) a.im = 0.0;
  }

  void increase_norm(gmp_RRmutable norm, const ElementType& a) const
  {
    double d;
    abs(d, a);
    if (mpfr_cmp_d(norm, d) < 0) mpfr_set_d(norm, d, MPFR_RNDN);
  }
};

};  // end namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
