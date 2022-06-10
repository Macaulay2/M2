// Copyright 2012 Michael E. Stillman

#ifndef _aring_CCC_hpp_
#define _aring_CCC_hpp_

#include <iostream>
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"

#include "aring-RRR.hpp"

class RingMap;

namespace M2 {
/**
\ingroup rings
*/
class ARingCCC : public RingInterface
{
  // complex numbers represented as pairs of MPFRs.

 private:
  const ARingRRR mRRR;  // reals with the same precision
 public:
  static const RingID ringID = ring_CCC;

  typedef cc_struct elem;  // ??? staighten this out!!!
  typedef elem ElementType;
  typedef ARingRRR RealRingType;
  typedef RealRingType::ElementType RealElementType;

  ARingCCC(unsigned long precision) : mRRR(precision) {}
  ARingCCC() : mRRR(53) {}
  // ring informational
  size_t characteristic() const { return 0; }
  unsigned long get_precision() const { return mRRR.get_precision(); }
  void text_out(buffer& o) const;

  const RealRingType& real_ring() const { return mRRR; }
  unsigned int computeHashValue(const ElementType& a) const
  {
    double a1 = mpfr_get_d(&a.re, MPFR_RNDN);
    double b1 = mpfr_get_d(&a.im, MPFR_RNDN);
    return static_cast<unsigned int>(12347. * a1 + 865800. * b1);
  }

  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

  bool is_unit(const ElementType& f) const { return !is_zero(f); }
  bool is_zero(const ElementType& f) const
  {
    return mpfr_cmp_si(&f.re, 0) == 0 && mpfr_cmp_si(&f.im, 0) == 0;
  }

  bool is_equal(const ElementType& f, const ElementType& g) const
  {
    return mpfr_cmp(&f.re, &g.re) == 0 && mpfr_cmp(&f.im, &g.im) == 0;
  }

  int compare_elems(const ElementType& f, const ElementType& g) const
  {
    int cmp_re = mpfr_cmp(&f.re, &g.re);
    if (cmp_re < 0) return -1;
    if (cmp_re > 0) return 1;
    int cmp_im = mpfr_cmp(&f.im, &g.im);
    if (cmp_im < 0) return -1;
    if (cmp_im > 0) return 1;
    return 0;
  }

  ////////////////////////////
  // to/from ringelem ////////
  ////////////////////////////
  void to_ring_elem(ring_elem& result, const ElementType& a) const
  {
    cc_ptr res = getmemstructtype(cc_ptr);
    init(*res);
    set(*res, a);
    mpfr_reallocate_limbs(&res->re);
    mpfr_reallocate_limbs(&res->im);
    result = ring_elem(res);
  }

  void from_ring_elem(ElementType& result, const ring_elem& a) const
  {
    set(result, *a.get_cc());
  }

  // 'init', 'init_set' functions

  void init(ElementType& result) const
  {
    mpfr_init2(&result.re, get_precision());
    mpfr_init2(&result.im, get_precision());
  }

  void init_set(ElementType& result, const ElementType& a) const
  {
    init(result);
    mpfr_set(&result.re, &a.re, MPFR_RNDN);
    mpfr_set(&result.im, &a.im, MPFR_RNDN);
  }

  void set(ElementType& result, const ElementType& a) const
  {
    mpfr_set(&result.re, &a.re, MPFR_RNDN);
    mpfr_set(&result.im, &a.im, MPFR_RNDN);
  }

  void set_zero(ElementType& result) const
  {
    mpfr_set_si(&result.re, 0, MPFR_RNDN);
    mpfr_set_si(&result.im, 0, MPFR_RNDN);
  }

  void clear(ElementType& result) const
  {
    mpfr_clear(&result.re);
    mpfr_clear(&result.im);
  }

  void copy(ElementType& result, const ElementType& a) const { set(result, a); }
  void set_from_long(ElementType& result, long a) const
  {
    mpfr_set_si(&result.re, a, MPFR_RNDN);
    mpfr_set_si(&result.im, 0, MPFR_RNDN);
  }

  void set_var(ElementType& result, int v) const { set_from_long(result, 1); }
  void set_from_mpz(ElementType& result, mpz_srcptr a) const
  {
    mpfr_set_z(&result.re, a, MPFR_RNDN);
    mpfr_set_si(&result.im, 0, MPFR_RNDN);
  }

  bool set_from_mpq(ElementType& result, mpq_srcptr a) const
  {
    mpfr_set_q(&result.re, a, MPFR_RNDN);
    mpfr_set_si(&result.im, 0, MPFR_RNDN);
    return true;
  }

  bool set_from_BigReal(ElementType& result, gmp_RR a) const
  {
    mpfr_set(&result.re, a, MPFR_RNDN);
    mpfr_set_si(&result.im, 0, MPFR_RNDN);
    return true;
  }
  bool set_from_BigComplex(ElementType& result, gmp_CC a) const
  {  //???
    mpfr_set(&result.re, a->re, MPFR_RNDN);
    mpfr_set(&result.im, a->im, MPFR_RNDN);
    return true;
  }
  bool set_from_double(ElementType& result, double a) const
  {
    mpfr_set_d(&result.re, a, MPFR_RNDN);
    mpfr_set_si(&result.im, 0, MPFR_RNDN);
    return true;
  }
  bool set_from_complex_double(ElementType& result, double re, double im) const
  {
    mpfr_set_d(&result.re, re, MPFR_RNDN);
    mpfr_set_d(&result.im, im, MPFR_RNDN);
    return true;
  }
  bool set_from_complex_mpfr(ElementType& result, mpfr_srcptr re, const mpfr_srcptr im) const
  {
    mpfr_set(&result.re, re, MPFR_RNDN);
    mpfr_set(&result.im, im, MPFR_RNDN);
    return true;
  }

  // arithmetic
  void negate(ElementType& result, const ElementType& a) const
  {
    mpfr_neg(&result.re, &a.re, MPFR_RNDN);
    mpfr_neg(&result.im, &a.im, MPFR_RNDN);
  }

  void invert(ElementType& result, const ElementType& a) const
  // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
  {
    mpfr_t p, denom;
    mpfr_init2(p, get_precision());
    mpfr_init2(denom, get_precision());

    if (mpfr_cmpabs(&a.re, &a.im) >= 0)
      {
        // double p = &a.im/&a.re;
        // double denom = &a.re + p * &a.im;
        // &result.re = 1.0/denom;
        // &result.im = - p/denom;

        mpfr_div(p, &a.im, &a.re, MPFR_RNDN);
        mpfr_mul(denom, p, &a.im, MPFR_RNDN);
        mpfr_add(denom, denom, &a.re, MPFR_RNDN);
        mpfr_si_div(&result.re, 1, denom, MPFR_RNDN);
        mpfr_div(&result.im, p, denom, MPFR_RNDN);
        mpfr_neg(&result.im, &result.im, MPFR_RNDN);
      }
    else
      {
        // double p = &a.re/&a.im;
        // double denom = &a.im + p * &a.re;
        // &result.re = p/denom;
        // &result.im = -1.0/denom;

        mpfr_div(p, &a.re, &a.im, MPFR_RNDN);
        mpfr_mul(denom, p, &a.re, MPFR_RNDN);
        mpfr_add(denom, denom, &a.im, MPFR_RNDN);
        mpfr_si_div(&result.im, 1, denom, MPFR_RNDN);
        mpfr_neg(&result.im, &result.im, MPFR_RNDN);
        mpfr_div(&result.re, p, denom, MPFR_RNDN);
      }

    mpfr_clear(p);
    mpfr_clear(denom);
  }

  void add(ElementType& result,
           const ElementType& a,
           const ElementType& b) const
  {
    mpfr_add(&result.re, &a.re, &b.re, MPFR_RNDN);
    mpfr_add(&result.im, &a.im, &b.im, MPFR_RNDN);
  }

  void addMultipleTo(ElementType& result,
                     const RealElementType& a,
                     const ElementType& b) const
  {
    mRRR.addMultipleTo(result.re, a, b.re);
    mRRR.addMultipleTo(result.im, a, b.im);
  }

  void addMultipleTo(ElementType& result,
                     const ElementType& a,
                     const ElementType& b) const
  {
    ElementType ab;
    init(ab);
    mult(ab, a, b);
    add(result, result, ab);
    clear(ab);
  }

  void subtract(ElementType& result,
                const ElementType& a,
                const ElementType& b) const
  {
    mpfr_sub(&result.re, &a.re, &b.re, MPFR_RNDN);
    mpfr_sub(&result.im, &a.im, &b.im, MPFR_RNDN);
  }

  void subtract_multiple(ElementType& result,
                         const ElementType& a,
                         const ElementType& b) const
  {
    // result -= a*b
    ElementType ab;
    init(ab);
    mult(ab, a, b);
    subtract(result, result, ab);
    clear(ab);
  }

  void mult(ElementType& res,
            const ElementType& a,
            const RealElementType& b) const
  {
    mpfr_t tmp;
    ElementType result;
    init(result);
    mpfr_init2(tmp, get_precision());

    // &result.re = &a.re*&b;
    mpfr_mul(tmp, &a.re, &b, MPFR_RNDN);
    mpfr_set(&result.re, tmp, MPFR_RNDN);

    // &result.im = &a.im*&b;
    mpfr_mul(tmp, &a.im, &b, MPFR_RNDN);
    mpfr_set(&result.im, tmp, MPFR_RNDN);

    set(res, result);
    clear(result);
    mpfr_clear(tmp);
  }

  void mult(ElementType& res, const ElementType& a, const ElementType& b) const
  {
    mpfr_t tmp;
    ElementType result;
    init(result);
    mpfr_init2(tmp, get_precision());

    // &result.re = &a.re*&b.re - &a.im*&b.im;
    mpfr_mul(tmp, &a.re, &b.re, MPFR_RNDN);
    mpfr_set(&result.re, tmp, MPFR_RNDN);
    mpfr_mul(tmp, &a.im, &b.im, MPFR_RNDN);
    mpfr_sub(&result.re, &result.re, tmp, MPFR_RNDN);

    // &result.im = &a.re*&b.im + &a.im*&b.re;
    mpfr_mul(tmp, &a.re, &b.im, MPFR_RNDN);
    mpfr_set(&result.im, tmp, MPFR_RNDN);
    mpfr_mul(tmp, &a.im, &b.re, MPFR_RNDN);
    mpfr_add(&result.im, &result.im, tmp, MPFR_RNDN);

    set(res, result);
    clear(result);
    mpfr_clear(tmp);
  }

  void divide(ElementType& res,
              const ElementType& a,
              const RealElementType& b) const
  {
    mpfr_t tmp;
    ElementType result;
    init(result);
    mpfr_init2(tmp, get_precision());

    mpfr_div(tmp, &a.re, &b, MPFR_RNDN);
    mpfr_set(&result.re, tmp, MPFR_RNDN);

    mpfr_div(tmp, &a.im, &b, MPFR_RNDN);
    mpfr_set(&result.im, tmp, MPFR_RNDN);

    set(res, result);
    clear(result);
    mpfr_clear(tmp);
  }

  void divide(ElementType& res,
              const ElementType& a,
              const ElementType& b) const
  {
    mpfr_t p, denom;
    mpfr_init2(p, get_precision());
    mpfr_init2(denom, get_precision());
    ElementType result;
    init(result);

    if (mpfr_cmpabs(&b.re, &b.im) >= 0)
      {
        // for v = c + d*i,
        // p = d/c
        // c+di = c(1+p*i), so denom is c(1+p^2)
        // which is c + d*p

        // double p = v.im/v.re;
        // double denom = v.re + p * v.im;
        // result.re = (u.re + p*u.im)/denom;
        // result.im = (u.im - p*u.re)/denom;

        mpfr_div(p, &b.im, &b.re, MPFR_RNDN);
        mpfr_mul(denom, p, &b.im, MPFR_RNDN);
        mpfr_add(denom, denom, &b.re, MPFR_RNDN);

        mpfr_mul(&result.re, p, &a.im, MPFR_RNDN);
        mpfr_add(&result.re, &result.re, &a.re, MPFR_RNDN);
        mpfr_div(&result.re, &result.re, denom, MPFR_RNDN);

        mpfr_mul(&result.im, p, &a.re, MPFR_RNDN);
        mpfr_neg(&result.im, &result.im, MPFR_RNDN);
        mpfr_add(&result.im, &result.im, &a.im, MPFR_RNDN);
        mpfr_div(&result.im, &result.im, denom, MPFR_RNDN);
      }
    else
      {
        // double p = v.re/v.im;
        // double denom = v.im + p * v.re;
        // result.re = (u.re * p + u.im)/denom;
        // result.im = (-u.re + p * u.im)/denom;

        mpfr_div(p, &b.re, &b.im, MPFR_RNDN);
        mpfr_mul(denom, p, &b.re, MPFR_RNDN);
        mpfr_add(denom, denom, &b.im, MPFR_RNDN);

        mpfr_mul(&result.re, p, &a.re, MPFR_RNDN);
        mpfr_add(&result.re, &result.re, &a.im, MPFR_RNDN);
        mpfr_div(&result.re, &result.re, denom, MPFR_RNDN);

        mpfr_mul(&result.im, p, &a.im, MPFR_RNDN);
        mpfr_sub(&result.im, &result.im, &a.re, MPFR_RNDN);
        mpfr_div(&result.im, &result.im, denom, MPFR_RNDN);
      }
    mpfr_clear(p);
    mpfr_clear(denom);
    set(res, result);
    clear(result);
  }

  void abs_squared(ARingRRR::ElementType& result, const ElementType& a) const
  {
    mRRR.mult(result, realPartReference(a), realPartReference(a));
    ARingRRR::ElementType s;
    mRRR.init(s);
    mRRR.mult(s, imaginaryPartReference(a), imaginaryPartReference(a));
    mRRR.add(result, result, s);
    mRRR.clear(s);
  }

  void abs(ARingRRR::ElementType& result, const ElementType& a) const
  {
    abs_squared(result, a);
    mpfr_sqrt(&result, &result, MPFR_RNDN);  // should we have ARingRRR::sqrt ???
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

  void swap(ElementType& a, ElementType& b) const
  {
    mpfr_swap(&a.re, &b.re);
    mpfr_swap(&a.im, &b.im);
  }

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
    randomMpfr(&result.re);
    randomMpfr(&result.im);
  }

  void eval(const RingMap* map,
            ElementType& f,
            int first_var,
            ring_elem& result) const
  {
    gmp_CC_struct g;
    g.re = &f.re;
    g.im = &f.im;
    if (!map->get_ring()->from_BigComplex(&g, result))
      {
        result = map->get_ring()->from_long(0);
        ERROR("cannot map CC value to ring type");
      }
  }

  gmp_CC toBigComplex(const ElementType& a) const
  {
    gmp_CCmutable result = getmemstructtype(gmp_CCmutable);
    result->re = getmemstructtype(gmp_RRmutable);
    result->im = getmemstructtype(gmp_RRmutable);
    mpfr_init2(result->re, get_precision());
    mpfr_init2(result->im, get_precision());
    mpfr_set(result->re, &a.re, MPFR_RNDN);
    mpfr_set(result->im, &a.im, MPFR_RNDN);
    return moveTo_gmpCC(result);
  }

  bool set_from_RRR(ElementType& result, const ARingRRR::ElementType& a) const
  {
    mpfr_set(&result.re, &a, MPFR_RNDN);
    mpfr_set_si(&result.im, 0, MPFR_RNDN);
    return true;
  }

  const ARingRRR::ElementType& realPartReference(const ElementType& a) const
  {
    return a.re;
  }
  const ARingRRR::ElementType& imaginaryPartReference(
      const ElementType& a) const
  {
    return a.im;
  }
  void set_real_part(ElementType& c, ARingRRR::ElementType& a) const
  {
    mpfr_set(&c.re, &a, MPFR_RNDN);
  }
  void set_imaginary_part(ElementType& c, ARingRRR::ElementType& a) const
  {
    mpfr_set(&c.im, &a, MPFR_RNDN);
  }
  void set_from_BigReals(ElementType& result, gmp_RR re, gmp_RR im) const
  {
    mpfr_set(&result.re, re, MPFR_RNDN);
    mpfr_set(&result.im, im, MPFR_RNDN);
  }
  void set_from_doubles(ElementType& result, double re, double im) const
  {
    mRRR.set_from_double(result.re, re);
    mRRR.set_from_double(result.im, im);
  }

  void zeroize_tiny(gmp_RR epsilon, ElementType& a) const
  {
    mRRR.zeroize_tiny(epsilon, a.re);
    mRRR.zeroize_tiny(epsilon, a.im);
  }
  void increase_norm(gmp_RRmutable norm, const ElementType& a) const
  {
    ARingRRR::ElementType n;
    mRRR.init(n);
    abs(n, a);
    if (mpfr_cmp(&n, norm) > 0) mRRR.set(*norm, n);
    mRRR.clear(n);
  }
};

};  // end namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
