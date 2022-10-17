// Copyright 2012 Michael E. Stillman

#ifndef _aring_RRR_hpp_
#define _aring_RRR_hpp_

#include "interface/random.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"

class RRR;
class RingMap;

namespace M2 {
/**
\ingroup rings
*/
class ARingRRR : public RingInterface
{
  // Higher precision real numbers

 public:
  static const RingID ringID = ring_RRR;

  typedef __mpfr_struct elem;
  typedef elem ElementType;

  ARingRRR(unsigned long precision) : mPrecision(precision) {}
  // ring informational
  size_t characteristic() const { return 0; }
  unsigned long get_precision() const { return mPrecision; }
  void text_out(buffer &o) const;

  unsigned int computeHashValue(const elem &a) const
  {
    double d = mpfr_get_d(&a, MPFR_RNDN);
    return static_cast<unsigned int>(d);
  }

  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

  bool is_unit(const ElementType &f) const { return !is_zero(f); }
  bool is_zero(const ElementType &f) const { return mpfr_cmp_si(&f, 0) == 0; }
  bool is_equal(const ElementType &f, const ElementType &g) const
  {
    return mpfr_cmp(&f, &g) == 0;
  }

  int compare_elems(const ElementType &f, const ElementType &g) const
  {
    int cmp = mpfr_cmp(&f, &g);
    if (cmp < 0) return -1;
    if (cmp > 0) return 1;
    return 0;
  }

  ////////////////////////////
  // to/from ringelem ////////
  ////////////////////////////
  // These simply repackage the element as either a ringelem or an
  // 'ElementType'.
  // No reinitialization is done.
  // Do not take the same element and store it as two different ring_elem's!!
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    mpfr_ptr res = getmemstructtype(mpfr_ptr);
    mpfr_init2(res, mPrecision);
    mpfr_set(res, &a, MPFR_RNDN);
    result = ring_elem(moveTo_gmpRR(res));
  }

  void from_ring_elem(ElementType &result, const ring_elem &a) const
  {
    mpfr_set(&result, a.get_mpfr(), MPFR_RNDN);
  }

  // 'init', 'init_set' functions

  void init(ElementType &result) const { mpfr_init2(&result, mPrecision); }
  void init_set(ElementType &result, const ElementType &a) const
  {
    init(result);
    mpfr_set(&result, &a, MPFR_RNDN);
  }

  void set(ElementType &result, const ElementType &a) const
  {
    mpfr_set(&result, &a, MPFR_RNDN);
  }

  void set_zero(ElementType &result) const
  {
    mpfr_set_si(&result, 0, MPFR_RNDN);
  }

  void clear(ElementType &result) const { mpfr_clear(&result); }
  void copy(ElementType &result, const ElementType &a) const
  {
    mpfr_set(&result, &a, MPFR_RNDN);
  }

  void set_from_long(ElementType &result, long a) const
  {
    mpfr_set_si(&result, a, MPFR_RNDN);
  }

  void set_var(ElementType &result, int v) const
  {
    mpfr_set_si(&result, 1, MPFR_RNDN);
  }

  void set_from_mpz(ElementType &result, mpz_srcptr a) const
  {
    mpfr_set_z(&result, a, MPFR_RNDN);
  }

  bool set_from_mpq(ElementType &result, mpq_srcptr a) const
  {
    mpfr_set_q(&result, a, MPFR_RNDN);
    return true;
  }

  bool set_from_double(ElementType &result, double a) const
  {
    mpfr_set_d(&result, a, MPFR_RNDN);
    return true;
  }
  bool set_from_BigReal(ElementType &result, gmp_RR a) const
  {
    mpfr_set(&result, a, MPFR_RNDN);
    return true;
  }

  // arithmetic
  void negate(ElementType &result, const ElementType &a) const
  {
    mpfr_neg(&result, &a, MPFR_RNDN);
  }

  void invert(ElementType &result, const ElementType &a) const
  // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
  {
    mpfr_si_div(&result, 1, &a, MPFR_RNDN);
  }

  void add(ElementType &result,
           const ElementType &a,
           const ElementType &b) const
  {
    mpfr_add(&result, &a, &b, MPFR_RNDN);
  }

  void addMultipleTo(ElementType &result,
                     const ElementType &a,
                     const ElementType &b) const
  {
    mpfr_fma(&result, &a, &b, &result, MPFR_RNDN);
  }

  void subtract(ElementType &result,
                const ElementType &a,
                const ElementType &b) const
  {
    mpfr_sub(&result, &a, &b, MPFR_RNDN);
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
    mpfr_mul(&result, &a, &b, MPFR_RNDN);
  }

  void divide(ElementType &result,
              const ElementType &a,
              const ElementType &b) const
  {
    mpfr_div(&result, &a, &b, MPFR_RNDN);
  }

  void power(ElementType &result, const ElementType &a, int n) const
  {
    mpfr_pow_si(&result, &a, n, MPFR_RNDN);
  }

  void power_mpz(ElementType &result, const ElementType &a, mpz_srcptr n) const
  {
    mpfr_pow_z(&result, &a, n, MPFR_RNDN);
  }

  void swap(ElementType &a, ElementType &b) const { mpfr_swap(&a, &b); }
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

  void random(ElementType &result) const  // redo?
  {
    randomMpfr(&result);
  }

  void eval(const RingMap *map,
            ElementType &f,
            int first_var,
            ring_elem &result) const
  {
    if (!map->get_ring()->from_BigReal(&f, result))
      {
        result = map->get_ring()->from_long(0);
        ERROR("cannot coerce RRR value to ring type");
      }
  }

  void zeroize_tiny(gmp_RR epsilon, ElementType &a) const
  {
    if (mpfr_cmpabs(&a, epsilon) < 0) set_zero(a);
  }
  void increase_norm(gmp_RRmutable norm, const ElementType &a) const
  {
    if (mpfr_cmpabs(&a, norm) > 0)
      {
        set(*norm, a);
        abs(*norm, *norm);
      }
  }

  void abs_squared(ElementType &result, const ElementType &a) const
  {
    mult(result, a, a);
  }

  void abs(ElementType &result, const ElementType &a) const
  {
    if (mpfr_cmp_si(&a, 0) < 0)
      negate(result, a);
    else
      set(result, a);
  }

  double coerceToDouble(const ElementType &a) const
  {
    return mpfr_get_d(&a, MPFR_RNDN);
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
