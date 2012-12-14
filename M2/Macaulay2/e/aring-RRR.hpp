// Copyright 2012 Michael E. Stillman

#ifndef _aring_RRR_hpp_
#define _aring_RRR_hpp_

// Ring_RRR is the placeholder for ARingRRR; shall be replaced when CoefficientRingRRR is phased out completely

#ifdef use_new_RRR
#define Ring_RRR M2::ARingRRR
#else 
#define Ring_RRR CoefficientRingRRR 
#endif

//class Ring_RRR;
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
    // complex numbers represented as pairs of doubles.

  public:
    static const RingID ringID = ring_RRR;
    const RRR *R; // ???

    typedef RRR ring_type;
    typedef __mpfr_struct elem;
    typedef elem ElementType;

    ARingRRR(unsigned long precision) : mPrecision(precision) {}
    ARingRRR(const RRR *R0) : R(R0) {}

    // ring informational
    size_t characteristic() const { return 0; }

    void text_out(buffer &o) const;

    /////////////////////////////////
    // ElementType informational ////
    /////////////////////////////////

    bool is_unit(ElementType f) const { 
      return !is_zero(f); 
    }

    bool is_zero(ElementType f) const {
      return mpfr_cmp_si(&f, 0) == 0;
    }
    
    bool is_equal(ElementType f, ElementType g) const { 
      return mpfr_cmp(&f,&g) == 0;
    }

    int compare_elems(ElementType f, ElementType g) const {
      int cmp = mpfr_cmp(&f,&g);
      if (cmp < 0) return -1;
      if (cmp > 0) return 1;
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
      mpfr_ptr res = getmemstructtype(mpfr_ptr);
      mpfr_init2(res,mPrecision);
      mpfr_set(res, &a, GMP_RNDN);
      result = MPF_RINGELEM(res);
    }

    void from_ring_elem(ElementType &result, const ring_elem &a) const
    {
      init(result);
      mpfr_set(&result, reinterpret_cast<mpfr_ptr>(a.poly_val), GMP_RNDN);
    }

    // 'get' functions

    int get_int(elem f) const { ASSERT(false); return 0; }

    int get_repr(elem f) const { ASSERT(false); return 0; }

    // 'init', 'init_set' functions

    void init(elem &result) const { 
      mpfr_init2(&result, mPrecision); 
    }

    void init_set(elem &result, elem a) const { 
      init(result);
      mpfr_set(&result, &a, GMP_RNDN);
    }

    void set(elem &result, elem a) const { 
      mpfr_set(&result, &a, GMP_RNDN);
    }

    void set_zero(elem &result) const { 
      init(result);
      mpfr_set_si(&result, 0, GMP_RNDN); 
    }

    void clear(elem &result) const { 
      mpfr_clear(&result); 
    }

    void copy(elem &result, elem a) const { mpfr_set(&result, &a, GMP_RNDN); }

    void set_from_int(elem &result, int a) const {
      mpfr_set_si(&result, a, GMP_RNDN);
    }

    void set_var(elem &result, int v) const { 
      mpfr_set_si(&result, 1, GMP_RNDN); 
    }

    void set_from_mpz(elem &result, mpz_ptr a) const {
      mpfr_set_z(&result, a, GMP_RNDN);
    }

    void set_from_mpq(elem &result, mpq_ptr a) const {
      mpfr_set_q(&result, a, GMP_RNDN);
    }

    bool set_from_BigReal(elem &result, gmp_RR a) const {
      mpfr_set(&result, a, GMP_RNDN);
      return true;
    }

    // arithmetic
    void negate(elem &result, elem a) const
    {
      mpfr_neg(&result, &a, GMP_RNDN);
    }

    void invert(elem &result, elem a) const
      // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
    {
      mpfr_si_div(&result, 1, &a, GMP_RNDN);
    }

    void add(elem &result, elem a, elem b) const
    {
      mpfr_add(&result, &a, &b, GMP_RNDN);
    }

    void subtract(elem &result, elem a, elem b) const
    {
      mpfr_sub(&result, &a, &b, GMP_RNDN);
    }

    void subtract_multiple(elem &result, elem a, elem b) const
    {
      //TODO: write this
      // we assume: a, b are NONZERO!!
      // result -= a*b
    }

    void mult(elem &result, elem a, elem b) const
    {
      mpfr_mul(&result, &a, &b, GMP_RNDN);
    }

    void divide(elem &result, elem a, elem b) const
    {
      mpfr_div(&result, &a, &b, GMP_RNDN);
    }

    void power(elem &result, elem a, int n) const
    {
      mpfr_pow_si(&result, &a, n, GMP_RNDN);
    }

    void power_mpz(elem &result, elem a, mpz_ptr n) const
    {
      mpfr_pow_z(&result, &a, n, GMP_RNDN);
    }

    void swap(ElementType &a, ElementType &b) const
    {
      mpfr_swap(&a, &b);
    }

    void elem_text_out(buffer &o,
                       ElementType &a,
                       bool p_one,
                       bool p_plus,
                       bool p_parens) const;
      //TODO

    void syzygy(ElementType a, ElementType b,
                ElementType &x, ElementType &y) const
    // returns x,y s.y. x*a + y*b == 0.
    // if possible, x is set to 1.
    // no need to consider the case a==0 or b==0.
    {
      //TODO
    }

    void random(ElementType &result) const
    {
      rawRandomMpfr(&result, mPrecision);
    }

    void eval(const RingMap *map, elem &f, int first_var, ring_elem &result) const
    {
      map->get_ring()->from_BigReal(&f, result);
    }

    // TODO: promote, lift.
  private:
      unsigned long mPrecision;
  };

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
