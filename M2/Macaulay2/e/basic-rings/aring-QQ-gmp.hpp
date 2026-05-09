// Copyright 2013 Michael E. Stillman

#ifndef M2_BASIC_RINGS_ARING_QQ_GMP_HPP_
#define M2_BASIC_RINGS_ARING_QQ_GMP_HPP_

#include "interface/gmp-util.h"  // for mpz_reallocate_limbs
#include "interface/random.h"    // for rawSetRandomQQ

#include "basic-rings/aring.hpp"
#include "buffer.hpp"
#include "rings/ringelem.hpp"
#include <iosfwd>
#include "exceptions.hpp"

// promote needs ring.hpp.  After moving promote out, remove it here!
#include "rings/ring.hpp"

namespace M2 {
/**
   @ingroup rings

   @brief wrapper for the gmp mpq_t integer representation
*/

class ARingQQGMP : public SimpleARing<ARingQQGMP>
{
 public:
  static const RingID ringID = ring_QQ;

  typedef __mpq_struct ElementType;
  typedef ElementType elem;
  typedef std::vector<elem> ElementContainerType;

  ARingQQGMP();
  ~ARingQQGMP();

 public:
  // ring informational
  size_t characteristic() const { return 0; }
  size_t cardinality() const { return static_cast<size_t>(-1); }
  unsigned int computeHashValue(const ElementType& a) const
  {
    unsigned long numhash = mpz_get_ui(mpq_numref(&a));
    unsigned long denhash = mpz_get_ui(mpq_denref(&a));
    return static_cast<unsigned int>(13253 * numhash + 7647 * denhash);
  }

  /** @name properties
      @{
  */

  bool is_pm_one(const ElementType& f) const
  {
    return (mpz_cmp_si(mpq_denref(&f), 1) == 0 and
            (mpz_cmp_si(mpq_numref(&f), 1) == 0 or
             mpz_cmp_si(mpq_numref(&f), -1) == 0));
  }
  bool is_unit(const ElementType& f) const { return not is_zero(f); }
  bool is_zero(const ElementType& f) const { return mpq_sgn(&f) == 0; }
  /** @} */

  /** @name operators
      @{ */

  bool is_equal(const ElementType& f, const ElementType& g) const
  {
    return mpq_equal(&f, &g);
  }
  int compare_elems(const ElementType& f, const ElementType& g) const
  {
    int cmp = mpq_cmp(&f, &g);
    if (cmp > 0) return 1;
    if (cmp < 0) return -1;
    return 0;
  }
  /** @} */

  /** @name init_set
      @{ */

  void init_set(ElementType& result, const ElementType& a) const
  {
    mpq_init(&result);
    mpq_set(&result, &a);
  }

  void init(ElementType& result) const { mpq_init(&result); }
  static void clear(ElementType& result) { mpq_clear(&result); }
  void set(ElementType& result, const ElementType& a) const
  {
    mpq_set(&result, &a);
  }

  void set_zero(ElementType& result) const { mpq_set_si(&result, 0, 1); }
  void set_from_long(ElementType& result, long a) const
  {
    mpq_set_si(&result, a, 1);
  }

  void set_from_mpz(ElementType& result, mpz_srcptr a) const
  {
    mpz_set(mpq_numref(&result), a);
    mpz_set_ui(mpq_denref(&result), 1);
  }

  bool lift_to_mpz(mpz_ptr result, const ElementType& a) const
  {
    if (mpz_cmp_si(mpq_denref(&a), 1) == 0)
      {
        mpz_set(result, mpq_numref(&a));
        return true;
      }
    return false;
  }

  bool set_from_mpq(ElementType& result, mpq_srcptr a) const
  {
    mpq_set(&result, a);
    return true;
  }

  // Use nearest-integer continued fraction approximation to find the
  // simplest rational that rounds back to a at its precision.  The
  // nearest-integer variant (rounding each remainder rather than taking
  // the floor) keeps remainders in (-1/2, 1/2], so convergents grow
  // faster than with the standard floor-based algorithm.  At each step
  // we compute the next convergent n0/d0 and check whether it
  // round-trips to a; if so, we return it.  Otherwise, once the
  // convergents grow as large as the exact bit representation of a, we
  // fall back to returning that exact representation.
  //
  // Because remainders can be negative, partial quotients can be
  // negative, which means both n0 and d0 can come out with the same
  // negative sign.  We negate both before storing into the mpq_t,
  // which requires a positive denominator.
  bool set_from_BigReal(ElementType& result, gmp_RR a) const
  {
    bool negative, success;
    mpfr_prec_t prec;
    mpfr_t q, r;
    mpz_t c, d0, d1, n0, n1, p2, tmp;

    if (!mpfr_number_p(a)) return false;
    if (mpfr_zero_p(a)) {
      mpq_set_si(&result, 0, 1);
      return true;
    }

    negative = mpfr_sgn(a) < 0;
    prec = mpfr_get_prec(a);

    // p2 = 2^prec, the bound used to detect when convergents are large enough
    mpz_init(p2);
    mpz_setbit(p2, (mp_bitcnt_t)prec);

    mpz_init_set_ui(n0, 1);
    mpz_init_set_ui(n1, 0);
    mpz_init_set_ui(d0, 0);
    mpz_init_set_ui(d1, 1);

    mpfr_init2(r, prec);
    mpfr_abs(r, a, MPFR_RNDN);

    mpz_init(c);
    mpz_init(tmp);

    mpfr_init2(q, prec);

    success = false;

    while (true) {
      mpfr_get_z(c, r, MPFR_RNDN);

      mpz_swap(n0, n1);
      mpz_swap(d0, d1);

      mpz_mul(tmp, c, n1);
      mpz_add(n0, n0, tmp);
      mpz_mul(tmp, c, d1);
      mpz_add(d0, d0, tmp);

      mpfr_sub_z(r, r, c, MPFR_RNDN);

      // return the first convergent that rounds back to a
      mpfr_set_z(q, n0, MPFR_RNDN);
      mpfr_div_z(q, q, d0, MPFR_RNDN);
      if (mpfr_cmpabs(a, q) == 0) {
        if (mpz_sgn(d0) < 0) {
          mpz_neg(n0, n0);
          mpz_neg(d0, d0);
        }
        mpz_set(mpq_numref(&result), n0);
        mpz_set(mpq_denref(&result), d0);
        if (negative) mpq_neg(&result, &result);
        success = true;
        break;
      }

      mpz_mul(tmp, n0, d0);
      mpz_abs(tmp, tmp);
      if (mpfr_zero_p(r) || mpz_cmp(tmp, p2) > 0) {
        mpfr_get_q(&result, a);
        success = true;
        break;
      }

      mpfr_ui_div(r, 1, r, MPFR_RNDN);  // r = 1/r
    }

    mpz_clears(c, d0, d1, n0, n1, p2, tmp, nullptr);
    mpfr_clears(q, r, nullptr);

    return success;
  }

  void set_var(ElementType& result, int v) const
  {
    (void) v;
    mpq_set_si(&result, 1, 1);
  }

  /** @} */

  /** @name arithmetic
      @{ */
  void negate(ElementType& result, const ElementType& a) const
  {
    mpq_neg(&result, &a);
  }

  bool invert(ElementType& result, const ElementType& a) const
  {
    if (is_unit(a))
      {
        mpq_inv(&result, &a);
        return true;
      }
    set_zero(result);
    return false;
  }

  void add(ElementType& result,
           const ElementType& a,
           const ElementType& b) const
  {
    mpq_add(&result, &a, &b);
  }

  void subtract(ElementType& result,
                const ElementType& a,
                const ElementType& b) const
  {
    mpq_sub(&result, &a, &b);
  }

  void subtract_multiple(ElementType& result,
                         const ElementType& a,
                         const ElementType& b) const
  {
    mpq_t tmp;
    mpq_init(tmp);
    mpq_mul(tmp, &a, &b);
    mpq_sub(&result, &result, tmp);
    mpq_clear(tmp);
  }

  void mult(ElementType& result,
            const ElementType& a,
            const ElementType& b) const
  {
    mpq_mul(&result, &a, &b);
  }

  ///@brief test doc
  void divide(ElementType& result,
              const ElementType& a,
              const ElementType& b) const
  {
    if (is_zero(b)) throw exc::division_by_zero_error();
    mpq_div(&result, &a, &b);
  }

  void power(ElementType& result, const ElementType& a, long n) const
  {
    bool n_is_negative = false;
    if (n < 0)
      {
        if (is_zero(a)) throw exc::division_by_zero_error();
        n_is_negative = true;
        n = -n;
      }
    mpz_pow_ui(mpq_numref(&result), mpq_numref(&a), n);
    mpz_pow_ui(mpq_denref(&result), mpq_denref(&a), n);
    if (n_is_negative)
      mpq_inv(&result, &result);
  }

  void power_mpz(ElementType& result,
                 const ElementType& a,
                 mpz_srcptr n) const
  {
    std::pair<bool, int> n1 = RingZZ::get_si(n);
    if (n1.first)
      power(result, a, n1.second);
    else
      throw exc::engine_error("exponent too large");
  }

  void syzygy(const ElementType& a,
              const ElementType& b,
              ElementType& x,
              ElementType& y) const;
  /** @} */

  /** @name misc
      @{ */
  void swap(ElementType& a, ElementType& b) const { mpq_swap(&a, &b); }
  void random(ElementType& result) const
  {
    rawSetRandomQQ(&result, nullptr);
#if 0
      mpz_urandomb(mpq_numref(&result), mRandomState, mMaxHeight);
      mpz_urandomb(mpq_denref(&result), mRandomState, mMaxHeight);
      mpz_add_ui(mpq_numref(&result), mpq_numref(&result), 1);
      mpz_add_ui(mpq_denref(&result), mpq_denref(&result), 1);
      mpq_canonicalize(&result);
#endif
  }
  /** @} */

  /** @name IO
      @{
  */
  void text_out(buffer& o) const { o << "QQGMP"; }
  void elem_text_out(buffer& o,
                     const ElementType& a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;
  /** @} */

  /** @name translation functions
      @{ */

  void to_ring_elem(ring_elem& result, const ElementType& a) const
  {
    mpq_ptr b = getmemstructtype(mpq_ptr);
    mpq_init(b);
    mpq_set(b, &a);
    mpz_reallocate_limbs(mpq_numref(b));
    mpz_reallocate_limbs(mpq_denref(b));
    result = ring_elem(b);
  }

  void from_ring_elem(ElementType& result, const ring_elem& a) const
  {
    // Currently, until QQ becomes a ConcreteRing, elements of QQ are gmp_QQ
    // (aka mpq_t)
    mpq_set(&result, a.get_mpq());
  }

  const ElementType& from_ring_elem_const(const ring_elem& a) const
  {
    return *a.get_mpq();
  }

/** @} */

#if 0    
    bool promote(const Ring *Rf, const ring_elem f, ElementType& result) const {
      printf("ARingQQGMP::calling promote\n");
      // Rf = ZZ ---> QQ
      if (Rf->is_ZZ())
        {
          set_from_mpz(result, f.get_mpz());
          return true;
        }
      return false;
    }
    
    bool lift(const Ring *Rg, const ElementType& f, ring_elem &result) const {
      return false;
    }
#endif

  // map : this --> target(map)
  //       primelem --> map->elem(first_var)
  // evaluate map(f)
  void eval(const RingMap* map,
            const ElementType& f,
            int first_var,
            ring_elem& result) const;

 private:
  mutable gmp_randstate_t mRandomState;
  long int mMaxHeight;
};
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
