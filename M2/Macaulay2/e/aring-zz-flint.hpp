// Copyright 2013 Michael E. Stillman

#ifndef _aring_zz_flint_hpp_
#define _aring_zz_flint_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include <iosfwd>
#include "exceptions.hpp"
#include "ZZ.hpp"

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/arith.h>
#pragma GCC diagnostic pop

namespace M2 {
/**
   @ingroup rings

   @brief wrapper for the flint fmpz_t integer representation
*/

class ARingZZ : public RingInterface
{
 public:
  static const RingID ringID = ring_ZZFlint;

  typedef fmpz ElementType;
  typedef ElementType elem;

  ARingZZ();
  ~ARingZZ();

 public:
  // ring informational
  size_t characteristic() const { return 0; }
  size_t cardinality() const { return static_cast<size_t>(-1); }
  unsigned int computeHashValue(const ElementType& a) const
  {
    return static_cast<unsigned int>(fmpz_get_ui(&a));
  }

  /** @name properties
      @{
  */

  bool is_unit(const ElementType& f) const
  {
    return fmpz_is_one(&f) or fmpz_cmp_si(&f, -1) == 0;
  }
  bool is_zero(const ElementType& f) const { return fmpz_is_zero(&f); }
  /** @} */

  /** @name operators
      @{ */

  bool is_equal(const ElementType& f, const ElementType& g) const
  {
    return fmpz_equal(&f, &g);
  }
  int compare_elems(const ElementType& f, const ElementType& g) const
  {
    int cmp = fmpz_cmp(&f, &g);
    if (cmp > 0) return 1;
    if (cmp < 0) return -1;
    return 0;
  }
  /** @} */

  /** @name init_set
      @{ */

  void init_set(ElementType& result, const ElementType& a) const
  {
    fmpz_init_set(&result, &a);
  }

  void init(ElementType& result) const { fmpz_init(&result); }
  void clear(ElementType& result) const { fmpz_clear(&result); }
  void set(ElementType& result, const ElementType& a) const
  {
    fmpz_set(&result, &a);
  }

  void set_zero(ElementType& result) const { fmpz_set_si(&result, 0); }
  void set_from_long(ElementType& result, long a) const
  {
    fmpz_set_si(&result, a);
  }

void set_from_mpz(ElementType& result, mpz_srcptr a) const
  {
    // printf("ARingZZ::calling set_from_mpz\n");
    fmpz_set_mpz(&result, a);
  }

  bool set_from_mpq(ElementType& result, mpq_srcptr a) const
  {
    if (mpz_cmp_si(mpq_denref(a), 1) == 0)
      {
        set_from_mpz(result, mpq_numref(a));
        return true;
      }
    return false;
  }

  bool set_from_BigReal(ElementType& result, gmp_RR a) const { return false; }
  void set_var(ElementType& result, int v) const { fmpz_set_si(&result, 1); }
  /** @} */

  /** @name arithmetic
      @{ */
  void negate(ElementType& result, const ElementType& a) const
  {
    fmpz_neg(&result, &a);
  }

  void invert(ElementType& result, const ElementType& a) const
  {
    if (is_unit(a))
      set(result, a);
    else
      set_zero(result);
  }

  void add(ElementType& result,
           const ElementType& a,
           const ElementType& b) const
  {
    fmpz_add(&result, &a, &b);
  }

  void subtract(ElementType& result,
                const ElementType& a,
                const ElementType& b) const
  {
    fmpz_sub(&result, &a, &b);
  }

  void subtract_multiple(ElementType& result,
                         const ElementType& a,
                         const ElementType& b) const
  {
    fmpz_submul(&result, &a, &b);
  }

  void mult(ElementType& result,
            const ElementType& a,
            const ElementType& b) const
  {
    fmpz_mul(&result, &a, &b);
  }

  ///@brief test doc
  bool divide(ElementType& result,
              const ElementType& a,
              const ElementType& b) const
  {
    if (fmpz_divisible(&a, &b))
      {
        fmpz_divexact(&result, &a, &b);
        return true;
      }
    else
      return false;
  }

  void power(ElementType& result,
             const ElementType& a,
             const unsigned long n) const
  {
    assert(n >= 0);
    return fmpz_pow_ui(&result, &a, n);
  }

  void power_mpz(ElementType& result,
                 const ElementType& a,
                 mpz_srcptr n) const
  {
    if (mpz_sgn(n)<0) ERROR("can only raise to a nonnegative power");
    std::pair<bool, int> n1 = RingZZ::get_si(n);
    if (n1.first)
      fmpz_pow_ui(&result, &a, n1.second);
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
  void swap(ElementType& a, ElementType& b) const { fmpz_swap(&a, &b); }
  void random(ElementType& result) const
  {
    fmpz_randm(&result, mRandomState, mMaxHeight);
  }
  /** @} */

  /** @name IO
      @{
  */
  void text_out(buffer& o) const { o << "ZZFlint"; }
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
    mpz_ptr b = getmemstructtype(mpz_ptr);
    mpz_init(b);
    fmpz_get_mpz(b, &a);
    mpz_reallocate_limbs(b);
    result = ring_elem(b);
  }

  void from_ring_elem(ElementType& result, const ring_elem& a) const
  {
    fmpz_set_mpz(&result, a.get_mpz());
  }

  /** @} */

  bool promote(const Ring* Rf, const ring_elem f, ElementType& result) const
  {
    return false;
  }

  bool lift(const Ring* Rg, const ElementType& f, ring_elem& result) const
  {
    return false;
  }

  // map : this --> target(map)
  //       primelem --> map->elem(first_var)
  // evaluate map(f)
  void eval(const RingMap* map,
            const ElementType& f,
            int first_var,
            ring_elem& result) const;

  bool coerceToLongInteger(long& result, const ElementType& n) const
  {
    result = fmpz_get_si(&n);
    return fmpz_fits_si(&n);
  }

 private:
  mutable flint_rand_t mRandomState;
  fmpz_t mMaxHeight;
};
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
