// Copyright 2013 Michael E. Stillman

#ifndef _aring_zz_gmp_hpp_
#define _aring_zz_gmp_hpp_

/**
 * @file aring-zz-gmp.hpp
 * @brief `M2::ARingZZGMP` --- aring integer ring backed straight by GMP `mpz_t`.
 *
 * `ARingZZGMP` is a `SimpleARing<ARingZZGMP>` whose `ElementType`
 * is GMP's `__mpz_struct` (the single-element form of `mpz_t`).
 * Arithmetic is delegated to the corresponding GMP primitives
 * (`mpz_add`, `mpz_sub`, `mpz_mul`, `mpz_submul`, `mpz_divexact`,
 * `mpz_pow_ui`, ...); `divide` raises `exc::engine_error("division
 * not exact")` when the dividend is not actually divisible. The
 * `to_ring_elem` path calls
 * `interface/gmp-util.h::mpz_reallocate_limbs` on the exported
 * `mpz_ptr` so its limbs are migrated onto the GC heap before the
 * value is handed to legacy `Ring` code; in-place arithmetic on
 * `ARingZZGMP` values keeps GMP's own limb storage.
 *
 * `ring.cpp::makeIntegerRing` actually returns
 * `new RingZZ` --- the legacy `RingZZ` (`ZZ.hpp`) class, not a
 * `ConcreteRing<ARing*>` --- and `RingZZ` internally carries an
 * `ARingZZGMP *coeffR` member that it forwards arithmetic to.
 * The FLINT-backed sibling `aring-zz-flint.hpp::ARingZZ` is a
 * parallel aring choice but not the one M2's integer ring goes
 * through. `RingZZ::makeMutableMatrix` (in `mat.cpp`) is where
 * `ARingZZGMP` becomes the matrix back end: it builds
 * `MutableMat<DMat<ARingZZGMP>>` or `MutableMat<SMat<ARingZZGMP>>`
 * depending on the `dense` flag, and `mat-linalg.hpp` aliases
 * `DMat<ARingZZGMP>` as `DMatZZGMP` for the matching dense
 * linear-algebra paths.
 *
 * @see ZZ.hpp
 * @see aring-zz-flint.hpp
 * @see aring.hpp
 */

#include "interface/gmp-util.h"  // for mpz_reallocate_limbs
#include "interface/random.h"    // for rawRandomInteger

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "exceptions.hpp"
#include "ZZ.hpp"

namespace M2 {
/**
   @ingroup rings

   @brief wrapper for the mpz_struct integer representation
*/

class ARingZZGMP : public SimpleARing<ARingZZGMP>
{
 public:
  static const RingID ringID = ring_ZZ;

  typedef __mpz_struct ElementType;
  typedef ElementType elem;

  ARingZZGMP();
  ~ARingZZGMP();

 public:
  // ring informational
  size_t characteristic() const { return 0; }
  size_t cardinality() const { return static_cast<size_t>(-1); }
  unsigned int computeHashValue(const elem& a) const
  {
    return static_cast<unsigned int>(mpz_get_ui(&a));
  }

  /** @name properties
      @{
  */

  bool is_unit(const ElementType& f) const
  {
    return mpz_cmp_si(&f, 1) == 0 or mpz_cmp_si(&f, -1) == 0;
  }
  bool is_zero(const ElementType& f) const { return mpz_sgn(&f) == 0; }
  /** @} */

  /** @name operators
      @{ */

  bool is_equal(const ElementType& f, const ElementType& g) const
  {
    return mpz_cmp(&f, &g) == 0;
  }
  int compare_elems(const ElementType& f, const ElementType& g) const
  {
    int cmp = mpz_cmp(&f, &g);
    if (cmp > 0) return 1;
    if (cmp < 0) return -1;
    return 0;
  }
  /** @} */

  /** @name init_set
      @{ */

  void init_set(ElementType& result, const ElementType& a) const
  {
    mpz_init_set(&result, &a);
  }

  void init(ElementType& result) const { mpz_init(&result); }
  static void clear(ElementType& result) { mpz_clear(&result); }

  void set(ElementType& result, const ElementType& a) const
  {
    mpz_set(&result, &a);
  }

  void set_zero(ElementType& result) const { mpz_set_si(&result, 0); }
  void set_from_long(ElementType& result, long a) const
  {
    mpz_set_si(&result, a);
  }

  void set_from_mpz(ElementType& result, mpz_srcptr a) const
  {
    // printf("ARingZZ::calling set_from_mpz\n");
    mpz_set(&result, a);
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

  bool set_from_BigReal(ElementType& result, gmp_RR a) const
  {
    (void) result;
    (void) a;
    return false;
  }

  void set_var(ElementType& result, int v) const
  {
    (void) v;
    mpz_set_si(&result, 1);
  }

  /** @} */

  /** @name arithmetic
      @{ */
  void negate(ElementType& result, const ElementType& a) const
  {
    mpz_neg(&result, &a);
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
    mpz_add(&result, &a, &b);
  }

  void subtract(ElementType& result,
                const ElementType& a,
                const ElementType& b) const
  {
    mpz_sub(&result, &a, &b);
  }

  void subtract_multiple(ElementType& result,
                         const ElementType& a,
                         const ElementType& b) const
  {
    mpz_submul(&result, &a, &b);
  }

  void mult(ElementType& result,
            const ElementType& a,
            const ElementType& b) const
  {
    mpz_mul(&result, &a, &b);
  }

  ///@brief exact division of integers.

  void divide(ElementType& result,
              const ElementType& a,
              const ElementType& b) const
  {
    if (mpz_divisible_p(&a, &b))
      {
        mpz_divexact(&result, &a, &b);
      }
    else
      {
        throw exc::engine_error("division not exact");
      }
  }

  void power(ElementType& result, const ElementType& a, unsigned long n) const
  {
    return mpz_pow_ui(&result, &a, n);
  }

  void power_mpz(ElementType& result,
                 const ElementType& a,
                 mpz_srcptr n) const
  {
    if (mpz_sgn(n) < 0) throw exc::engine_error("can only raise to a nonnegative power");
    std::pair<bool, int> n1 = RingZZ::get_si(n);
    if (n1.first)
      mpz_pow_ui(&result, &a, n1.second);
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
  void swap(ElementType& a, ElementType& b) const { mpz_swap(&a, &b); }
  void random(ElementType& result) const
  {
    rawSetRandomInteger(&result, nullptr);
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
    mpz_set(b, &a);
    mpz_reallocate_limbs(b);
    result = ring_elem(b);
  }

  void from_ring_elem(ElementType& result, const ring_elem& a) const
  {
    const ElementType* t = a.get_mpz();
    mpz_set(&result, t);
  }

  const ElementType& from_ring_elem_const(const ring_elem& a) const
  {
    return *a.get_mpz();
  }

  /** @} */

  bool promote(const Ring* Rf, const ring_elem f, ElementType& result) const
  {
    (void) Rf;
    (void) f;
    (void) result;
    return false;
  }

  bool lift(const Ring* Rg, const ElementType& f, ring_elem& result) const
  {
    (void) Rg;
    (void) f;
    (void) result;
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
    result = mpz_get_si(&n);
    return mpz_fits_slong_p(&n);
  }
};
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
