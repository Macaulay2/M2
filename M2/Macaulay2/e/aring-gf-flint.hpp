// Copyright 2014 Michael E. Stillman

#ifndef _aring_gf_flint_hpp_
#define _aring_gf_flint_hpp_

/**
 * @file aring-gf-flint.hpp
 * @brief `M2::ARingGFFlint` --- small `GF(p^k)` via FLINT Zech-logarithm tables.
 *
 * `ARingGFFlint` (registered as `ringID = ring_GFFlintZech`)
 * represents a Galois field `GF(q)` of small order via FLINT's
 * `fq_zech_*` interface. The element type is FLINT's
 * `fq_zech_struct`, which wraps a single `mp_limb_t` Zech log
 * index of a chosen primitive root; every non-zero operation
 * reduces to O(1) integer arithmetic on that index
 * (multiplication is `(i + j) mod (q - 1)`, addition uses a
 * Zech table built once at construction). One word per element
 * and no per-call allocations make this the fastest path for
 * small `q`; FLINT's Zech table sizing imposes the upper limit
 * on `q`. The class inherits from `RingInterface` directly (not
 * `SimpleARing<ARingGFFlint>`) so its nested `Element` can hold
 * an `fq_zech_ctx_struct*` for the destructor.
 *
 * The context is initialised via `fq_zech_ctx_init_modulus`
 * against a user-supplied minimal polynomial; the M2-side entry
 * point is `rawARingGaloisFieldFlintZech` in
 * `interface/aring.cpp`. Larger extensions are reached by the
 * separate `rawARingGaloisFieldFlintBig` entry point (which
 * builds an `ARingGFFlintBig` from `aring-gf-flint-big.hpp`
 * using polynomial-quotient `fq_nmod_*` arithmetic). The native
 * `aring-m2-gf.hpp` path provides the same algebra without a
 * FLINT dependency, and `GF.hpp` is the legacy table-based
 * class.
 *
 * @see aring-gf-flint-big.hpp
 * @see aring-m2-gf.hpp
 * @see GF.hpp
 * @see aring.hpp
 */

#include <vector>

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

// includes gmp.h, which is required for FLINT functions that use GMP
#include <M2/math-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/flint.h>      // for flint_rand_t, fmpz_t, ulong
#include <flint/fmpz.h>       // for fmpz_clear_readonly, fmpz_init_set_...
#include <flint/fq_nmod.h>    // for fq_nmod_clear, fq_nmod_init, fq_nmod_c...
#include <flint/fq_zech.h>    // for fq_zech_clear, fq_zech_ctx_clear, fq_z...
#include <flint/nmod_poly.h>  // for nmod_poly_set_coeff_ui, nmod_poly_clear
#pragma GCC diagnostic pop

#include "interface/random.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "exceptions.hpp"           // for exc::division_by_zero_error

class PolynomialRing;
class RingElement;

namespace M2 {

/**
 * @brief `aring`-style adapter for FLINT's Zech-logarithm representation
 * of small finite fields `GF(p^n)`.
 *
 * @details `ElementType` is `fq_zech_struct` and every arithmetic call
 * delegates to FLINT's `fq_zech_*` routines through the held
 * `mContext` (`fq_zech_ctx_struct*`). The table-driven Zech
 * representation makes addition fast, so this is the preferred
 * `GF(p^n)` backend when the field is small enough for the
 * lookup tables to fit. `ringID = ring_GFFlintZech` so
 * `VectorArithmetic` can dispatch to the matching concrete
 * specialisation. For larger fields where the Zech tables would
 * exceed memory, use `ARingGFFlintBig` instead.
 *
 * @ingroup rings
 */
class ARingGFFlint : public RingInterface
{
 public:
  static const RingID ringID = ring_GFFlintZech;
  typedef fq_zech_struct ElementType;
  typedef ElementType elem;
  typedef std::vector<elem> ElementContainerType;

  /**
   * \brief A wrapper class for ElementType
   *
   * This keeps a pointer to the fq_zech_ctx_struct as it's needed to
   * implement the destructor
   */
  class Element : public ElementImpl<ElementType>
  {
   public:
    Element() = delete;
    Element(Element&& other) : mContext(other.mContext)
    {
      // figure out how to move the value without the context
      fq_zech_init2(&mValue, mContext);
      fq_zech_set(&mValue, &other.mValue, mContext);
    }
    explicit Element(const ARingGFFlint& R) : mContext(R.mContext)
    {
      fq_zech_init2(&mValue, mContext);
    }
    Element(const ARingGFFlint& R, const ElementType& value) : mContext(R.mContext)
    {
      R.init_set(mValue, value);
    }
    ~Element() { fq_zech_clear(&mValue, mContext); }

   private:
    const fq_zech_ctx_struct* mContext;
  };

  /**
   * @brief Fixed-size owned array of `fq_zech_struct` slots tied to an
   * `ARingGFFlint` for the matching FLINT context.
   *
   * @details Each slot is `fq_zech_init2`-initialised at construction and
   * `fq_zech_clear`-released in the destructor, so the array owns
   * its FLINT-side data. Used as the flat per-row buffer the dense
   * linear-algebra templates expect.
   */
  class ElementArray
  {
    const fq_zech_ctx_struct* mContext;
    const size_t mSize;
    std::unique_ptr<ElementType[]> mData;

   public:
    ElementArray(const ARingGFFlint& R, size_t size)
        : mContext(R.mContext), mSize(size), mData(new ElementType[size])
    {
      for (size_t i = 0; i < mSize; i++) fq_zech_init2(&mData[i], mContext);
    }
    ~ElementArray()
    {
      for (size_t i = 0; i < mSize; i++) fq_zech_clear(&mData[i], mContext);
    }
    ElementType& operator[](size_t idx) { return mData[idx]; }
    const ElementType& operator[](size_t idx) const { return mData[idx]; }
    ElementType *data() { return mData.get(); }
    const ElementType *data() const { return mData.get(); }
  };

  ARingGFFlint(const PolynomialRing& R, const ring_elem a);

  ~ARingGFFlint();

  const fq_zech_ctx_struct* flintContext() const { return mContext; }
  long characteristic() const { return mCharacteristic; }
  long dimension() const { return mDimension; }
  const PolynomialRing& originalRing() const { return mOriginalRing; }
  void getGenerator(ElementType& result_gen) const;

  void text_out(buffer& o) const;

 private:
  fq_zech_ctx_t mContext;
  fq_nmod_ctx_t mBigContext;

  const PolynomialRing& mOriginalRing;   // This is a quotient ring k[a]/f(a).
  const RingElement* mPrimitiveElement;  // element in the original ring
  ulong*
      mPPowers;  // array 0..mDimension of powers of mCharacteristic (mod 2^64)
  long mCharacteristic;
  long mDimension;
  mutable flint_rand_t mRandomState;

  mutable ElementType mCachedGenerator;
  mutable bool mGeneratorComputed;

  ////////////////////////////////
  /// Arithmetic functions ///////
  ////////////////////////////////

 public:
  unsigned int computeHashValue(const elem& a) const
  {
    return static_cast<unsigned int>(a.value);
  }

  void to_ring_elem(ring_elem& result, const ElementType& a) const
  {
    // we only use this data type for GF's smaller than 32 bits, since
    // this class creates lookup tables that are way too big otherwise.
    result = ring_elem(static_cast<int>(a.value));
  }

  void from_ring_elem(ElementType& result, const ring_elem& a) const
  {
    result.value = a.get_int();
  }

  ElementType from_ring_elem_const(const ring_elem& a) const
  {
    return {.value = static_cast<mp_limb_t>(a.get_int())};
  }

  bool is_unit(const ElementType& f) const { return not is_zero(f); }
  bool is_zero(const ElementType& f) const
  {
    return fq_zech_is_zero(&f, mContext);
  }
  bool is_equal(const ElementType& f, const ElementType& g) const
  {
    return fq_zech_equal(&f, &g, mContext);
  }

  int compare_elems(const ElementType& f, const ElementType& g) const;

  void copy(ElementType& result, const ElementType& a) const
  {
    fq_zech_set(&result, &a, mContext);
  }
  void init(ElementType& result) const { fq_zech_init2(&result, mContext); }
  void init_set(ElementType& result, const ElementType& a) const
  {
    init(result);
    copy(result, a);
  }
  void set(ElementType& result, const ElementType& a) const { copy(result, a); }
  void set_zero(ElementType& result) const { fq_zech_zero(&result, mContext); }
  void clear(ElementType& result) const { fq_zech_clear(&result, mContext); }
  void set_from_long(ElementType& result, long a) const
  {
    long a1 = a % characteristic();
    if (a1 < 0) a1 += characteristic();
    fq_zech_set_ui(&result, a1, mContext);
  }

  void set_var(ElementType& result, int v) const
  {
    if (v != 0) set_from_long(result, 1);
    std::vector<long> poly = {0, 1};
    fromSmallIntegerCoefficients(result, poly);
    // printf("variable is %lu\n", result.value);
  }

  void set_from_mpz(ElementType& result, mpz_srcptr a) const
  {
    int b = static_cast<int>(mpz_fdiv_ui(a, characteristic()));
    set_from_long(result, b);
  }

  bool set_from_mpq(ElementType& result, mpq_srcptr a) const
  {
    ElementType n, d;
    init(n);
    init(d);
    set_from_mpz(n, mpq_numref(a));
    set_from_mpz(d, mpq_denref(a));
    if (is_zero(d)) return false;
    divide(result, n, d);
    return true;
  }

  bool set_from_BigReal(ElementType& result, gmp_RR a) const
  {
    (void) result;
    (void) a;
    return false;
  }

  void negate(ElementType& result, const ElementType& a) const
  {
    fq_zech_neg(&result, &a, mContext);
  }

  void invert(ElementType& result, const ElementType& a) const
  {
    if (is_zero(a))
      throw exc::division_by_zero_error();
    else fq_zech_inv(&result, &a, mContext);
  }

  void add(ElementType& result,
           const ElementType& a,
           const ElementType& b) const
  {
    fq_zech_add(&result, &a, &b, mContext);
    // printf("zech add %lu + %lu = %lu\n", a.value, b.value, result.value);
  }

  void subtract(ElementType& result,
                const ElementType& a,
                const ElementType& b) const
  {
    fq_zech_sub(&result, &a, &b, mContext);
  }

  void subtract_multiple(ElementType& result,
                         const ElementType& a,
                         const ElementType& b) const
  {
    ElementType c;
    init(c);
    mult(c, a, b);
    subtract(result, result, c);
    clear(c);
  }

  void mult(ElementType& result,
            const ElementType& a,
            const ElementType& b) const
  {
    fq_zech_mul(&result, &a, &b, mContext);
  }

  void divide(ElementType& result,
              const ElementType& a,
              const ElementType& b) const
  {
    // We need to handle the case when result is a, or b.
    // This is why we use the temporary value 'c'.
    ElementType c;
    init(c);
#if 0
      printf("entering divide\n");
      printf("  a = ");
      fq_zech_print_pretty(&a, mContext);
      printf("\n  b = ");
      fq_zech_print_pretty(&b, mContext);
#endif
    invert(c, b);
#if 0
      printf("\n  1/b = ");
      fq_zech_print_pretty(&c, mContext);
#endif
    mult(result, c, a);
#if 0
      printf("\n  a/b = ");
      fq_zech_print_pretty(&result, mContext);
      printf("\n");
#endif
    clear(c);
  }

  void power(ElementType& result, const ElementType& a, int n) const
  {
    if (n < 0)
      {
        invert(result, a);
        fq_zech_pow_ui(&result, &result, -n, mContext);
      }
    else
      fq_zech_pow_ui(&result, &a, n, mContext);
  }

  void power_mpz(ElementType& result, const ElementType& a, mpz_srcptr n) const
  {
    if (mpz_sgn(n) < 0)
      invert(result, a);
    else
      copy(result, a);

    mpz_t abs_n;
    mpz_init(abs_n);
    mpz_abs(abs_n, n);
    
    fmpz_t fn;
    fmpz_init_set_readonly(fn, abs_n);
    fq_zech_pow(&result, &result, fn, mContext);
    fmpz_clear_readonly(fn);
    mpz_clear(abs_n);
  }

  void swap(ElementType& a, ElementType& b) const
  {
    fq_zech_swap(&a, &b, mContext);
  }

  void elem_text_out(buffer& o,
                     const ElementType& a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;

  void syzygy(const ElementType& a,
              const ElementType& b,
              ElementType& x,
              ElementType& y) const
  // returns x,y s.y. x*a + y*b == 0.
  // if possible, x is set to 1.
  // no need to consider the case a==0 or b==0.
  {
    assert(not is_zero(a));
    assert(not is_zero(b));
    set_from_long(x, 1);
    divide(y, a, b);
    negate(y, y);
  }

  void random(ElementType& result) const
  {
    std::vector<long> poly;
    for (int i = 0; i < dimension(); ++i)
      poly.push_back(rawRandomULong(characteristic()));
    fromSmallIntegerCoefficients(result, poly);
    //    fq_zech_randtest(&result, mRandomState, mContext);
  }

  void fromSmallIntegerCoefficients(ElementType& result,
                                    const std::vector<long>& poly) const;

  void getSmallIntegerCoefficients(const ElementType& a,
                                   std::vector<long>& poly) const;

  bool promote(const Ring* Rf, const ring_elem f, ElementType& result) const;

  void lift_to_original_ring(ring_elem& result, const ElementType& f) const;
  // GF specific routine, used in getRepresentation

  bool lift(const Ring* Rg, const ElementType& f, ring_elem& result) const;

  // map : this --> target(map)
  //       primelem --> map->elem(first_var)
  // evaluate map(f)
  void eval(const RingMap* map,
            const ElementType& f,
            int first_var,
            ring_elem& result) const;
};
};

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
