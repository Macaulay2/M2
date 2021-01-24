// Copyright 2013 Michael E. Stillman

#ifndef _aring_QQ_gmp_hpp_
#define _aring_QQ_gmp_hpp_

#include "interface/random.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include <iosfwd>
#include "exceptions.hpp"

// promote needs ring.hpp.  After moving promote out, remove it here!
#include "ring.hpp"

namespace M2 {
/**
   @ingroup rings

   @brief wrapper for the gmp mpq_t integer representation
*/

class ARingQQGMP : public RingInterface
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
  void clear(ElementType& result) const { mpq_clear(&result); }
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

  bool set_from_BigReal(ElementType& result, gmp_RR a) const { return false; }
  void set_var(ElementType& result, int v) const { mpq_set_si(&result, 1, 1); }
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
  bool divide(ElementType& result,
              const ElementType& a,
              const ElementType& b) const
  {
    if (is_zero(b)) return false;
    mpq_div(&result, &a, &b);
    return true;
  }

  void power(ElementType& result, const ElementType& a, long n) const
  {
    assert(n >= 0);
    if (n >= 0)
      {
        mpz_pow_ui(mpq_numref(&result), mpq_numref(&a), n);
        mpz_pow_ui(mpq_denref(&result), mpq_denref(&a), n);
      }
    else
      {
        n = -n;
        mpz_pow_ui(mpq_numref(&result), mpq_denref(&a), n);
        mpz_pow_ui(mpq_denref(&result), mpq_numref(&a), n);
      }
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
    rawSetRandomQQ(&result, 0);
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
