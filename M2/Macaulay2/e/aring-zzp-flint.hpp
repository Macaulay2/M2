// Copyright 2013 Michael E. Stillman

#ifndef _aring_zzp_flint_hpp_
#define _aring_zzp_flint_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "rand.h"

class RingMap;

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>


#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include "flint/arith.h"
#include "flint/nmod_vec.h"
#pragma GCC diagnostic pop

namespace M2 {
/**
\ingroup rings
*/
  class ARingZZpFlint : public RingInterface
  {
    // Integers mod p, implemented as
    // residues in 0..p-1, where
    // p is a word length in size.
    // the largest p is the largest prime < 2^64

  public:
    static const RingID ringID = ring_ZZpFlint;
    typedef mp_limb_t ElementType;  // most of the time an unsigned long.
    typedef ElementType elem;

    ARingZZpFlint(size_t prime);

    ~ARingZZpFlint();

    nmod_t flintModulus() const { return mModulus; }

    // ring informational
    size_t characteristic() const { return mCharac; }

    size_t cardinality() const { return mCharac; }

    unsigned int computeHashValue(const elem& a) const 
    { 
      return static_cast<unsigned int>(a);
    }

    long coerceToLongInteger(const elem& f) const
    {
      long result = f;
      if (result > characteristic()/2) result -= characteristic();
      return result;
    }

    void getGenerator(elem& gen) const { gen = mGenerator; }

    long discreteLog(const elem& a) const; // returns -1 if a is 0

    void text_out(buffer &o) const;

    /////////////////////////////////
    // ElementType informational ////
    /////////////////////////////////

    bool is_unit(ElementType f) const { return f != 0; }
    bool is_zero(ElementType f) const { return f == 0; }
    bool is_equal(ElementType f, ElementType g) const { return f == g; }

    int compare_elems(ElementType f, ElementType g) const {
      if (f > g) return -1;
      if (f < g) return 1;
      return 0;
    }
    
    // 'init', 'init_set' functions

    void init(ElementType &result) const { result = 0; }

    void init_set(ElementType &result, ElementType a) const { result = a; }

    void clear(ElementType &result) const { /* nothing */ }

    void set(ElementType &result, ElementType a) const { result = a; }

    void set_zero(ElementType &result) const { result = 0; }

    void set_from_long(ElementType &result, long a) const {
      //printf("called deprecated and inefficient ARingZZpFlint::set_from_long\n");
      fmpz_t b;
      fmpz_init(b);
      fmpz_set_si(b, a);
      result = fmpz_fdiv_ui(b, mCharac);
      fmpz_clear(b);
    }

    void set_var(ElementType &result, int v) const { result = 1; }

    void set_from_mpz(ElementType &result, mpz_ptr a) const {
      result = mpz_fdiv_ui(a, mCharac);
    }

    void set_from_mpq(ElementType &result, mpq_ptr a) const {
      ElementType n, d;
      set_from_mpz(n, mpq_numref(a));
      set_from_mpz(d, mpq_denref(a));
      if (is_zero(d)) {
         init(result);
         ERROR("fraction cannot be promoted as it would have zero denominator");
         return;
      }
      divide(result,n,d);
    }

    bool set_from_BigReal(ElementType &result, gmp_RR a) const { return false; }

    // arithmetic
    void negate(ElementType &result, ElementType a) const
    {
      result = nmod_neg(a, mModulus);
    }

    void invert(ElementType &result, ElementType a) const
      // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
    {
      result = n_invmod(a, mCharac);
    }

    void add(ElementType &result, ElementType a, ElementType b) const
    {
      result = nmod_add(a, b, mModulus);
    }

    void subtract(ElementType &result, ElementType a, ElementType b) const
    {
      result = nmod_sub(a, b, mModulus);
    }

    void subtract_multiple(ElementType &result, ElementType a, ElementType b) const
    {
    }

    void mult(ElementType &result, ElementType a, ElementType b) const
    {
      result = nmod_mul(a,b, mModulus);
    }

    void divide(ElementType &result, ElementType a, ElementType b) const
    {
      M2_ASSERT(b != 0);
      result = nmod_div(a,b, mModulus);
    }

    void power(ElementType &result, ElementType a, long n) const
    {
      result = n_powmod2_preinv(a, n, mModulus.n, mModulus.ninv);
    }

    void power_mpz(ElementType &result, ElementType a, mpz_ptr n) const
    {
      unsigned long nbar = mpz_fdiv_ui(n, mCharac-1);
      result = n_powmod2_ui_preinv(a, nbar, mModulus.n, mModulus.ninv);
    }

    void swap(ElementType &a, ElementType &b) const
    {
      ElementType tmp = a;
      a = b;
      b = tmp;
    }

    void elem_text_out(buffer &o,
                       ElementType a,
                       bool p_one=true,
                       bool p_plus=false,
                       bool p_parens=false) const;

    void syzygy(ElementType a, ElementType b,
                ElementType &x, ElementType &y) const
    // returns x,y s.y. x*a + y*b == 0.
    // if possible, x is set to 1.
    // no need to consider the case a==0 or b==0.
    {
      M2_ASSERT(a != 0);
      M2_ASSERT(b != 0);
      x = 1;
      divide(y,a,b);
      negate(y,y);
    }

    void random(ElementType &result) const
    {
      result = rawRandomULong(mCharac);
    }
#if 0 
    {     
      fmpz a;
      fmpz_init(&a);
      fmpz_randm(&a, mRandomState, mFmpzCharac);
      result = fmpz_get_ui(&a);
      fmpz_clear(&a);
    }
#endif
    void eval(const RingMap *map, const ElementType f, int first_var, ring_elem &result) const;

    ////////////////////////////
    // to/from ringelem ////////
    ////////////////////////////
    // These simply repackage the element as either a ringelem or an 'ElementType'.
    // No reinitialization is done.
    // Do not take the same element and store it as two different ring_elem's!!
    void to_ring_elem(ring_elem &result, const ElementType &a) const
    {
      ElementType b = a;
      result.poly_val = reinterpret_cast<Nterm*>(b);
    }

    void from_ring_elem(ElementType &result, const ring_elem &a) const
    {
      ElementType b = reinterpret_cast<ElementType>(a.poly_val);
      result = b;
    }



  private:
    nmod_t mModulus;
    size_t mCharac;  // not needed, as it is in mModulus
    mutable flint_rand_t mRandomState;
    fmpz_t mFmpzCharac;
    mp_limb_t mGenerator; // do we need to compute this eagerly?
  };

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
