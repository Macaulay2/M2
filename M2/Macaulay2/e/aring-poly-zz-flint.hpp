// Copyright 2013 Michael E. Stillman

#ifndef _aring_poly_zz_flint_hpp_
#define _aring_poly_zz_flint_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "rand.h"

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>
#include "flint/arith.h"

class RingMap;

namespace M2 {
/**
\ingroup rings
*/
  class ARingZZxFlint : public RingInterface
  {
    // Integers mod p, implemented as
    // residues in 0..p-1, where
    // p is a word length in size.
    // the largest p is the largest prime < 2^64

  public:
    static const RingID ringID = ring_ZZxFlint;
    typedef fmpz_poly_struct ElementType;
    typedef ElementType elem;

    ARingZZxFlint(); // no name, no degree info

    ~ARingZZxFlint();

    // ring informational
    size_t characteristic() const { return 0; }

    size_t cardinality() const { return static_cast<size_t>(-1); }

    void text_out(buffer &o) const;

    /////////////////////////////////
    // ElementType informational ////
    /////////////////////////////////

    bool is_unit(const ElementType& f) const { return fmpz_poly_is_unit(&f); }
    bool is_zero(const ElementType& f) const { return fmpz_poly_is_zero(&f); }
    bool is_equal(const ElementType& f, const ElementType& g) const { return fmpz_poly_equal(&f, &g); }

    int compare_elems(const ElementType& f, const ElementType& g) const {
      // TODO: write this
      throw exc::engine_error("compare_elems for ARingZZxFlint not yet implemented");
#if 0
      if (f > g) return -1;
      if (f < g) return 1;
      return 0;
#endif
    }
    
    // 'init', 'init_set' functions

    void init(ElementType& result) const { fmpz_poly_init(&result); }

    void init_set(ElementType& result, const ElementType& a) const { fmpz_poly_init(&result, &a); }

    void clear(ElementType& result) const { fmpz_poly_clear(&result); }

    void set(ElementType& result, const ElementType& a) const { fmpz_poly_init_set(result, a); }

    void set_zero(ElementType& result) const { fmpz_poly_set_zero(&result); }

    void set_from_long(ElementType& result, long a) const {
      printf("called ARingZZxFlint::set_from_long\n");
      M2_ASSERT(0);
    }

    void set_var(ElementType& result, int v) const { if (v == 0) fmpz_poly_set_coeff_si(result, 1, 1); }

    void set_from_mpz(ElementType& result, mpz_ptr a) const {
      fmpz_poly_set_mpz(&result, a);
    }

    void set_from_mpq(ElementType& result, mpq_ptr a) const {
      M2_ASSERT(false); // TODO: what to do here??
    }

    bool set_from_BigReal(ElementType& result, gmp_RR a) const { return false; }

    // arithmetic
    void negate(ElementType& result, const ElementType& a) const
    {
      fmpz_poly_neg(&result, &a);
    }

    void invert(ElementType &result, ElementType a) const
      // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
    {
      M2_ASSERT(false); //TODO: what to do here?
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
      fmpz a;
      fmpz_init(&a);
      fmpz_randm(&a, mRandomState, mFmpzCharac);
      result = fmpz_get_ui(&a);
    }

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
  };

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
