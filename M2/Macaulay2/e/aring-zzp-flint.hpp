// Copyright 2013 Michael E. Stillman

#ifndef _aring_zzp_flint_hpp_
#define _aring_zzp_flint_hpp_

#include "interface/random.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "error.h"           // for ERROR

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
  typedef std::vector<elem> ElementContainerType;

  ARingZZpFlint(size_t prime);

  ~ARingZZpFlint();

  nmod_t flintModulus() const { return mModulus; }
  // ring informational
  size_t characteristic() const { return mCharac; }
  size_t cardinality() const { return mCharac; }
  unsigned int computeHashValue(const elem &a) const
  {
    return static_cast<unsigned int>(a);
  }

  long coerceToLongInteger(const elem &f) const
  {
    long result = f;
    if (result > characteristic() / 2) result -= characteristic();
    return result;
  }

  void getGenerator(elem &gen) const { gen = mGenerator; }
  long discreteLog(const elem &a) const;  // returns -1 if a is 0

  void text_out(buffer &o) const;

  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

  bool is_unit(ElementType f) const { return f != 0; }
  bool is_zero(ElementType f) const { return f == 0; }
  bool is_equal(ElementType f, ElementType g) const { return f == g; }
  int compare_elems(ElementType f, ElementType g) const
  {
    if (f > g) return -1;
    if (f < g) return 1;
    return 0;
  }

  // 'init', 'init_set' functions

  void init(ElementType &result) const { result = 0; }
  void init_set(ElementType &result, ElementType a) const { result = a; }
  void clear(ElementType &result) const { /* nothing */}

  void set(ElementType &result, ElementType a) const { result = a; }
  void set_zero(ElementType &result) const { result = 0; }
  void set_from_long(ElementType &result, long a) const
  {
    // printf("called deprecated and inefficient
    // ARingZZpFlint::set_from_long\n");
    fmpz_t b;
    fmpz_init(b);
    fmpz_set_si(b, a);
    result = fmpz_fdiv_ui(b, mCharac);
    fmpz_clear(b);
  }

  void set_var(ElementType &result, int v) const { result = 1; }
  void set_from_mpz(ElementType &result, mpz_srcptr a) const
  {
    result = mpz_fdiv_ui(a, mCharac);
  }

  bool set_from_mpq(ElementType &result, mpq_srcptr a) const
  {
    ElementType n, d;
    set_from_mpz(n, mpq_numref(a));
    set_from_mpz(d, mpq_denref(a));
    if (is_zero(d)) return false;
    divide(result, n, d);
    return true;
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

  void subtract_multiple(ElementType &result,
                         ElementType a,
                         ElementType b) const
  {
    ElementType a1 = nmod_neg(a, mModulus);

    // the code below was an attempt to prevent flint from calling the
    // version of the general multiply code if the product of the two flint
    // integers always fits in a single limb.  Speedup was not significant in testing.

    //if (mModulus.norm >= FLINT_BITS/2) /* addmul will fit in a limb */
    //{
    //   mp_limb_t ab_hi, ab_lo;
    //   umul_ppmm(ab_hi, ab_lo, a1, b); // a_hi is not needed in this case
    //   ab_lo = nmod_add(result, ab_lo, mModulus);
    //   NMOD_RED(result,ab_lo,mModulus);
    //}
    //else // product does not fit in a single limb
    
    NMOD_ADDMUL(result, a1, b, mModulus);
  }

  void mult(ElementType &result, ElementType a, ElementType b) const
  {
    result = nmod_mul(a, b, mModulus);
  }

  void divide(ElementType &result, ElementType a, ElementType b) const
  {
    //    assert(b != 0);
    if (b == 0) ERROR("division by zero");
    result = nmod_div(a, b, mModulus);
  }

  void power(ElementType &result, ElementType a, long n) const
  {
    //    assert(a != 0 || n>=0 );
    if (a==0 && n<0) ERROR("division by zero");
    result = n_powmod2_preinv(a, n, mModulus.n, mModulus.ninv);
  }

  void power_mpz(ElementType &result, ElementType a, mpz_srcptr n) const
  {
    //    assert( a != 0 || mpz_sgn(n)>=0);
    if (a==0 && mpz_sgn(n)<0) ERROR("division by zero");
    unsigned long nbar = mpz_fdiv_ui(n, mCharac - 1);
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
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;

  void syzygy(ElementType a,
              ElementType b,
              ElementType &x,
              ElementType &y) const
  // returns x,y s.y. x*a + y*b == 0.
  // if possible, x is set to 1.
  // no need to consider the case a==0 or b==0.
  {
    assert(a != 0);
    assert(b != 0);
    x = 1;
    divide(y, a, b);
    negate(y, y);
  }

  void random(ElementType &result) const { result = rawRandomULong(mCharac); }
#if 0 
    {     
      fmpz a;
      fmpz_init(&a);
      fmpz_randm(&a, mRandomState, mFmpzCharac);
      result = fmpz_get_ui(&a);
      fmpz_clear(&a);
    }
#endif
  void eval(const RingMap *map,
            const ElementType f,
            int first_var,
            ring_elem &result) const;

  ////////////////////////////
  // to/from ringelem ////////
  ////////////////////////////
  
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    assert(sizeof(mp_limb_t) <= sizeof(long));
    result = ring_elem(static_cast<long>(a));
  }

  void from_ring_elem(ElementType &result, const ring_elem &a) const
  {
    result = static_cast<mp_limb_t>(a.get_long());
  }

 private:
  nmod_t mModulus;
  size_t mCharac;  // not needed, as it is in mModulus
  mutable flint_rand_t mRandomState;
  fmpz_t mFmpzCharac;
  mp_limb_t mGenerator;  // do we need to compute this eagerly?
};
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
