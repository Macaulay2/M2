// Copyright 2013 Michael E. Stillman

#include "aring-zzp.hpp"
#include "ringmap.hpp"

#include "aring-zzp-flint.hpp"

namespace M2 {

  ARingZZpFlint::ARingZZpFlint(size_t p0)
    : mCharac(p0)
  {
    nmod_init(&mModulus, p0);
    flint_randinit(mRandomState);
    fmpz_init(mFmpzCharac);
    fmpz_set_ui(mFmpzCharac, mCharac);
    mGenerator = n_primitive_root_prime(mCharac);
  }

  ARingZZpFlint::~ARingZZpFlint()
  {
    flint_randclear(mRandomState);
    fmpz_clear(mFmpzCharac);
  }

  void ARingZZpFlint::text_out(buffer &o) const 
  { 
    o << "AZZFlint/" << characteristic(); 
  }

  void ARingZZpFlint::elem_text_out(buffer &o,
                               ElementType a,
                               bool p_one,
                               bool p_plus,
                               bool p_parens) const
  {
    long n = coerceToLongInteger(a);
    if (n < 0)
      {
        o << '-';
        n = -n;
      }
    else if (p_plus)
      o << '+';
    if (p_one || n != 1) o << n;
  }

  void ARingZZpFlint::eval(const RingMap *map, const elem f, int first_var, ring_elem &result) const
  {
    long a = coerceToLongInteger(f);
    result = map->get_ring()->from_long(a);
  }

  long ARingZZpFlint::discreteLog(const elem& a) const
  {
    if (is_zero(a)) return -1;
    long result = n_discrete_log_bsgs(a, mGenerator, mCharac);
    return result;
  }

};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
