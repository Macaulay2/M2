// Copyright 2013 Michael E. Stillman

#include "aring-zzp.hpp"
#include "aring-promoter.hpp"
#include "ringmap.hpp"

#ifdef HAVE_FLINT
#include "aring-zzp-flint.hpp"

namespace M2 {

  ARingZZpFlint::ARingZZpFlint(size_t p0)
    : mCharac(p0)
  {
    nmod_init(&mModulus, p0);
    flint_randinit(mRandomState);
    fmpz_init(mFmpzCharac);
    fmpz_set_ui(mFmpzCharac, mCharac);
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
    ElementType n = a;
    if (n < 0) // can't happen.  TODO: make sure printing is done balanced.
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
    result = map->get_ring()->from_int(f);
  }

};
#endif // HAVE_FLINT

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
