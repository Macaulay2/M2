#include "aring-CCi.hpp"

namespace M2 {

void ARingCCi::text_out(buffer &o) const { o << "ACCi_" << mPrecision; }
void ARingCCi::elem_text_out(buffer &o,
                             const ElementType &ap,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{

  if (p_plus)
     o << "+";

  if (p_parens && !mpfi_is_zero(&ap.re) && !mpfi_is_zero(&ap.im))
    o << "(" << &ap << ")";
  else if (p_one || mpfi_cmp_si(&ap.re, 1) != 0 || !mpfi_is_zero(&ap.im))
    o << &ap;
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
