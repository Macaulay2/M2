#include "aring-RRi.hpp"

namespace M2 {

void ARingRRi::text_out(buffer &o) const { o << "ARRi_" << mPrecision; }
void ARingRRi::elem_text_out(buffer &o,
                             const ElementType &ap,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  (void) p_parens;

  if(p_plus)
    o << "+";

  // TODO: how do we want to handle -1?  intervals w/ matching endpoints?
  if (p_one || mpfr_cmp_si(&ap.left, 1) != 0 || mpfr_cmp_si(&ap.right, 1) != 0)
    o << &ap;
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
