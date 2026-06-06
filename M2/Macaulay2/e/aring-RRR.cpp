#include "aring-RRR.hpp"

namespace M2 {

void ARingRRR::text_out(buffer &o) const { o << "ARRR_" << mPrecision; }
void ARingRRR::elem_text_out(buffer &o,
                             const ElementType &ap,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  (void) p_parens;

  if (p_plus && mpfr_cmp_si(&ap, 0) > 0)
    o << "+";

  if (!p_one && mpfr_cmp_si(&ap, -1) == 0)
    o << "-";
  else if (p_one || mpfr_cmp_si(&ap, 1) != 0)
    o << &ap;
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
