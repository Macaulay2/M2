#include "coeffrings.hpp"

void CoefficientRingR::elem_text_out(buffer &o,
                                     ElementType a,
                                     bool p_one,
                                     bool p_plus,
                                     bool p_parens) const
{
  return R->elem_text_out(o, a, p_one, p_plus, p_parens);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
