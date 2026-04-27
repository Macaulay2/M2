#include "aring-RR.hpp"

namespace M2 {

void ARingRR::text_out(buffer &o) const { o << "ARR_53"; }
void ARingRR::elem_text_out(buffer &o,
                            const ElementType &ap,
                            bool p_one,
                            bool p_plus,
                            bool p_parens) const
{
  (void) p_parens;

  if (p_plus && ap > 0)
    o << "+";

  if (!p_one && ap == -1)
    o << "-";
  else if (p_one || ap != 1)
    o << ap;
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
