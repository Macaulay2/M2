#include "aring-CC.hpp"

namespace M2 {

void ARingCC::text_out(buffer &o) const { o << "ACC_53"; }
void ARingCC::elem_text_out(buffer &o,
                            const ElementType &ap,
                            bool p_one,
                            bool p_plus,
                            bool p_parens) const
{
  if (p_plus && (ap.re > 0 || (ap.re == 0 && ap.im > 0)))
    o << "+";

  if (p_parens && ap.re != 0 && ap.im != 0) {
    if (ap.re < 0) {
      ElementType neg;

      init(neg);
      negate(neg, ap);
      o << "-(" << &neg << ")";
      clear(neg);
    } else {
      o << "(" << &ap << ")";
    }
  } else {
    if (!p_one && ap.re == -1 && ap.im == 0)
      o << "-";
    else if (p_one || ap.re != 1 || ap.im != 0)
      o << &ap;
  }
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
