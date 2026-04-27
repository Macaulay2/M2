#include "aring-CCC.hpp"
#include "text-io.hpp"

namespace M2 {

void ARingCCC::text_out(buffer &o) const { o << "ACCC_" << get_precision(); }
void ARingCCC::elem_text_out(buffer &o,
                             const ElementType &f,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  if (p_plus && (mpfr_cmp_si(&f.re, 0) > 0 ||
                 (mpfr_cmp_si(&f.re, 0) == 0 && mpfr_cmp_si(&f.im, 0) > 0)))
    o << "+";

  if (p_parens && mpfr_cmp_si(&f.re, 0) != 0 && mpfr_cmp_si(&f.im, 0) != 0) {
    if (mpfr_cmp_si(&f.re, 0) < 0) {
      ElementType neg;

      init(neg);
      negate(neg, f);
      o << "-(" << &neg << ")";
      clear(neg);
    } else {
      o << "(" << &f << ")";
    }
  } else {
    if (!p_one && mpfr_cmp_si(&f.re, -1) == 0 && mpfr_cmp_si(&f.im, 0) == 0)
      o << "-";
    else if (p_one || mpfr_cmp_si(&f.re, 1) != 0 || mpfr_cmp_si(&f.im, 0) != 0)
      o << &f;
  }
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
