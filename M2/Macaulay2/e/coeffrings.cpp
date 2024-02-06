#include "coeffrings.hpp"

void CoefficientRingZZp::elem_text_out(buffer &o,
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
