#include "aring-RRi.hpp"

namespace M2 {

void ARingRRi::text_out(buffer &o) const { o << "ARRi_" << mPrecision; }
void ARingRRi::elem_text_out(buffer &o,
                             const ElementType &ap,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  mpfi_ptr a = &const_cast<ElementType &>(ap);
  M2_string s1 = (*gmp_tostringRRpointer)(&(a->left));
  M2_string s2 = (*gmp_tostringRRpointer)(&(a->left));
  bool prepend_plus1 = p_plus && (s1->array[0] != '-');
  bool strip_last1 =
      !p_one && ((s1->len == 1 && s1->array[0] == '1') ||
                 (s1->len == 2 && s1->array[1] == '1' && s1->array[0] == '-'));
  bool prepend_plus2 = p_plus && (s2->array[0] != '-');
  bool strip_last2 =
    !p_one && ((s2->len == 1 && s2->array[0] == '1') ||
               (s2->len == 2 && s2->array[1] == '1' && s2->array[0] == '-'));

  o << "[";
  if (prepend_plus1) o << "+";
    if (strip_last1)
      o.put(s1->array, s1->len - 1);
    else
      o.put(s1->array, s1->len);
  o << ",";
  if (prepend_plus2) o << "+";
    if (strip_last2)
      o.put(s2->array, s2->len - 1);
    else
      o.put(s2->array, s2->len);
  o << "]";
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
