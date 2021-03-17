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
  M2_string s2 = (*gmp_tostringRRpointer)(&(a->right));

  if(p_plus) o << "+";
  o << "[";
  o.put((char *)s1->array, s1->len);
  o << ",";
  o.put((char *)s2->array, s2->len);
  o << "]";
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
