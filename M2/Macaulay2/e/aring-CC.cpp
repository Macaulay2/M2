#include "aring-CC.hpp"

namespace M2 {

void ARingCC::text_out(buffer &o) const { o << "ACC_53"; }
void ARingCC::elem_text_out(buffer &o,
                            const ElementType &ap,
                            bool p_one,
                            bool p_plus,
                            bool p_parens) const
{
  gmp_CC g = toBigComplex(ap);
  M2_string s =
    p_parens ? (*gmp_tonetCCparenpointer)(g) : (*gmp_tonetCCpointer)(g);
  
  bool prepend_plus = p_plus && (s->array[0] != '-');
  bool strip_last =
      !p_one && ((s->len == 1 && s->array[0] == '1') ||
                 (s->len == 2 && s->array[1] == '1' && s->array[0] == '-'));

  if (prepend_plus) o << "+";
  if (strip_last)
    o.put((char *)s->array, s->len - 1);
  else
    o.put((char *)s->array, s->len);
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
