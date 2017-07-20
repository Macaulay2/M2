#include "aring-CC.hpp"

namespace M2 {

void ARingCC::text_out(buffer &o) const { o << "ACC_53"; }
void ARingCC::elem_text_out(buffer &o,
                            const ElementType &ap,
                            bool p_one,
                            bool p_plus,
                            bool p_parens) const
{
  ElementType &ap1 = const_cast<ElementType &>(ap);
  gmp_CC_struct g;
  g.re = getmemstructtype(gmp_RR);
  g.im = getmemstructtype(gmp_RR);
  mpfr_init2(g.re, 53);
  mpfr_init2(g.im, 53);
  mpfr_set_d(g.re, ap1.re, GMP_RNDN);
  mpfr_set_d(g.im, ap1.im, GMP_RNDN);
  M2_string s =
      p_parens ? (*gmp_tonetCCparenpointer)(&g) : (*gmp_tonetCCpointer)(&g);
  mpfr_clear(g.im);
  mpfr_clear(g.re);
  delete g.re;
  delete g.im;

  bool prepend_plus = p_plus && (s->array[0] != '-');
  bool strip_last =
      !p_one && ((s->len == 1 && s->array[0] == '1') ||
                 (s->len == 2 && s->array[1] == '1' && s->array[0] == '-'));

  if (prepend_plus) o << "+";
  if (strip_last)
    o.put(s->array, s->len - 1);
  else
    o.put(s->array, s->len);
}

};  // end namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
