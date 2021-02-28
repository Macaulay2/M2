// Copyright 2013 Michael E. Stillman

#include "aring-zz-gmp.hpp"
#include "ringmap.hpp"

namespace M2 {

ARingZZGMP::ARingZZGMP() {}
// This function will likely not ever get called.
ARingZZGMP::~ARingZZGMP() {}
void ARingZZGMP::eval(const RingMap* map,
                      const ElementType& f,
                      int first_var,
                      ring_elem& result) const
{
  mpz_ptr f1 = static_cast<mpz_ptr>(const_cast<ElementType*>(&f));
  result = map->get_ring()->from_int(f1);
}

void ARingZZGMP::elem_text_out(buffer& o,
                               const ElementType& a,
                               bool p_one,
                               bool p_plus,
                               bool p_parens) const
{
  char* str;

  bool is_neg = (mpz_cmp_si(&a, 0) == -1);
  bool is_one = (mpz_cmp_si(&a, 1) == 0 || mpz_cmp_si(&a, -1) == 0);

  if (!is_neg && p_plus) o << '+';
  if (is_one)
    {
      if (is_neg) o << '-';
      if (p_one) o << '1';
    }
  else
    {
      str = mpz_get_str(static_cast<char*>(0), 10, &a);
      o << str;
      delete str;
    }
}

void ARingZZGMP::syzygy(const ElementType& a,
                        const ElementType& b,
                        ElementType& x,
                        ElementType& y) const
{
  assert(!is_zero(b));
  // First check the special cases a = 0, b = 1, -1.  Other cases: use gcd.
  if (is_zero(a))
    {
      set_from_long(x, 1);
      set_zero(y);
      return;
    }
  if (mpz_cmp_ui(&b, 1) == 0)
    {
      set_from_long(x, 1);
      negate(y, a);
      return;
    }
  if (mpz_cmp_si(&b, -1) == 0)
    {
      set_from_long(x, 1);
      set(y, a);
      return;
    }
  elem g;
  init(g);
  mpz_gcd(&g, &a, &b);
  divide(y, a, g);
  divide(x, b, g);
  if (mpz_sgn(&x) > 0)
    negate(y, y);
  else
    negate(x, x);
  clear(g);
}
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
