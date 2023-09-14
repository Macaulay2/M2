// Copyright 2013 Michael E. Stillman

#include "aring-zz-flint.hpp"
#include "ringmap.hpp"

namespace M2 {

ARingZZ::ARingZZ()
{
  flint_randinit(mRandomState);
  fmpz_init(mMaxHeight);
  fmpz_set_ui(mMaxHeight, 100);
}

// This function will likely not ever get called.
ARingZZ::~ARingZZ()
{
  flint_randclear(mRandomState);
  fmpz_clear(mMaxHeight);
}

void ARingZZ::eval(const RingMap* map,
                   const ElementType& f,
                   int first_var,
                   ring_elem& result) const
{
  mpz_t temp;
  flint_mpz_init_set_readonly(temp, &f);
  result = map->get_ring()->from_int(temp);
  flint_mpz_clear_readonly(temp);
}

void ARingZZ::elem_text_out(buffer& o,
                            const ElementType& a,
                            bool p_one,
                            bool p_plus,
                            bool p_parens) const
{
  char* str;

  bool is_neg = (fmpz_cmp_si(&a, 0) == -1);
  bool is_one = (fmpz_cmp_si(&a, 1) == 0 || fmpz_cmp_si(&a, -1) == 0);

  if (!is_neg && p_plus) o << '+';
  if (is_one)
    {
      if (is_neg) o << '-';
      if (p_one) o << '1';
    }
  else
    {
      str = fmpz_get_str(static_cast<char*>(0), 10, &a);
      o << str;
    }
}

void ARingZZ::syzygy(const ElementType& a,
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
  if (fmpz_cmp_ui(&b, 1) == 0)
    {
      set_from_long(x, 1);
      negate(y, a);
      return;
    }
  if (fmpz_cmp_si(&b, -1) == 0)
    {
      set_from_long(x, 1);
      set(y, a);
      return;
    }
  ElementType g;
  init(g);
  fmpz_gcd(&g, &a, &b);
  divide(y, a, g);
  divide(x, b, g);
  if (fmpz_sgn(&x) > 0)
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
