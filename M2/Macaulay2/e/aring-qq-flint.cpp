// Copyright 2013 Michael E. Stillman

#include "aring-qq-flint.hpp"
#include "ringmap.hpp"

#include <iostream>
namespace M2 {

ARingQQFlint::ARingQQFlint()
{
  flint_randinit(mRandomState);
  mMaxHeight = 50;
}

// This function will likely not ever get called.
ARingQQFlint::~ARingQQFlint()
{
  flint_randclear(mRandomState);
}

void ARingQQFlint::eval(const RingMap* map,
                        const ElementType& f,
                        int first_var,
                        ring_elem& result) const
{
  mpq_t temp;
  flint_mpq_init_set_readonly(temp, &f);
  bool ok = map->get_ring()->from_rational(temp, result);
  flint_mpq_clear_readonly(temp);
  if (!ok)
    {
      // if there is already an error message don't add in another
      throw exc::engine_error("cannot map rational to this ring");
    }
}

void ARingQQFlint::elem_text_out(buffer& o,
                                 const ElementType& a,
                                 bool p_one,
                                 bool p_plus,
                                 bool p_parens) const
{
  char s[1000];
  char* str;

  bool is_neg = (fmpq_sgn(&a) == -1);
  bool one = is_pm_one(a);

  size_t size = fmpz_sizeinbase(fmpq_numref(&a), 10) +
                fmpz_sizeinbase(fmpq_denref(&a), 10) + 3;

  char* allocstr = (size > 1000 ? newarray_atomic(char, size) : s);

  // std::cout << "size = " << size << std::endl;
  if (!is_neg && p_plus) o << '+';
  if (one)
    {
      if (is_neg) o << '-';
      if (p_one) o << '1';
    }
  else
    {
      str = fmpq_get_str(allocstr, 10, &a);
      o << str;
    }
  // std::cout << "output = " << o.str() << std::endl;
  if (size > 1000) freemem(allocstr);
}

void ARingQQFlint::syzygy(const ElementType& a,
                          const ElementType& b,
                          ElementType& x,
                          ElementType& y) const
{
  assert(!is_zero(b));
  set_from_long(x, 1);
  divide(y, a, b);
  negate(y, y);
}
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
