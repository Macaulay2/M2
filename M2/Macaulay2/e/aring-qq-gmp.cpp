// Copyright 2013 Michael E. Stillman

#include "aring-qq-gmp.hpp"

#include "interface/random.h"
#include "ringmap.hpp"

namespace M2 {

ARingQQGMP::ARingQQGMP()
{
  gmp_randinit_default(mRandomState);
  mMaxHeight = 50;
}

// This function will likely not ever get called.
ARingQQGMP::~ARingQQGMP() { gmp_randclear(mRandomState); }
void ARingQQGMP::eval(const RingMap* map,
                      const ElementType& f,
                      int first_var,
                      ring_elem& result) const
{
  bool ok = map->get_ring()->from_rational(&f, result);
  if (!ok)
    {
      // if there is already an error message don't add in another
      if (not error()) ERROR("cannot map rational to this ring");
      result = map->get_ring()->from_long(0);
    }
}

void ARingQQGMP::elem_text_out(buffer& o,
                               const ElementType& a,
                               bool p_one,
                               bool p_plus,
                               bool p_parens) const
{
  char s[1000];
  char* str;

  bool is_neg = (mpq_sgn(&a) == -1);
  bool one = is_pm_one(a);

  size_t size = mpz_sizeinbase(mpq_numref(&a), 10) +
                mpz_sizeinbase(mpq_denref(&a), 10) + 3;

  char* allocstr = (size > 1000 ? newarray_atomic(char, size) : s);

  if (!is_neg && p_plus) o << '+';
  if (one)
    {
      if (is_neg) o << '-';
      if (p_one) o << '1';
    }
  else
    {
      str = mpq_get_str(allocstr, 10, &a);
      o << str;
    }
  if (size > 1000) freemem(allocstr);
}

void ARingQQGMP::syzygy(const ElementType& a,
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
