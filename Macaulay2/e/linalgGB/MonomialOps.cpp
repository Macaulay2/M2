// Copyright 2004 Michael E. Stillman

#include "MonomialOps.hpp"
#include "monoms.h"

uninterned_monomial MonomialOps::mult(MemoryBlock &B, monomial a, monomial b)
{
  int len = *a + *b;
  len = 2*len + 1;
  monomial c = B.reserve(len);
  monomial_mult(a,b,c);
  B.intern(MONOMIAL_LENGTH(c));
  return c;
}

uninterned_monomial MonomialOps::quotient(MemoryBlock &B, monomial a, monomial b)
{
  int len = MONOMIAL_LENGTH(a);
  monomial c = B.reserve(len);
  monomial_quotient(a,b,c);
  B.intern(MONOMIAL_LENGTH(c));
  return c;
}

uninterned_monomial MonomialOps::lcm(MemoryBlock &B, monomial a, monomial b)
{
  int len = *a + *b;
  len = 2*len + 1;
  monomial c = B.reserve(len);
  monomial_lcm(a,b,c);
  B.intern(MONOMIAL_LENGTH(c));
  return c;
}


monomial MonomialOps::mult(MonomialSet *H, monomial a, monomial b)
{
  monomial d;
  int len = *a + *b;
  len = 2*len + 1;
  monomial c = H->reserve(len);
  monomial_mult(a,b,c);
  H->find_or_insert(c,d);
  return d;
}

monomial MonomialOps::quotient(MonomialSet *H, monomial a, monomial b)
{
  monomial d;
  int len = MONOMIAL_LENGTH(a);
  monomial c = H->reserve(len);

  monomial_quotient(a,b,c);
  H->find_or_insert(c,d);

  return d;
}

monomial MonomialOps::lcm(MonomialSet *H, monomial a, monomial b)
{
  monomial d;
  int len = *a + *b;
  len = 2*len + 1;
  monomial c = H->reserve(len);
  monomial_lcm(a,b,c);
  H->find_or_insert(c,d);
  return d;
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
