// Copyright 1997-2006 Michael E. Stillman

#ifndef _ntuple_monomial_hpp_
#define _ntuple_monomial_hpp_

#include "ExponentVector.hpp"

// CAVEAT: NO overflow checking is done with this class.
// TODO: make this file obsolete

// Legacy specialization
using ntuple_monomials = ExponentVector<int64_t, false>;

typedef ntuple_monomials::Exponent ntuple_word;
typedef ntuple_word *ntuple_monomial;
typedef const ntuple_word *const_ntuple_monomial;

// FIXME: different implementation
template <>
inline ntuple_monomials::HashExponent ntuple_monomials::mask(int nvars,
                                                             ConstExponents a)
{
  HashExponent result = 0;
  int i;
  size_t j;
  for (i = 0, j = 0; i < nvars; i++, j++)
    {
      if (j == 8 * sizeof(HashExponent)) j = 0;
      if (a[i] > 0) result |= (1 << j);
    }
  return result;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
