// Copyright 1997-2006 Michael E. Stillman

#ifndef _ntuple_monomial_hpp_
#define _ntuple_monomial_hpp_

/**
 * @file f4/ntuple-monomial.hpp
 * @brief F4's dense `int64_t` exponent-vector specialisation of `ExponentVector` (legacy).
 *
 * Aliases `ntuple_monomials = ExponentVector<int64_t, false>`
 * for the F4 inner loop's dense monomial encoding: 64-bit
 * signed exponents indexed by variable, with overflow checking
 * disabled. The custom `mask` specialisation defined below
 * derives the F4-side bit-mask used to short-circuit
 * divisibility tests. Per the header's `CAVEAT`, no overflow
 * protection happens at this layer --- F4 trades the safe-add
 * branch for inner-loop speed and relies on sugar tracking,
 * `GBWeight`, and degree checks higher up to reject unreasonable
 * inputs before they reach monomial arithmetic.
 *
 * Counterpart to the sparse `varpower_monomial`; the TODO at the
 * top of the file marks this as a planned eventual deletion
 * once F4 migrates fully to the templated `ExponentVector`
 * surface.
 *
 * @see ExponentVector.hpp
 * @see varpower-monomial.hpp
 * @see overflow.hpp
 */

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
