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

template <>
inline void ntuple_monomials::mult(int nvars,
                                   ntuple_monomials::ConstExponents a,
                                   ntuple_monomials::ConstExponents b,
                                   ntuple_monomials::Exponents result)
{
  for (int i = 0; i < nvars; i++)
    {
      Exponent x = *a++;
      Exponent y = *b++;
      *result++ = x + y;
    }
}

template <>
inline void ntuple_monomials::divide(int nvars,
                                     ntuple_monomials::ConstExponents a,
                                     ntuple_monomials::ConstExponents b,
                                     ntuple_monomials::Exponents result)
{
  for (int i = 0; i < nvars; i++)
    {
      Exponent x = *a++;
      Exponent y = *b++;
      *result++ = x - y;
    }
}

template <>
inline void ntuple_monomials::quotient(int nvars,
                                       ntuple_monomials::ConstExponents a,
                                       ntuple_monomials::ConstExponents b,
                                       ntuple_monomials::Exponents result)
{
  for (int i = 0; i < nvars; i++)
    {
      Exponent x = *a++;
      Exponent y = *b++;
      if (x <= y)
        *result++ = 0;
      else
        *result++ = x - y;
    }
}

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

template <>
inline ntuple_monomials::Exponent ntuple_monomials::simple_degree(
    int nvars,
    ntuple_monomials::ConstExponents a)
{
  Exponent sum = 0;
  for (int i = 0; i < nvars; i++) sum += a[i];
  return sum;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
