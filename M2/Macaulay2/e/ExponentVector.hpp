// Copyright 1997 Michael E. Stillman
#pragma once

#include <assert.h>     // for assert
#include <string.h>     // for memcpy
#include <type_traits>  // for make_unsigned
#include <vector>       // for vector

#include "overflow.hpp"  // for add, mult, sub, sub_pos
#include "style.hpp"     // for EQ, GT, LT
#include "buffer.hpp"
#include "util.hpp"

/** Implements safe operations on monomials represented as an array of exponents.
 *
 * This class implements monomial operations on n-tuples, i.e. exponent vectors.
 * No allocation is done by any of these routines.
 *
 * The format for exponents_t is an array [e_0,...,e_nvars-1] of
 * Exponent types representing the monomial Prod T_i^e_i.
 * The 'nvars' information is not stored with the object.
 */
template <class Exponent, bool overflow_check>
class ExponentVector;
// TODO: take advantage of SIMD for operations
// TODO: confirm that if constexpr (overflow_check) is optimized away
// TODO: add packing to the template

template <class E, bool OC>
class ExponentVector
{
 public:
  typedef E Exponent;
  typedef Exponent *Exponents;
  typedef const Exponent *ConstExponents;
  typedef typename std::make_unsigned<Exponent>::type HashExponent;

  // result = a
  static inline void copy(int nvars, ConstExponents a, Exponents result)
  {
    memcpy(result, a, nvars * sizeof(Exponent));
  }

  // result = (0, 0, ..., 0)
  static inline void one(int nvars, Exponents result)
  {
    for (int i = 0; i < nvars; i++) *result++ = 0;
  }

  // returns whether a_i == 0, all i
  static inline bool is_one(int nvars, ConstExponents a)
  {
    for (int i = 0; i < nvars; i++)
      if (a[i] != 0) return false;
    return true;
  }

  // returns whether a_i == b_i, all i
  static inline bool equal(int nvars, ConstExponents a, ConstExponents b)
  {
    return std::equal(a, a + nvars, b);
  }

  // result = a + b
  static inline void mult(int nvars,
                          ConstExponents a,
                          ConstExponents b,
                          Exponents result)
  {
    if constexpr (OC)
      for (int i = nvars; i > 0; i--) *result++ = safe::add(*a++, *b++);
    else
      for (int i = nvars; i > 0; i--) *result++ = *a++ + *b++;
  }

  // result = n * a
  static inline void power(int nvars,
                           ConstExponents a,
                           const Exponent n,
                           Exponents result)
  {
    if constexpr (OC)
      for (int i = nvars; i > 0; i--) *result++ = safe::mult(*a++, n);
    else
      for (int i = nvars; i > 0; i--) *result++ = *a++ * n;
  }

  // result = a + n * b
  static inline void multpower(int nvars,
                               ConstExponents a,
                               ConstExponents b,
                               const Exponent n,
                               Exponents result)
  {
    if constexpr (OC)
      for (int i = nvars; i > 0; i--)
        *result++ = safe::add(*a++, safe::mult(*b++, n));
    else
      for (int i = nvars; i > 0; i--) *result++ = *a++ + *b++ * n;
  }

  // returns whether b_i >= a_i, all i
  static inline bool divides(int nvars, ConstExponents a, ConstExponents b)
  {
    // we go upward, because some rings have unused variables at the end
    for (int i = 0; i < nvars; i++)
      if (a[i] > b[i]) return false;
    return true;
  }

  // result = a - b
  static inline void divide(int nvars,
                            ConstExponents a,
                            ConstExponents b,
                            Exponents result)
  {
    if constexpr (OC)
      for (int i = nvars; i > 0; i--) *result++ = safe::sub(*a++, *b++);
    else
      for (int i = 0; i < nvars; i++) *result++ = *a++ - *b++;
  }

  // result = max(a - b, 0)
  static inline void quotient(int nvars,
                              ConstExponents a,
                              ConstExponents b,
                              Exponents result)
  {
    if constexpr (OC)
      for (int i = nvars; i > 0; i--)
        {
          // TODO: if a,b>=0 always, there would never be an overflow
          assert(*a >= 0 && *b >= 0);
          *result++ = safe::sub_pos(*a++, *b++);
        }
    else
      for (int i = 0; i < nvars; i++)
        {
          Exponent x = *a++;
          Exponent y = *b++;
          *result++ = (x <= y ? 0 : x - y);
        }
  }

  // result = min(a_i, b_i), all i
  static inline void gcd(int nvars,
                         ConstExponents a,
                         ConstExponents b,
                         Exponents result)
  {
    for (int i = nvars; i > 0; i--)
      {
        Exponent x = *a++;
        Exponent y = *b++;
        *result++ = (x < y ? x : y);
      }
  }

  // result = max(a_i, b_i), all i
  static inline void lcm(int nvars,
                         ConstExponents a,
                         ConstExponents b,
                         Exponents result)
  {
    for (int i = nvars; i > 0; i--)
      {
        Exponent x = *a++;
        Exponent y = *b++;
        *result++ = (x > y ? x : y);
      }
  }

  // returns GT, LT, or EQ
  static inline int lex_compare(int nvars, ConstExponents a, ConstExponents b)
  {
    for (int i = 0; i < nvars; i++)
      if (a[i] > b[i])
        return GT;
      else if (a[i] < b[i])
        return LT;
    return EQ;
  }

  // returns sum_i a_i
  static inline Exponent simple_degree(int nvars, ConstExponents a)
  {
    // TODO: use std::accumulate?
    if (nvars == 0) return 0;
    Exponent sum = a[0];
    if constexpr (OC)
      for (int i = 1; i < nvars; i++)
        sum = safe::add(sum, a[i], "degree overflow");
    else
      for (int i = 1; i < nvars; i++) sum += a[i];
    return sum;
  }

  // returns sum_i a_i * wt_i
  static inline Exponent weight(int nvars,
                                ConstExponents a,
                                const std::vector<Exponent> &wts)
  {
    // TODO: use std::inner_product?
    Exponent top = wts.size();
    if (nvars < top) top = nvars;
    if (top == 0) return 0;
    Exponent sum = safe::mult(a[0], wts[0]);
    for (int i = 1; i < top; i++)
      sum = safe::add(sum, safe::mult(a[i], wts[i]), "weight overflow");
    return sum;
  }
  static inline Exponent weight(int nvars, ConstExponents a, M2_arrayint wts)
  {
    return weight(nvars, a, M2_arrayint_to_stdvector<int>(wts));
  }

  // i-th bit of output is 1 if 0 < a[i + k * size_of(int)] for some k
  static HashExponent mask(int nvars, ConstExponents a);
  // FIXME: merge diverging specializations

  // assigns c and d such that a*c = d*b = lcm(a,b)
  static inline void syz(int nvars,
                         ConstExponents a,
                         ConstExponents b,
                         Exponents c,
                         Exponents d)
  {
    for (int i = 0; i < nvars; i++)
      {
        Exponent t = a[i] - b[i];
        if (t >= 0)
          {
            c[i] = 0;
            d[i] = t;
          }
        else
          {
            c[i] = -t;
            d[i] = 0;
          }
      }
  }

  // TODO: do we need this to work without a monoid?
  // compare with elem_text_out in monoid.hpp
  static inline void elem_text_out(buffer &o,
                                   int nvars,
                                   ConstExponents a,
                                   const std::vector<std::string> &varnames,
                                   bool p_one)
  {
    int len_ = 0;
    for (unsigned int v = 0; v < nvars; v++)
      if (a[v] != 0)
        {
          len_++;
          if (varnames.size() < v)
            o << ".";
          else
            o << varnames[v];
          int e = a[v];
          int single = (varnames[v].size() == 1);
          if (e > 1 && single)
            o << e;
          else if (e > 1)
            o << "^" << e;
          else if (e < 0)
            o << "^(" << e << ")";
        }
    if (len_ == 0 && p_one) o << "1";
  }
};

// Legacy specialization
using exponents = ExponentVector<int, true>;
using exponents_t = exponents::Exponents;
using const_exponents = exponents::ConstExponents;

// TODO: compare with ntuple_monomials in e/f4/
template <>
inline exponents::HashExponent exponents::mask(int nvars, ConstExponents exp)
{
  HashExponent result = 0, bit = 1;
  for (int i = nvars - 1; i >= 0; i--)
    {
      if (exp[i] > 0) result |= bit;
      bit <<= 1;
      if (bit == 0) bit = 1;
    }
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
