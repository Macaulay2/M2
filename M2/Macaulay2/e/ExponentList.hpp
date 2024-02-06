/* Copyright 2006 by Michael E. Stillman */
#pragma once

#include <cstdio>  // for FILE
#include <vector>  // for vector

#include "engine-includes.hpp"  // for M2_arrayint, M2_ArrayString

#include "ExponentVector.hpp"
#include "buffer.hpp"

class buffer;

/** Implements operations on monomials represented as a list of (var, pow)'s
 *
 * This class implements monomial operations on lists of variable and exponent
 * pairs. No allocation is done by any of these routines.
 *
 * The format for is an array [length, v1, e1, ..., vr, er]
 * with v1 > v2 > ... > vr >= 0, and all exponents ei > 0 and length is 2r+1.
 */
// TODO: reimplement as a std::vector?
template <class Exponent, bool legacy_length>
class ExponentList;

template <class Exponent, bool legacy_length>
class ExponentListIterator;

template <class E, bool L>
class ExponentList
{
  using Iterator = ExponentListIterator<E, L>;
  friend class ExponentListIterator<E, L>;

 public:
  typedef E Exponent;
  typedef Exponent *Exponents;
  typedef const Exponent *ConstExponents;
  typedef typename std::make_unsigned<Exponent>::type HashExponent;
  typedef gc_vector<Exponent> Vector;

  static HashExponent computeHashValue(ConstExponents vp)
  {
    HashExponent hashval = *vp;
    for (Iterator i = vp; i.valid(); ++i)
      hashval = 4624296 * hashval + 2341 * i.var() + i.exponent();
    return hashval;
  }

  static void elem_text_out(buffer &o, ConstExponents m, bool p_one = true)
  {
    Iterator i = m;
    if (!i.valid() and p_one) o << "1";
    for (; i.valid(); ++i)
      {
        Exponent v = i.var();
        Exponent e = i.exponent();
        if (v < 26)
          o << char('a' + v);
        else if (v < 52)
          o << char('A' + v - 26);
        else
          o << "x[" << v << "]";
        if (e > 1)
          o << e;
        else if (e < 0)
          o << "^(" << e << ")";
      }
  }

  static const Exponent length(ConstExponents m)
  {
    if constexpr (L)
      return *m;
    else
      return 2 * *m + 1;
  }
  static const Exponent npairs(ConstExponents m)
  {
    if constexpr (L)
      return (*m - 1) / 2;
    else
      return *m;
  }

  static void one(Vector& result) { result = {1}; }
  static bool is_one(ConstExponents a) { return length(a) == 1; }
  static bool is_equal(ConstExponents a, ConstExponents b)
  {
    return std::equal(a, a + length(a), b);
  }

  static Exponent topvar(ConstExponents a)
  {
    assert(length(a) > 1);
    return a[1];
  }
  static void var(Exponent v, Exponent e, Vector& result)
  {
    // TODO: decide on 1 or 3
    if (e == 0)
      result = {1};
    else
      result = {3, v, e};
  }
  static void copy(ConstExponents vp, Vector& result)
  {
    std::copy(vp, vp + length(vp), std::back_inserter(result));
  }

  // return EQ, LT, or GT for m1 == m2, m1 < m2, or m1 > m2.
  static int compare(ConstExponents a, ConstExponents b)
  {
    Exponent alen = length(a++) - 1;
    Exponent blen = length(b++) - 1;
    // TODO: try using std::lexicographical_compare?
    for (size_t i = 0; i < std::min(alen, blen); i++)
      {
        Exponent c = *a++ - *b++;
        if (c == 0) continue;
        return (c > 0 ? GT : LT);
      }
    if (alen == blen) return EQ;
    return (alen > blen ? GT : LT);
  }

  static void to_expvector(int n, ConstExponents a, exponents::Exponents result);
  static void from_expvector(int n, exponents::ConstExponents a, Vector& result);

  // TODO: make this return by reference?
  static M2_arrayint to_arrayint(ConstExponents vp);
  static void from_arrayint(M2_arrayint m, Vector& result);

  template <typename T>
  static Exponent weight(ConstExponents m, const std::vector<T> &wts)
  {
    Exponent sum = 0;
    auto num_wts = wts.size();
    for (Iterator i = m; i.valid(); ++i)
      sum += i.exponent() * (i.var() < num_wts ? wts[i.var()] : 1);
    return sum;
  }
  static Exponent simple_degree(ConstExponents m)
  {
    Exponent deg = 0;
    for (Iterator i = m; i.valid(); ++i) deg += i.exponent();
    return deg;
  }

  static void mult(ConstExponents a, ConstExponents b, Vector& result);
  // compute the quotient a:b
  static void quotient(ConstExponents a, ConstExponents b, Vector& result);
  static void power(ConstExponents a, Exponent n, Vector& result);

  static void monsyz(ConstExponents a,
                     ConstExponents b,
                     Vector& sa,
                     Vector& sb);

  // whether a is divisible by b
  static bool divides(ConstExponents a, ConstExponents b);
  static void lcm(ConstExponents a, ConstExponents b, Vector& result);
  static void gcd(ConstExponents a, ConstExponents b, Vector& result);

  // divide a by b^infinity
  static void erase(ConstExponents a, ConstExponents b, Vector& result);
  static void radical(ConstExponents a, Vector& result);
  // if a=v^e, then set v and e appropriately, otherwise return false.
  static bool is_pure_power(ConstExponents a, Exponent &v, Exponent &e)
  {
    if (npairs(a) != 1) return false;
    v = a[1];
    e = a[2];
    return true;
  }

  /* These should satisfy: lcm(p,q) == pq
     returns 0 if the pair (p,q) should be REMOVED
     Returns 1 iff either (a) m does not divide pq, or
     (b) m does divide pq, and lcm(m,p) == lcm(m,q) */
  static Exponent buchberger_moeller_keep(ConstExponents m,
                                          ConstExponents p,
                                          ConstExponents q,
                                          ConstExponents pq)
  {
#ifdef DEVELOPMENT
#  warning "buchberger-moeller still to write"
#endif
    return 0;
  }
};

// Legacy specialization
using varpower = ExponentList<int, true>;
using index_varpower = ExponentListIterator<int, true>;
using const_varpower = varpower::ConstExponents;

// TODO: upgrade so you can use one of:
// for (auto it = values.begin(); it != values.end();  ++it )
// for (auto& value : values)
template <class E, bool L>
class ExponentListIterator
{
  typedef E Exponent;
  typedef Exponent *Exponents;
  typedef const Exponent *ConstExponents;

  const Exponent *loc;
  const Exponent *hi;

 public:
  ExponentListIterator() : loc(0), hi(0) {}
  ExponentListIterator(const Exponent *m)
    : loc(m + 1), hi(m + ExponentList<E, L>::length(m)) {}

  ExponentListIterator(const ExponentListIterator &i) : loc(i.loc), hi(i.hi) {}

  bool valid() { return loc < hi; }
  ExponentListIterator &operator++() { loc += 2; return *this; }
  // ExponentListIterator &operator--() { loc -= 2; return *this; }

  Exponent var() { return *loc; }
  Exponent exponent() { return loc[1]; }
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
