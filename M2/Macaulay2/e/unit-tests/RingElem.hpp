// Copyright 2026, The Macaulay2 Authors.
//
// RingElem: A lightweight value-semantics wrapper around (const Ring*, ring_elem).
// Unlike RingElement (which is heap-allocated and returns pointers from operators),
// RingElem lives on the stack and operators return values, making test code concise:
//
//   auto x = RingElem::var(W, 0);
//   auto Dx = RingElem::var(W, 2);
//   auto one = RingElem::fromInt(W, 1);
//   EXPECT_EQ(Dx * x - x * Dx, one);
//
//   auto f = RingElem::fromString(R, "x^2+3*x*y-1");

#ifndef M2_UNIT_TESTS_RINGELEM_HPP
#define M2_UNIT_TESTS_RINGELEM_HPP

#include <cassert>
#include <iostream>
#include <string>
#include <vector>

#include "buffer.hpp"
#include "rings/ring.hpp"
#include "rings/polyring.hpp"
#include "monoid.hpp"
#include "BasicPoly.hpp"

class RingElem
{
  const Ring *mRing;
  ring_elem mValue;

 public:
  // Primary constructor: wraps a pre-computed ring_elem.
  // Use the static factories below for common cases.
  RingElem(const Ring *R, ring_elem f) : mRing(R), mValue(f) {}

  // Factory: create the i-th variable of the ring
  static RingElem var(const Ring *R, int i) { return RingElem(R, R->var(i)); }

  // Factory: create a ring element from an integer
  static RingElem fromInt(const Ring *R, long n)
  {
    return RingElem(R, R->from_long(n));
  }

  // Factory: create a ring element from a string.
  // For polynomial rings: parses "x^2+3*x*y-1" using variable names from the ring.
  // For base rings (ZZ, ZZ/p): parses an integer.
  // Throws parsing_error on failure.
  // NOTE: fromString and toString are not yet inverses of each other.
  // toString outputs e.g. "x3+2xyz" while fromString expects "x^3+2*x*y*z".
  // TODO: make these round-trip compatible.
  // TODO: fromDouble
  static RingElem fromString(const Ring *R, const std::string &s);

  // Accessors
  const Ring *ring() const { return mRing; }
  ring_elem value() const { return mValue; }

  // Predicates
  bool isZero() const { return mRing->is_zero(mValue); }
  bool isUnit() const { return mRing->is_unit(mValue); }

  // Comparison
  bool operator==(const RingElem &b) const
  {
    assert(mRing == b.mRing && "RingElem comparison requires elements from the same ring");
    return mRing->is_equal(mValue, b.mValue);
  }
  bool operator!=(const RingElem &b) const { return !(*this == b); }

  // Arithmetic (returns values, not pointers)
  RingElem operator-() const { return RingElem(mRing, mRing->negate(mValue)); }

  RingElem operator+(const RingElem &b) const
  {
    assert(mRing == b.mRing && "RingElem addition requires elements from the same ring");
    return RingElem(mRing, mRing->add(mValue, b.mValue));
  }

  RingElem operator-(const RingElem &b) const
  {
    assert(mRing == b.mRing && "RingElem subtraction requires elements from the same ring");
    return RingElem(mRing, mRing->subtract(mValue, b.mValue));
  }

  RingElem operator*(const RingElem &b) const
  {
    assert(mRing == b.mRing && "RingElem multiplication requires elements from the same ring");
    return RingElem(mRing, mRing->mult(mValue, b.mValue));
  }

  RingElem operator/(const RingElem &b) const
  {
    assert(mRing == b.mRing && "RingElem division requires elements from the same ring");
    return RingElem(mRing, mRing->divide(mValue, b.mValue));
  }

  // Scalar multiplication
  RingElem operator*(long n) const
  {
    return RingElem(mRing, mRing->mult(mRing->from_long(n), mValue));
  }
  friend RingElem operator*(long n, const RingElem &f) { return f * n; }

  RingElem power(int n) const { return RingElem(mRing, mRing->power(mValue, n)); }

  // String output (for gtest diagnostics)
  std::string toString() const
  {
    buffer o;
    mRing->elem_text_out(o, mValue);
    return std::string(o.str());
  }

  friend std::ostream &operator<<(std::ostream &os, const RingElem &f)
  {
    return os << f.toString();
  }
};

#endif
