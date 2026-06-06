// Copyright 2026, The Macaulay2 Authors.
// Tests for ideal creation, Groebner bases, and quotient rings.

#include <gtest/gtest.h>
#include "unit-tests/util-polyring-creation.hpp"
#include "unit-tests/RingElem.hpp"
#include "matrices/matrix.hpp"
#include "debug.hpp"
// Step 1: idealFromStrings
TEST(IdealCreation, fromStrings)
{
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y", "z"});
  const Matrix* I = idealFromStrings(R, {"x^2+y", "y^2-z"});
  dmatrix(I);
  ASSERT_NE(I, nullptr);
  EXPECT_EQ(I->n_rows(), 1);
  EXPECT_EQ(I->n_cols(), 2);
}

// Step 2: computeGB
TEST(GroebnerBasis, simple)
{
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y"});
  const Matrix* I = idealFromStrings(R, {"x^2-y", "x*y-1"});
  const Matrix* gb = computeGB(I);
  ASSERT_NE(gb, nullptr);
  dmatrix(gb);
  EXPECT_GE(gb->n_cols(), 2);
}

// Step 3: simpleQuotientRing
TEST(QuotientRing, arithmetic)
{
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y"});
  const Ring* Q = simpleQuotientRing(R, {"x^2-1"});
  ASSERT_NE(Q, nullptr);
  auto x = RingElem::var(Q, 0);
  auto one = RingElem::fromInt(Q, 1);
  EXPECT_EQ(x * x, one);  // x^2 == 1 in Q
}

TEST(QuotientRing, sphere)
{
  // QQ[x,y,z] / (x^2+y^2+z^2-1)
  const PolynomialRing* R = simplePolynomialRing(0, {"x", "y", "z"});
  const Ring* Q = simpleQuotientRing(R, {"x^2+y^2+z^2-1"});
  ASSERT_NE(Q, nullptr);

  auto x = RingElem::var(Q, 0);
  auto y = RingElem::var(Q, 1);
  auto z = RingElem::var(Q, 2);
  auto one = RingElem::fromInt(Q, 1);

  // The defining relation: x^2 + y^2 + z^2 == 1
  EXPECT_EQ(x*x + y*y + z*z, one);

  // Consequence: (x+y+z)^2 == 1 + 2*(x*y + x*z + y*z)
  auto lhs = (x + y + z).power(2);
  auto rhs = one + 2 * (x*y + x*z + y*z);
  EXPECT_EQ(lhs, rhs);
}

// Local Variables:
// indent-tabs-mode: nil
// End:
