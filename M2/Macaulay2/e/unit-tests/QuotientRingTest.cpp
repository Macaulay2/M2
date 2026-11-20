// Copyright 2026, The Macaulay2 Authors.
// Tests for ideal creation, Groebner bases, and quotient rings.

#include <gtest/gtest.h>
#include "unit-tests/util-polyring-creation.hpp"
#include "unit-tests/RingElem.hpp"
#include "matrices/matrix.hpp"
TEST(IdealCreation, fromStrings)
{
  // Parsing preserves the ordered generators as entries in a one-row matrix.
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y", "z"});
  ASSERT_NE(R, nullptr);
  const Matrix* I = idealFromStrings(R, {"x^2+y", "y^2-z"});
  ASSERT_NE(I, nullptr);
  EXPECT_EQ(I->n_rows(), 1);
  ASSERT_EQ(I->n_cols(), 2);
  EXPECT_EQ(RingElem(R, I->elem(0, 0)), RingElem::fromString(R, "x^2+y"));
  EXPECT_EQ(RingElem(R, I->elem(0, 1)), RingElem::fromString(R, "y^2-z"));
}

TEST(GroebnerBasis, simple)
{
  // The reduced grevlex basis adds y^2-x to the two input relations.
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y"});
  ASSERT_NE(R, nullptr);
  const Matrix* I = idealFromStrings(R, {"x^2-y", "x*y-1"});
  ASSERT_NE(I, nullptr);
  const Matrix* gb = computeGB(I);
  ASSERT_NE(gb, nullptr);
  ASSERT_EQ(gb->n_rows(), 1);
  ASSERT_EQ(gb->n_cols(), 3);
  for (const auto& expected : {"x^2-y", "x*y-1", "y^2-x"})
    {
      SCOPED_TRACE(expected);
      const auto relation = RingElem::fromString(R, expected);
      bool found = false;
      for (int column = 0; column < gb->n_cols(); ++column)
        found |= RingElem(R, gb->elem(0, column)) == relation;
      EXPECT_TRUE(found);
    }
}

TEST(QuotientRing, arithmetic)
{
  // Squaring the generator reduces to one modulo the defining quadratic.
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y"});
  ASSERT_NE(R, nullptr);
  const Ring* Q = simpleQuotientRing(R, {"x^2-1"});
  ASSERT_NE(Q, nullptr);
  auto x = RingElem::var(Q, 0);
  auto one = RingElem::fromInt(Q, 1);
  EXPECT_EQ(x * x, one);  // x^2 == 1 in Q
}

TEST(QuotientRing, sphere)
{
  // The sphere relation and its square-of-a-sum consequence reduce over QQ.
  const PolynomialRing* R = simplePolynomialRing(0, {"x", "y", "z"});
  ASSERT_NE(R, nullptr);
  const Ring* Q = simpleQuotientRing(R, {"x^2+y^2+z^2-1"});
  ASSERT_NE(Q, nullptr);

  auto x = RingElem::var(Q, 0);
  auto y = RingElem::var(Q, 1);
  auto z = RingElem::var(Q, 2);
  auto one = RingElem::fromInt(Q, 1);

  // The defining relation: x^2 + y^2 + z^2 == 1
  EXPECT_EQ(x * x + y * y + z * z, one);

  // Consequence: (x+y+z)^2 == 1 + 2*(x*y + x*z + y*z)
  auto lhs = (x + y + z).power(2);
  auto rhs = one + 2 * (x * y + x * z + y * z);
  EXPECT_EQ(lhs, rhs);
}

// Local Variables:
// indent-tabs-mode: nil
// End:
