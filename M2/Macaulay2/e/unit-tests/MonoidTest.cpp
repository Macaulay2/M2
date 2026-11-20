#include "monomials/ExponentVector.hpp"

#include <gtest/gtest.h>

#include <vector>

TEST(ExponentVector, All)
{
  // Known exponent vectors pin arithmetic, order, and syzygy operations.
  const int n = 3;
  {
    // Copying and the identity vector preserve degree and zero exponents.
    SCOPED_TRACE("copy and identity");
    int a[] = {1, 2, 3}, b[] = {7, 5, 3}, result[3];
    exponents::copy(n, a, result);
    EXPECT_TRUE(exponents::equal(n, a, result));
    EXPECT_FALSE(exponents::equal(n, b, result));
    EXPECT_EQ(exponents::simple_degree(n, a), 6);
    exponents::one(n, result);
    EXPECT_FALSE(exponents::is_one(n, a));
    EXPECT_TRUE(exponents::is_one(n, result));
  }
  {
    // Multiplication adds exponents; division and negative powers undo that
    // sum.
    SCOPED_TRACE("multiply, power, and divide");
    int a[] = {1, 2, 3}, b[] = {7, 5, 3}, square[] = {2, 4, 6}, result[3];
    exponents::mult(n, a, a, result);
    EXPECT_TRUE(exponents::equal(n, result, square));
    exponents::power(n, a, 2, result);
    EXPECT_TRUE(exponents::equal(n, result, square));
    EXPECT_TRUE(exponents::divides(n, a, b));
    EXPECT_FALSE(exponents::divides(n, b, a));
    exponents::divide(n, square, a, result);
    EXPECT_TRUE(exponents::equal(n, result, a));
    exponents::divide(n, a, square, result);
    exponents::power(n, result, -1, result);
    EXPECT_TRUE(exponents::equal(n, result, a));
    exponents::quotient(n, a, b, result);
    EXPECT_TRUE(exponents::is_one(n, result));
  }
  {
    // The gcd/lcm use componentwise minima/maxima; multpower adds a multiple.
    SCOPED_TRACE("gcd, lcm, and multpower");
    int a[] = {1, 2, 3}, b[] = {7, 5, 3}, square[] = {2, 4, 6}, result[3];
    int gcd[] = {2, 4, 3}, lcm[] = {7, 5, 6}, product[] = {9, 9, 9};
    exponents::gcd(n, b, square, result);
    EXPECT_TRUE(exponents::equal(n, result, gcd));
    exponents::lcm(n, b, square, result);
    EXPECT_TRUE(exponents::equal(n, result, lcm));
    exponents::multpower(n, b, a, 2, result);
    EXPECT_TRUE(exponents::equal(n, result, product));
  }
  {
    // Unequal leading coordinates decide lex order; every variable occurs in a.
    SCOPED_TRACE("order, weight, and support mask");
    int a[] = {1, 2, 3}, b[] = {7, 5, 3};
    EXPECT_EQ(exponents::lex_compare(n, a, b), LT);
    EXPECT_EQ(exponents::lex_compare(n, b, a), GT);
    EXPECT_EQ(exponents::lex_compare(n, a, a), EQ);
    EXPECT_EQ(exponents::weight(n, a, std::vector<int> {3, 2, 1}), 10);
    EXPECT_EQ(exponents::mask(n, a), 7);
  }
  {
    // Since a divides b, the syzygy multipliers are b/a and one.
    SCOPED_TRACE("syzygy: comparable monomials");
    int a[] = {1, 2, 3}, b[] = {7, 5, 3}, left[3], right[3];
    int expected[] = {6, 3, 0};
    exponents::syz(n, a, b, left, right);
    EXPECT_TRUE(exponents::equal(n, left, expected));
    EXPECT_TRUE(exponents::is_one(n, right));
  }
}
