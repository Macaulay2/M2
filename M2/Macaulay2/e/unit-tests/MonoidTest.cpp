#include <gtest/gtest.h>

#include <iostream>
#include <memory>
#include <vector>

#include "ExponentVector.hpp"
#include "monoid.hpp"

/** Monoid tests */

/** ExponentVector tests */

TEST(ExponentVector, All)
{
  int n = 3;
  int a0[3] = {1, 2, 3}, b0[3] = {7, 5, 3}, c0[3], d0[3];
  exponents_t a = static_cast<exponents_t>(a0), b = static_cast<exponents_t>(b0),
            c = static_cast<exponents_t>(c0), d = static_cast<exponents_t>(d0);
  ntuple::copy(n, a, c);
  EXPECT_EQ(ntuple::equal(n, a, c), true);
  EXPECT_EQ(ntuple::equal(n, b, c), false);
  EXPECT_EQ(ntuple::simple_degree(n, a), 6);
  ntuple::one(n, c);
  EXPECT_EQ(ntuple::is_one(n, a), false);
  EXPECT_EQ(ntuple::is_one(n, c), true);
  ntuple::mult(n, a, a, c);
  ntuple::power(n, a, 2, d);
  EXPECT_EQ(ntuple::equal(n, c, d), true);
  EXPECT_EQ(ntuple::divides(n, a, b), true);
  EXPECT_EQ(ntuple::divides(n, b, a), false);
  ntuple::divide(n, c, a, d);
  EXPECT_EQ(ntuple::equal(n, a, d), true);
  ntuple::divide(n, a, c, d);
  ntuple::power(n, d, -1, d);
  EXPECT_EQ(ntuple::equal(n, a, d), true);
  ntuple::quotient(n, a, b, d);
  EXPECT_EQ(ntuple::is_one(n, d), true);
  ntuple::gcd(n, b, c, d);
  EXPECT_TRUE(d0[0] == 2 && d0[1] == 4 && d0[2] == 3);
  ntuple::lcm(n, b, c, d);
  EXPECT_TRUE(d0[0] == 7 && d0[1] == 5 && d0[2] == 6);
  ntuple::multpower(n, b, a, 2, d);
  EXPECT_TRUE(d0[0] == 9 && d0[1] == 9 && d0[2] == 9);
  EXPECT_EQ(ntuple::lex_compare(n, a, b), LT);
  EXPECT_EQ(ntuple::lex_compare(n, b, a), GT);
  EXPECT_EQ(ntuple::lex_compare(n, a, a), EQ);
  EXPECT_EQ(ntuple::weight(n, a, std::vector<int> {3, 2, 1}), 10);
  // TODO: improve mask and syz tests
  EXPECT_EQ(ntuple::mask(n, a), 7);
  ntuple::syz(n, a, b, c, d);
  EXPECT_TRUE(c0[0] == 6 && c0[1] == 3 && c0[2] == 0);
  EXPECT_TRUE(d0[0] == 0 && d0[1] == 0 && d0[2] == 0);
}
