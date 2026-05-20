/**
 * @file unit-tests/MonoidTest.cpp
 * @brief gtest coverage for the `ExponentVector` exponent-array primitives.
 *
 * Hosts the `TEST(ExponentVector, All)` battery that exercises
 * the raw `exponents_t` (`int*`) primitives every polynomial
 * multiplication in the engine sits on top of: `copy`, `equal`,
 * `simple_degree`, `one` / `is_one`, `mult`, `power`,
 * `multpower`, `divides`, `divide`, `quotient`, `gcd`, `lcm`,
 * `lex_compare`, `weight`, `mask`, and the `syz` syzygy split.
 * The `Monoid tests` comment block is a placeholder for future
 * coverage; today the file only exercises the underlying
 * exponent-array layer.
 *
 * Subtle bugs in these primitives surface several layers up as
 * "F4 returns the wrong GB" or "Hilbert function is off by
 * one"; catching them here keeps the failure local. Exponents
 * are stored as raw `int*` rather than `std::vector<int>` so
 * the F4 inner loop can read entries with a single load --- the
 * tests use pre-allocated `int[3]` stack arrays passed through
 * `static_cast<exponents_t>` to match that representation.
 *
 * @see ExponentVector.hpp
 * @see monoid.hpp
 * @see ARingTest.hpp
 * @see f4/f4.hpp
 */

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
  exponents::copy(n, a, c);
  EXPECT_EQ(exponents::equal(n, a, c), true);
  EXPECT_EQ(exponents::equal(n, b, c), false);
  EXPECT_EQ(exponents::simple_degree(n, a), 6);
  exponents::one(n, c);
  EXPECT_EQ(exponents::is_one(n, a), false);
  EXPECT_EQ(exponents::is_one(n, c), true);
  exponents::mult(n, a, a, c);
  exponents::power(n, a, 2, d);
  EXPECT_EQ(exponents::equal(n, c, d), true);
  EXPECT_EQ(exponents::divides(n, a, b), true);
  EXPECT_EQ(exponents::divides(n, b, a), false);
  exponents::divide(n, c, a, d);
  EXPECT_EQ(exponents::equal(n, a, d), true);
  exponents::divide(n, a, c, d);
  exponents::power(n, d, -1, d);
  EXPECT_EQ(exponents::equal(n, a, d), true);
  exponents::quotient(n, a, b, d);
  EXPECT_EQ(exponents::is_one(n, d), true);
  exponents::gcd(n, b, c, d);
  EXPECT_TRUE(d0[0] == 2 && d0[1] == 4 && d0[2] == 3);
  exponents::lcm(n, b, c, d);
  EXPECT_TRUE(d0[0] == 7 && d0[1] == 5 && d0[2] == 6);
  exponents::multpower(n, b, a, 2, d);
  EXPECT_TRUE(d0[0] == 9 && d0[1] == 9 && d0[2] == 9);
  EXPECT_EQ(exponents::lex_compare(n, a, b), LT);
  EXPECT_EQ(exponents::lex_compare(n, b, a), GT);
  EXPECT_EQ(exponents::lex_compare(n, a, a), EQ);
  EXPECT_EQ(exponents::weight(n, a, std::vector<int> {3, 2, 1}), 10);
  // TODO: improve mask and syz tests
  EXPECT_EQ(exponents::mask(n, a), 7);
  exponents::syz(n, a, b, c, d);
  EXPECT_TRUE(c0[0] == 6 && c0[1] == 3 && c0[2] == 0);
  EXPECT_TRUE(d0[0] == 0 && d0[1] == 0 && d0[2] == 0);
}
