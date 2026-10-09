// Copyright 2013 Michael E. Stillman

#include "rings/tower.hpp"

#include <gtest/gtest.h>

#include <string>
#include <vector>

#include "unit-tests/RingTest.hpp"
#include "util.hpp"

TEST(RingTower, create)
{
  // Construction retains both variable names and the prime coefficient field.
  const Tower* R = Tower::create(101, stdvector_to_M2_ArrayString({"a", "b"}));
  ASSERT_NE(R, nullptr);
  EXPECT_EQ(ringName(*R), "Tower[ZZ/101[a,b]]");
  EXPECT_EQ(R->n_vars(), 2);
  EXPECT_EQ(R->characteristic(), 101);
}

TEST(RingTower, elems)
{
  // Expand (a+b+2)^2 with independently specified coefficients in
  // characteristic 101.
  const Tower* R = Tower::create(101, stdvector_to_M2_ArrayString({"a", "b"}));
  ASSERT_NE(R, nullptr);
  const auto a = RingElem::var(R, 0);
  const auto b = RingElem::var(R, 1);
  const auto two = RingElem::fromInt(R, 2);
  const auto four = RingElem::fromInt(R, 4);

  EXPECT_EQ((a + b + two).power(2),
            a.power(2) + b.power(2) + 2 * (a * b) + 4 * a + 4 * b + four);
}
