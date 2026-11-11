// Copyright 2016 Michael E. Stillman

#include "NAG/NAG.hpp"

#include <gtest/gtest.h>

#include <cstdlib>
#include <vector>

TEST(PointArray, constructor)
{
  // Explicit and generated weights both construct empty, usable point indexes.
  {
    SCOPED_TRACE("constructor: explicit weights");
    PointArray points(0.001, {0.3, 0.7});
    EXPECT_DOUBLE_EQ(points.weight({1, 1}), 1);
    EXPECT_EQ(points.lookup({0, 0}), -1);
  }
  {
    SCOPED_TRACE("constructor: generated weights, seed 0");
    std::srand(0);
    PointArray points(0.001, 10);
    EXPECT_NEAR(points.weight(std::vector<double>(10, 1)), 1, 1e-14);
    EXPECT_EQ(points.lookup(std::vector<double>(10, 0)), -1);
  }
}

TEST(PointArray, lookup)
{
  // Distinct points get new indices; exact and nearby repeats reuse their
  // index.
  {
    SCOPED_TRACE("lookup: distinct four-dimensional points");
    PointArray points(0.0001, {0.1, 0.2, 0.3, 0.4});
    EXPECT_EQ(points.lookup_or_append({1, 0, .54344, .80331}), 0);
    EXPECT_EQ(points.lookup_or_append({1, 0, -.25281, 1.032}), 1);
  }
  {
    SCOPED_TRACE("lookup: duplicate and nearby two-dimensional points");
    PointArray points(0.001, {0.3, 0.7});
    EXPECT_EQ(points.lookup({0.1, -0.1}), -1);
    EXPECT_EQ(points.lookup_or_append({0.1, -0.1}), 0);
    EXPECT_EQ(points.lookup_or_append({0.1, -0.1}), 0);
    EXPECT_EQ(points.lookup_or_append({0.1, -0.2}), 1);
    EXPECT_EQ(points.lookup({0.1, -0.10001}), 0);
    EXPECT_EQ(points.lookup_or_append({0.1, -0.10001}), 0);
  }
}
