// Copyright 2016 Michael E. Stillman

/**
 * @file unit-tests/PointArray.cpp
 * @brief gtest coverage for `PointArray` --- NAG's epsilon-bucketed numerical-point clustering structure.
 *
 * Hosts the `TEST(PointArray, *)` battery that builds
 * `PointArray` instances at fixed epsilons (`0.0001` and
 * `0.001` in the smoke tests), confirms `lookup` returns `-1`
 * on misses, `lookup_or_append` adds new clusters and returns
 * their indices, and re-querying a near-duplicate within
 * epsilon resolves back to the same bucket. The structure
 * underpins Numerical Algebraic Geometry's witness-set and
 * monodromy paths, where the same numerical point can be
 * encountered repeatedly under floating-point perturbation and
 * must be identified across orderings of arrival.
 *
 * Companion files in the `file-misc-tests` family
 * (`SubsetTest.cpp` for subset combinatorics,
 * `basics-test.cpp` for the harness-itself liveness check) get
 * their own `@file` blocks on separate firings.
 *
 * @see NAG.hpp
 * @see interface/NAG.h
 * @see SubsetTest.cpp
 */

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "NAG.hpp"

TEST(PointArray, constructor)
{
  PointArray p(0.001, {0.3, 0.7});
  PointArray q(0.001, 10);
}

TEST(PointArray, lookup)
{
  PointArray q(0.0001, 4);
  EXPECT_EQ(q.lookup_or_append({1, 0, .54344, .80331}), 0);
  EXPECT_EQ(q.lookup_or_append({1, 0, -.25281, 1.032}), 1);
  PointArray p(0.001, 2);
  EXPECT_EQ(p.lookup({0.1, -0.1}), -1);
  EXPECT_EQ(p.lookup_or_append({0.1, -0.1}), 0);
  EXPECT_EQ(p.lookup_or_append({0.1, -0.1}), 0);
  EXPECT_EQ(p.lookup_or_append({0.1, -0.2}), 1);
  EXPECT_EQ(p.lookup({0.1, -0.10001}), 0);
  EXPECT_EQ(p.lookup_or_append({0.1, -0.10001}), 0);
}
