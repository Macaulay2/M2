// Copyright 2016 Michael E. Stillman

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
