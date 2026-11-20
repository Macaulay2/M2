// Copyright 2013 Michael E. Stillman

#include <gtest/gtest.h>

#include <algorithm>

#include "comb.hpp"

TEST(Subsets, encode1)
{
  // All ten two-element subsets of five round-trip through their indices.
  Subsets C(5, 2);

  Subset a(2, 0);

  for (int i = 0; i < 10; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i, j);
    }
}

TEST(Subsets, encode2)
{
  // All six-element subsets of twelve round-trip through their indices.
  Subsets C(12, 6);

  Subset a(6, 0);

  for (int i = 0; i < 924; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i, j);
    }
}

TEST(Subsets, encode3)
{
  // The empty subset has exactly one encoding.
  Subsets C(12, 0);

  Subset a(0, 0);

  for (int i = 0; i < 1; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i, j);
    }
}

TEST(Subsets, encode4)
{
  // A larger binomial table preserves all seven-element subset encodings.
  Subsets C(21, 7);

  Subset a(7, 0);

  for (int i = 0; i < 116280; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i, j);
    }
}

TEST(Subsets, encode5)
{
  // The full set has exactly one encoding.
  Subsets C(21, 21);

  Subset a(21, 0);

  for (int i = 0; i < 1; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i, j);
    }
}

TEST(Subsets, encode6)
{
  // Decoding each index agrees with incrementing the preceding subset.
  const int n = 21;
  const int p = 7;
  const int n_choose_p = 116280;

  Subsets C(n, p);

  Subset a(p, 0);
  Subset b(p, 0);
  for (size_t i = 0; i < p; i++) b[i] = i;

  for (size_t i = 0; i < n_choose_p; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      EXPECT_EQ(a, b);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i, j);
      bool ret = Subsets::increment(n, b);
      EXPECT_EQ(ret, i + 1 != n_choose_p);
    }
}

TEST(Subsets, concatenateSubsets)
{
  // Disjoint subsets merge with the alternating sign; intersections return
  // zero.
  const int n = 7;
  const int p = 3;
  const int q = 2;
  const int n_choose_p = 35;
  Subsets C(n, std::max(p, q));

  Subset a(p, 0);
  Subset b(q, 0);
  Subset c(p + q, 0);
  Subset d(p + q, 0);

  int sign;
  if ((p % 2 == 1) && (q % 2 == 1))
    sign = -1;
  else
    sign = 1;
  for (size_t i = 0; i < n_choose_p; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      for (size_t j = 0; j < 21; j++)  // C(7, 2) subsets of size q
        {
          SCOPED_TRACE(::testing::Message() << "second index " << j);
          C.decode(j, b);
          EXPECT_TRUE(C.isValid(b));
          int ret1 = Subsets::concatenateSubsets(a, b, c);
          int ret2 = Subsets::concatenateSubsets(b, a, d);
          if (ret1 == 0 || ret2 == 0)
            {
              EXPECT_EQ(ret1, ret2);
              continue;
            }
          EXPECT_EQ(ret1, sign * ret2);
          EXPECT_EQ(c, d);
        }
    }
}

TEST(Subsets, outOfRange)
{
  // A table sized for triples also decodes every two-element subset.
  const int n = 7;
  const int p = 3;
  const int q = 2;
  Subsets C(n, std::max(p, q));

  Subset b(q, 0);

  for (size_t i = 0; i < 21; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, b);
      EXPECT_TRUE(C.isValid(b));
    }
}

TEST(Subsets, encodeBoundary)
{
  // Each boundary encoding removes the specified element from the subset.
  const int n = 7;
  const int p = 3;
  const int n_choose_p = 35;
  Subsets C(n, p);

  Subset a(p, 0);
  Subset b(p - 1, 0);

  for (size_t i = 0; i < n_choose_p; i++)
    {
      SCOPED_TRACE(::testing::Message() << "index " << i);
      C.decode(i, a);
      for (size_t j = 0; j < p; j++)
        {
          SCOPED_TRACE(::testing::Message() << "removed position " << j);
          size_t x = C.encodeBoundary(j, a);
          C.decode(x, b);
          Subset expected = a;
          expected.erase(expected.begin() + j);
          EXPECT_EQ(b, expected);
        }
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
