// Copyright 2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "comb.hpp"
constexpr int binom(int n, int k) {
    return (k < 1 || n <= 1 || n <= k) ? 1 : (binom(n-1, k) * n/(n-k));
}

#define TESTSubsets(n, k, e)            \
TEST(Subsets, e) {                      \
  Subsets C(n, k);                      \
  Subset a(k, 0);                       \
  for (int i = 0; i < binom(n,k); i++) {\
      C.decode(i, a);                   \
      EXPECT_TRUE(C.isValid(a));        \
      size_t j = C.encode(a);           \
      EXPECT_EQ(i, j); } }

TESTSubsets(5,  2, encode1)
TESTSubsets(12, 6, encode2)
TESTSubsets(12, 0, encode3)
TESTSubsets(21, 7, encode4)
TESTSubsets(21,21, encode5)

bool sameSubset(const Subset &a, const Subset &b)
{
  if (a.size() != b.size()) return false;
  for (size_t i = 0; i < a.size(); i++)
    if (a[i] != b[i]) return false;
  return true;
}

TEST(Subsets, encode6)
{
  // test the increment and decrement functions too
  const int n = 21;
  const int p = 7;
  const int n_choose_p = 116280;

  Subsets C(n, p);

  Subset a(p, 0);
  Subset b(p, 0);
  for (size_t i = 0; i < p; i++) b[i] = i;

  for (size_t i = 0; i < n_choose_p; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(sameSubset(a, b));
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i, j);
      bool ret = Subsets::increment(n, b);
      EXPECT_EQ(ret, i + 1 != n_choose_p);
    }
}

TEST(Subsets, concatenateSubsets)
{
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
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      for (size_t j = 0; j < n_choose_p; j++)
        {
          C.decode(j, b);
          EXPECT_TRUE(C.isValid(b));
          int ret1 = Subsets::concatenateSubsets(a, b, c);
          int ret2 = Subsets::concatenateSubsets(b, a, d);
          if (ret1 == 0 || ret2 == 0)
            {
              EXPECT_EQ(ret1, ret2);
              break;
            }
          EXPECT_EQ(ret1, sign * ret2);
          EXPECT_TRUE(sameSubset(c, d));
        }
    }
}

TEST(Subsets, outOfRange)
{
  const int n = 7;
  const int p = 3;
  const int q = 2;
  Subsets C(n, std::max(p, q));

  Subset b(q, 0);

  for (size_t i = 0; i < 21; i++)
    {
      C.decode(i, b);
      std::cout << "i=" << i << " set=";
      Subsets::show(std::cout, b);
      std::cout << std::endl;
      EXPECT_TRUE(C.isValid(b));
    }
}

TEST(Subsets, encodeBoundary)
{
  const int n = 7;
  const int p = 3;
  const int n_choose_p = 35;
  Subsets C(n, p);

  Subset a(p, 0);
  Subset b(p - 1, 0);

  for (size_t i = 0; i < n_choose_p; i++)
    {
      C.decode(i, a);
      std::cout << "i=" << i << "set=";
      Subsets::show(std::cout, a);
      std::cout << " bds= ";
      for (size_t j = 0; j < p; j++)
        {
          size_t x = C.encodeBoundary(j, a);
          C.decode(x, b);
          Subsets::show(std::cout, b);
          std::cout << " ";
        }
      std::cout << std::endl;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
