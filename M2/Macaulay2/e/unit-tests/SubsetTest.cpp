// Copyright 2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "comb.hpp"

TEST(Subsets, encode1)
{
  Subsets C(5,2);

  Subset a(2, 0);

  for (int i=0; i<10; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i,j);
    }
}

TEST(Subsets, encode2)
{
  Subsets C(12,6);

  Subset a(6, 0);

  for (int i=0; i<924; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i,j);
    }
}

TEST(Subsets, encode3)
{
  Subsets C(12,0);

  Subset a(0, 0);

  for (int i=0; i<1; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i,j);
    }
}

TEST(Subsets, encode4)
{
  Subsets C(21,7);

  Subset a(7, 0);

  for (int i=0; i<116280; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i,j);
    }
}

TEST(Subsets, encode5)
{
  Subsets C(21,21);

  Subset a(21, 0);

  for (int i=0; i<1; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i,j);
    }
}

bool sameSubset(const Subset &a, const Subset &b)
{
  if (a.size() != b.size()) return false;
  for (size_t i=0; i<a.size(); i++)
    if (a[i] != b[i]) return false;
  return true;
}

TEST(Subsets, encode6)
{
  // test the increment and decrement functions too
  const int n = 21;
  const int p = 7;
  const int n_choose_p = 116280;

  Subsets C(n,p);

  Subset a(p, 0);
  Subset b(p, 0);
  for (size_t i=0; i<p; i++)
    b[i] = i;
  
  for (size_t i=0; i<n_choose_p; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(sameSubset(a,b));
      EXPECT_TRUE(C.isValid(a));
      size_t j = C.encode(a);
      EXPECT_EQ(i,j);
      bool ret = Subsets::increment(n, b);
      EXPECT_EQ(ret, i+1 != n_choose_p);
    }
}

TEST(Subsets, concatenateSubsets)
{
  const int n = 7;
  const int p = 3;
  const int q = 2;
  const int n_choose_p = 35;
  Subsets C(n,std::max(p,q));

  Subset a(p, 0);
  Subset b(q, 0);
  Subset c(p+q, 0);
  Subset d(p+q, 0);

  int sign;
  if ((p % 2 == 1) && (q % 2 == 1))
    sign = -1;
  else 
    sign = 1;
  for (size_t i=0; i<n_choose_p; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
      for (size_t j=0; j<n_choose_p; j++)
        {
          C.decode(j,b);
          EXPECT_TRUE(C.isValid(b));
          int ret1 = Subsets::concatenateSubsets(a,b,c);
          int ret2 = Subsets::concatenateSubsets(b,a,d);
          if (ret1 == 0 || ret2 == 0) 
            {
              EXPECT_EQ(ret1, ret2);
              break;
            }
          EXPECT_EQ(ret1, sign * ret2);
          EXPECT_TRUE(sameSubset(c,d));
        }
    }
}

TEST(Subsets, outOfRange)
{
  const int n = 7;
  const int p = 3;
  const int q = 2;
  Subsets C(n,std::max(p,q));

  Subset b(q, 0);

  for (size_t i=0; i<21; i++)
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
  Subsets C(n,p);

  Subset a(p, 0);
  Subset b(p-1, 0);

  for (size_t i=0; i<n_choose_p; i++)
    {
      C.decode(i, a);
      std::cout << "i=" << i << "set=";
      Subsets::show(std::cout, a);
      std::cout << " bds= ";
      for (size_t j=0; j<p; j++)
        {
          size_t x = C.encodeBoundary(j,a);
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
