// Copyright 2012 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "buffer.hpp"
#include "text-io.hpp"
#include "util.hpp"

bool testfcn() { return true; }
TEST(Nothing, ideal)
{
  EXPECT_EQ(true, testfcn());
  EXPECT_FALSE(!(testfcn()));
}

TEST(Buffer, make1)
{
  buffer o;
  char *s = o.str();
  // fprintf(stderr, ".[%s].\n", s);
  int c = strcmp("", s);
  EXPECT_EQ(c, 0);
}

TEST(Buffer, make2)
{
  buffer o;
  o << "hi there";
  char *s = o.str();
  // fprintf(stderr, "..%s..\n", s);
  int c = strcmp("hi there", s);
  EXPECT_EQ(c, 0);
}

TEST(Util, m2array2stdvec)
{
  std::vector<int> a{1, 3, 6, 4};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  std::vector<int> c = M2_arrayint_to_stdvector<int>(b);
  EXPECT_EQ(a, c);
}

TEST(Util, m2arrayint_zero)
{
  std::vector<int> a{};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  std::vector<int> c = M2_arrayint_to_stdvector<int>(b);
  EXPECT_EQ(a, c);
}

TEST(Util, m2array2stdvec_big)
{
  std::vector<long long> a{-1453853049583, 3, 6, 4, -2};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  std::vector<long long> c = M2_arrayint_to_stdvector<long long>(b);
  EXPECT_FALSE(a == c);
}

TEST(Util, m2array2stdvec_check)
{
  std::vector<int> a{-145385, 3, 6, 4, -2};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  auto c = M2_arrayint_to_stdvector<int>(b);
  EXPECT_EQ(a, c);
}

#if 0
TEST(Util, m2strings_basic) {
  std::vector<std::string> a {"a", "b", "c1", "d2", "e_3"};
  M2_ArrayString b = stdvector_to_M2_ArrayString(a);
  auto c = M2_ArrayString_to_stdvector(b);
  EXPECT_EQ(a,c);
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
