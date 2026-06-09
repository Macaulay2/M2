/* This code written by Franziska Hinkelmann is in the public domain */

#include <initializer_list>

#include <gtest/gtest.h>

#include "computations/BRP.hpp"

namespace
{

BRP makeBRP(std::initializer_list<brMonomial> terms)
{
  monomials result;
  for (brMonomial term : terms)
    result.push_back(term);
  return BRP(result);
}

}  // namespace

TEST(BRP, ZeroEquality)
{
  BRP zero;

  EXPECT_EQ(zero, 0);
  EXPECT_NE(zero, 1);
  EXPECT_TRUE(zero == 0);
  EXPECT_FALSE(zero == 1);
}

TEST(BRP, OneEquality)
{
  BRP one = makeBRP({0});

  EXPECT_EQ(one, 1);
  EXPECT_NE(one, 0);
  EXPECT_TRUE(one == 1);
  EXPECT_FALSE(one == 0);
}

TEST(BRP, PolynomialEquality)
{
  BRP polynomial = makeBRP({8, 3, 0});

  EXPECT_EQ(polynomial, polynomial);
  EXPECT_EQ(makeBRP({8, 3, 0}), polynomial);
  EXPECT_NE(makeBRP({8, 3, 1}), polynomial);
}

TEST(BRP, AdditionCancelsCommonTerms)
{
  BRP sum = makeBRP({3, 2, 1});
  sum + makeBRP({5, 4, 3});

  EXPECT_EQ(sum, makeBRP({5, 4, 2, 1}));

  BRP a = makeBRP({16, 15, 5, 2});
  BRP b = makeBRP({13, 12, 6, 4, 2, 1});
  a + b;

  EXPECT_EQ(a, makeBRP({16, 15, 13, 12, 6, 5, 4, 1}));
}

TEST(BRP, AdditionPreservesLexOrder)
{
  BRP decreasing = BRP(9);
  decreasing + BRP(7);

  BRP increasing = BRP(7);
  increasing + BRP(9);

  EXPECT_EQ(decreasing, makeBRP({9, 7}));
  EXPECT_EQ(increasing, makeBRP({9, 7}));
}

TEST(BRP, AdditionWithLargerLeadingTerms)
{
  BRP a = makeBRP({35, 16, 15, 5, 2});
  BRP b = makeBRP({38, 13, 12, 6, 4, 2, 1});
  a + b;

  EXPECT_EQ(a, makeBRP({38, 35, 16, 15, 13, 12, 6, 5, 4, 1}));
}

TEST(BRP, AdditionRetainsConstantTerm)
{
  BRP a = makeBRP({35, 16, 15, 5, 2, 0});
  BRP b = makeBRP({38, 13, 12, 6, 4, 2, 1});
  a + b;

  EXPECT_EQ(a, makeBRP({38, 35, 16, 15, 13, 12, 6, 5, 4, 1, 0}));
}

TEST(BRP, AdditionCancelsLeadingTerm)
{
  BRP a = makeBRP({35, 16, 15, 5, 2, 0});
  BRP b = makeBRP({35, 13, 12, 6, 4, 2, 1});
  a + b;

  EXPECT_EQ(a, makeBRP({16, 15, 13, 12, 6, 5, 4, 1, 0}));
}

TEST(BRP, Multiplication)
{
  EXPECT_EQ(makeBRP({14, 1}) * BRP(8), makeBRP({14, 9}));

  BRP a = makeBRP({13, 12, 6, 4, 2, 1});

  EXPECT_EQ(a * BRP(13), BRP());
  EXPECT_EQ(a * static_cast<brMonomial>(13), BRP());

  a = makeBRP({16, 13, 12, 6, 4, 2, 1});

  EXPECT_EQ(a * BRP(13), BRP(29));
  EXPECT_EQ(a * static_cast<brMonomial>(13), BRP(29));

  EXPECT_EQ(makeBRP({16, 13, 12, 6, 4}) * static_cast<brMonomial>(220),
            makeBRP({222, 221, 220}));
}

TEST(BRP, Divisibility)
{
  EXPECT_FALSE(BRP::isDivisibleBy(14, 1));
  EXPECT_TRUE(BRP::isDivisibleBy(1, 0));
  EXPECT_EQ(14 ^ 8, 6);
}

TEST(BRP, LeadingTerm)
{
  BRP polynomial = makeBRP({8, 3});

  EXPECT_EQ(polynomial.LT(), 8u);

  BRP one = makeBRP({0});

  EXPECT_EQ(one.LT(), 0u);
  EXPECT_EQ(BRP(one.LT()), one);
}

TEST(BRP, LeadingReducibleBy)
{
  BRP f = BRP(2) * BRP(8);
  f + BRP(7);

  EXPECT_TRUE(f.isLeadingReducibleBy(BRP(8)));
  EXPECT_TRUE(f.isLeadingReducibleBy(BRP(2)));
  EXPECT_FALSE(f.isLeadingReducibleBy(BRP(1)));
}

TEST(BRP, Remainder)
{
  BRP f = BRP(2) * BRP(8);
  f + BRP(7);

  EXPECT_EQ(f.remainder(BRP(8)), BRP(7));
  EXPECT_NE(f.remainder(BRP(8)), BRP(2));
  EXPECT_EQ(f.remainder(BRP(7)), BRP(8) * BRP(2));
}

TEST(BRP, RelativelyPrimeLeadingTerms)
{
  BRP f = BRP(2) * BRP(8);
  f + BRP(7);

  EXPECT_FALSE(BRP::isRelativelyPrime(f.LT(), BRP(8).LT()));
  EXPECT_FALSE(BRP::isRelativelyPrime(BRP(8).LT(), f.LT()));
  EXPECT_FALSE(BRP::isRelativelyPrime(BRP(2).LT(), f.LT()));
  EXPECT_TRUE(BRP::isRelativelyPrime(f.LT(), BRP(1).LT()));
  EXPECT_TRUE(BRP::isRelativelyPrime(BRP(1).LT(), f.LT()));
  EXPECT_TRUE(BRP::isRelativelyPrime(BRP(1).LT(), BRP(0).LT()));
  EXPECT_TRUE(BRP::isRelativelyPrime(f.LT(), BRP(0).LT()));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
