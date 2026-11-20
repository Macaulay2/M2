/* This code written by Franziska Hinkelmann is in the public domain */

#include <initializer_list>

#include <gtest/gtest.h>

#include "computations/BRP.hpp"

namespace {

BRP makeBRP(std::initializer_list<brMonomial> terms)
{
  monomials result;
  for (brMonomial term : terms) result.push_back(term);
  return BRP(result);
}

}  // namespace

TEST(BRP, ZeroEquality)
{
  // The empty term list represents zero, not one.
  BRP zero;

  EXPECT_EQ(zero, 0);
  EXPECT_NE(zero, 1);
  EXPECT_TRUE(zero == 0);
  EXPECT_FALSE(zero == 1);
}

TEST(BRP, OneEquality)
{
  // The zero exponent bitmask represents the constant one.
  BRP one = makeBRP({0});

  EXPECT_EQ(one, 1);
  EXPECT_NE(one, 0);
  EXPECT_TRUE(one == 1);
  EXPECT_FALSE(one == 0);
}

TEST(BRP, PolynomialEquality)
{
  // Equal ordered term lists compare equal; changing one term breaks equality.
  BRP polynomial = makeBRP({8, 3, 0});

  EXPECT_EQ(polynomial, polynomial);
  EXPECT_EQ(makeBRP({8, 3, 0}), polynomial);
  EXPECT_NE(makeBRP({8, 3, 1}), polynomial);
}

TEST(BRP, AdditionCancelsCommonTerms)
{
  // Boolean addition cancels common monomials while retaining other terms.
  {
    SCOPED_TRACE("addition: one shared middle term");
    BRP sum = makeBRP({3, 2, 1});
    sum + makeBRP({5, 4, 3});

    EXPECT_EQ(sum, makeBRP({5, 4, 2, 1}));
  }
  {
    SCOPED_TRACE("addition: interleaved term lists");
    BRP a = makeBRP({16, 15, 5, 2});
    BRP b = makeBRP({13, 12, 6, 4, 2, 1});
    a + b;

    EXPECT_EQ(a, makeBRP({16, 15, 13, 12, 6, 5, 4, 1}));
  }
}

TEST(BRP, AdditionPreservesLexOrder)
{
  // Inserting terms in either order produces a decreasing term list.
  BRP decreasing = BRP(9);
  decreasing + BRP(7);

  BRP increasing = BRP(7);
  increasing + BRP(9);

  EXPECT_EQ(decreasing, makeBRP({9, 7}));
  EXPECT_EQ(increasing, makeBRP({9, 7}));
}

TEST(BRP, AdditionWithLargerLeadingTerms)
{
  // Merging a larger leading monomial preserves the order of the remaining
  // terms.
  BRP a = makeBRP({35, 16, 15, 5, 2});
  BRP b = makeBRP({38, 13, 12, 6, 4, 2, 1});
  a + b;

  EXPECT_EQ(a, makeBRP({38, 35, 16, 15, 13, 12, 6, 5, 4, 1}));
}

TEST(BRP, AdditionRetainsConstantTerm)
{
  // A constant present in only one summand survives addition.
  BRP a = makeBRP({35, 16, 15, 5, 2, 0});
  BRP b = makeBRP({38, 13, 12, 6, 4, 2, 1});
  a + b;

  EXPECT_EQ(a, makeBRP({38, 35, 16, 15, 13, 12, 6, 5, 4, 1, 0}));
}

TEST(BRP, AdditionCancelsLeadingTerm)
{
  // Matching leading monomials cancel in characteristic two.
  BRP a = makeBRP({35, 16, 15, 5, 2, 0});
  BRP b = makeBRP({35, 13, 12, 6, 4, 2, 1});
  a + b;

  EXPECT_EQ(a, makeBRP({16, 15, 13, 12, 6, 5, 4, 1, 0}));
}

TEST(BRP, Multiplication)
{
  // Boolean products unite variable supports and cancel repeated resulting
  // monomials.
  {
    SCOPED_TRACE("multiply: distinct resulting terms");
    EXPECT_EQ(makeBRP({14, 1}) * BRP(8), makeBRP({14, 9}));
  }

  {
    SCOPED_TRACE("multiply: all resulting terms cancel");
    BRP a = makeBRP({13, 12, 6, 4, 2, 1});

    EXPECT_EQ(a * BRP(13), BRP());
    EXPECT_EQ(a * static_cast<brMonomial>(13), BRP());
  }
  {
    SCOPED_TRACE("multiply: one term survives cancellation");
    BRP a = makeBRP({16, 13, 12, 6, 4, 2, 1});

    EXPECT_EQ(a * BRP(13), BRP(29));
    EXPECT_EQ(a * static_cast<brMonomial>(13), BRP(29));
  }
  {
    SCOPED_TRACE("multiply: overlapping variable supports");
    EXPECT_EQ(makeBRP({16, 13, 12, 6, 4}) * static_cast<brMonomial>(220),
              makeBRP({222, 221, 220}));
  }
}

TEST(BRP, Divisibility)
{
  // A divisor must use only variables present in the dividend; one divides
  // every term.
  EXPECT_FALSE(BRP::isDivisibleBy(14, 1));
  EXPECT_TRUE(BRP::isDivisibleBy(1, 0));
  EXPECT_TRUE(BRP::isDivisibleBy(14, 8));
}

TEST(BRP, LeadingTerm)
{
  // The first ordered term is leading, including the constant polynomial.
  BRP polynomial = makeBRP({8, 3});

  EXPECT_EQ(polynomial.LT(), 8u);

  BRP one = makeBRP({0});

  EXPECT_EQ(one.LT(), 0u);
  EXPECT_EQ(BRP(one.LT()), one);
}

TEST(BRP, LeadingReducibleBy)
{
  // Leading reduction requires a divisor of the leading monomial.
  BRP f = BRP(2) * BRP(8);
  f + BRP(7);

  EXPECT_TRUE(f.isLeadingReducibleBy(BRP(8)));
  EXPECT_TRUE(f.isLeadingReducibleBy(BRP(2)));
  EXPECT_FALSE(f.isLeadingReducibleBy(BRP(1)));
}

TEST(BRP, Remainder)
{
  // Reduction removes a divisible leading term and retains the other term.
  BRP f = BRP(2) * BRP(8);
  f + BRP(7);

  EXPECT_EQ(f.remainder(BRP(8)), BRP(7));
  EXPECT_NE(f.remainder(BRP(8)), BRP(2));
  EXPECT_EQ(f.remainder(BRP(7)), BRP(8) * BRP(2));
}

TEST(BRP, RelativelyPrimeLeadingTerms)
{
  // Leading monomials are relatively prime exactly when their variable supports
  // are disjoint.
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
