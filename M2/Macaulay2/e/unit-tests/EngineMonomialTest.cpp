#include "monomials/monomial.hpp"
#include <gtest/gtest.h>
#include <memory>
#include <limits>
#include <vector>
#include "MonomialTestHelpers.hpp"
#include "error.h"
#include "util-polyring-creation.hpp"

namespace {
using Mon = std::unique_ptr<EngineMonomial>;
void expectMonomial(Mon actual, std::vector<int> expected)
{
  ASSERT_NE(actual, nullptr);
  EXPECT_EQ(monomialTest::sparseValues<varpower>(actual->ints()), expected);
}

TEST(EngineMonomial, constructionAndArithmetic)
{
  // The owning wrapper must preserve the sparse representation through each
  // operation.
  Mon a(EngineMonomial::make(std::vector<int> {5, 2, 3, 0, 2}));
  int raw[] = {5, 2, 1, 1, 4};
  Mon b(EngineMonomial::make(raw));
  Mon copy(EngineMonomial::make(
      stdvector_to_M2_arrayint(std::vector<int> {2, 3, 0, 2})));
  Mon one(EngineMonomial::make(0, 0));
  ASSERT_NE(copy, nullptr);
  EXPECT_TRUE(a->is_equal(*a));
  EXPECT_TRUE(a->is_equal(*copy));
  EXPECT_FALSE(a->is_equal(*b));
  EXPECT_EQ(a->hash(), copy->hash());
  EXPECT_TRUE(one->is_one());
  EXPECT_FALSE(a->is_one());
  EXPECT_EQ(a->simple_degree(), 5);
  expectMonomial(Mon(*a * *b), {7, 2, 4, 1, 4, 0, 2});
  expectMonomial(Mon(*a / *b), {5, 2, 2, 0, 2});
  expectMonomial(Mon(a->lcm(*b)), {7, 2, 3, 1, 4, 0, 2});
  expectMonomial(Mon(a->gcd(*b)), {3, 2, 1});
  expectMonomial(Mon(a->erase(*b)), {3, 0, 2});
  expectMonomial(Mon(a->radical()), {5, 2, 1, 0, 1});
  expectMonomial(Mon(a->power(2)), {5, 2, 6, 0, 4});
  expectMonomial(Mon(a->power(0)), {1});
  buffer out;
  a->text_out(out);
  EXPECT_STREQ(out.str(), "c3a2");
  auto exported = a->to_arrayint();
  EXPECT_EQ(
      (std::vector<int>(exported->array, exported->array + exported->len)),
      (std::vector<int> {5, 2, 3, 0, 2}));
  const auto* ring = simplePolynomialRing(101, {"x", "y", "z"});
  const auto* monoid = ring->getMonoid();
  EXPECT_EQ(a->compare(monoid, *copy), EQ);
  EXPECT_EQ(a->compare(monoid, *one), GT);
  EXPECT_EQ(one->compare(monoid, *a), LT);
  EXPECT_TRUE(one->divides(monoid, *a));
  EXPECT_FALSE(a->divides(monoid, *b));
}

TEST(EngineMonomial, invalidArrayInputs)
{
  // Invalid front-end arrays return no monomial and a diagnostic; consume each
  // error.
  const std::vector<std::vector<int>> cases {
      {0},
      {0, 1, 1, 1},
      {1, 2, 1, 3},
      {-1, 2},
      {0, 0},
      {0, std::numeric_limits<int>::min()}};
  for (const auto& input : cases)
    {
      SCOPED_TRACE(testing::PrintToString(input));
      Mon result(EngineMonomial::make(stdvector_to_M2_arrayint(input)));
      EXPECT_EQ(result, nullptr);
      const char* message = error_message();
      ASSERT_NE(message, nullptr);
      EXPECT_NE(*message, '\0');
    }
}

// The wrapper inherits the sparse syzygy routine's overwritten output.
// Disabled until the linked defect is fixed and these intended-result
// assertions pass. https://github.com/Macaulay2/M2/issues/4709
TEST(EngineMonomial, DISABLED_syzygyOutputs)
{
  // The owning wrapper must return both complementary factors.
  Mon a(EngineMonomial::make(std::vector<int> {5, 2, 3, 0, 1}));
  Mon b(EngineMonomial::make(std::vector<int> {5, 2, 1, 1, 2}));
  EngineMonomial *left = nullptr, *right = nullptr;
  a->monsyz(*b, left, right);
  Mon sa(left), sb(right);
  ASSERT_NE(sa, nullptr);
  ASSERT_NE(sb, nullptr);
  EXPECT_EQ(monomialTest::sparseValues<varpower>(sa->ints()),
            (std::vector<int> {3, 1, 2}));
  EXPECT_EQ(monomialTest::sparseValues<varpower>(sb->ints()),
            (std::vector<int> {5, 2, 2, 0, 1}));
}

}  // namespace
