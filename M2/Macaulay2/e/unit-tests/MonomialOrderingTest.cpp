#include "interface/monomial-ordering.h"
#include "monomials/monordering.hpp"
#include <gtest/gtest.h>
#include <string>
#include <vector>
#include "error.h"

namespace {
TEST(MonomialOrdering, constructors)
{
  // Packing changes representation, while order type and variable count stay
  // explicit.
  struct Sample
  {
    const char* name;
    MonomialOrdering* order;
    MonomialOrdering_type type;
  };
  const Sample cases[] = {
      {"Lex", MonomialOrderings::Lex(3), MO_LEX},
      {"LexSmall", MonomialOrderings::Lex2(3), MO_LEX2},
      {"LexTiny", MonomialOrderings::Lex4(3), MO_LEX4},
      {"GRevLex", MonomialOrderings::GRevLex(3), MO_GREVLEX},
      {"GRevLexSmall", MonomialOrderings::GRevLex2(3), MO_GREVLEX2},
      {"GRevLexTiny", MonomialOrderings::GRevLex4(3), MO_GREVLEX4},
      {"RevLex", MonomialOrderings::RevLex(3), MO_REVLEX},
      {"GroupLex", MonomialOrderings::GroupLex(3), MO_LAURENT},
      {"GroupRevLex", MonomialOrderings::GroupRevLex(3), MO_LAURENT_REVLEX},
      {"GRevLex",
       MonomialOrderings::GRevLex(std::vector<int> {2, 1, 3}),
       MO_GREVLEX_WTS},
      {"GRevLexSmall",
       MonomialOrderings::GRevLex2(std::vector<int> {2, 1, 3}),
       MO_GREVLEX2_WTS},
      {"GRevLexTiny",
       MonomialOrderings::GRevLex4(std::vector<int> {2, 1, 3}),
       MO_GREVLEX4_WTS},
      {"Weights", MonomialOrderings::Weights({2, -1, 3}), MO_WEIGHTS}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      ASSERT_NE(sample.order, nullptr);
      ASSERT_EQ(sample.order->len, 1);
      EXPECT_EQ(sample.order->array[0]->type, sample.type);
      EXPECT_EQ(sample.order->array[0]->nvars, 3);
      EXPECT_NE(MonomialOrderings::toString(sample.order).find(sample.name),
                std::string::npos);
    }
  for (auto weights : std::vector<std::vector<int>> {{0, 1}, {-1, 2}})
    {
      SCOPED_TRACE(testing::PrintToString(weights));
      EXPECT_EQ(MonomialOrderings::GRevLex(weights), nullptr);
      EXPECT_NE(std::string(error_message()).find("positive"),
                std::string::npos);
    }
}

TEST(MonomialOrdering, blocksAndMatrix)
{
  // Lex and reverse-lex blocks preserve component placement in the matrix form.
  auto* product = MonomialOrderings::product(
      {MonomialOrderings::Lex(1), MonomialOrderings::GRevLex(2)});
  struct Sample
  {
    MonomialOrdering* order;
    std::vector<int> matrix;
    bool reverse;
    int direction, position;
  };
  const Sample cases[] = {
      {MonomialOrderings::Lex(3), {}, false, 0, -2},
      {MonomialOrderings::GRevLex(3), {1, 1, 1}, true, 0, -2},
      {MonomialOrderings::GRevLex(std::vector<int> {2, 1, 3}),
       {2, 1, 3},
       true,
       0,
       -2},
      {MonomialOrderings::RevLex(3), {}, true, 0, -2},
      {MonomialOrderings::join(
           {MonomialOrderings::PositionUp(), MonomialOrderings::Lex(3)}),
       {},
       false,
       1,
       0},
      {MonomialOrderings::join(
           {MonomialOrderings::Lex(3), MonomialOrderings::PositionDown()}),
       {},
       false,
       -1,
       -1},
      {product, {1, 0, 0, 0, 1, 1}, true, 0, -2}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(MonomialOrderings::toString(sample.order));
      std::vector<int> matrix;
      bool reverse = false;
      int direction = 99, position = 99;
      ASSERT_TRUE(monomialOrderingToMatrix(
          *sample.order, matrix, reverse, direction, position));
      EXPECT_EQ(matrix, sample.matrix);
      EXPECT_EQ(reverse, sample.reverse);
      EXPECT_EQ(direction, sample.direction);
      EXPECT_EQ(position, sample.position);
    }
  for (auto* order :
       {MonomialOrderings::GroupLex(2), MonomialOrderings::GroupRevLex(2)})
    {
      std::vector<int> matrix;
      bool reverse;
      int direction, position;
      EXPECT_FALSE(monomialOrderingToMatrix(
          *order, matrix, reverse, direction, position));
    }
}

// Offsetting a weight block never stores it in the product, leaving a null
// block. Disabled until the linked defect is fixed and these intended-result
// assertions pass. https://github.com/Macaulay2/M2/issues/4714
TEST(MonomialOrdering, DISABLED_weightedProduct)
{
  // Weight offsets in products and component placement must survive matrix
  // conversion.
  auto* weighted = MonomialOrderings::join(
      {MonomialOrderings::Weights({2, 3}), MonomialOrderings::GRevLex(2)});
  ASSERT_EQ(weighted->len, 2);
  auto* product =
      MonomialOrderings::product({MonomialOrderings::Lex(1), weighted});
  ASSERT_EQ(product->len, 3);
  ASSERT_NE(product->array[1], nullptr);
  EXPECT_EQ(product->array[1]->nvars, 3);
  EXPECT_EQ(
      (std::vector<int>(product->array[1]->wts, product->array[1]->wts + 3)),
      (std::vector<int> {0, 2, 3}));
}

// Copying weights overwrites the loop index and skips the following order
// block. Disabled until the linked defect is fixed and the intended result is
// returned. https://github.com/Macaulay2/M2/issues/4717
TEST(MonomialOrdering, DISABLED_nestedJoinRetainsBlocks)
{
  // Joining an existing weight-plus-order block must preserve both entries.
  auto* weighted = MonomialOrderings::join(
      {MonomialOrderings::Weights({2, 3}), MonomialOrderings::GRevLex(2)});
  auto* result = MonomialOrderings::join({weighted});
  ASSERT_EQ(result->len, 2);
  ASSERT_NE(result->array[0], nullptr);
  ASSERT_NE(result->array[1], nullptr);
  EXPECT_EQ(result->array[0]->type, MO_WEIGHTS);
  EXPECT_EQ(result->array[1]->type, MO_GREVLEX);
}

// Extra entries must be ignored, not written past the end of the buffer.
TEST(MonomialOrdering, overlongWeightVector)
{
  auto* mo = MonomialOrderings::join(
      {MonomialOrderings::Weights({0, 0, 0, 0, 0, 1, 2, 3}),
       MonomialOrderings::Lex(4)});
  ASSERT_EQ(MonomialOrderings::numberOfVariables(mo), 4);
  EXPECT_EQ(MonomialOrderings::firstWeightVector(mo),
            (std::vector<int> {0, 0, 0, 0}));
  EXPECT_TRUE(MonomialOrderings::nonTermOrderVariables(mo).empty());

  auto* negative = MonomialOrderings::join(
      {MonomialOrderings::Weights({0, -1, 0, 0, 5, 6}),
       MonomialOrderings::Lex(4)});
  EXPECT_EQ(MonomialOrderings::firstWeightVector(negative),
            (std::vector<int> {0, -1, 0, 0}));
  EXPECT_EQ(MonomialOrderings::nonTermOrderVariables(negative),
            (std::vector<int> {1}));
}

// A weight vector shorter than the number of variables is zero-padded.
TEST(MonomialOrdering, shortWeightVector)
{
  auto* mo = MonomialOrderings::join(
      {MonomialOrderings::Weights({1, 1}), MonomialOrderings::Lex(4)});
  EXPECT_EQ(MonomialOrderings::firstWeightVector(mo),
            (std::vector<int> {1, 1, 0, 0}));
}
}  // namespace
