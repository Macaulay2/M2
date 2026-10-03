#include "monomials/monsort.hpp"
#include <gtest/gtest.h>
#include <algorithm>
#include <array>
#include <vector>
#include "monomials/ExponentVector.hpp"

namespace {
struct ExponentSorter
{
  using value = std::array<int, 3>;
  int compare(const value& a, const value& b)
  { return exponents::lex_compare(3, a.data(), b.data()); }
};
TEST(MonomialSort, ordersExponentVectors)
{
  // Empty, ordered, reversed, and duplicate-heavy inputs exercise the public
  // quicksort entry point.
  const std::vector<std::vector<ExponentSorter::value>> cases {
      {},
      {{1, 0, 0}},
      {{2, 0, 0}, {1, 1, 0}},
      {{0, 0, 0}, {0, 1, 0}, {1, 0, 0}},
      {{2, 0, 0}, {1, 1, 0}, {0, 2, 0}, {0, 0, 0}},
      {{1, 2, 3}, {1, 2, 3}, {1, 2, 3}, {1, 2, 3}},
      {{2, 1, 0},
       {0, 2, 1},
       {2, 0, 1},
       {0, 2, 1},
       {1, 1, 1},
       {2, 1, 0},
       {0, 0, 0}}};
  for (auto input : cases)
    {
      SCOPED_TRACE(testing::PrintToString(input));
      auto expected = input;
      std::sort(expected.begin(), expected.end());
      ExponentSorter sorter;
      QuickSorter<ExponentSorter>::sort(&sorter, input.data(), input.size());
      EXPECT_EQ(input, expected);
    }
}

}  // namespace
