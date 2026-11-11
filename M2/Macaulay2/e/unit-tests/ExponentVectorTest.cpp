#include "monomials/ExponentVector.hpp"
#include <gtest/gtest.h>
#include <array>
#include <cstdint>
#include <limits>
#include <vector>
#include "MonomialTestHelpers.hpp"
#include "exceptions.hpp"
#include "util.hpp"

namespace {
template <class T>
class ExponentVectorTest : public testing::Test
{
};
using DenseTypes = testing::Types<ExponentVector<int, true>,
                                  ExponentVector<int32_t, false>,
                                  ExponentVector<int64_t, false>>;
TYPED_TEST_SUITE(ExponentVectorTest, DenseTypes);

TYPED_TEST(ExponentVectorTest, arithmetic)
{
  // Cover the production dense representations with independently computed
  // answers.
  monomialTest::checkDenseArithmetic<TypeParam>();
}

TYPED_TEST(ExponentVectorTest, printing)
{
  // Single-character names, long names, and Laurent exponents print
  // differently.
  using E = typename TypeParam::Exponent;
  E a[] = {2, 3, -2, 1, 0};
  buffer out;
  TypeParam::elem_text_out(out, 5, a, {"x", "alpha", "z", "t", "u"}, true);
  EXPECT_STREQ(out.str(), "x2alpha^3z^(-2)t");
  E one[] = {0};
  buffer visible, hidden;
  TypeParam::elem_text_out(visible, 1, one, {"x"}, true);
  TypeParam::elem_text_out(hidden, 1, one, {"x"}, false);
  EXPECT_STREQ(visible.str(), "1");
  EXPECT_STREQ(hidden.str(), "");
}

TEST(ExponentVector, masksAndArrayWeights)
{
  // Support masks fold at the unsigned word boundary and ignore nonpositive
  // powers.
  constexpr int width = std::numeric_limits<exponents::HashExponent>::digits;
  std::vector<int> a(width + 3, 0);
  a[0] = 1;
  a[width] = 2;
  a[width + 1] = -1;
  EXPECT_EQ(exponents::mask(a.size(), a.data()), 4u);
  a[width + 2] = 1;
  EXPECT_EQ(exponents::mask(a.size(), a.data()), 5u);
  int b[] = {2, 4};
  EXPECT_EQ(exponents::weight(
                2, b, stdvector_to_M2_arrayint(std::vector<int> {3, -1})),
            2);
}

TEST(ExponentVector, checkedOverflow)
{
  // Each checked arithmetic operation must reject an unrepresentable exponent
  // or degree.
  int high[] = {std::numeric_limits<int>::max()},
      low[] = {std::numeric_limits<int>::min()};
  int one[] = {1}, minusOne[] = {-1}, out[2];
  EXPECT_THROW(exponents::mult(1, high, one, out), exc::overflow_exception);
  EXPECT_THROW(exponents::divide(1, low, one, out), exc::overflow_exception);
  EXPECT_THROW(exponents::power(1, low, -1, out), exc::overflow_exception);
  EXPECT_THROW(exponents::multpower(1, high, one, 1, out),
               exc::overflow_exception);
  EXPECT_THROW(exponents::multpower(1, one, high, 2, out),
               exc::overflow_exception);
  int degree[] = {std::numeric_limits<int>::max(), 1};
  EXPECT_THROW(exponents::simple_degree(2, degree), exc::overflow_exception);
  EXPECT_THROW(exponents::weight(2, degree, std::vector<int> {1, 1}),
               exc::overflow_exception);
  EXPECT_THROW(exponents::weight(1, high, std::vector<int> {2}),
               exc::overflow_exception);
  exponents::divide(1, low, minusOne, out);
  EXPECT_EQ(out[0], std::numeric_limits<int>::min() + 1);
}

TEST(ExponentVector, wideExponents)
{
  // The unchecked 64-bit production backend must preserve values beyond a
  // 32-bit exponent.
  using Ops = ExponentVector<int64_t, false>;
  const int64_t large = int64_t {1} << 40;
  int64_t a[] = {large, 3}, b[] = {1, 2}, out[2];
  Ops::mult(2, a, b, out);
  EXPECT_EQ(out[0], large + 1);
  EXPECT_EQ(out[1], 5);
  EXPECT_EQ(Ops::simple_degree(2, a), large + 3);
  Ops::power(2, a, 2, out);
  EXPECT_EQ(out[0], large * 2);
  EXPECT_EQ(out[1], 6);
}
// Weight arithmetic and formatting narrow 64-bit exponents to int32_t.
// Disabled until both operations preserve the full width.
// https://github.com/Macaulay2/M2/issues/4715
TEST(ExponentVector, DISABLED_wideWeightsAndPrinting)
{
  // Weight evaluation and formatting must retain the full exponent width.
  using Ops = ExponentVector<int64_t, false>;
  const int64_t large = int64_t {1} << 40;
  int64_t a[] = {large};
  EXPECT_EQ(Ops::weight(1, a, std::vector<int64_t> {1}), large);
  buffer out;
  Ops::elem_text_out(out, 1, a, {"x"}, true);
  EXPECT_STREQ(out.str(), "x1099511627776");
}
}  // namespace
