#include "monomials/ExponentList.hpp"
#include <gtest/gtest.h>
#include <limits>
#include <vector>
#include "MonomialTestHelpers.hpp"
#include "exceptions.hpp"
#include "util.hpp"

namespace {
template <class E, bool L>
struct SparseFormat
{
  using Ops = ExponentList<E, L>;
  static constexpr bool legacy = L;
};
template <class T>
class ExponentListTest : public testing::Test
{
};
using SparseTypes = testing::Types<SparseFormat<int, true>,
                                   SparseFormat<int, false>,
                                   SparseFormat<long, false>>;
TYPED_TEST_SUITE(ExponentListTest, SparseTypes);

TYPED_TEST(ExponentListTest, queries)
{
  // Both stored-length conventions must agree on support, ordering, and
  // weights.
  monomialTest::checkSparseQueries<typename TypeParam::Ops,
                                   TypeParam::legacy>();
}

TYPED_TEST(ExponentListTest, arithmeticAvailability)
{
  // Only the legacy int representation has out-of-line arithmetic definitions.
  if constexpr (!TypeParam::legacy)
    GTEST_SKIP() << "Nonlegacy ExponentList arithmetic and conversions are "
                    "declared but not defined.";
  else
    {
      varpower::Vector a {3, 1, 2}, b {3, 0, 3}, out;
      varpower::mult(a.data(), b.data(), out);
      EXPECT_EQ(out, (varpower::Vector {5, 1, 2, 0, 3}));
    }
}

TEST(ExponentList, constructorsConversionsAndPrinting)
{
  // Conversions preserve descending variable order; the array exporter includes
  // its length word.
  varpower::Vector a, out;
  varpower::one(a);
  EXPECT_EQ(a, (varpower::Vector {1}));
  varpower::var(5, 0, a);
  EXPECT_EQ(a, (varpower::Vector {1}));
  varpower::var(5, 2, a);
  EXPECT_EQ(a, (varpower::Vector {3, 5, 2}));
  int dense[] = {2, 0, 3, 0};
  varpower::from_expvector(4, dense, a);
  EXPECT_EQ(a, (varpower::Vector {5, 2, 3, 0, 2}));
  int restored[] = {99, 99, 99, 99};
  varpower::to_expvector(4, a.data(), restored);
  EXPECT_EQ((std::vector<int>(restored, restored + 4)),
            (std::vector<int> {2, 0, 3, 0}));
  auto array = varpower::to_arrayint(a.data());
  ASSERT_EQ(array->len, 5);
  EXPECT_EQ((std::vector<int>(array->array, array->array + array->len)),
            (std::vector<int> {5, 2, 3, 0, 2}));
  varpower::from_arrayint(
      stdvector_to_M2_arrayint(std::vector<int> {2, 3, 0, 2}), out);
  EXPECT_EQ(out, a);
  varpower::from_expvector(0, dense, a);
  EXPECT_EQ(a, (varpower::Vector {1}));
  buffer visible, hidden, names;
  varpower::elem_text_out(visible, a.data());
  varpower::elem_text_out(hidden, a.data(), false);
  int mixed[] = {7, 52, 2, 26, 1, 0, -3};
  varpower::elem_text_out(names, mixed);
  EXPECT_STREQ(visible.str(), "1");
  EXPECT_STREQ(hidden.str(), "");
  EXPECT_STREQ(names.str(), "x[52]2Aa^(-3)");
}

TEST(ExponentList, arithmetic)
{
  // Interleaved supports exercise both merge tails, equal powers, and missing
  // variables.
  struct Sample
  {
    const char* name;
    std::vector<int> a, b, product, quotient, gcd, lcm, erased;
  };
  const Sample samples[] = {
      {"interleaved",
       {7, 4, 2, 2, 3, 0, 1},
       {7, 3, 4, 2, 1, 0, 2},
       {9, 4, 2, 3, 4, 2, 4, 0, 3},
       {5, 4, 2, 2, 2},
       {5, 2, 1, 0, 1},
       {9, 4, 2, 3, 4, 2, 3, 0, 2},
       {3, 4, 2}},
      {"identity left", {1}, {3, 0, 2}, {3, 0, 2}, {1}, {1}, {3, 0, 2}, {1}},
      {"identity right",
       {3, 0, 2},
       {1},
       {3, 0, 2},
       {3, 0, 2},
       {1},
       {3, 0, 2},
       {3, 0, 2}},
      {"equal",
       {3, 2, 3},
       {3, 2, 3},
       {3, 2, 6},
       {1},
       {3, 2, 3},
       {3, 2, 3},
       {1}}};
  for (const auto& s : samples)
    {
      SCOPED_TRACE(s.name);
      varpower::Vector out;
      varpower::mult(s.a.data(), s.b.data(), out);
      EXPECT_EQ(monomialTest::sparseValues<varpower>(out.data()), s.product);
      varpower::quotient(s.a.data(), s.b.data(), out);
      EXPECT_EQ(monomialTest::sparseValues<varpower>(out.data()), s.quotient);
      varpower::gcd(s.a.data(), s.b.data(), out);
      EXPECT_EQ(monomialTest::sparseValues<varpower>(out.data()), s.gcd);
      varpower::lcm(s.a.data(), s.b.data(), out);
      EXPECT_EQ(monomialTest::sparseValues<varpower>(out.data()), s.lcm);
      varpower::erase(s.a.data(), s.b.data(), out);
      EXPECT_EQ(monomialTest::sparseValues<varpower>(out.data()), s.erased);
    }
  int a[] = {5, 2, 3, 0, 2}, b[] = {3, 2, 1}, absent[] = {3, 3, 1},
      tooLarge[] = {3, 2, 4}, one[] = {1};
  EXPECT_TRUE(varpower::divides(b, a));
  EXPECT_FALSE(varpower::divides(a, b));
  EXPECT_FALSE(varpower::divides(absent, a));
  EXPECT_FALSE(varpower::divides(tooLarge, a));
  EXPECT_TRUE(varpower::divides(one, a));
  varpower::Vector radical, power, identity;
  varpower::radical(a, radical);
  EXPECT_EQ(radical, (varpower::Vector {5, 2, 1, 0, 1}));
  varpower::power(a, 3, power);
  EXPECT_EQ(power, (varpower::Vector {5, 2, 9, 0, 6}));
  varpower::power(a, 0, identity);
  EXPECT_EQ(identity, (varpower::Vector {1}));
  int inverse[] = {5, 2, -3, 0, -2};
  varpower::mult(a, inverse, power);
  EXPECT_EQ(power, (varpower::Vector {1}));
}

TEST(ExponentList, overflow)
{
  // Checked multiplication and powers must reject exponents beyond the int
  // range.
  int a[] = {3, 0, std::numeric_limits<int>::max()}, b[] = {3, 0, 1};
  varpower::Vector out;
  EXPECT_THROW(varpower::mult(a, b, out), exc::overflow_exception);
  EXPECT_THROW(varpower::power(a, 2, out), exc::overflow_exception);
}

TEST(ExponentList, buchbergerMoellerAvailability)
{
  // This declared criterion has only an unconditional-return stub in the
  // header.
  GTEST_SKIP() << "buchberger_moeller_keep is explicitly unimplemented.";
}

// Both output cursors currently point into sa, leaving sb invalid.
// Disabled until the linked defect is fixed and these intended-result
// assertions pass. https://github.com/Macaulay2/M2/issues/4709
TEST(ExponentList, DISABLED_syzygyOutputs)
{
  // Each output must multiply its input to the least common multiple.
  int a[] = {5, 2, 3, 0, 1}, b[] = {5, 2, 1, 1, 2};
  varpower::Vector sa, sb;
  varpower::monsyz(a, b, sa, sb);
  EXPECT_EQ(monomialTest::sparseValues<varpower>(sa.data()),
            (std::vector<int> {3, 1, 2}));
  EXPECT_EQ(sb, (varpower::Vector {5, 2, 2, 0, 1}));
}
// The zero-power branch appends instead of replacing the result.
// Disabled until the linked defect is fixed and these intended-result
// assertions pass. https://github.com/Macaulay2/M2/issues/4710
TEST(ExponentList, DISABLED_zeroPowerReplacesOutput)
{
  // Reusing an output vector must not leave its old monomial at the front.
  int a[] = {3, 0, 2};
  varpower::Vector output {3, 1, 4};
  varpower::power(a, 0, output);
  EXPECT_EQ(output, (varpower::Vector {1}));
}
template <class T>
class ExponentListNonlegacy : public testing::Test
{
};
using NonlegacyTypes =
    testing::Types<ExponentList<int, false>, ExponentList<long, false>>;
TYPED_TEST_SUITE(ExponentListNonlegacy, NonlegacyTypes);

// Nonlegacy constructors write total lengths instead of pair counts.
// Disabled until the linked defect is fixed and these intended-result
// assertions pass. https://github.com/Macaulay2/M2/issues/4711
TYPED_TEST(ExponentListNonlegacy, DISABLED_constructorLengthConvention)
{
  // Constructors must use the same length convention as the readers.
  {
    using Ops = TypeParam;
    typename Ops::Vector out;
    Ops::one(out);
    ASSERT_EQ(out.size(), 1);
    EXPECT_EQ(out[0], 0);
    Ops::var(2, 3, out);
    ASSERT_EQ(out.size(), 3);
    EXPECT_EQ(out[0], 1);
    EXPECT_EQ(out[1], 2);
    EXPECT_EQ(out[2], 3);
    Ops::var(2, 0, out);
    ASSERT_EQ(out.size(), 1);
    EXPECT_EQ(out[0], 0);
  }
}

}  // namespace
