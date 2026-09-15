// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-ZZ-flint.hpp"

#include <gtest/gtest.h>
#include <flint/fmpz.h>
#include <gmp.h>

#include <initializer_list>
#include <string>

#include "basic-rings/aring-QQ-gmp.hpp"
#include "basic-rings/aring-ZZ-gmp.hpp"
#include "unit-tests/ARingTest.hpp"

extern gmp_ZZ getRandomInteger();

template <>
void getElement<M2::ARingZZ>(const M2::ARingZZ& R,
                             int index,
                             M2::ARingZZ::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set(result, a);
    }
}

namespace {

class ARingZZ : public ::testing::Test
{
 protected:
  using Ring = M2::ARingZZ;
  Ring R;

  ::testing::AssertionResult hasValue(const Ring::ElementType& a, long expected)
  {
    if (fmpz_cmp_si(&a, expected) == 0) return ::testing::AssertionSuccess();
    buffer out;
    R.elem_text_out(out, a, true, false, false);
    return ::testing::AssertionFailure()
           << "expected " << expected << ", got " << out.str();
  }
};

TEST_F(ARingZZ, create)
{
  // The integer ring reports characteristic zero and unbounded cardinality.
  EXPECT_EQ(ringName(R), "ZZFlint");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
}

TEST_F(ARingZZ, arithmetic)
{
  // Check the integer arithmetic identities over generated inputs.
  seedRandom(0x5a5a);
  SCOPED_TRACE("seed 0x5a5a");
  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testAxioms(R, ntrials);
}

TEST_F(ARingZZ, finiteFieldContracts)
{
  // The shared reciprocal and power checks require a finite field.
  GTEST_SKIP() << "ZZ is not a field; invert, power and power_mpz have "
                  "integer-specific tests";
}

TEST_F(ARingZZ, is_unit)
{
  // Only +/-1 are units, including when a nonunit needs heap storage.
  for (int value : {1, -1, 0, 2})
    {
      SCOPED_TRACE(::testing::Message() << "is_unit: " << value);
      Ring::Element a(R);
      R.set(a, value);

      EXPECT_EQ(R.is_unit(a), value == 1 || value == -1);
    }

  // 2^65 uses FLINT's large-integer representation.
  SCOPED_TRACE("is_unit: heap integer");
  Ring::Element large(R);
  ASSERT_EQ(fmpz_set_str(&large.value(), "36893488147419103232", 10), 0);
  EXPECT_FALSE(R.is_unit(large));
}

TEST_F(ARingZZ, compare_elems)
{
  // Adjacent integers must keep their order in either storage representation.
  struct ComparisonCase
  {
    const char* name;
    const char* left;
    const char* right;
  };
  const ComparisonCase cases[] = {
      {"small integers", "0", "1"},
      {"heap integers", "36893488147419103232", "36893488147419103233"},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Ring::Element a(R), b(R);
      ASSERT_EQ(fmpz_set_str(&a.value(), sample.left, 10), 0);
      ASSERT_EQ(fmpz_set_str(&b.value(), sample.right, 10), 0);

      EXPECT_EQ(R.compare_elems(a, b), -1);
      EXPECT_EQ(R.compare_elems(b, a), 1);
      EXPECT_EQ(R.compare_elems(a, a), 0);
    }
}

TEST_F(ARingZZ, init_set)
{
  // A copy must own its large-integer storage independently of the source.
  Ring::Element a(R);
  ASSERT_EQ(fmpz_set_str(&a.value(), "36893488147419103232", 10), 0);
  Ring::Element b(R, a);

  R.set_zero(a);

  buffer out;
  R.elem_text_out(out, b, true, false, false);
  EXPECT_STREQ(out.str(), "36893488147419103232");
}

TEST_F(ARingZZ, set)
{
  // Copying a heap integer preserves its value; only integral rationals
  // convert.
  {
    // A const element reference selects copying instead of integer coercion.
    SCOPED_TRACE("set: heap integer copy");
    Ring::Element a(R), b(R);
    ASSERT_EQ(fmpz_set_str(&a.value(), "36893488147419103232", 10), 0);
    const auto& source = a.value();

    R.set(b, source);

    EXPECT_TRUE(R.is_equal(a, b));
    R.set_zero(a);
    buffer out;
    R.elem_text_out(out, b, true, false, false);
    EXPECT_STREQ(out.str(), "36893488147419103232");
  }

  // The denominator distinguishes the successful and rejected conversions.
  struct RationalCase
  {
    const char* name;
    int denominator;
    bool converts;
  };
  const RationalCase cases[] = {{"integral rational", 1, true},
                                {"fractional rational", 2, false}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      M2::ARingQQGMP Q;
      M2::ARingQQGMP::Element numerator(Q), denominator(Q), rational(Q);
      Ring::Element a(R);
      Q.set(numerator, 57);
      Q.set(denominator, sample.denominator);
      Q.divide(rational, numerator, denominator);

      EXPECT_EQ(R.set(a, &rational.value()), sample.converts);
      if (sample.converts) EXPECT_TRUE(hasValue(a, 57));
    }
}

TEST_F(ARingZZ, set_var)
{
  // A coefficient ring has no variables; set_var replaces any old value by 1.
  for (int initial : {0, 57})
    {
      SCOPED_TRACE(::testing::Message() << "set_var: initial " << initial);
      Ring::Element a(R);
      R.set(a, initial);

      R.set_var(a, initial == 0 ? 5 : 3);

      EXPECT_TRUE(hasValue(a, 1));
    }
}

TEST_F(ARingZZ, invert)
{
  // Integer inversion preserves +/-1 and returns zero for nonunits.
  for (int value : {1, -1, 57})
    {
      SCOPED_TRACE(::testing::Message() << "invert: " << value);
      Ring::Element a(R), result(R);
      R.set(a, value);

      R.invert(result, a);

      EXPECT_TRUE(hasValue(result, value == 57 ? 0 : value));
    }
}

TEST_F(ARingZZ, divide)
{
  // FLINT reports inexact integer division by returning false.
  struct DivisionCase
  {
    const char* name;
    int numerator;
    int denominator;
    bool exact;
    int quotient;
  };
  const DivisionCase cases[] = {{"exact", 8, 4, true, 2},
                                {"inexact", 2, 3, false, 0}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Ring::Element a(R), b(R), result(R);
      R.set(a, sample.numerator);
      R.set(b, sample.denominator);

      EXPECT_EQ(R.divide(result, a, b), sample.exact);
      if (sample.exact) EXPECT_TRUE(hasValue(result, sample.quotient));
    }
}

TEST_F(ARingZZ, power)
{
  // A small known power catches incorrect multiplication or exponent handling.
  Ring::Element a(R), result(R);
  R.set(a, 2);

  R.power(result, a, 3);

  EXPECT_TRUE(hasValue(result, 8));
}

TEST_F(ARingZZ, power_mpz)
{
  // The mpz entry point accepts bounded nonnegative exponents and rejects
  // others.
  {
    // 2^31 exceeds signed 32-bit range but is still a small FLINT integer.
    SCOPED_TRACE("power_mpz: exponent 31");
    Ring::Element a(R), result(R);
    M2::ARingZZGMP Z;
    M2::ARingZZGMP::Element exponent(Z);
    R.set(a, 2);
    Z.set(exponent, 31);

    R.power_mpz(result, a, &exponent.value());

    EXPECT_TRUE(hasValue(result, 2147483648L));
  }

  // Negative and oversized exponents take distinct rejection paths.
  struct ExponentCase
  {
    const char* name;
    const char* exponent;
  };
  const ExponentCase cases[] = {
      {"negative exponent", "-3"},
      {"oversized exponent", "37778931862957161709568"}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Ring::Element a(R), result(R);
      M2::ARingZZGMP Z;
      M2::ARingZZGMP::Element exponent(Z);
      R.set(a, 2);
      ASSERT_EQ(mpz_set_str(&exponent.value(), sample.exponent, 10), 0);

      EXPECT_THROW(R.power_mpz(result, a, &exponent.value()),
                   exc::engine_error);
    }
}

TEST_F(ARingZZ, swap)
{
  // Distinct values expose a swap that overwrites either input.
  Ring::Element a(R), b(R);
  R.set(a, 57);
  R.set(b, 2);

  R.swap(a, b);

  EXPECT_TRUE(hasValue(a, 2));
  EXPECT_TRUE(hasValue(b, 57));
}

TEST_F(ARingZZ, random)
{
  // Generated integers must survive copying and conversion without losing
  // value.
  seedRandom(0x5a5a);
  SCOPED_TRACE("seed 0x5a5a");
  Ring::Element a(R), b(R);
  for (int trial = 0; trial < ntrials; ++trial)
    {
      R.random(a);
      buffer out;
      R.elem_text_out(out, a, true, false, false);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ": a=" << out.str());
      ring_elem stored;

      R.to_ring_elem(stored, a);
      R.from_ring_elem(b, stored);

      EXPECT_TRUE(R.is_equal(a, b));
    }
}

TEST_F(ARingZZ, display)
{
  // Sign and unit-coefficient flags must not suppress a nonunit's digits.
  struct FormatCase
  {
    const char* name;
    int value;
    bool printOne;
    bool printPlus;
    const char* expected;
  };
  const FormatCase cases[] = {
      {"positive", 24, false, false, "24"},
      {"leading plus", 24, true, true, "+24"},
      {"negative unit", -1, true, false, "-1"},
      {"omitted negative unit", -1, false, false, "-"},
      {"positive unit", 1, true, false, "1"},
      {"omitted positive unit", 1, false, false, ""},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Ring::Element a(R);
      R.set(a, sample.value);
      buffer out;

      R.elem_text_out(out, a, sample.printOne, sample.printPlus, false);

      EXPECT_STREQ(out.str(), sample.expected);
    }
}

TEST_F(ARingZZ, syzygy)
{
  // Check primitive cancelling pairs, including zero, unit, and sign branches.
  struct SyzygyCase
  {
    const char* name;
    int a, b, x, y;
  };
  const SyzygyCase cases[] = {
      {"zero first input", 0, 1, 1, 0},
      {"positive unit divisor", 5, 1, 1, -5},
      {"negative unit divisor", 5, -1, 1, 5},
      {"positive divisor", 6, 8, 4, -3},
      {"negative divisor", 6, -8, 4, 3},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Ring::Element a(R), b(R), x(R), y(R), sum(R), term(R);
      R.set(a, sample.a);
      R.set(b, sample.b);

      R.syzygy(a, b, x, y);

      EXPECT_TRUE(hasValue(x, sample.x));
      EXPECT_TRUE(hasValue(y, sample.y));
      R.mult(sum, a, x);
      R.mult(term, b, y);
      R.add(sum, sum, term);
      EXPECT_TRUE(R.is_zero(sum));
    }
}

TEST_F(ARingZZ, computeHashValue)
{
  // Equal integers must hash equally; the hash itself is not a ring element.
  Ring::Element a(R), b(R);
  R.set(a, 5);
  R.set(b, 5);

  EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
}

TEST_F(ARingZZ, coerceToLongInteger)
{
  // Coercion succeeds only when the integer fits in a machine long.
  {
    // 2^80 exceeds the range of long on the supported platforms.
    SCOPED_TRACE("coerceToLongInteger: oversized integer");
    Ring::Element a(R);
    ASSERT_EQ(fmpz_set_str(&a.value(), "1208925819614629174706176", 10), 0);
    long result = 0;

    EXPECT_FALSE(R.coerceToLongInteger(result, a));
  }

  {
    // A modest exact integer checks the value as well as the success flag.
    SCOPED_TRACE("coerceToLongInteger: representable integer");
    Ring::Element a(R);
    R.set(a, 1048576);
    long result = 0;

    ASSERT_TRUE(R.coerceToLongInteger(result, a));
    EXPECT_EQ(result, 1048576);
  }
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check"
// indent-tabs-mode: nil
// End:
