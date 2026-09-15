// Copyright 2013 Michael E. Stillman
#include <gtest/gtest.h>
#include <string>

#include "unit-tests/RingTest.hpp"
#include <limits>
#include <sstream>
#include <gmpxx.h>

template <>
ring_elem getElement<RingZZ>(const RingZZ& R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  mpz_t maxH;
  mpz_init_set_str(maxH, "100000000000", 10);
  gmp_ZZ a1 = rawRandomInteger(maxH);
  mpz_clear(maxH);
  return R.from_int(a1);
}

TEST(RingZZ, create)
{
  // Construction exposes the expected coefficient type and ring name.
  ASSERT_NE(globalZZ, nullptr);

  EXPECT_TRUE(dynamic_cast<const RingZZ*>(globalZZ) != nullptr);
  EXPECT_EQ(globalZZ->coefficient_type(), Ring::COEFF_ZZ);
  EXPECT_TRUE(globalZZ->is_ZZ());
  EXPECT_EQ(ringName(*globalZZ), "ZZ");
}
TEST(RingZZ, ones)
{
  // Integer coercion agrees with the cached zero and unit constants.
  EXPECT_TRUE(globalZZ->is_equal(globalZZ->one(), globalZZ->from_long(1)));
  EXPECT_TRUE(
      globalZZ->is_equal(globalZZ->minus_one(), globalZZ->from_long(-1)));
  EXPECT_TRUE(globalZZ->is_equal(globalZZ->zero(), globalZZ->from_long(0)));
  EXPECT_TRUE(globalZZ->is_zero(globalZZ->from_long(0)));
}
TEST(RingZZ, random)
{
  // Decimal output must reconstruct each generated integer exactly.
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  mpz_class bound("100000000000");
  for (int trial = 0; trial < 11; ++trial)
    {
      const ring_elem value =
          globalZZ->from_int(rawRandomInteger(bound.get_mpz_t()));
      const std::string text = RingElem(globalZZ, value).toString();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", value=" << text);
      mpz_class parsed;
      ASSERT_EQ(mpz_set_str(parsed.get_mpz_t(), text.c_str(), 10), 0);
      EXPECT_TRUE(
          ringEquals(globalZZ, value, globalZZ->from_int(parsed.get_mpz_t())));
    }
}
TEST(RingZZ, get_si)
{
  // Conversion succeeds exactly inside the signed-int bounds.
  // GMP values also express the out-of-range cases on platforms with 32-bit
  // long.
  const mpz_class minimum(std::numeric_limits<int>::min());
  const mpz_class maximum(std::numeric_limits<int>::max());
  for (const mpz_class& boundary : {minimum, maximum})
    for (int offset = -5; offset <= 5; ++offset)
      {
        const mpz_class input = boundary + offset;
        SCOPED_TRACE(::testing::Message() << "input " << input);
        const auto converted = RingZZ::get_si(input.get_mpz_t());
        const bool fits = input >= minimum && input <= maximum;
        EXPECT_EQ(converted.first, fits);
        if (fits) EXPECT_EQ(converted.second, input.get_si());
      }
}
TEST(RingZZ, negate)
{  // Generated values cancel their additive inverses.
  testRingNegate(globalZZ, ntrials);
}
TEST(RingZZ, add)
{  // Adding and then subtracting the same value recovers the input.
  testRingAdd(globalZZ, ntrials);
}
TEST(RingZZ, subtract)
{  // Subtraction is undone by adding the subtrahend.
  testRingSubtract(globalZZ, ntrials);
}
TEST(RingZZ, multDivide)
{
  // Exact multiplication/division must recover each generated integer.
  testRingDivide(globalZZ, ntrials);
}
TEST(RingZZ, axioms)
{  // Generated inputs satisfy the commutative ring identities.
  testRingAxioms(globalZZ, ntrials);
}
TEST(RingZZ, power)
{  // Machine and GMP exponents agree with the power addition law.
  testRingPower(globalZZ, ntrials);
}
TEST(RingZZ, gcd)
{
  // The generated gcd divides both inputs and satisfies the Bezout identity.
  testRingGCD(globalZZ, ntrials);
}
TEST(RingZZ, remainder)
{  // Quotient and remainder reconstruct the dividend for nonzero divisors.
  testRingRemainder(globalZZ, ntrials);
}
TEST(RingZZ, syzygy)
{  // Syzygy coefficients cancel their inputs when the second input is nonzero.
  testRingSyzygy(globalZZ, ntrials);
}
TEST(RingZZ, content)
{
  // Associate normalization returns a unit; lowering a divisor keeps its first
  // sign.
  {
    // Positive and negative inputs need opposite normalizing units.
    SCOPED_TRACE("preferred_associate: signs");
    EXPECT_EQ(RingElem(globalZZ,
                       globalZZ->preferred_associate(globalZZ->from_long(-5))),
              RingElem::fromInt(globalZZ, -1));
    EXPECT_EQ(RingElem(globalZZ,
                       globalZZ->preferred_associate(globalZZ->from_long(5))),
              RingElem::fromInt(globalZZ, 1));
  }
  {
    // Each row gives the initial divisor, new input, and expected unit.
    SCOPED_TRACE("lower_associate_divisor: initial zero and established signs");
    struct Case
    {
      const char* name;
      long initial;
      long input;
      long expected;
    };
    const Case cases[] = {{"both zero", 0, 0, 0},
                          {"first negative", 0, -10, -1},
                          {"keep negative", -1, 15, -1},
                          {"keep positive", 1, -15, 1}};
    for (const auto& sample : cases)
      {
        SCOPED_TRACE(sample.name);
        ring_elem divisor = globalZZ->from_long(sample.initial);
        EXPECT_EQ(globalZZ->lower_associate_divisor(
                      divisor, globalZZ->from_long(sample.input)),
                  sample.expected != 0);
        EXPECT_EQ(RingElem(globalZZ, divisor),
                  RingElem::fromInt(globalZZ, sample.expected));
      }
  }
}

TEST(RingZZ, divisionErrors)
{
  // Inexact integer division and division by zero have distinct error
  // contracts.
  EXPECT_THROW(globalZZ->divide(globalZZ->from_long(5), globalZZ->from_long(2)),
               exc::engine_error);
  EXPECT_THROW(globalZZ->divide(globalZZ->from_long(3), globalZZ->zero()),
               exc::division_by_zero_error);
}

TEST(RingZZ, gcdExamples)
{
  // Zero and unit inputs pin the gcd normalization independently of generated
  // cases.
  EXPECT_EQ(
      RingElem(globalZZ, globalZZ->gcd(globalZZ->zero(), globalZZ->zero())),
      RingElem::fromInt(globalZZ, 0));
  EXPECT_EQ(
      RingElem(globalZZ, globalZZ->gcd(globalZZ->one(), globalZZ->minus_one())),
      RingElem::fromInt(globalZZ, 1));
  EXPECT_EQ(
      RingElem(globalZZ,
               globalZZ->gcd(globalZZ->minus_one(), globalZZ->minus_one())),
      RingElem::fromInt(globalZZ, 1));
}

TEST(RingZZ, remainderByZero)
{
  // The GMP-backed remainder routines require a nonzero divisor.
  GTEST_SKIP() << "Remainder and quotient properties exclude a zero divisor";
}

TEST(RingZZ, fromStream)
{
  // Signed and arbitrary-size integers parse without consuming a following
  // operator.
  const char* inputs[] = {"+123", "-45", "123456789123456789123456789"};
  for (const char* text : inputs)
    {
      SCOPED_TRACE(text);
      std::istringstream input(std::string(text) + "*x");
      ring_elem value;
      ASSERT_TRUE(fromStream(input, *globalZZ, value));
      const mpz_class expected(text[0] == '+' ? text + 1 : text);
      EXPECT_TRUE(ringEquals(
          globalZZ, globalZZ->from_int(expected.get_mpz_t()), value));
      EXPECT_EQ(input.peek(), '*');
      EXPECT_FALSE(fromStream(input, *globalZZ, value));
      EXPECT_TRUE(ringEquals(
          globalZZ, globalZZ->from_int(expected.get_mpz_t()), value));
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
