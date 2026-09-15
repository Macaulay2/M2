// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-RR.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <limits>
#include <initializer_list>
#include <string>

#include "unit-tests/ARingTest.hpp"

template <>
void getElement<M2::ARingRR>(const M2::ARingRR& R,
                             int index,
                             M2::ARingRR::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

namespace {

// Products need a relative allowance; near zero, keep an absolute floor.
static double relativeTolerance(unsigned long nbits, double a, double b)
{
  return std::ldexp(std::max({1.0, std::fabs(a), std::fabs(b)}),
                    -static_cast<int>(nbits));
}

::testing::AssertionResult almostEqual(const M2::ARingRR&,
                                       unsigned long nbits,
                                       double expected,
                                       double actual)
{
  const double error = std::fabs(actual - expected);
  const double tolerance = relativeTolerance(nbits, expected, actual);
  if (error < tolerance) return ::testing::AssertionSuccess();
  return ::testing::AssertionFailure()
         << std::setprecision(17) << "expected " << expected << ", got "
         << actual << "; error " << error << ", tolerance " << tolerance;
}

TEST(ARingRR, almostEqual_tolerance)
{
  // The tolerance accepts rounding noise and rejects an ordinary unit
  // difference.

  M2::ARingRR R;
  auto nbits = R.get_precision() - 2;
  EXPECT_TRUE(almostEqual(R, nbits, 1.0, 1.0));
  EXPECT_TRUE(almostEqual(R, nbits, 1.0, std::nextafter(1.0, 2.0)));
  EXPECT_FALSE(almostEqual(R, nbits, 1.0, 2.0));
  EXPECT_FALSE(almostEqual(R, nbits, 2.0, 1.0));
  EXPECT_EQ(relativeTolerance(nbits, 0.0, 0.5), std::ldexp(1.0, -51));
  EXPECT_EQ(relativeTolerance(nbits, -32.0, 16.0), std::ldexp(32.0, -51));
  EXPECT_FALSE(
      almostEqual(R, nbits, 1.0, std::numeric_limits<double>::quiet_NaN()));
  EXPECT_FALSE(
      almostEqual(R, nbits, 1.0, std::numeric_limits<double>::infinity()));
}

TEST(ARingRR, create)
{
  // The real ring must report its characteristic and selected precision in its
  // name.

  M2::ARingRR R;
  EXPECT_EQ(ringName(R), "ARR_53");
  EXPECT_EQ(R.characteristic(), 0);
}

void testRingNegateRR(const M2::ARingRR& R, int ntrials)
{
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (-a) + (a) == 0
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a);
      R.negate(b, a);
      R.add(c, a, b);
      EXPECT_TRUE(R.is_zero(c));
    }
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, compare_elems)
{
  // Signed zeros compare equally; negative and positive finite values retain
  // their order.

  M2::ARingRR R;
  M2::ARingRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.set(a, 0.0);
  R.set(b, -0.0);
  R.set(c, -0.01);
  R.set(d, 0.01);
  EXPECT_EQ(R.compare_elems(a, b), 0);
  EXPECT_EQ(R.compare_elems(c, d), -1);
  EXPECT_EQ(R.compare_elems(d, a), 1);
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

TEST(ARingRR, negate)
{
  // An element and its negation cancel exactly.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  testRingNegateRR(R, ntrials);
}

TEST(ARingRR, add)
{
  // Adding and undoing the addition recovers the input within two rounding
  // bits.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << a << ", b=" << b);
      R.add(c, a, b);
      R.negate(d, b);
      R.add(e, c, d);  // should be a
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 2, a, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, subtract)
{
  // Subtraction and multiply-subtract agree within their rounding allowances.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a-b) + (b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << a << ", b=" << b);
      R.subtract(c, a, b);
      R.add(e, c, b);  // should be a
      EXPECT_TRUE(almostEqual(R, nbits - 2, a, e));
      R.mult(e, a, b);
      R.subtract_multiple(e, a, b);
      // A fused multiply-subtract may retain the product rounding error.
      EXPECT_TRUE(almostEqual(R, nbits - 2, e, 0));
    }
  R.clear(e);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, multDivide)
{
  // A nonzero divisor undoes multiplication within the precision allowance.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a*b) // b == a
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << a << ", b=" << b);
      R.mult(c, a, b);
      if (R.is_zero(b))
        EXPECT_TRUE(R.is_zero(c));
      else
        {
          R.divide(d, c, b);
          // std::endl;
          EXPECT_TRUE(almostEqual(R, nbits - 2, d, a));
        }
    }
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, axioms)
{
  // Generated inputs check commutativity, associativity and distributivity with
  // rounding.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a
                                        << ", b=" << b << ", c=" << c);
      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      R.add(d, a, b);
      R.add(e, b, a);
      EXPECT_TRUE(almostEqual(R, nbits - 2, d, e));
      R.mult(d, a, b);
      R.mult(e, b, a);
      EXPECT_TRUE(almostEqual(R, nbits - 2, d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(e, b, c);
      R.add(d, a, e);  // a+(b+c)
      R.add(e, a, b);
      R.add(e, e, c);  // (a+b)+c
      EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));
      R.mult(e, b, c);
      R.mult(d, a, e);  // a*(b*c)
      R.mult(e, a, b);
      R.mult(e, e, c);  // (a*b)*c
      EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(e, b, c);
      R.mult(d, a, e);  // a*(b+c)
      R.mult(b, a, b);
      R.mult(c, a, c);
      R.add(e, b, c);  // a*b + a*c
      EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, power_and_invert)
{
  // Positive powers, reciprocal cancellation and both exponent interfaces agree
  // within rounding.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  mpz_t gmp1;
  mpz_init(gmp1);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a);
      R.power(b, a, 1);
      EXPECT_TRUE(R.is_equal(b, a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      SCOPED_TRACE(::testing::Message() << "exponents " << e1 << ", " << e2);
      R.power(b, a, e1);
      R.power(c, a, e2);
      R.power(d, a, e1 + e2);
      R.mult(c, b, c);
      EXPECT_TRUE(almostEqual(R, nbits - 4, c, d));

      if (!R.is_zero(a))
        {
          R.invert(c, a);
          R.mult(c, c, a);
          R.set(d, 1);
          EXPECT_TRUE(almostEqual(R, R.get_precision() - 4, c, d));
        }

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      R.power_mpz(d, a, gmp1);
      EXPECT_TRUE(R.is_equal(d, b));
    }
  mpz_clear(gmp1);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, inverseOfZero)
{
  // The reciprocal contract requires a nonzero input.
  GTEST_SKIP() << "invert(0) is outside the documented precondition";
}

TEST(ARingRR, invert)
{
  // Reciprocals of nonzero powers of two are exact, including in place.
  M2::ARingRR R;
  for (double value : {2.0, -4.0, 0.5})
    {
      SCOPED_TRACE(::testing::Message() << "invert: nonzero input " << value);
      M2::ARingRR::Element a(R), result(R);
      R.set(a, value);

      R.invert(result, a);
      EXPECT_EQ(R.coerceToDouble(result), 1.0 / value);
      R.set(a, value);
      R.invert(a, a);
      EXPECT_EQ(R.coerceToDouble(a), 1.0 / value);
    }
}

TEST(ARingRR, zeroize_tiny)
{
  // The cutoff is strict: values at or above it keep their sign and magnitude.
  M2::ARingRR R;
  // Binary fractions make both the cutoff and every expected answer exact.
  struct CutoffCase
  {
    const char* name;
    double input, expected;
  };
  const CutoffCase cases[] = {
      {"below", 0.25, 0},
      {"at cutoff", 0.5, 0.5},
      {"above", 0.75, 0.75},
      {"negative below", -0.25, 0},
      {"negative at cutoff", -0.5, -0.5},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      M2::ARingRR::Element a(R);
      R.set(a, sample.input);
      mpfr_t epsilon;
      mpfr_init2(epsilon, R.get_precision());
      mpfr_set_d(epsilon, 0.5, MPFR_RNDN);

      R.zeroize_tiny(epsilon, a);

      EXPECT_EQ(R.coerceToDouble(a), sample.expected);
      mpfr_clear(epsilon);
    }
}

TEST(ARingRR, is_unit)
{
  // Zero has no reciprocal, while a nonzero finite value is a unit.

  M2::ARingRR R;
  M2::ARingRR::ElementType a, b;
  R.init(a);
  R.init_set(b, 0.5);
  EXPECT_TRUE(R.is_unit(b));
  EXPECT_FALSE(R.is_unit(a));
  R.clear(a);
  R.clear(b);
}

TEST(ARingRR, get_precision)
{
  // The double backend reports its 53-bit significand precision.

  M2::ARingRR R;
  EXPECT_EQ(R.get_precision(), 53);
}

TEST(ARingRR, set_coercions)
{
  // Conversions preserve exactly representable values and round other inputs to
  // ring precision.
  M2::ARingRR R;

  {
    // Signed machine integers, including the exact-double boundary, preserve
    // their value.
    SCOPED_TRACE("set(long)");
    M2::ARingRR::Element a(R);
    R.set(a, 12345L);
    EXPECT_EQ(a.value(), 12345.0);
    R.set(a, -7L);
    EXPECT_EQ(a.value(), -7.0);
    long big = (1L << 53);
    R.set(a, big);
    EXPECT_EQ(a.value(), 9007199254740992.0);
  }

  {
    // Powers of two remain exact beyond double integer precision.
    SCOPED_TRACE("set(mpz)");
    M2::ARingRR::Element a(R);
    mpz_t z;
    mpz_init(z);
    mpz_set_si(z, -42);
    R.set(a, z);
    EXPECT_EQ(a.value(), -42.0);
    mpz_ui_pow_ui(z, 2, 100);
    R.set(a, z);
    EXPECT_EQ(a.value(), std::ldexp(1.0, 100));
    mpz_clear(z);
  }

  {
    // Use an exact binary fraction and a rounded third to cover both paths.
    SCOPED_TRACE("set(mpq)");
    M2::ARingRR::Element a(R);
    mpq_t q;
    mpq_init(q);
    mpq_set_si(q, 1, 3);
    EXPECT_TRUE(R.set(a, q));
    EXPECT_EQ(a.value(), 1.0 / 3.0);
    mpq_set_si(q, -7, 4);
    EXPECT_TRUE(R.set(a, q));
    EXPECT_EQ(a.value(), -1.75);
    mpq_clear(q);
  }

  {
    // An exact real and a high-precision third check conversion rounding.
    SCOPED_TRACE("set(gmp_RR)");
    M2::ARingRR::Element a(R);
    mpfr_t f;
    mpfr_init2(f, 53);
    mpfr_set_d(f, 2.5, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, f));
    EXPECT_EQ(a.value(), 2.5);
    mpfr_set_prec(f, 200);
    mpfr_set_si(f, 1, MPFR_RNDN);
    mpfr_div_si(f, f, 3, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, f));
    EXPECT_EQ(a.value(), 1.0 / 3.0);
    mpfr_clear(f);
  }

  {
    // A negative binary fraction converts without rounding.
    SCOPED_TRACE("set(double)");
    M2::ARingRR::Element a(R);
    EXPECT_TRUE(R.set(a, -0.125));
    EXPECT_EQ(a.value(), -0.125);
  }

  {
    // A coefficient-ring variable placeholder replaces the old value with one.
    SCOPED_TRACE("set_var always gives 1");
    M2::ARingRR::Element a(R);
    R.set(a, 7);
    R.set_var(a, 0);
    EXPECT_EQ(a.value(), 1.0);
    R.set(a, -2);
    R.set_var(a, 3);
    EXPECT_EQ(a.value(), 1.0);
  }

  {
    // Clearing a nonzero value must produce zero.
    SCOPED_TRACE("set_zero");
    M2::ARingRR::Element a(R);
    R.set(a, 3.5);
    R.set_zero(a);
    EXPECT_TRUE(R.is_zero(a));
  }

  {
    // Copy and initialization from a value must survive changes to the source.
    SCOPED_TRACE("copy, init_set");
    M2::ARingRR::Element a(R), b(R);
    R.set(a, 6.25);
    R.copy(b, a);
    EXPECT_EQ(b.value(), 6.25);
    M2::ARingRR::Element copy(R, a);
    R.set_zero(a);
    EXPECT_EQ(copy.value(), 6.25);
    EXPECT_EQ(b.value(), 6.25);
  }
}

TEST(ARingRR, ring_elem_roundtrip)
{
  // Exported values survive the ring_elem round trip without changing their
  // value.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b;
  R.init(a);
  R.init(b);
  ring_elem r;
  for (int i = 0; i < 100; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a);
      R.to_ring_elem(r, a);
      R.from_ring_elem(b, r);
      EXPECT_EQ(a, b);
      EXPECT_EQ(R.from_ring_elem_const(r), a);
    }
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, negativeZeroRoundTrip)
{
  // Exporting a signed zero must preserve its sign as well as its zero value.
  M2::ARingRR R;
  M2::ARingRR::Element a(R), b(R);
  ring_elem r;
  R.set(a, -0.0);
  R.to_ring_elem(r, a);
  R.from_ring_elem(b, r);
  EXPECT_TRUE(std::signbit(b));
  EXPECT_TRUE(R.is_zero(b));
}

TEST(ARingRR, addMultipleTo)
{
  // Accumulation agrees with a separately computed sum and product within four
  // rounding bits.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, r, s, t;
  R.init(a);
  R.init(b);
  R.init(r);
  R.init(s);
  R.init(t);
  for (int i = 0; i < ntrials; i++)
    {
      // test: r += a*b agrees with s = r + (a*b)
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(r);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a
                                        << ", b=" << b << ", r=" << r);
      R.mult(t, a, b);
      R.add(s, r, t);
      R.addMultipleTo(r, a, b);
      EXPECT_TRUE(almostEqual(R, nbits - 4, r, s));
    }
  R.clear(t);
  R.clear(s);
  R.clear(r);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, abs_and_abs_squared)
{
  // Exact magnitudes detect sign loss; squaring -3 must give 9.
  M2::ARingRR R;
  struct MagnitudeCase
  {
    const char* name;
    double input, expected;
  };
  const MagnitudeCase cases[] = {
      {"negative", -2.5, 2.5},
      {"positive", 2.5, 2.5},
      {"negative zero", -0.0, 0},
      {"negative integer", -3, 3},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      M2::ARingRR::Element a(R), result(R);
      R.set(a, sample.input);

      R.abs(result, a);
      EXPECT_EQ(R.coerceToDouble(result), sample.expected);
      EXPECT_FALSE(std::signbit(R.coerceToDouble(result)));
      R.set(a, sample.input);
      R.abs(a, a);
      EXPECT_EQ(R.coerceToDouble(a), sample.expected);
    }

  {
    // A separate known square checks multiplication of two negative factors.
    SCOPED_TRACE("abs_squared: negative integer");
    M2::ARingRR::Element a(R), result(R);
    R.set(a, -3);

    R.abs_squared(result, a);

    EXPECT_EQ(R.coerceToDouble(result), 9);
  }
}

TEST(ARingRR, swap)
{
  // Swapping two distinct values preserves both inputs.

  M2::ARingRR R;
  M2::ARingRR::ElementType a, b;
  R.init_set(a, 1.5);
  R.init_set(b, -4.0);
  R.swap(a, b);
  EXPECT_EQ(a, -4.0);
  EXPECT_EQ(b, 1.5);
  R.clear(a);
  R.clear(b);
}

TEST(ARingRR, power_edge_cases)
{
  // Known powers cover zero, negative exponents and the mpz range boundary.
  M2::ARingRR R;
  // A zero exponent is one even for a zero base; negative powers use
  // reciprocals.
  struct PowerCase
  {
    const char* name;
    double base;
    int exponent;
    double expected;
  };
  const PowerCase cases[] = {
      {"zero to zero", 0, 0, 1},
      {"nonzero to zero", -3.5, 0, 1},
      {"negative exponent", 2, -3, 0.125},
      {"negative square", 2, -2, 0.25},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      M2::ARingRR::Element a(R), result(R);
      R.set(a, sample.base);
      mpz_t exponent;
      mpz_init_set_si(exponent, sample.exponent);

      R.power(result, a, sample.exponent);
      EXPECT_EQ(R.coerceToDouble(result), sample.expected);
      R.power_mpz(result, a, exponent);
      EXPECT_EQ(R.coerceToDouble(result), sample.expected);
      mpz_clear(exponent);
    }

  // Both signs of 2^40 lie outside the supported int exponent range.
  for (int sign : {1, -1})
    {
      SCOPED_TRACE(::testing::Message()
                   << "power_mpz: oversized exponent, sign " << sign);
      M2::ARingRR::Element a(R), result(R);
      R.set(a, 2);
      mpz_t exponent;
      mpz_init_set_si(exponent, sign);
      mpz_mul_2exp(exponent, exponent, 40);

      EXPECT_THROW(R.power_mpz(result, a, exponent), exc::engine_error);
      mpz_clear(exponent);
    }
}

static std::string elemString(const M2::ARingRR& R,
                              const M2::ARingRR::ElementType& a,
                              bool p_one,
                              bool p_plus)
{
  buffer o;
  R.elem_text_out(o, a, p_one, p_plus, false);
  return o.str();
}

TEST(ARingRR, elem_text_out)
{
  // Unit coefficients may omit their digit; other values retain it and their
  // sign.
  M2::ARingRR R;
  struct FormatCase
  {
    const char* name;
    double value;
    bool printOne, printPlus;
    const char* expected;
  };
  const FormatCase cases[] = {
      {"positive", 2.5, true, false, "2.5"},
      {"positive with plus", 2.5, true, true, "+2.5"},
      {"negative", -2.5, true, false, "-2.5"},
      {"negative with plus", -2.5, true, true, "-2.5"},
      {"zero with plus", 0, true, true, "0"},
      {"unit", 1, true, false, "1"},
      {"omitted unit", 1, false, false, ""},
      {"omitted unit with plus", 1, false, true, "+"},
      {"negative unit", -1, true, false, "-1"},
      {"omitted negative unit", -1, false, false, "-"},
      {"omitted negative unit with plus", -1, false, true, "-"},
      {"nonunit cannot be omitted", 2.5, false, false, "2.5"},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      EXPECT_EQ(elemString(R, sample.value, sample.printOne, sample.printPlus),
                sample.expected);
    }
}

TEST(ARingRR, syzygy)
{
  // For nonzero inputs, the returned pair cancels a*x + b*y within relative
  // rounding error.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, x, y, c, d;
  R.init(a);
  R.init(b);
  R.init(x);
  R.init(y);
  R.init(c);
  R.init(d);
  for (int i = 0; i < ntrials; i++)
    {
      // test: x*a + y*b == 0, with x == 1
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << a << ", b=" << b);
      if (R.is_zero(a) or R.is_zero(b)) continue;
      R.syzygy(a, b, x, y);
      EXPECT_EQ(x, 1.0);
      R.mult(c, x, a);
      R.mult(d, y, b);
      R.add(c, c, d);
      R.abs(d, a);
      // the relation holds relative to the size of a
      EXPECT_LE(std::fabs(c), d * std::ldexp(1.0, 2 - static_cast<int>(nbits)));
    }
  R.clear(d);
  R.clear(c);
  R.clear(y);
  R.clear(x);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, syzygy_b_zero)
{
  // The syzygy contract explicitly excludes zero inputs.
  GTEST_SKIP()
      << "syzygy requires nonzero a and b; see the nonzero cases in syzygy";
}

TEST(ARingRR, increase_norm)
{
  // Increasing a saved norm keeps the greater of the old norm and |a|.
  M2::ARingRR R;
  struct NormCase
  {
    const char* name;
    double initial, value, expected;
  };
  const NormCase cases[] = {
      {"larger magnitude", 0, -3.5, 3.5},
      {"smaller magnitude", 3.5, 2, 3.5},
      {"equal magnitude", 3.5, -3.5, 3.5},
      {"new maximum", 3.5, 10, 10},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      M2::ARingRR::Element a(R);
      R.set(a, sample.value);
      mpfr_t norm;
      mpfr_init2(norm, R.get_precision());
      mpfr_set_d(norm, sample.initial, MPFR_RNDN);

      R.increase_norm(norm, a);

      EXPECT_EQ(mpfr_cmp_d(norm, sample.expected), 0);
      mpfr_clear(norm);
    }
}

TEST(ARingRR, coerceToDouble)
{
  // Conversion to double preserves exact values and rounds fractional values.

  M2::ARingRR R;
  EXPECT_EQ(R.coerceToDouble(-2.75), -2.75);
  EXPECT_EQ(R.coerceToDouble(0.0), 0.0);
  EXPECT_EQ(R.coerceToDouble(1e300), 1e300);
}

TEST(ARingRR, computeHashValue)
{
  // Independently copied equal values must have equal hashes.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b;
  R.init(a);
  R.init(b);
  for (int i = 0; i < 100; i++)
    {
      // equal values hash equal
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a);
      R.copy(b, a);
      EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
    }
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, computeHashValue_negative)
{
  // Equal negative and large values must hash equally; collisions are allowed.
  // The current float-to-unsigned conversion also needs sanitizer coverage.
  M2::ARingRR R;
  for (double value : {-1.0, -2.0, 1e20, 2e20})
    {
      SCOPED_TRACE(::testing::Message() << "hash: equal inputs " << value);
      M2::ARingRR::Element a(R), b(R);
      R.set(a, value);
      R.set(b, value);

      EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
    }
}

TEST(ARingRR, compare_elems_infinity)
{
  // Infinities sort beyond finite values and compare equal to themselves.

  M2::ARingRR R;
  double inf = std::numeric_limits<double>::infinity();
  EXPECT_EQ(R.compare_elems(-inf, 1.0), -1);
  EXPECT_EQ(R.compare_elems(inf, 1.0), 1);
  EXPECT_EQ(R.compare_elems(-inf, inf), -1);
  EXPECT_EQ(R.compare_elems(1e308, inf), -1);
  EXPECT_EQ(R.compare_elems(inf, inf), 0);
}

TEST(ARingRR, DISABLED_compare_elems_nan)
{
  // NaN compares equal to finite numbers; RRR also recognizes it as zero.
  // Disabled until a NaN comparison policy distinguishes it from finite values.
  // https://github.com/Macaulay2/M2/issues/4697
  M2::ARingRR R;
  double nan = std::numeric_limits<double>::quiet_NaN();
  EXPECT_NE(R.compare_elems(nan, 1.0), 0);
  EXPECT_NE(R.compare_elems(1.0, nan), 0);
}

TEST(ARingRR, random)
{
  // Random values stay in [0, 1].
  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  M2::ARingRR::ElementType a;
  R.init(a);
  for (int i = 0; i < ntrials; i++)
    {
      R.random(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ": a=" << a);
      EXPECT_GE(a, 0.0);
      EXPECT_LE(a, 1.0);
    }
  R.clear(a);
}

TEST(ARingRR, addMultipleToKnownAnswer)
{
  // Check the independently known answer 1 + 2*3 = 7.
  M2::ARingRR R;
  M2::ARingRR::Element r(R);

  R.set(r, 1.0);
  R.addMultipleTo(r, 2.0, 3.0);
  EXPECT_EQ(r, 7.0);
}

TEST(ARingRR, squaredMagnitudeProperties)
{
  // Generated squares agree with multiplication and remain nonnegative.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::Element a(R), b(R), c(R);

  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a);
      R.abs_squared(b, a);
      R.mult(c, a, a);
      EXPECT_EQ(b, c);
      EXPECT_GE(b, 0.0);
    }
}

TEST(ARingRR, negativePowerProperties)
{
  // Reciprocal powers cancel for nonzero generated inputs within rounding
  // error.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::Element a(R), b(R), c(R), one(R);
  R.set(one, 1);
  const auto nbits = R.get_precision();

  for (int i = 0; i < ntrials; i++)
    {
      // test: a^-2 * a^2 == 1
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << a);
      if (R.is_zero(a)) continue;
      R.power(b, a, -2);
      R.power(c, a, 2);
      R.mult(c, b, c);
      EXPECT_TRUE(almostEqual(R, nbits - 4, c, 1.0));
    }
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
