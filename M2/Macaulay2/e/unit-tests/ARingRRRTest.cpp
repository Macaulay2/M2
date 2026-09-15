// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-RRR.hpp"

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
void getElement<M2::ARingRRR>(const M2::ARingRRR& R,
                              int index,
                              M2::ARingRRR::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

namespace {

std::string describe(const M2::ARingRRR::ElementType& value)
{
  char out[128];
  mpfr_snprintf(out, sizeof(out), "%.65Rg", &value);
  return out;
}

// Retain nbits of relative agreement, with an absolute floor near zero.
::testing::AssertionResult almostEqual(
    const M2::ARingRRR& R,
    int nbits,
    const M2::ARingRRR::ElementType& expected,
    const M2::ARingRRR::ElementType& actual)
{
  M2::ARingRRR::Element tolerance(R), difference(R), scale(R);
  mpfr_abs(&scale.value(), &expected, MPFR_RNDN);
  mpfr_abs(&difference.value(), &actual, MPFR_RNDN);
  mpfr_max(&scale.value(), &scale.value(), &difference.value(), MPFR_RNDN);
  if (mpfr_cmp_ui(&scale.value(), 1) < 0) R.set(scale, 1);
  mpfr_mul_2si(&tolerance.value(), &scale.value(), -nbits, MPFR_RNDN);
  mpfr_sub(&difference.value(), &actual, &expected, MPFR_RNDN);
  mpfr_abs(&difference.value(), &difference.value(), MPFR_RNDN);
  if (mpfr_number_p(&difference.value()) &&
      mpfr_cmp(&difference.value(), &tolerance.value()) < 0)
    return ::testing::AssertionSuccess();
  return ::testing::AssertionFailure()
         << "expected " << describe(expected) << ", got " << describe(actual)
         << "; error " << describe(difference) << ", tolerance "
         << describe(tolerance);
}

TEST(ARingRRR, approximationTolerance)
{
  // The helper accepts a one-bit rounding change and rejects a clear difference
  // or NaN.
  M2::ARingRRR R(100);
  M2::ARingRRR::Element a(R), b(R);
  R.set(a, 1);
  R.set(b, 1);
  EXPECT_TRUE(almostEqual(R, 98, a, b));
  mpfr_nextabove(&b.value());
  EXPECT_TRUE(almostEqual(R, 98, a, b));
  R.set(b, 2);
  EXPECT_FALSE(almostEqual(R, 98, a, b));
  mpfr_set_nan(&b.value());
  EXPECT_FALSE(almostEqual(R, 98, a, b));
}

TEST(ARingRRR, create)
{
  // The real ring must report its characteristic and selected precision in its
  // name.

  M2::ARingRRR R(100);
  EXPECT_EQ(ringName(R), "ARRR_100");
  EXPECT_EQ(R.characteristic(), 0);
}

void testRingNegateRRR(const M2::ARingRRR& R, int ntrials)
{
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (-a) + (a) == 0
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a));
      R.negate(b, a);
      R.add(c, a, b);
      EXPECT_TRUE(R.is_zero(c));
    }
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, negate)
{
  // An element and its negation cancel exactly.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  testRingNegateRRR(R, ntrials);
}

TEST(ARingRRR, add)
{
  // Adding and undoing the addition recovers the input within two rounding
  // bits.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d, e;
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
                   << "trial " << i << ", a=" << describe(a)
                   << ", b=" << describe(b));
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

TEST(ARingRRR, subtract)
{
  // Subtraction and multiply-subtract agree within their rounding allowances.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, e;
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
                   << "trial " << i << ", a=" << describe(a)
                   << ", b=" << describe(b));
      R.subtract(c, a, b);
      R.add(e, c, b);  // should be a
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 2, a, e));
      R.mult(e, a, b);
      R.subtract_multiple(e, a, b);
      EXPECT_TRUE(R.is_zero(e));
    }
  R.clear(e);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, multDivide)
{
  // A nonzero divisor undoes multiplication within the precision allowance.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d;
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
                   << "trial " << i << ", a=" << describe(a)
                   << ", b=" << describe(b));
      R.mult(c, a, b);
      if (R.is_zero(b))
        EXPECT_TRUE(R.is_zero(c));
      else
        {
          R.divide(d, c, b);
          EXPECT_TRUE(almostEqual(R, R.get_precision() - 6, d, a));
        }
    }
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, axioms)
{
  // Generated inputs check commutativity, associativity and distributivity with
  // rounding.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d, e;
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
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a)
                   << ", b=" << describe(b) << ", c=" << describe(c));
      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      R.add(d, a, b);
      R.add(e, b, a);
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 2, d, e));
      R.mult(d, a, b);
      R.mult(e, b, a);
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 2, d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(e, b, c);
      R.add(d, a, e);  // a+(b+c)
      R.add(e, a, b);
      R.add(e, e, c);  // (a+b)+c
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 6, d, e));
      R.mult(e, b, c);
      R.mult(d, a, e);  // a*(b*c)
      R.mult(e, a, b);
      R.mult(e, e, c);  // (a*b)*c
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 6, d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(e, b, c);
      R.mult(d, a, e);  // a*(b+c)
      R.mult(b, a, b);
      R.mult(c, a, c);
      R.add(e, b, c);  // a*b + a*c
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 6, d, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, power_and_invert)
{
  // Positive powers, reciprocal cancellation and both exponent interfaces agree
  // within rounding.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  mpz_t gmp1;
  mpz_init(gmp1);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a));
      R.power(b, a, 1);
      EXPECT_TRUE(R.is_equal(b, a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      SCOPED_TRACE(::testing::Message() << "exponents " << e1 << ", " << e2);
      R.power(b, a, e1);
      R.power(c, a, e2);
      R.power(d, a, e1 + e2);
      R.mult(c, b, c);
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 4, c, d));

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

TEST(ARingRRR, get_precision)
{
  // The ring and its elements retain the requested precision.

  M2::ARingRRR R(100);
  EXPECT_EQ(R.get_precision(), 100);
  M2::ARingRRR S(200);
  EXPECT_EQ(S.get_precision(), 200);
  EXPECT_EQ(ringName(S), "ARRR_200");

  // elements are initialized at the ring's precision
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  EXPECT_EQ(mpfr_get_prec(&a), 100);
  S.init_set(b, a);
  EXPECT_EQ(mpfr_get_prec(&b), 200);
  R.clear(a);
  S.clear(b);
}

TEST(ARingRRR, init_is_zero)
{
  // init allocates MPFR storage; it does not promise an initial value.
  GTEST_SKIP() << "ARingRRR::init does not initialize to zero; set_zero is "
                  "tested in set_coercions";
}

TEST(ARingRRR, precision_matters)
{
  // 1/3 at 100 bits differs from 1/3 at 53 bits, and from 1/3 at 200 bits
  M2::ARingRRR R(100);
  M2::ARingRRR S(200);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  R.set(a, 1);
  R.set(b, 3);
  R.divide(c, a, b);  // c = 1/3 at 100 bits
  EXPECT_NE(mpfr_cmp_d(&c, 1.0 / 3.0), 0);

  M2::ARingRRR::ElementType d;
  S.init(d);
  mpfr_set_si(&d, 1, MPFR_RNDN);
  mpfr_div_si(&d, &d, 3, MPFR_RNDN);  // d = 1/3 at 200 bits
  EXPECT_NE(mpfr_cmp(&c, &d), 0);
  // Compare the absolute error; a negative difference must not pass by sign
  // alone.
  EXPECT_TRUE(almostEqual(S, R.get_precision() - 2, c, d));
  S.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, coercions)
{
  // Small integer and rational conversions preserve the shared ARing contract.

  M2::ARingRRR R(100);
  testCoercions(R);
}

TEST(ARingRRR, set_coercions)
{
  // Conversions preserve exactly representable values and round other inputs to
  // ring precision.
  M2::ARingRRR R(100);

  {
    // Both signed integer overloads preserve small exact values.
    SCOPED_TRACE("set(long), set(int)");
    M2::ARingRRR::Element a(R);
    R.set(a, 12345L);
    EXPECT_EQ(mpfr_cmp_si(&a.value(), 12345), 0);
    R.set(a, -7);
    EXPECT_EQ(mpfr_cmp_si(&a.value(), -7), 0);
  }

  {
    // The low bit in 2^80 + 1 must survive the 100-bit conversion.
    SCOPED_TRACE("set(mpz)");
    M2::ARingRRR::Element a(R);
    mpz_t z;
    mpz_init(z);
    mpz_ui_pow_ui(z, 2, 80);
    mpz_add_ui(z, z, 1);
    R.set(a, z);
    EXPECT_EQ(mpfr_cmp_z(&a.value(), z), 0);
    mpz_clear(z);
  }

  {
    // Use an exact binary fraction and a rounded third to cover both paths.
    SCOPED_TRACE("set(mpq)");
    M2::ARingRRR::Element a(R), b(R), c(R);
    mpq_t q;
    mpq_init(q);
    mpq_set_si(q, -7, 4);
    EXPECT_TRUE(R.set(a, q));
    EXPECT_EQ(mpfr_cmp_d(&a.value(), -1.75), 0);
    mpq_set_si(q, 1, 3);
    EXPECT_TRUE(R.set(a, q));
    R.set(b, 1);
    R.set(c, 3);
    R.divide(b, b, c);  // 1/3 at 100 bits
    EXPECT_TRUE(R.is_equal(a, b));
    mpq_clear(q);
  }

  {
    // A negative binary fraction converts without rounding.
    SCOPED_TRACE("set(double)");
    M2::ARingRRR::Element a(R);
    EXPECT_TRUE(R.set(a, -0.125));
    EXPECT_EQ(mpfr_cmp_d(&a.value(), -0.125), 0);
  }

  {
    // A 300-bit third must round to the ring precision.
    SCOPED_TRACE("set(gmp_RR)");
    M2::ARingRRR::Element a(R);
    mpfr_t f;
    mpfr_init2(f, 300);
    mpfr_set_si(f, 1, MPFR_RNDN);
    mpfr_div_si(f, f, 3, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, f));
    EXPECT_EQ(mpfr_get_prec(&a.value()), 100);
    EXPECT_NE(mpfr_cmp(&a.value(), f), 0);
    mpfr_set_prec(f, 100);
    mpfr_set_si(f, 1, MPFR_RNDN);
    mpfr_div_si(f, f, 3, MPFR_RNDN);
    EXPECT_EQ(mpfr_cmp(&a.value(), f), 0);
    mpfr_clear(f);
  }

  {
    // Assigning a finite value must copy its contents.
    SCOPED_TRACE("set(ElementType)");
    M2::ARingRRR::Element a(R), b(R);
    R.set(a, 6.25);
    R.set(b, a);
    EXPECT_TRUE(R.is_equal(a, b));
  }

  {
    // A coefficient-ring variable placeholder replaces the old value with one.
    SCOPED_TRACE("set_var always gives 1");
    M2::ARingRRR::Element a(R);
    R.set(a, 7);
    R.set_var(a, 0);
    EXPECT_EQ(mpfr_cmp_si(&a.value(), 1), 0);
    R.set(a, -2);
    R.set_var(a, 3);
    EXPECT_EQ(mpfr_cmp_si(&a.value(), 1), 0);
  }

  {
    // Clearing a nonzero value must produce zero.
    SCOPED_TRACE("set_zero");
    M2::ARingRRR::Element a(R);
    R.set(a, 3.5);
    R.set_zero(a);
    EXPECT_TRUE(R.is_zero(a));
  }

  {
    // Copy and initialization from a value must survive changes to the source.
    SCOPED_TRACE("copy, init_set");
    M2::ARingRRR::Element a(R), b(R);
    R.set(a, 6.25);
    R.copy(b, a);
    EXPECT_TRUE(R.is_equal(a, b));
    M2::ARingRRR::Element copy(R, a);
    R.set_zero(a);
    EXPECT_EQ(mpfr_cmp_d(&copy.value(), 6.25), 0);
    EXPECT_EQ(mpfr_cmp_d(&b.value(), 6.25), 0);
  }
}

TEST(ARingRRR, ring_elem_roundtrip)
{
  // Exported values survive the ring_elem round trip without changing their
  // value.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  ring_elem r;
  for (int i = 0; i < 100; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a));
      R.to_ring_elem(r, a);
      // to_ring_elem makes a copy: changing a afterwards doesn't change r
      R.from_ring_elem(b, r);
      EXPECT_TRUE(R.is_equal(a, b));
      EXPECT_TRUE(R.is_equal(R.from_ring_elem_const(r), a));
      EXPECT_EQ(mpfr_get_prec(r.get_mpfr()), 100);
      R.set(a, 12345);
      EXPECT_TRUE(R.is_equal(R.from_ring_elem_const(r), b));
    }
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, addMultipleTo)
{
  // Accumulation agrees with a separately computed sum and product within four
  // rounding bits.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, r, s, t;
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
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a)
                   << ", b=" << describe(b) << ", r=" << describe(r));
      R.mult(t, a, b);
      R.add(s, r, t);
      R.addMultipleTo(r, a, b);
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 4, r, s));
    }
  R.clear(t);
  R.clear(s);
  R.clear(r);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, abs_and_abs_squared)
{
  // Exact magnitudes detect sign loss; squaring -3 must give 9.
  M2::ARingRRR R(100);
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
      M2::ARingRRR::Element a(R), result(R);
      R.set(a, sample.input);

      R.abs(result, a);
      EXPECT_EQ(R.coerceToDouble(result), sample.expected);
      R.set(a, sample.input);
      R.abs(a, a);
      EXPECT_EQ(R.coerceToDouble(a), sample.expected);
    }

  {
    // A separate known square checks multiplication of two negative factors.
    SCOPED_TRACE("abs_squared: negative integer");
    M2::ARingRRR::Element a(R), result(R);
    R.set(a, -3);

    R.abs_squared(result, a);

    EXPECT_EQ(R.coerceToDouble(result), 9);
  }
}

TEST(ARingRRR, compare_elems)
{
  // Signed zeros compare equally; negative and positive finite values retain
  // their order.

  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b, c, d;
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

  // values that differ only beyond 53 bits still compare correctly
  R.set(a, 1);
  mpfr_set_ui_2exp(&b, 1, -80, MPFR_RNDN);
  R.add(b, a, b);  // b = 1 + 2^-80
  EXPECT_EQ(R.compare_elems(a, b), -1);
  EXPECT_EQ(R.compare_elems(b, a), 1);
  EXPECT_FALSE(R.is_equal(a, b));

  // infinities
  mpfr_set_inf(&a, -1);
  mpfr_set_inf(&b, 1);
  R.set(c, 1);
  EXPECT_EQ(R.compare_elems(a, c), -1);
  EXPECT_EQ(R.compare_elems(b, c), 1);
  EXPECT_EQ(R.compare_elems(a, b), -1);
  EXPECT_EQ(R.compare_elems(b, b), 0);

  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, DISABLED_compare_elems_nan)
{
  // NaN compares equal to finite numbers; RRR also recognizes it as zero.
  // Disabled until a NaN comparison policy distinguishes it from finite values.
  // https://github.com/Macaulay2/M2/issues/4697
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType nan, one;
  R.init(nan);
  R.init(one);
  mpfr_set_nan(&nan);
  R.set(one, 1);
  EXPECT_NE(R.compare_elems(nan, one), 0);
  EXPECT_NE(R.compare_elems(one, nan), 0);
  EXPECT_FALSE(R.is_equal(nan, one));
  EXPECT_FALSE(R.is_zero(nan));
  R.clear(one);
  R.clear(nan);
}

TEST(ARingRRR, is_unit)
{
  // Zero has no reciprocal, while a nonzero finite value is a unit.

  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  R.set(a, 0);  // init leaves a as NaN, so set it explicitly
  R.set(b, 0.5);
  EXPECT_TRUE(R.is_unit(b));
  EXPECT_FALSE(R.is_unit(a));
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, swap)
{
  // Swapping two distinct values preserves both inputs.

  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  R.set(a, 1.5);
  R.set(b, -4.0);
  R.swap(a, b);
  EXPECT_EQ(mpfr_cmp_d(&a, -4.0), 0);
  EXPECT_EQ(mpfr_cmp_d(&b, 1.5), 0);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, inverseOfZero)
{
  // The reciprocal contract requires a nonzero input.
  GTEST_SKIP() << "invert(0) is outside the documented precondition";
}

TEST(ARingRRR, invert)
{
  // Reciprocals of nonzero powers of two are exact, including in place.
  M2::ARingRRR R(100);
  for (double value : {2.0, -4.0, 0.5})
    {
      SCOPED_TRACE(::testing::Message() << "invert: nonzero input " << value);
      M2::ARingRRR::Element a(R), result(R);
      R.set(a, value);

      R.invert(result, a);
      EXPECT_EQ(R.coerceToDouble(result), 1.0 / value);
      R.set(a, value);
      R.invert(a, a);
      EXPECT_EQ(R.coerceToDouble(a), 1.0 / value);
    }
}

TEST(ARingRRR, power_edge_cases)
{
  // Known powers cover zero, negative exponents and the mpz range boundary.
  M2::ARingRRR R(100);
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
      M2::ARingRRR::Element a(R), result(R);
      R.set(a, sample.base);
      mpz_t exponent;
      mpz_init_set_si(exponent, sample.exponent);

      R.power(result, a, sample.exponent);
      EXPECT_EQ(R.coerceToDouble(result), sample.expected);
      R.power_mpz(result, a, exponent);
      EXPECT_EQ(R.coerceToDouble(result), sample.expected);
      mpz_clear(exponent);
    }

  // MPFR accepts large exponents; a unit base stays finite while base 2
  // over/underflows.
  struct LargePowerCase
  {
    const char* name;
    int base, sign;
  };
  const LargePowerCase largeCases[] = {
      {"odd exponent of negative unit", -1, 1},
      {"positive exponent overflow", 2, 1},
      {"negative exponent underflow", 2, -1},
  };
  for (const auto& sample : largeCases)
    {
      SCOPED_TRACE(sample.name);
      M2::ARingRRR::Element a(R), result(R);
      R.set(a, sample.base);
      mpz_t exponent;
      mpz_init_set_ui(exponent, 1);
      mpz_mul_2exp(exponent, exponent, 40);
      mpz_add_ui(exponent, exponent, 1);
      if (sample.sign < 0) mpz_neg(exponent, exponent);

      R.power_mpz(result, a, exponent);

      if (sample.base == -1)
        EXPECT_EQ(mpfr_cmp_si(&result.value(), -1), 0);
      else if (sample.sign > 0)
        EXPECT_TRUE(mpfr_inf_p(&result.value()));
      else
        EXPECT_TRUE(R.is_zero(result));
      mpz_clear(exponent);
    }
}

static std::string elemString(const M2::ARingRRR& R,
                              double d,
                              bool p_one,
                              bool p_plus)
{
  M2::ARingRRR::ElementType a;
  R.init(a);
  R.set(a, d);
  buffer o;
  R.elem_text_out(o, a, p_one, p_plus, false);
  R.clear(a);
  return o.str();
}

TEST(ARingRRR, elem_text_out)
{
  // Unit coefficients may omit their digit; other values retain it and their
  // sign.
  M2::ARingRRR R(100);
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

TEST(ARingRRR, syzygy)
{
  // For nonzero inputs, the returned pair cancels a*x + b*y within relative
  // rounding error.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, x, y, c, d;
  R.init(a);
  R.init(b);
  R.init(x);
  R.init(y);
  R.init(c);
  R.init(d);
  mpfr_t eps;
  mpfr_init2(eps, 53);
  mpfr_set_ui_2exp(eps, 1, -96, MPFR_RNDN);
  for (int i = 0; i < ntrials; i++)
    {
      // test: x*a + y*b == 0, with x == 1
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a)
                   << ", b=" << describe(b));
      if (R.is_zero(a) or R.is_zero(b)) continue;
      R.syzygy(a, b, x, y);
      EXPECT_EQ(mpfr_cmp_si(&x, 1), 0);
      R.mult(c, x, a);
      R.mult(d, y, b);
      R.add(c, c, d);
      // the relation holds relative to the size of a
      mpfr_div(&c, &c, &a, MPFR_RNDN);
      EXPECT_LT(mpfr_cmpabs(&c, eps), 0);
    }
  mpfr_clear(eps);
  R.clear(d);
  R.clear(c);
  R.clear(y);
  R.clear(x);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, syzygy_b_zero)
{
  // The syzygy contract explicitly excludes zero inputs.
  GTEST_SKIP()
      << "syzygy requires nonzero a and b; see the nonzero cases in syzygy";
}

TEST(ARingRRR, zeroize_tiny)
{
  // The cutoff is strict: values at or above it keep their sign and magnitude.
  M2::ARingRRR R(100);
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
      M2::ARingRRR::Element a(R);
      R.set(a, sample.input);
      mpfr_t epsilon;
      mpfr_init2(epsilon, R.get_precision());
      mpfr_set_d(epsilon, 0.5, MPFR_RNDN);

      R.zeroize_tiny(epsilon, a);

      EXPECT_EQ(R.coerceToDouble(a), sample.expected);
      mpfr_clear(epsilon);
    }
}

TEST(ARingRRR, increase_norm)
{
  // Increasing a saved norm keeps the greater of the old norm and |a|.
  M2::ARingRRR R(100);
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
      M2::ARingRRR::Element a(R);
      R.set(a, sample.value);
      mpfr_t norm;
      mpfr_init2(norm, R.get_precision());
      mpfr_set_d(norm, sample.initial, MPFR_RNDN);

      R.increase_norm(norm, a);

      EXPECT_EQ(mpfr_cmp_d(norm, sample.expected), 0);
      mpfr_clear(norm);
    }
}

TEST(ARingRRR, coerceToDouble)
{
  // Conversion to double preserves exact values and rounds fractional values.

  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a;
  R.init(a);
  R.set(a, -2.75);
  EXPECT_EQ(R.coerceToDouble(a), -2.75);
  R.set(a, 3);
  mpfr_si_div(
      &a, 1, &a, MPFR_RNDN);  // 1/3 at 100 bits rounds to the double 1/3
  EXPECT_EQ(R.coerceToDouble(a), 1.0 / 3.0);
  R.clear(a);
}

TEST(ARingRRR, computeHashValue)
{
  // Independently copied equal values must have equal hashes.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  for (int i = 0; i < 100; i++)
    {
      // equal values hash equal
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a));
      R.copy(b, a);
      EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
    }
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, computeHashValue_negative)
{
  // Equal negative and large values must hash equally; collisions are allowed.
  // The current float-to-unsigned conversion also needs sanitizer coverage.
  M2::ARingRRR R(100);
  for (double value : {-1.0, -2.0, 1e20, 2e20})
    {
      SCOPED_TRACE(::testing::Message() << "hash: equal inputs " << value);
      M2::ARingRRR::Element a(R), b(R);
      R.set(a, value);
      R.set(b, value);

      EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
    }
}

TEST(ARingRRR, random)
{
  // Random values stay in [0, 1] at the requested precision.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a;
  R.init(a);
  for (int i = 0; i < ntrials; i++)
    {
      R.random(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ": a=" << describe(a));
      EXPECT_GE(mpfr_sgn(&a), 0);
      EXPECT_LE(mpfr_cmp_si(&a, 1), 0);
      EXPECT_EQ(mpfr_get_prec(&a), 100);
    }
  R.clear(a);
}

TEST(ARingRRR, addMultipleToKnownAnswer)
{
  // Check the independently known answer 1 + 2*3 = 7.
  M2::ARingRRR R(100);
  M2::ARingRRR::Element a(R), b(R), r(R);

  R.set(r, 1);
  R.set(a, 2);
  R.set(b, 3);
  R.addMultipleTo(r, a, b);
  EXPECT_EQ(mpfr_cmp_si(&r.value(), 7), 0);
}

TEST(ARingRRR, squaredMagnitudeProperties)
{
  // Generated squares agree with multiplication and remain nonnegative.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::Element a(R), b(R), c(R);

  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a));
      R.abs_squared(b, a);
      R.mult(c, a, a);
      EXPECT_TRUE(R.is_equal(b, c));
      EXPECT_GE(mpfr_sgn(&b.value()), 0);
    }
}

TEST(ARingRRR, negativePowerProperties)
{
  // Reciprocal powers cancel for nonzero generated inputs within rounding
  // error.

  seedRandom(0x5252);
  SCOPED_TRACE("seed 0x5252");

  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::Element a(R), b(R), c(R), one(R);
  R.set(one, 1);

  for (int i = 0; i < ntrials; i++)
    {
      // test: a^-2 * a^2 == 1
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << describe(a));
      if (R.is_zero(a)) continue;
      R.power(b, a, -2);
      R.power(c, a, 2);
      R.mult(c, b, c);
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 6, c, one));
    }
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
