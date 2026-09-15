#include "basic-rings/aring-RRi.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <string>

#include "unit-tests/ARingTest.hpp"

template <>
void getElement<M2::ARingRRi>(const M2::ARingRRi& R,
                              int index,
                              M2::ARingRRi::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

namespace {

class ARingRRi : public ::testing::Test
{
 protected:
  using Ring = M2::ARingRRi;
  Ring R {100};

  void SetUp() override { seedRandom(0x525269); }

  std::string describe(const Ring::ElementType& value) const
  {
    char text[256];
    mpfr_snprintf(
        text, sizeof(text), "[%.35Rg, %.35Rg]", &value.left, &value.right);
    return text;
  }

  bool numeric(const Ring::ElementType& value) const
  {
    return !mpfr_nan_p(&value.left) && !mpfr_nan_p(&value.right);
  }

  ::testing::AssertionResult contains(const Ring::ElementType& outer,
                                      const Ring::ElementType& inner) const
  {
    if (numeric(outer) && numeric(inner) &&
        mpfr_cmp(&outer.left, &inner.left) <= 0 &&
        mpfr_cmp(&outer.right, &inner.right) >= 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
           << describe(outer) << " does not contain " << describe(inner);
  }

  // Different expressions can have different interval widths because repeated
  // occurrences of an operand are treated independently. Their ranges overlap.
  ::testing::AssertionResult overlaps(const Ring::ElementType& a,
                                      const Ring::ElementType& b) const
  {
    if (numeric(a) && numeric(b) && mpfr_cmp(&a.left, &b.right) <= 0 &&
        mpfr_cmp(&b.left, &a.right) <= 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
           << "disjoint ranges: " << describe(a) << " and " << describe(b);
  }

  ::testing::AssertionResult hasBounds(const Ring::ElementType& value,
                                       double left,
                                       double right) const
  {
    if (numeric(value) && mpfr_cmp_d(&value.left, left) == 0 &&
        mpfr_cmp_d(&value.right, right) == 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
           << "expected [" << left << ", " << right << "], got "
           << describe(value);
  }
};

TEST_F(ARingRRi, create)
{
  // Construction retains the requested precision and identifies a real interval
  // ring.
  EXPECT_EQ(ringName(R), "ARRi_100");
  EXPECT_EQ(R.characteristic(), 0);
  EXPECT_EQ(R.get_precision(), 100);
}

TEST_F(ARingRRi, arithmeticExamples)
{
  // Dyadic endpoints make the expected interval bounds exactly representable.
  // Each case starts with fresh operands, including the aliasing examples.
  {
    // Negation reverses the endpoint order as well as the signs.
    SCOPED_TRACE("negate: interval crosses zero");
    Ring::Element a {R}, result {R};
    R.set_from_doubles(a, -2, 3);
    R.negate(result, a);
    EXPECT_TRUE(hasBounds(result, -3, 2));
    R.negate(a, a);
    EXPECT_TRUE(hasBounds(a, -3, 2));
  }
  {
    // Independent intervals add and subtract endpoint bounds.
    SCOPED_TRACE("add/subtract: disjoint positive intervals");
    Ring::Element a {R}, b {R}, result {R};
    R.set_from_doubles(a, 1, 2);
    R.set_from_doubles(b, 3, 4);
    R.add(result, a, b);
    EXPECT_TRUE(hasBounds(result, 4, 6));
    R.subtract(result, a, b);
    EXPECT_TRUE(hasBounds(result, -3, -1));
  }
  {
    // Multiplication takes the smallest and largest endpoint products.
    SCOPED_TRACE("multiply: mixed signs");
    Ring::Element a {R}, b {R}, result {R};
    R.set_from_doubles(a, -2, 3);
    R.set_from_doubles(b, 4, 5);
    R.mult(result, a, b);
    EXPECT_TRUE(hasBounds(result, -10, 15));
  }
  {
    // Division and inversion use an interval that excludes zero.
    SCOPED_TRACE("divide/invert: positive denominator");
    Ring::Element a {R}, b {R}, result {R};
    R.set_from_doubles(a, 2, 4);
    R.set_from_doubles(b, 2, 4);
    R.divide(result, a, b);
    EXPECT_TRUE(hasBounds(result, 0.5, 2));
    R.invert(result, b);
    EXPECT_TRUE(hasBounds(result, 0.25, 0.5));
  }
  {
    // Point intervals have the ordinary integer power values, including 0^0.
    SCOPED_TRACE("power: exact point values");
    Ring::Element a {R}, result {R};
    R.set(a, -2);
    R.power(result, a, 3);
    EXPECT_TRUE(hasBounds(result, -8, -8));
    R.set_zero(a);
    R.power(result, a, 0);
    EXPECT_TRUE(hasBounds(result, 1, 1));
  }
}

TEST_F(ARingRRi, negate)
{
  // Adding an interval to its negation must enclose zero, even for wide
  // intervals.
  SCOPED_TRACE("seed 0x525269");
  ARingElementGenerator<Ring> gen(R);
  Ring::Element a {R}, b {R}, result {R}, zero {R};
  R.set_zero(zero);
  for (int trial = 0; trial < ntrials; ++trial)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", a=" << describe(a));
      R.negate(b, a);
      R.add(result, a, b);
      EXPECT_TRUE(contains(result, zero));
    }
}

TEST_F(ARingRRi, add)
{
  // Cancellation may widen an interval; it must still contain the original
  // range.
  SCOPED_TRACE("seed 0x525269");
  ARingElementGenerator<Ring> gen(R);
  Ring::Element a {R}, b {R}, sum {R}, negative {R}, result {R};
  for (int trial = 0; trial < ntrials; ++trial)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", a=" << describe(a)
                   << ", b=" << describe(b));
      R.add(sum, a, b);
      R.negate(negative, b);
      R.add(result, sum, negative);
      EXPECT_TRUE(contains(result, a));
    }
}

TEST_F(ARingRRi, subtract)
{
  // Undoing subtraction encloses the input; subtracting a product encloses
  // zero.
  SCOPED_TRACE("seed 0x525269");
  ARingElementGenerator<Ring> gen(R);
  Ring::Element a {R}, b {R}, difference {R}, result {R}, zero {R};
  R.set_zero(zero);
  for (int trial = 0; trial < ntrials; ++trial)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", a=" << describe(a)
                   << ", b=" << describe(b));
      {
        SCOPED_TRACE("subtract then add");
        R.subtract(difference, a, b);
        R.add(result, difference, b);
        EXPECT_TRUE(contains(result, a));
      }
      {
        SCOPED_TRACE("subtract_multiple: accumulator starts at the product");
        R.mult(result, a, b);
        R.subtract_multiple(result, a, b);
        EXPECT_TRUE(contains(result, zero));
      }
    }
}

TEST_F(ARingRRi, multDivide)
{
  // Multiplication followed by division encloses the input when zero is
  // excluded.
  SCOPED_TRACE("seed 0x525269");
  ARingElementGenerator<Ring> gen(R);
  Ring::Element a {R}, b {R}, product {R}, result {R}, zero {R};
  R.set_zero(zero);
  for (int trial = 0; trial < ntrials; ++trial)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", a=" << describe(a)
                   << ", b=" << describe(b));
      R.mult(product, a, b);
      if (R.is_member(0L, b))
        EXPECT_TRUE(contains(product, zero));
      else
        {
          R.divide(result, product, b);
          EXPECT_TRUE(contains(result, a));
        }
    }
}

TEST_F(ARingRRi, axioms)
{
  // Commutativity preserves bounds. Reassociated and distributed expressions
  // must overlap, since interval dependency can give them different widths.
  SCOPED_TRACE("seed 0x525269");
  ARingElementGenerator<Ring> gen(R);
  Ring::Element a {R}, b {R}, c {R}, left {R}, right {R}, temp {R};
  for (int trial = 0; trial < ntrials; ++trial)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", a=" << describe(a)
                   << ", b=" << describe(b) << ", c=" << describe(c));
      {
        // Swapping operands does not change interval dependency.
        SCOPED_TRACE("commutativity");
        R.add(left, a, b);
        R.add(right, b, a);
        EXPECT_TRUE(R.is_equal(left, right));
        R.mult(left, a, b);
        R.mult(right, b, a);
        EXPECT_TRUE(R.is_equal(left, right));
      }
      {
        // Different rounding sequences must describe compatible ranges.
        SCOPED_TRACE("associativity");
        R.add(temp, b, c);
        R.add(left, a, temp);
        R.add(temp, a, b);
        R.add(right, temp, c);
        EXPECT_TRUE(overlaps(left, right));
        R.mult(temp, b, c);
        R.mult(left, a, temp);
        R.mult(temp, a, b);
        R.mult(right, temp, c);
        EXPECT_TRUE(overlaps(left, right));
      }
      {
        // Distribution repeats a, which can widen the right-hand enclosure.
        SCOPED_TRACE("distributivity");
        R.add(temp, b, c);
        R.mult(left, a, temp);
        R.mult(temp, a, b);
        R.mult(right, a, c);
        R.add(right, temp, right);
        EXPECT_TRUE(overlaps(left, right));
      }
    }
}

TEST_F(ARingRRi, power_and_invert)
{
  // Power interfaces agree for nonnegative exponents. A reciprocal of an
  // interval excluding zero gives a product containing one.
  SCOPED_TRACE("seed 0x525269");
  ARingElementGenerator<Ring> gen(R);
  Ring::Element a {R}, b {R}, c {R}, result {R}, one {R};
  R.set(one, 1);
  mpz_t exponent;
  mpz_init(exponent);
  for (int trial = 0; trial < ntrials; ++trial)
    {
      gen.nextElement(a);
      const int e1 = rawRandomInt(10) + 1;
      const int e2 = rawRandomInt(10) + 1;
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", a=" << describe(a)
                   << ", exponents=" << e1 << ", " << e2);
      {
        // Repeated operands may widen products, but their enclosures overlap.
        SCOPED_TRACE("power: compatible enclosures and exponent interfaces");
        R.power(b, a, 0);
        EXPECT_TRUE(hasBounds(b, 1, 1));
        R.power(b, a, 1);
        EXPECT_TRUE(R.is_equal(b, a));
        R.power(b, a, e1);
        R.power(c, a, e2);
        R.mult(c, b, c);
        R.power(result, a, e1 + e2);
        EXPECT_TRUE(overlaps(c, result));
        mpz_set_si(exponent, e1);
        R.power_mpz(result, a, exponent);
        EXPECT_TRUE(R.is_equal(result, b));
      }
      if (!R.is_member(0L, a))
        {
          // Reciprocal cancellation requires the entire interval to exclude
          // zero.
          SCOPED_TRACE("invert: zero excluded");
          R.invert(b, a);
          R.mult(result, a, b);
          EXPECT_TRUE(contains(result, one));
        }
    }
  mpz_clear(exponent);
}

TEST_F(ARingRRi, inverseContainingZero)
{
  // A finite reciprocal enclosure requires an interval that excludes zero.
  GTEST_SKIP() << "Reciprocal properties exclude intervals containing zero";
}

}  // namespace
