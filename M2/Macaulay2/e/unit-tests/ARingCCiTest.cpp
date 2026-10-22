#include "basic-rings/aring-CCi.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <string>

#include "unit-tests/ARingComplexTest.hpp"

namespace {

template <>
struct ComplexTraits<M2::ARingCCi>
{
  static bool supportsPrecision(unsigned long) { return true; }
  static const char* precisionLimit() { return "none"; }
  // The fixture ring uses 100 bits.
  static M2::ARingCCi make(unsigned long precision = 100)
  {
    return M2::ARingCCi(precision);
  }
  static const char* ringNamePrefix() { return "ACCi_"; }

  // Comparisons alone cannot reject NaN endpoints: MPFR returns zero for them.
  static bool hasNumericBounds(const M2::ARingCCi::ElementType& value)
  {
    return !mpfr_nan_p(&value.re.left) && !mpfr_nan_p(&value.re.right) &&
           !mpfr_nan_p(&value.im.left) && !mpfr_nan_p(&value.im.right);
  }

  // Check for one exact complex value. Both ends of each range must match
  // that value.
  static bool isPoint(const M2::ARingCCi::ElementType& value,
                      double real,
                      double imaginary)
  {
    return hasNumericBounds(value) && mpfr_cmp_d(&value.re.left, real) == 0 &&
           mpfr_cmp_d(&value.re.right, real) == 0 &&
           mpfr_cmp_d(&value.im.left, imaginary) == 0 &&
           mpfr_cmp_d(&value.im.right, imaginary) == 0;
  }

  // Print enough digits to see small differences in a failure message.
  static std::string describe(const M2::ARingCCi::ElementType& value)
  {
    char out[512];
    mpfr_snprintf(out,
                  sizeof(out),
                  "[%.65Rg, %.65Rg] + [%.65Rg, %.65Rg]i",
                  &value.re.left,
                  &value.re.right,
                  &value.im.left,
                  &value.im.right);
    return out;
  }
};

}  // namespace

INSTANTIATE_TYPED_TEST_SUITE_P(ARingCCi,
                               ComplexRing,
                               ::testing::Types<M2::ARingCCi>);

namespace {

// Cases specific to CCi, whose elements are rectangles of possible values.
// Everything shared with CC and CCC is in the typed suite above.
class ARingCCi : public ComplexRing<M2::ARingCCi>
{
 protected:
  Ring::Element a {C}, b {C}, result {C};

  // Set the real range first, then the imaginary range. In each pair, give
  // the lower bound before the upper bound.
  void setBounds(Ring::ElementType& value,
                 double reLeft,
                 double reRight,
                 double imLeft,
                 double imRight) const
  {
    C.set_real_part_from_doubles(value, reLeft, reRight);
    C.set_imaginary_part_from_doubles(value, imLeft, imRight);
  }

  // Check all four bounds against a known rectangle. A failure prints both
  // rectangles for comparison.
  ::testing::AssertionResult hasBounds(const Ring::ElementType& value,
                                       long reLeft,
                                       long reRight,
                                       long imLeft,
                                       long imRight) const
  {
    if (Traits::hasNumericBounds(value) &&
        mpfr_cmp_si(&value.re.left, reLeft) == 0 &&
        mpfr_cmp_si(&value.re.right, reRight) == 0 &&
        mpfr_cmp_si(&value.im.left, imLeft) == 0 &&
        mpfr_cmp_si(&value.im.right, imRight) == 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
           << "expected [" << reLeft << ", " << reRight << "] + [" << imLeft
           << ", " << imRight << "]i, got " << describe(value);
  }

  // Check that the outer rectangle covers the inner one in both directions.
  // This allows an interval calculation to return a wider answer.
  ::testing::AssertionResult contains(const Ring::ElementType& outer,
                                      const Ring::ElementType& inner) const
  {
    if (Traits::hasNumericBounds(outer) && Traits::hasNumericBounds(inner) &&
        mpfr_cmp(&outer.re.left, &inner.re.left) <= 0 &&
        mpfr_cmp(&outer.re.right, &inner.re.right) >= 0 &&
        mpfr_cmp(&outer.im.left, &inner.im.left) <= 0 &&
        mpfr_cmp(&outer.im.right, &inner.im.right) >= 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure() << "expected " << describe(outer)
                                         << " to contain " << describe(inner);
  }
};

TEST_F(ARingCCi, Storage)
{
  // Storing a rectangle must keep all four bounds; single values are covered
  // by the shared suite.

  {
    // Copy a rectangle, then clear the original. The copy must keep all
    // four original bounds.
    SCOPED_TRACE("set: mutable rectangle");
    setBounds(a, 1, 3, -4, -2);

    ASSERT_TRUE(C.set_from_ComplexInterval(result.value(), a.value()));
    C.set_zero(a);

    EXPECT_TRUE(hasBounds(result, 1, 3, -4, -2));
  }
}

TEST_F(ARingCCi, Conversions)
{
  // Check the conversions that involve ranges: from real and complex
  // intervals, one part at a time, and back to a single complex number.

  {
    // Keep both ends of the real range when copying it. The imaginary range
    // must become exactly zero.
    SCOPED_TRACE("set: real interval");
    M2::ARingRRi R(100);
    M2::ARingRRi::Element source(R);
    mpfi_interv_si(&source.value(), 1, 3);
    C.set_from_doubles(a, 9.0, 8.0);

    ASSERT_TRUE(C.set_from_Interval(a, &source.value()));

    EXPECT_TRUE(hasBounds(a, 1, 3, 0, 0));
  }

  {
    // Turn one complex number into a rectangle with no width. Each pair of
    // bounds must equal the corresponding part of the number.
    SCOPED_TRACE("set: big complex point");
    M2::ARingCCC sourceRing(100);
    M2::ARingCCC::Element source(sourceRing);
    sourceRing.set_from_doubles(source, 2.0, -3.0);

    ASSERT_TRUE(C.set_from_BigComplex(result, &source.value()));

    EXPECT_TRUE(hasBounds(result, 2, 2, -3, -3));
  }

  {
    // Read both ranges from the external interval form. None of the four
    // bounds should move.
    SCOPED_TRACE("set: big complex interval");
    setBounds(a, 1, 3, -4, -2);
    gmp_CCi_struct source;
    source.re = &a.value().re;
    source.im = &a.value().im;

    ASSERT_TRUE(C.set_from_ComplexInterval(result, &source));

    EXPECT_TRUE(hasBounds(result, 1, 3, -4, -2));
  }

  {
    // Replace the real range only. The imaginary range must remain
    // unchanged.
    SCOPED_TRACE("set real part: existing rectangle");
    M2::ARingRRi R(100);
    M2::ARingRRi::Element source(R);
    mpfi_interv_si(&source.value(), -4, -2);
    setBounds(a, 1, 3, 2, 4);

    C.set_real_part(a, source);

    EXPECT_TRUE(hasBounds(a, -4, -2, 2, 4));
    EXPECT_TRUE(R.is_equal(C.realPartReference(a), source));
  }

  {
    // Replace the imaginary range only. The real range must remain
    // unchanged.
    SCOPED_TRACE("set imaginary part: existing rectangle");
    M2::ARingRRi R(100);
    M2::ARingRRi::Element source(R);
    mpfi_interv_si(&source.value(), -4, -2);
    setBounds(a, 1, 3, 2, 4);

    C.set_imaginary_part(a, source);

    EXPECT_TRUE(hasBounds(a, 1, 3, -4, -2));
    EXPECT_TRUE(R.is_equal(C.imaginaryPartReference(a), source));
  }

  {
    // Converting a whole rectangle to one number should pick its center.
    // Check the real and imaginary centers separately.
    SCOPED_TRACE("to big complex: rectangle");
    setBounds(a, 1, 3, -4, -2);

    auto midpoint = C.toBigComplex(a);

    EXPECT_TRUE(realEquals(midpoint->re, 2)) << "real midpoint";
    EXPECT_TRUE(realEquals(midpoint->im, -3)) << "imaginary midpoint";
  }
}

TEST_F(ARingCCi, IntervalPredicates)
{
  // Check empty ranges, equality, and whether one rectangle fits inside
  // another. A rectangle that contains zero is not necessarily the single
  // value zero.

  {
    // Zero imaginary endpoints must not hide a real interval away from zero.
    SCOPED_TRACE("is zero: positive real interval with zero imaginary part");
    setBounds(a, 1, 2, 0, 0);

    EXPECT_FALSE(C.is_zero(a));
  }

  {
    // These ranges include zero but also include other values. They must
    // not be reported as exactly zero.
    SCOPED_TRACE("is zero: rectangle contains zero");
    setBounds(a, 0, 1, 0, 0);
    EXPECT_FALSE(C.is_zero(a)) << "nonzero real width";

    setBounds(a, 0, 0, 0, 1);
    EXPECT_FALSE(C.is_zero(a)) << "nonzero imaginary width";
  }

  {
    // Move one bound at a time. Equality must notice changes to any of the
    // four bounds.
    SCOPED_TRACE("is equal: changed endpoint");
    setBounds(a, 1, 3, 2, 4);
    struct Bounds
    {
      long reLeft, reRight, imLeft, imRight;
      const char* changed;
    };
    const Bounds cases[] = {{0, 3, 2, 4, "real left"},
                            {1, 5, 2, 4, "real right"},
                            {1, 3, 0, 4, "imaginary left"},
                            {1, 3, 2, 5, "imaginary right"}};

    for (const auto& sample : cases)
      {
        SCOPED_TRACE(sample.changed);
        setBounds(
            b, sample.reLeft, sample.reRight, sample.imLeft, sample.imRight);
        EXPECT_FALSE(C.is_equal(a, b))
            << describe(a) << " versus " << describe(b);
      }
  }

  {
    // Points in the middle or on the edge should count as inside. A
    // rectangle must also contain itself.
    SCOPED_TRACE("is subset: interior and boundary points");
    setBounds(a, 1, 3, -1, 1);
    C.set_from_doubles(b, 2.0, 0.0);
    EXPECT_TRUE(C.is_subset(b, a)) << "interior point";

    C.set_from_doubles(b, 3.0, 1.0);
    EXPECT_TRUE(C.is_subset(b, a)) << "boundary point";
    EXPECT_TRUE(C.is_subset(a, a)) << "rectangle contains itself";
  }

  {
    // Let one bound extend beyond the outer rectangle. One bound outside is
    // enough to reject full containment.
    SCOPED_TRACE("is subset: endpoint outside rectangle");
    setBounds(a, 1, 3, -1, 1);
    struct Bounds
    {
      long reLeft, reRight, imLeft, imRight;
      const char* outside;
    };
    const Bounds cases[] = {{0, 2, 0, 0, "real left"},
                            {2, 4, 0, 0, "real right"},
                            {2, 2, -2, 0, "imaginary left"},
                            {2, 2, 0, 2, "imaginary right"}};

    for (const auto& sample : cases)
      {
        SCOPED_TRACE(sample.outside);
        setBounds(
            b, sample.reLeft, sample.reRight, sample.imLeft, sample.imRight);
        EXPECT_FALSE(C.is_subset(b, a))
            << describe(b) << " is outside " << describe(a);
      }
  }

  {
    // An ordinary pair of ranges describes a usable rectangle. It must not
    // be marked empty.
    SCOPED_TRACE("is empty: ordinary rectangle");
    setBounds(a, 1, 3, -1, 1);

    EXPECT_FALSE(C.is_empty(a));
  }

  {
    // Mark the real range invalid using NaN bounds. The whole rectangle
    // should then be reported empty.
    SCOPED_TRACE("is empty: invalid real interval");
    C.set_zero(a);
    mpfr_set_nan(&a.value().re.left);
    mpfr_set_nan(&a.value().re.right);

    EXPECT_TRUE(C.is_empty(a));
  }

  {
    // An invalid imaginary range also makes the rectangle empty. A valid
    // real range cannot rescue it.
    SCOPED_TRACE("is empty: invalid imaginary interval");
    C.set_zero(a);
    mpfr_set_nan(&a.value().im.left);
    mpfr_set_nan(&a.value().im.right);

    EXPECT_TRUE(C.is_empty(a));
  }
}

TEST_F(ARingCCi, Membership)
{
  // Check whether values in several number formats lie inside a rectangle.
  // Real values fit only if their real part is inside and the imaginary
  // range includes zero.

  {
    // Try the same inside, edge, and outside checks using an ordinary
    // integer. Moving the imaginary range away from zero must reject the
    // real value.
    SCOPED_TRACE("is member: long");
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(2L, a)) << "interior point";
    EXPECT_TRUE(C.is_member(1L, a)) << "real boundary point";
    EXPECT_FALSE(C.is_member(4L, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    EXPECT_FALSE(C.is_member(2L, a)) << "imaginary interval excludes zero";
  }

  {
    // Try the same inside, edge, and outside checks using an ordinary
    // floating-point number. Moving the imaginary range away from zero must
    // reject the real value.
    SCOPED_TRACE("is member: double");
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(2.0, a)) << "interior point";
    EXPECT_TRUE(C.is_member(1.0, a)) << "real boundary point";
    EXPECT_FALSE(C.is_member(4.0, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    EXPECT_FALSE(C.is_member(2.0, a)) << "imaginary interval excludes zero";
  }

  {
    // Try the same inside, edge, and outside checks using a GMP integer.
    // Moving the imaginary range away from zero must reject the real value.
    SCOPED_TRACE("is member: integer");
    mpz_t value;
    mpz_init_set_si(value, 2);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    mpz_set_si(value, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    mpz_set_si(value, 4);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    mpz_set_si(value, 2);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
    mpz_clear(value);
  }

  {
    // Try the same inside, edge, and outside checks using a GMP fraction.
    // Moving the imaginary range away from zero must reject the real value.
    SCOPED_TRACE("is member: rational");
    mpq_t value;
    mpq_init(value);
    mpq_set_si(value, 2, 1);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    mpq_set_si(value, 1, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    mpq_set_si(value, 4, 1);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    mpq_set_si(value, 2, 1);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
    mpq_clear(value);
  }

  {
    // Try the same inside, edge, and outside checks using a high-precision
    // real number. Moving the imaginary range away from zero must reject
    // the real value.
    SCOPED_TRACE("is member: big real");
    M2::ARingRRR R(100);
    M2::ARingRRR::Element value(R);
    R.set_from_long(value, 2);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    R.set_from_long(value, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    R.set_from_long(value, 4);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    R.set_from_long(value, 2);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
  }

  {
    // Try the same inside, edge, and outside checks using a real interval
    // containing one value. Moving the imaginary range away from zero must
    // reject the real value.
    SCOPED_TRACE("is member: real interval");
    M2::ARingRRi R(100);
    M2::ARingRRi::Element value(R);
    R.set_from_long(value, 2);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    R.set_from_long(value, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    R.set_from_long(value, 4);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    R.set_from_long(value, 2);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
  }

  {
    // Try the same inside, edge, and outside checks using a complex number
    // with zero imaginary part. Moving the imaginary range away from zero
    // must reject the real value.
    SCOPED_TRACE("is member: complex point");
    M2::ARingCCC R(100);
    M2::ARingCCC::Element value(R);
    R.set_from_long(value, 2);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    R.set_from_long(value, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    R.set_from_long(value, 4);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    R.set_from_long(value, 2);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
  }
}

TEST_F(ARingCCi, Arithmetic)
{
  // Check calculations on rectangles of possible values. Rectangle answers
  // may grow wider, but must still include the expected values.

  {
    // Now use ranges instead of single values. Undoing an operation may widen
    // the answer, but must not lose the original values.
    SCOPED_TRACE("arithmetic: rectangles; divisor excludes zero");
    setBounds(a, 1, 2, 3, 4);
    setBounds(b, 2, 3, -2, -1);
    C.negate(result, a);
    EXPECT_TRUE(hasBounds(result, -2, -1, -4, -3)) << "interval negation";
    C.add(result, a, b);
    EXPECT_TRUE(hasBounds(result, 3, 5, 1, 3)) << "interval addition";
    C.subtract(result, a, b);
    EXPECT_TRUE(hasBounds(result, -2, 0, 4, 6)) << "interval subtraction";
    C.add(result, result, b);
    EXPECT_TRUE(contains(result, a)) << "subtract then add";
    C.mult(result, a, b);
    EXPECT_TRUE(hasBounds(result, 5, 14, 2, 11)) << "interval multiplication";
    C.divide(result, result, b);
    EXPECT_TRUE(contains(result, a))
        << "multiply then divide; divisor excludes zero";

    // These answers need to include zero or one, not necessarily equal a
    // single point. Reusing an interval can make its range wider.
    Ring::Element zero(C), one(C);
    C.set_zero(zero);
    C.set_from_long(one, 1);
    C.mult(result, a, b);
    C.subtract_multiple(result, a, b);
    EXPECT_TRUE(contains(result, zero)) << "subtract product from itself";
    C.invert(result, b);
    C.mult(result, b, result);
    EXPECT_TRUE(contains(result, one)) << "multiply by reciprocal";

    // Subtracting a rectangle from itself still leaves a range of possible
    // differences. That range includes zero but is not just zero.
    C.subtract(result, a, a);
    EXPECT_TRUE(hasBounds(result, -1, 1, -1, 1)) << "rectangle minus itself";
    EXPECT_FALSE(C.is_zero(result))
        << "containing zero does not mean being the zero point";
  }
}

TEST_F(ARingCCi, Powers)
{
  // Unlike CC and CCC, interval rings do not take reciprocals through power;
  // the shared suite checks the nonnegative powers.

  {
    // Interval powers reject negative exponents through both entry points.
    SCOPED_TRACE("power: negative exponent unsupported");
    C.set_from_doubles(a, 0.0, 1.0);
    mpz_t exponent;
    mpz_init_set_si(exponent, -1);

    EXPECT_THROW(C.power(result, a, -1), int);
    EXPECT_THROW(C.power_mpz(result, a, exponent), int);
    mpz_clear(exponent);
  }
}

TEST_F(ARingCCi, Magnitude)
{
  // Check the center of a rectangle and distances from zero. Verify both
  // the distance and its square, including an answer stored over its input.

  {
    // The center is 2 in the real range and -3 in the imaginary range.
    // Check each part so a misplaced center is easy to locate.
    SCOPED_TRACE("midpoint: rectangle");
    M2::ARingCCC pointRing(100);
    M2::ARingCCC::Element midpoint(pointRing);
    setBounds(a, 1, 3, -4, -2);

    C.midpoint(midpoint, a);

    EXPECT_TRUE(realEquals(&midpoint.value().re, 2)) << "real midpoint";
    EXPECT_TRUE(realEquals(&midpoint.value().im, -3)) << "imaginary midpoint";
  }

  {
    // The distance from zero to 3 + 4i is 5. This checks that both parts
    // contribute to the distance.
    SCOPED_TRACE("abs: three plus four i");
    C.set_from_doubles(a, 3.0, 4.0);

    C.abs(result, a);

    EXPECT_TRUE(hasValue(result, 5, 0));
  }

  {
    // The squared distance for 3 + 4i is 25. This distinguishes it from the
    // ordinary distance of 5.
    SCOPED_TRACE("abs squared: three plus four i");
    C.set_from_doubles(a, 3.0, 4.0);

    C.abs_squared(result, a);

    EXPECT_TRUE(hasValue(result, 25, 0));
  }

  {
    // Store the distance back in the input itself. The calculation must
    // finish using the old value before replacing it.
    SCOPED_TRACE("abs: output aliases input");
    C.set_from_doubles(a, 3.0, 4.0);

    C.abs(a, a);

    EXPECT_TRUE(hasValue(a, 5, 0));
  }

  // Diameter remains untested: its MPFI temporary is uninitialized, and the
  // intended diameter convention needs clarification.
}

TEST_F(ARingCCi, Formatting)
{
  // Compare the printed answers with the expected text. The cases cover
  // signs, omitted ones, and parentheses.

  {
    // Check the usual text for zero, real, imaginary, and mixed values.
    // Each row gives the exact expected output.
    SCOPED_TRACE("format: plain values");
    struct Example
    {
      const char* name;
      double real;
      double imaginary;
      const char* expected;
    };
    const Example cases[] = {
        {"zero", 0, 0, "[0,-0]"},
        {"positive real unit", 1, 0, "[1,1]"},
        {"negative real unit", -1, 0, "[-1,-1]"},
        {"positive imaginary unit", 0, 1, "[1,1]i"},
        {"positive real, positive imaginary", 1, 1, "[1,1]+[1,1]i"},
    };

    for (const auto& sample : cases)
      {
        C.set_from_doubles(a, sample.real, sample.imaginary);
        SCOPED_TRACE(sample.name);
        buffer out;
        C.elem_text_out(out, a, true, false, false);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }

  {
    // When printing a coefficient, a factor of one may be left out. Check
    // that the remaining sign or imaginary part is still printed.
    SCOPED_TRACE("format: unit coefficient");
    struct Example
    {
      const char* name;
      double real;
      double imaginary;
      const char* expected;
    };
    const Example cases[] = {
        {"positive real unit", 1, 0, ""},
        {"negative real unit", -1, 0, "[-1,-1]"},
        {"positive real, positive imaginary", 1, 1, "[1,1]+[1,1]i"},
    };

    for (const auto& sample : cases)
      {
        C.set_from_doubles(a, sample.real, sample.imaginary);
        SCOPED_TRACE(sample.name);
        buffer out;
        C.elem_text_out(out, a, false, false, false);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }

  {
    // Ask for a leading plus sign. The examples show when that sign should
    // appear.
    SCOPED_TRACE("format: plus requested");
    struct Example
    {
      const char* name;
      double real;
      double imaginary;
      const char* expected;
    };
    const Example cases[] = {
        {"positive real unit", 1, 0, "+[1,1]"},
        {"negative real unit", -1, 0, "+[-1,-1]"},
    };

    for (const auto& sample : cases)
      {
        C.set_from_doubles(a, sample.real, sample.imaginary);
        SCOPED_TRACE(sample.name);
        buffer out;
        C.elem_text_out(out, a, true, true, false);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }

  {
    // Ask for parentheses around a value with both parts present. A purely
    // real or imaginary value should not need them.
    SCOPED_TRACE("format: parentheses requested");
    struct Example
    {
      const char* name;
      double real;
      double imaginary;
      const char* expected;
    };
    const Example cases[] = {
        {"positive real, positive imaginary", 1, 1, "([1,1]+[1,1]i)"},
        {"positive real unit", 1, 0, "[1,1]"},
        {"positive imaginary unit", 0, 1, "[1,1]i"},
    };

    for (const auto& sample : cases)
      {
        C.set_from_doubles(a, sample.real, sample.imaginary);
        SCOPED_TRACE(sample.name);
        buffer out;
        C.elem_text_out(out, a, true, false, true);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }
}

TEST_F(ARingCCi, RandomizedProperties)
{
  // Check that random rectangles have valid ranges. The lower end of each
  // range must not exceed its upper end.

  seedRandom(0x4343);
  SCOPED_TRACE("seed 0x4343");

  {
    // Draw fresh rectangles and check that their bounds make sense. The
    // trace prints the exact rectangle if a check fails.
    SCOPED_TRACE("random: generated rectangle");
    for (int trial = 0; trial < 100; ++trial)
      {
        C.random(a);
        SCOPED_TRACE(::testing::Message()
                     << "trial " << trial << ": " << describe(a));

        EXPECT_TRUE(Traits::hasNumericBounds(a));
        EXPECT_FALSE(C.is_empty(a));
        EXPECT_LE(mpfr_cmp(&a.value().re.left, &a.value().re.right), 0)
            << "real interval";
        EXPECT_LE(mpfr_cmp(&a.value().im.left, &a.value().im.right), 0)
            << "imaginary interval";
      }
  }
}

TEST_F(ARingCCi, comparisonHelpersRejectNaN)
{
  // MPFR's NaN comparison result must not make our value checks pass
  // accidentally.
  C.set_from_doubles(a, 1.0, 2.0);
  C.set_from_doubles(b, 1.0, 2.0);
  EXPECT_TRUE(hasValue(a, 1, 2));
  mpfr_set_nan(&a.value().re.left);
  EXPECT_FALSE(hasValue(a, 1, 2));
  EXPECT_FALSE(hasBounds(a, 1, 1, 2, 2));
  EXPECT_FALSE(contains(a, b));
  EXPECT_FALSE(contains(b, a));
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check"
// indent-tabs-mode: nil
// End:
