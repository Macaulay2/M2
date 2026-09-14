// Copyright 2012-2013 Michael E. Stillman

#include <string>
#include <gtest/gtest.h>

#include "basic-rings/aring-CCC.hpp"
#include "basic-rings/aring-glue.hpp"
#include "matrices/matrix.hpp"
#include "unit-tests/ARingTest.hpp"

namespace {

// Each test gets fresh values, which are cleaned up automatically.
// Trace labels name the case that failed; each case sets its own inputs.
class ARingCCC : public ::testing::Test
{
 protected:
  using Ring = M2::ARingCCC;
  Ring C{100};
  Ring::Element a{C}, b{C}, result{C};

  // Print enough digits to see small differences in a failure message.
  std::string describe(const Ring::ElementType& value) const
  {
    char out[256];
    mpfr_snprintf(out, sizeof(out), "(%.65Rg, %.65Rg)", &value.re, &value.im);
    return out;
  }

  // Check both parts against the expected number and show the actual answer
  // on failure.
  ::testing::AssertionResult hasValue(const Ring::ElementType& value,
                                      double real, double imaginary) const
  {
    if (mpfr_cmp_d(&value.re, real) == 0 &&
        mpfr_cmp_d(&value.im, imaginary) == 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
        << "expected (" << real << ", " << imaginary << "), got " << describe(value);
  }

  // Allow a few rounding steps when comparing calculated answers. The
  // allowance gets smaller as the ring precision increases.
  ::testing::AssertionResult near(const Ring::ElementType& actual,
                                 const Ring::ElementType& expected) const
  {
    const auto& R = C.real_ring();
    Ring::RealRingType::Element tolerance(R), difference(R);
    mpfr_set_ui_2exp(&tolerance.value(), 1, 6 - static_cast<int>(C.get_precision()), MPFR_RNDN);
    mpfr_sub(&difference.value(), &actual.re, &expected.re, MPFR_RNDN);
    bool realMatches = mpfr_cmpabs(&difference.value(), &tolerance.value()) <= 0;
    mpfr_sub(&difference.value(), &actual.im, &expected.im, MPFR_RNDN);
    bool imaginaryMatches = mpfr_cmpabs(&difference.value(), &tolerance.value()) <= 0;
    if (realMatches && imaginaryMatches) return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
        << "expected " << describe(expected) << ", got " << describe(actual)
        << "; component tolerance is 2^(6 - " << C.get_precision() << ")";
  }

  // Show the actual high-precision value if it differs from the expected
  // integer.
  ::testing::AssertionResult realEquals(mpfr_srcptr actual, long expected) const
  {
    if (mpfr_cmp_si(actual, expected) == 0) return ::testing::AssertionSuccess();
    char value[128];
    mpfr_snprintf(value, sizeof(value), "%.65Rg", actual);
    return ::testing::AssertionFailure()
        << "expected " << expected << ", got " << value;
  }

  // Build a map for constant numbers. There are no variables to assign, so
  // the list of images is empty.
  const RingMap* coefficientMap(const ::Ring* target) const
  {
    auto images = Matrix::zero(target->make_FreeModule(1), target->make_FreeModule(0));
    return RingMap::make(images);
  }
};

TEST_F(ARingCCC, Construction)
{
  // Check the ring name and the precision it reports. The default settings
  // and requested settings should agree with the values used to create the
  // ring.

  {
    // Try several requested precisions. Both the stored precision and the
    // printed name should reflect the request.
    SCOPED_TRACE("create: requested precision");
    for (unsigned long precision : {53UL, 100UL, 200UL})
      {
        SCOPED_TRACE(::testing::Message() << "precision " << precision);
        Ring ring(precision);

        EXPECT_EQ(ring.get_precision(), precision);
        EXPECT_EQ(ringName(ring), "ACCC_" + std::to_string(precision));
      }
  }

  {
    // Create the ring without settings. Check the defaults reported to
    // callers.
    SCOPED_TRACE("create: default precision");
    Ring defaultRing;

    EXPECT_EQ(defaultRing.get_precision(), 53);
    EXPECT_EQ(defaultRing.characteristic(), 0);
    EXPECT_EQ(ringName(defaultRing), "ACCC_53");
  }
}

TEST_F(ARingCCC, Storage)
{
  // Check that values can be saved, copied, cleared, and exchanged safely.
  // Changing one value must not damage a saved copy.

  {
    // Start with both parts nonzero. Clearing the value must clear both
    // parts.
    SCOPED_TRACE("set zero: nonzero value");
    C.set(a, 2.0, 3.0);

    C.set_zero(a);

    EXPECT_TRUE(hasValue(a, 0, 0));
    EXPECT_TRUE(C.is_zero(a));
  }

  {
    // Make a new value from an existing one, then clear the original. The
    // copy should still hold the old value.
    SCOPED_TRACE("init set: source changes");
    C.set(a, 2.0, 3.0);
    Ring::Element copy(C, a);

    C.set_zero(a);

    EXPECT_TRUE(hasValue(copy, 2, 3));
  }

  {
    // Assign a value whose real and imaginary parts differ. This makes a
    // missing or swapped part easier to spot.
    SCOPED_TRACE("set: complex value");
    C.set(a, 2.0, 3.0);
    const auto& source = a.value();

    C.set(result, source);

    EXPECT_TRUE(hasValue(result, 2, 3));
  }

  {
    // Check the explicit copy operation. It must preserve both parts of the
    // number.
    SCOPED_TRACE("copy: complex value");
    C.set(a, 2.0, 3.0);
    const auto& source = a.value();

    C.copy(result, source);

    EXPECT_TRUE(hasValue(result, 2, 3));
  }

  {
    // Exchange two different numbers. Both parts must move together.
    SCOPED_TRACE("swap: two complex values");
    C.set(a, 2.0, 3.0);
    C.set(b, 4.0, 5.0);

    C.swap(a, b);

    EXPECT_TRUE(hasValue(a, 4, 5));
    EXPECT_TRUE(hasValue(b, 2, 3));
  }

  {
    // Equal numbers must produce the same hash. Otherwise a lookup may miss
    // a value that is already present.
    SCOPED_TRACE("hash: equal values");
    C.set(a, 2.0, 3.0);
    C.set(b, 2.0, 3.0);

    EXPECT_EQ(C.computeHashValue(a), C.computeHashValue(b));
  }

  {
    // This ring has no polynomial variables. Its variable placeholder
    // should be the number one.
    SCOPED_TRACE("set var: coefficient ring");
    C.set_var(result, 0);

    EXPECT_TRUE(hasValue(result, 1, 0));
  }
}

TEST_F(ARingCCC, Conversions)
{
  // Check that moving a number between supported forms preserves its value.
  // Setting a real number must also remove any old imaginary part.

  {
    // Save the value in the engine's general ring_elem form, then clear the
    // original. Reading it back must recover the saved number.
    SCOPED_TRACE("ring element round trip: source changes");
    C.set(a, 2.0, 3.0);
    ring_elem stored;
    C.to_ring_elem(stored, a);
    C.set_zero(a);

    C.from_ring_elem(result, stored);

    EXPECT_TRUE(hasValue(result, 2, 3));
    EXPECT_TRUE(hasValue(C.from_ring_elem_const(stored), 2, 3));
  }

  {
    // Convert a single complex number to the high-precision form. Neither
    // part should change.
    SCOPED_TRACE("to big complex: point value");
    C.set(a, 2.0, -3.0);

    auto converted = C.toBigComplex(a);

    EXPECT_TRUE(realEquals(converted->re, 2)) << "real component";
    EXPECT_TRUE(realEquals(converted->im, -3)) << "imaginary component";
  }

  {
    // Read a complex number stored as two high-precision parts. Check that
    // both parts reach the destination.
    SCOPED_TRACE("set: big complex");
    M2::ARingCCC sourceRing(100);
    M2::ARingCCC::Element source(sourceRing);
    sourceRing.set(source, 2.0, -3.0);
    gmp_CC_struct value;
    value.re = &source.value().re;
    value.im = &source.value().im;

    ASSERT_TRUE(C.set(result, &value));

    EXPECT_TRUE(hasValue(result, 2, -3));
  }

  {
    // Replace a complex value with an integer. The old imaginary part must
    // disappear.
    SCOPED_TRACE("set: integer");
    C.set(a, 9.0, 8.0);
    mpz_t integer;
    mpz_init_set_si(integer, -7);

    C.set(a, integer);
    mpz_clear(integer);

    EXPECT_TRUE(hasValue(a, -7, 0));
  }

  {
    // Use 3/2, which has the exact answer 1.5. This checks fraction
    // conversion without a rounding allowance.
    SCOPED_TRACE("set: rational");
    C.set(a, 9.0, 8.0);
    mpq_t rational;
    mpq_init(rational);
    mpq_set_si(rational, 3, 2);

    bool converted = C.set(a, rational);
    mpq_clear(rational);

    ASSERT_TRUE(converted);
    EXPECT_TRUE(hasValue(a, 1.5, 0));
  }

  {
    // Read a high-precision real number into a complex value. Its imaginary
    // part must become zero.
    SCOPED_TRACE("set: big real");
    M2::ARingRRR R(100);
    M2::ARingRRR::Element real(R);
    R.set(real, 1.5);
    C.set(a, 9.0, 8.0);

    ASSERT_TRUE(C.set(a, &real.value()));

    EXPECT_TRUE(hasValue(a, 1.5, 0));
  }

  {
    // Replace a complex value with an ordinary real number. Check that the
    // old imaginary part is cleared.
    SCOPED_TRACE("set: double");
    C.set(a, 9.0, 8.0);

    ASSERT_TRUE(C.set(a, 1.5));

    EXPECT_TRUE(hasValue(a, 1.5, 0));
  }

  {
    // Supply the real and imaginary parts separately. Different signs help
    // reveal a swapped or missing part.
    SCOPED_TRACE("set: big real pair");
    M2::ARingRRR R(100);
    M2::ARingRRR::Element real(R), imaginary(R);
    R.set(real, 1.5);
    R.set(imaginary, -2);

    C.set(result, &real.value(), &imaginary.value());

    EXPECT_TRUE(hasValue(result, 1.5, -2));
  }

  {
    // Use the direct conversion from the high-precision real ring. An old
    // imaginary part must not survive.
    SCOPED_TRACE("set from rrr: real value");
    const auto& R = C.real_ring();
    M2::ARingRRR::Element source(R);
    R.set(source, 2);
    C.set(a, 9.0, 8.0);

    ASSERT_TRUE(C.set_from_RRR(a, source));

    EXPECT_TRUE(hasValue(a, 2, 0));
  }

  {
    // Change only the real part. The imaginary part should stay as it was.
    SCOPED_TRACE("set real part: existing complex value");
    const auto& R = C.real_ring();
    M2::ARingRRR::Element real(R);
    R.set(real, -3);
    C.set(a, 1.0, 2.0);

    C.set_real_part(a, real);

    EXPECT_TRUE(hasValue(a, -3, 2));
    EXPECT_TRUE(R.is_equal(C.realPartReference(a), real));
  }

  {
    // Change only the imaginary part. The real part should stay as it was.
    SCOPED_TRACE("set imaginary part: existing complex value");
    const auto& R = C.real_ring();
    M2::ARingRRR::Element imaginary(R);
    R.set(imaginary, -3);
    C.set(a, 1.0, 2.0);

    C.set_imaginary_part(a, imaginary);

    EXPECT_TRUE(hasValue(a, 1, -3));
    EXPECT_TRUE(R.is_equal(C.imaginaryPartReference(a), imaginary));
  }
}

TEST_F(ARingCCC, Comparisons)
{
  // Check how the ring recognizes zero, equal values, and their order.
  // Ordering compares the real parts first, then the imaginary parts if
  // needed.

  {
    // A zero real part does not make the whole number zero. The imaginary
    // part must be checked too.
    SCOPED_TRACE("is zero: imaginary value");
    C.set(a, 0.0, 1.0);

    EXPECT_FALSE(C.is_zero(a));
  }

  {
    // For these single values, zero has no reciprocal and a nonzero value
    // does. Check that the ring distinguishes them.
    SCOPED_TRACE("is unit: zero and nonzero points");
    C.set_zero(a);
    C.set(b, 0.0, 1.0);

    EXPECT_FALSE(C.is_unit(a));
    EXPECT_TRUE(C.is_unit(b));
  }

  {
    // Begin with equal values, then change one part at a time. Either
    // change must make the values unequal.
    SCOPED_TRACE("is equal: changed component");
    C.set(a, 2.0, 3.0);
    C.set(b, 2.0, 3.0);
    EXPECT_TRUE(C.is_equal(a, b));

    C.set(b, 4.0, 3.0);
    EXPECT_FALSE(C.is_equal(a, b)) << "different real components";
    C.set(b, 2.0, 4.0);
    EXPECT_FALSE(C.is_equal(a, b)) << "different imaginary components";
  }

  {
    // The real parts should decide the order here. The larger imaginary
    // part must not override that result.
    SCOPED_TRACE("compare: real parts differ");
    C.set(a, 1.0, 9.0);
    C.set(b, 2.0, 0.0);

    EXPECT_EQ(C.compare_elems(a, b), -1);
    EXPECT_EQ(C.compare_elems(b, a), 1);
  }

  {
    // When the real parts match, compare the imaginary parts. Reversing the
    // inputs must reverse the order.
    SCOPED_TRACE("compare: real parts equal");
    C.set(a, 2.0, 1.0);
    C.set(b, 2.0, 3.0);

    EXPECT_EQ(C.compare_elems(a, b), -1);
    EXPECT_EQ(C.compare_elems(b, a), 1);
  }

  {
    // Matching numbers should compare as equal. The result must be zero.
    SCOPED_TRACE("compare: equal values");
    C.set(a, 2.0, 3.0);
    C.set(b, 2.0, 3.0);

    EXPECT_EQ(C.compare_elems(a, b), 0);
  }
}

TEST_F(ARingCCC, Arithmetic)
{
  // Check the main calculations using answers we can write down exactly.
  // Also check that a calculation works when its answer replaces one of its
  // inputs.

  // Use a = 3 + 2i and b = 1 - 2i. Their answers are exact, so these checks
  // need no rounding allowance.
  C.set(a, 3.0, 2.0);
  C.set(b, 1.0, -2.0);

  C.negate(result, a);
  EXPECT_TRUE(hasValue(result, -3, -2)) << "negation";
  C.negate(result, b);
  EXPECT_TRUE(hasValue(result, -1, 2)) << "negation with negative imaginary part";
  C.add(result, a, b);
  EXPECT_TRUE(hasValue(result, 4, 0)) << "addition";
  C.subtract(result, a, b);
  EXPECT_TRUE(hasValue(result, 2, 4)) << "subtraction";
  C.mult(result, a, b);
  EXPECT_TRUE(hasValue(result, 7, -4)) << "multiplication";

  // Start with 1 + i already saved in result. Adding and then removing a*b
  // should recover that starting value.
  C.set(result, 1.0, 1.0);
  C.addMultipleTo(result, a, b);
  EXPECT_TRUE(hasValue(result, 8, -3)) << "add product to accumulator";
  C.subtract_multiple(result, a, b);
  EXPECT_TRUE(hasValue(result, 1, 1)) << "subtract product from accumulator";

  // Repeat multiplication with the answer stored over an input. Reset the
  // starting value before each case.
  C.set(result, a);
  C.mult(result, result, b);
  EXPECT_TRUE(hasValue(result, 7, -4)) << "multiply: output aliases left input";
  C.set(result, b);
  C.mult(result, a, result);
  EXPECT_TRUE(hasValue(result, 7, -4)) << "multiply: output aliases right input";
  C.set(result, a);
  C.mult(result, result, result);
  EXPECT_TRUE(hasValue(result, 5, 12)) << "multiply: output aliases both inputs";
  C.set(result, a);
  C.addMultipleTo(result, result, b);
  EXPECT_TRUE(hasValue(result, 10, -2)) << "accumulate: output aliases input";

  // Each row gives a number and its expected reciprocal, 1 / number. Try
  // both a separate answer and an answer stored back in the input.
  struct ReciprocalCase
  {
    const char* name;
    double real, imaginary, inverseReal, inverseImaginary;
  };
  const ReciprocalCase reciprocals[] = {
      {"real", 2, 0, 0.5, 0},
      {"imaginary", 0, 2, 0, -0.5},
      {"mixed", 1, 1, 0.5, -0.5},
      {"negative real component", -1, 1, -0.5, -0.5},
  };
  for (const auto& sample : reciprocals)
    {
      SCOPED_TRACE(::testing::Message() << "reciprocal: " << sample.name);
      C.set(a, sample.real, sample.imaginary);
      C.invert(result, a);
      EXPECT_TRUE(hasValue(result, sample.inverseReal, sample.inverseImaginary));

      C.invert(a, a);
      EXPECT_TRUE(hasValue(a, sample.inverseReal, sample.inverseImaginary))
          << "output aliases input";
    }

  // Divide the same number, 3 + 2i, by each listed divisor. The last two
  // columns give the expected real and imaginary parts.
  struct DivisionCase
  {
    const char* name;
    double real, imaginary, quotientReal, quotientImaginary;
  };
  const DivisionCase divisors[] = {
      {"real divisor", 2, 0, 1.5, 1},
      {"imaginary divisor", 0, 2, 1, -1.5},
      {"mixed divisor", 1, 1, 2.5, -0.5},
  };
  for (const auto& sample : divisors)
    {
      SCOPED_TRACE(sample.name);
      C.set(a, 3.0, 2.0);
      C.set(b, sample.real, sample.imaginary);
      C.divide(result, a, b);
      EXPECT_TRUE(hasValue(result, sample.quotientReal, sample.quotientImaginary));

      C.divide(a, a, b);
      EXPECT_TRUE(hasValue(a, sample.quotientReal, sample.quotientImaginary))
          << "output aliases numerator";
    }

  // Multiplying or dividing by the real number 2 should affect both parts.
  // Adding a product must keep the value already in result.
  const auto& R = C.real_ring();
  Ring::RealRingType::Element scalar(R);
  R.set(scalar, 2);
  C.set(a, 3.0, 4.0);

  C.mult(result, a, scalar);
  EXPECT_TRUE(hasValue(result, 6, 8)) << "scalar multiplication";
  C.divide(result, a, scalar);
  EXPECT_TRUE(hasValue(result, 1.5, 2)) << "scalar division";
  C.set(result, 1.0, 1.0);
  C.addMultipleTo(result, scalar, a);
  EXPECT_TRUE(hasValue(result, 7, 9)) << "scalar accumulation";
}

TEST_F(ARingCCC, Powers)
{
  // Check powers using known answers through both exponent interfaces.
  // Negative powers should work, while exponents too large for this
  // implementation should report an error.

  // Powers of i repeat every four steps. Run the same answers through
  // ordinary integer and GMP integer exponents.
  C.set(a, 0.0, 1.0);
  struct PowerCase { int exponent; double real, imaginary; };
  const PowerCase cases[] = {{0, 1, 0}, {1, 0, 1}, {2, -1, 0},
                             {3, 0, -1}, {4, 1, 0}, {9, 0, 1}};
  mpz_t exponent;
  mpz_init(exponent);

  for (const auto& sample : cases)
    {
      SCOPED_TRACE(::testing::Message() << "i^" << sample.exponent);
      C.power(result, a, sample.exponent);
      EXPECT_TRUE(hasValue(result, sample.real, sample.imaginary)) << "int exponent";
      mpz_set_si(exponent, sample.exponent);
      C.power_mpz(result, a, exponent);
      EXPECT_TRUE(hasValue(result, sample.real, sample.imaginary)) << "mpz exponent";
    }

  // Also cube a number with two nonzero parts. This checks more than the
  // short cycle for i.
  C.set(a, 3.0, 2.0);
  C.power(result, a, 3);
  EXPECT_TRUE(hasValue(result, -9, 46)) << "(3 + 2i)^3";
  mpz_set_si(exponent, 3);
  C.power_mpz(result, a, exponent);
  EXPECT_TRUE(hasValue(result, -9, 46)) << "(3 + 2i)^3 with mpz exponent";

  C.set(a, 0.0, 1.0);
  // The exponent -1 asks for a reciprocal.
  mpz_set_si(exponent, -1);
  C.power(result, a, -1);
  EXPECT_TRUE(hasValue(result, 0, -1)) << "negative int exponent";
  C.power_mpz(result, a, exponent);
  EXPECT_TRUE(hasValue(result, 0, -1)) << "negative mpz exponent";

  // Use 2^100 as the exponent to exceed the supported integer size. The
  // call should fail instead of using a shortened exponent.
  mpz_set_ui(exponent, 1);
  mpz_mul_2exp(exponent, exponent, 100);
  EXPECT_THROW(C.power_mpz(result, a, exponent), exc::engine_error);
  mpz_clear(exponent);
}

TEST_F(ARingCCC, Syzygy)
{
  // Find multipliers x and y that make x*a + y*b equal zero. Check both the
  // chosen multipliers and the final sum to help locate a wrong answer.

  {
    // Choose nonzero inputs with a known cancelling pair. First check x and
    // y, then check that the two products add to zero.
    SCOPED_TRACE("syzygy: nonzero divisor");
    C.set(a, 3.0, 2.0);
    C.set(b, 0.0, 2.0);
    Ring::Element x(C), y(C);

    C.syzygy(a, b, x, y);

    EXPECT_TRUE(hasValue(x, 1, 0));
    EXPECT_TRUE(hasValue(y, -1, 1.5));
    C.mult(x, x, a);
    C.mult(y, y, b);
    C.add(result, x, y);
    EXPECT_TRUE(hasValue(result, 0, 0));
  }

  {
    // When both inputs are zero, use the simple pair x = 1 and y = 0. This
    // also checks the path that does not divide by b.
    SCOPED_TRACE("syzygy: zero inputs");
    C.set_zero(a);
    C.set_zero(b);
    Ring::Element x(C), y(C);
    C.set_zero(y);

    C.syzygy(a, b, x, y);

    EXPECT_TRUE(hasValue(x, 1, 0));
    EXPECT_TRUE(hasValue(y, 0, 0));
  }
}

TEST_F(ARingCCC, Magnitude)
{
  // Check distances from zero and the handling of very small parts. Also
  // check that updating a saved maximum never makes it smaller.

  const auto& R = C.real_ring();
  Ring::RealRingType::Element magnitude(R);
  C.set(a, 3.0, 4.0);
  C.abs(magnitude, a);
  EXPECT_TRUE(realEquals(&magnitude.value(), 5));
  C.abs_squared(magnitude, a);
  EXPECT_TRUE(realEquals(&magnitude.value(), 25));

  // Only parts smaller than 0.5 should be cleared. A part equal to 0.5 must
  // stay.
  M2::ARingRRR bigRealRing(100);
  M2::ARingRRR::Element epsilon(bigRealRing), norm(bigRealRing);
  bigRealRing.set(epsilon, 0.5);
  C.set(a, 0.25, 0.5);
  C.zeroize_tiny(&epsilon.value(), a);
  EXPECT_TRUE(hasValue(a, 0, 0.5)) << "zeroize small real component";
  C.set(a, 0.5, -0.25);
  C.zeroize_tiny(&epsilon.value(), a);
  EXPECT_TRUE(hasValue(a, 0.5, 0)) << "zeroize small imaginary component";

  // Keep the largest distance seen so far. A smaller new distance must not
  // reduce the saved maximum.
  C.set(a, 3.0, 4.0);
  bigRealRing.set(norm, 0);
  C.increase_norm(&norm.value(), a);
  EXPECT_TRUE(realEquals(&norm.value(), 5)) << "larger magnitude";
  bigRealRing.set(norm, 10);
  C.increase_norm(&norm.value(), a);
  EXPECT_TRUE(realEquals(&norm.value(), 10)) << "smaller magnitude";
}

TEST_F(ARingCCC, Formatting)
{
  // Compare the printed answers with the expected text. The cases cover
  // signs, omitted ones, and parentheses.

  {
    // Check the usual text for zero, real, imaginary, and mixed values.
    // Each row gives the exact expected output.
    SCOPED_TRACE("format: plain values");
    struct Example { double real; double imaginary; const char* expected; };
    const Example cases[] = {
        {0, 0, "0"},
        {1, 0, "1"},
        {-1, 0, "-1"},
        {0, 1, "i"},
        {0, -1, "-i"},
        {1, 1, "1+i"},
        {-1, -1, "-1-i"},
    };

    for (const auto& sample : cases)
      {
        C.set(a, sample.real, sample.imaginary);
        SCOPED_TRACE(describe(a));
        buffer out;
        C.elem_text_out(out, a, true, false, false);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }

  {
    // When printing a coefficient, a factor of one may be left out. Check
    // that the remaining sign or imaginary part is still printed.
    SCOPED_TRACE("format: unit coefficient");
    struct Example { double real; double imaginary; const char* expected; };
    const Example cases[] = {
        {1, 0, ""},
        {-1, 0, "-"},
        {0, 1, "i"},
        {1, 1, "1+i"},
        {2, 0, "2"},
    };

    for (const auto& sample : cases)
      {
        C.set(a, sample.real, sample.imaginary);
        SCOPED_TRACE(describe(a));
        buffer out;
        C.elem_text_out(out, a, false, false, false);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }

  {
    // Ask for a leading plus sign. The examples show when that sign should
    // appear.
    SCOPED_TRACE("format: plus requested");
    struct Example { double real; double imaginary; const char* expected; };
    const Example cases[] = {
        {0, 0, "0"},
        {1, 0, "+1"},
        {-1, 0, "-1"},
        {0, 1, "+i"},
        {0, -1, "-i"},
    };

    for (const auto& sample : cases)
      {
        C.set(a, sample.real, sample.imaginary);
        SCOPED_TRACE(describe(a));
        buffer out;
        C.elem_text_out(out, a, true, true, false);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }

  {
    // Ask for parentheses around a value with both parts present. A purely
    // real or imaginary value should not need them.
    SCOPED_TRACE("format: parentheses requested");
    struct Example { double real; double imaginary; const char* expected; };
    const Example cases[] = {
        {1, 1, "(1+i)"},
        {-1, 1, "-(1-i)"},
        {-1, -1, "-(1+i)"},
        {0, 1, "i"},
        {1, 0, "1"},
    };

    for (const auto& sample : cases)
      {
        C.set(a, sample.real, sample.imaginary);
        SCOPED_TRACE(describe(a));
        buffer out;
        C.elem_text_out(out, a, true, false, true);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }
}

TEST_F(ARingCCC, Evaluation)
{
  // Check that a complex value can move to a compatible ring. A move to the
  // integer ring should fail with a useful error message.

  {
    // Move the number to a ring that accepts complex values. Reading the
    // result back should give the same number.
    SCOPED_TRACE("eval: compatible target");
    C.set(a, 2.0, -3.0);
    auto target = M2::ConcreteRing<Ring>::create();
    auto map = coefficientMap(target);
    ASSERT_NE(map, nullptr);
    ring_elem image;

    C.eval(map, a, 0, image);

    ASSERT_FALSE(error()) << error_message();
    C.from_ring_elem(result, image);
    EXPECT_TRUE(hasValue(result, 2, -3));
  }

  {
    // Try to move a number with an imaginary part into the integers. Check
    // the failure result, then read and clear the error message.
    SCOPED_TRACE("eval: integer target");
    C.set(a, 2.0, -3.0);
    auto map = coefficientMap(globalZZ);
    ASSERT_NE(map, nullptr);
    ring_elem image;

    C.eval(map, a, 0, image);

    EXPECT_TRUE(globalZZ->is_zero(image));
    EXPECT_TRUE(error());
    EXPECT_STRNE(error_message(), "");  // consume the error before the next test
  }
}

TEST_F(ARingCCC, RandomizedProperties)
{
  // Try the same arithmetic rules with many randomly chosen numbers. Each
  // failure prints the inputs so the calculation can be repeated.

  Ring::Element c(C), other(C), term(C);
  for (int trial = 0; trial < ntrials; ++trial)
    {
      C.random(a);
      C.random(b);
      C.random(c);
      SCOPED_TRACE(::testing::Message() << "trial " << trial
          << ": a=" << describe(a) << ", b=" << describe(b)
          << ", c=" << describe(c));

      // Switch the input order. Addition and multiplication should keep the
      // same answer.
      C.add(result, a, b);
      C.add(other, b, a);
      EXPECT_TRUE(near(result, other)) << "addition is commutative";
      C.mult(result, a, b);
      C.mult(other, b, a);
      EXPECT_TRUE(near(result, other)) << "multiplication is commutative";

      // Change which pair is calculated first. The final answer should
      // agree within the rounding allowance.
      C.add(result, b, c);
      C.add(result, a, result);
      C.add(other, a, b);
      C.add(other, other, c);
      EXPECT_TRUE(near(result, other)) << "addition is associative";
      C.mult(result, b, c);
      C.mult(result, a, result);
      C.mult(other, a, b);
      C.mult(other, other, c);
      EXPECT_TRUE(near(result, other)) << "multiplication is associative";

      // Compare a*(b+c) with a*b + a*c. Keep the original inputs unchanged
      // for the next checks.
      C.add(result, b, c);
      C.mult(result, a, result);
      C.mult(other, a, b);
      C.mult(term, a, c);
      C.add(other, other, term);
      EXPECT_TRUE(near(result, other)) << "multiplication distributes over addition";

      // Undo an addition, then undo a multiplication. Skip division when
      // the divisor is zero.
      C.add(result, a, b);
      C.subtract(result, result, b);
      EXPECT_TRUE(near(result, a)) << "add then subtract recovers the input";
      if (!C.is_zero(b))
        {
          C.mult(result, a, b);
          C.divide(result, result, b);
          EXPECT_TRUE(near(result, a)) << "multiply then divide recovers the input";
        }
    }
}

TEST_F(ARingCCC, ApproximationTolerance)
{
  // Check that our comparison helper accepts equal numbers but rejects a
  // clear difference. A helper that always accepts would hide broken
  // arithmetic.

  {
    // Compare a value with itself and with a slightly different value. Only
    // the identical pair should pass this check.
    SCOPED_TRACE("near: distinct values");
    C.set(a, 1.0, 2.0);
    C.set(b, 1.0, 2.01);

    EXPECT_TRUE(near(a, a));
    EXPECT_FALSE(near(a, b));
  }
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check"
// indent-tabs-mode: nil
// End:
