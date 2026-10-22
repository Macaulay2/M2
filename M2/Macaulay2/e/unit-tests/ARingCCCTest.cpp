// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-CCC.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <string>

#include "unit-tests/ARingComplexTest.hpp"

namespace {

template <>
struct ComplexTraits<M2::ARingCCC>
{
  static bool supportsPrecision(unsigned long) { return true; }
  static const char* precisionLimit() { return "none"; }
  // The fixture ring uses 100 bits.
  static M2::ARingCCC make(unsigned long precision = 100)
  {
    return M2::ARingCCC(precision);
  }
  static const char* ringNamePrefix() { return "ACCC_"; }

  static bool isPoint(const M2::ARingCCC::ElementType& value,
                      double real,
                      double imaginary)
  {
    return mpfr_number_p(&value.re) && mpfr_number_p(&value.im) &&
           mpfr_cmp_d(&value.re, real) == 0 &&
           mpfr_cmp_d(&value.im, imaginary) == 0;
  }

  // Print enough digits to see small differences in a failure message.
  static std::string describe(const M2::ARingCCC::ElementType& value)
  {
    char out[256];
    mpfr_snprintf(out, sizeof(out), "(%.65Rg, %.65Rg)", &value.re, &value.im);
    return out;
  }

  // Allow a few rounding steps when comparing calculated answers. The
  // allowance gets smaller as the ring precision increases.
  static ::testing::AssertionResult near(
      const M2::ARingCCC& C,
      const M2::ARingCCC::ElementType& actual,
      const M2::ARingCCC::ElementType& expected)
  {
    const auto& R = C.real_ring();
    M2::ARingCCC::RealRingType::Element tolerance(R), difference(R);
    mpfr_set_ui_2exp(&tolerance.value(),
                     1,
                     6 - static_cast<int>(C.get_precision()),
                     MPFR_RNDN);
    mpfr_sub(&difference.value(), &actual.re, &expected.re, MPFR_RNDN);
    bool realMatches =
        mpfr_number_p(&difference.value()) &&
        mpfr_cmpabs(&difference.value(), &tolerance.value()) <= 0;
    mpfr_sub(&difference.value(), &actual.im, &expected.im, MPFR_RNDN);
    bool imaginaryMatches =
        mpfr_number_p(&difference.value()) &&
        mpfr_cmpabs(&difference.value(), &tolerance.value()) <= 0;
    if (realMatches && imaginaryMatches) return ::testing::AssertionSuccess();
    char imaginaryError[128];
    mpfr_snprintf(
        imaginaryError, sizeof(imaginaryError), "%.65Rg", &difference.value());
    mpfr_sub(&difference.value(), &actual.re, &expected.re, MPFR_RNDN);
    char realError[128];
    mpfr_snprintf(realError, sizeof(realError), "%.65Rg", &difference.value());
    return ::testing::AssertionFailure()
           << "expected " << describe(expected) << ", got " << describe(actual)
           << "; signed component errors (" << realError << ", "
           << imaginaryError << ")"
           << "; component tolerance is 2^(6 - " << C.get_precision() << ")";
  }
};

}  // namespace

INSTANTIATE_TYPED_TEST_SUITE_P(ARingCCC,
                               ComplexRing,
                               ::testing::Types<M2::ARingCCC>);
INSTANTIATE_TYPED_TEST_SUITE_P(ARingCCC,
                               ComplexPointRing,
                               ::testing::Types<M2::ARingCCC>);

namespace {

// Cases specific to CCC.  Everything shared with CC and CCi is in the typed
// suites above.
class ARingCCC : public ComplexRing<M2::ARingCCC>
{
 protected:
  Ring::Element a {C};
};

TEST_F(ARingCCC, Conversions)
{
  // CCC also converts directly from its real ring and can replace one part
  // of a value while keeping the other.

  {
    // Use the direct conversion from the high-precision real ring. An old
    // imaginary part must not survive.
    SCOPED_TRACE("set from rrr: real value");
    const auto& R = C.real_ring();
    M2::ARingRRR::Element source(R);
    R.set_from_long(source, 2);
    C.set_from_doubles(a, 9.0, 8.0);

    ASSERT_TRUE(C.set_from_RRR(a, source));

    EXPECT_TRUE(hasValue(a, 2, 0));
  }

  {
    // Change only the real part. The imaginary part should stay as it was.
    SCOPED_TRACE("set real part: existing complex value");
    const auto& R = C.real_ring();
    M2::ARingRRR::Element real(R);
    R.set_from_long(real, -3);
    C.set_from_doubles(a, 1.0, 2.0);

    C.set_real_part(a, real);

    EXPECT_TRUE(hasValue(a, -3, 2));
    EXPECT_TRUE(R.is_equal(C.realPartReference(a), real));
  }

  {
    // Change only the imaginary part. The real part should stay as it was.
    SCOPED_TRACE("set imaginary part: existing complex value");
    const auto& R = C.real_ring();
    M2::ARingRRR::Element imaginary(R);
    R.set_from_long(imaginary, -3);
    C.set_from_doubles(a, 1.0, 2.0);

    C.set_imaginary_part(a, imaginary);

    EXPECT_TRUE(hasValue(a, 1, -3));
    EXPECT_TRUE(R.is_equal(C.imaginaryPartReference(a), imaginary));
  }
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check"
// indent-tabs-mode: nil
// End:
