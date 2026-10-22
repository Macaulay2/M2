// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-CC.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <sstream>
#include <string>

#include "unit-tests/ARingComplexTest.hpp"

namespace {

template <>
struct ComplexTraits<M2::ARingCC>
{
  // Elements are pairs of doubles, so the precision is fixed.
  static bool supportsPrecision(unsigned long precision)
  {
    return precision == 53;
  }
  static const char* precisionLimit() { return "pairs of doubles; 53 bits"; }
  static M2::ARingCC make(unsigned long = 53) { return M2::ARingCC(); }
  static const char* ringNamePrefix() { return "ACC_"; }

  static bool isPoint(const M2::ARingCC::ElementType& value,
                      double real,
                      double imaginary)
  {
    return value.re == real && value.im == imaginary;
  }

  // Print enough digits to see small differences in a failure message.
  static std::string describe(const M2::ARingCC::ElementType& value)
  {
    std::ostringstream out;
    out << std::setprecision(17) << "(" << value.re << ", " << value.im << ")";
    return out.str();
  }

  // Allow small rounding differences while keeping about 47 bits of
  // agreement. Near zero, keep a small fixed allowance instead of scaling
  // it down further.
  static ::testing::AssertionResult near(
      const M2::ARingCC&,
      const M2::ARingCC::ElementType& actual,
      const M2::ARingCC::ElementType& expected)
  {
    double scale = std::max({1.0,
                             std::hypot(actual.re, actual.im),
                             std::hypot(expected.re, expected.im)});
    double tolerance = std::ldexp(scale, -47);
    double error = std::hypot(actual.re - expected.re, actual.im - expected.im);
    if (std::isfinite(error) && error <= tolerance)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
           << "expected " << describe(expected) << ", got " << describe(actual)
           << "; error " << error << " exceeds tolerance " << tolerance;
  }
};

}  // namespace

INSTANTIATE_TYPED_TEST_SUITE_P(ARingCC,
                               ComplexRing,
                               ::testing::Types<M2::ARingCC>);
INSTANTIATE_TYPED_TEST_SUITE_P(ARingCC,
                               ComplexPointRing,
                               ::testing::Types<M2::ARingCC>);

namespace {

// Cases specific to CC.  Everything shared with CCC and CCi is in the typed
// suites above.
class ARingCC : public ComplexRing<M2::ARingCC>
{
 protected:
  Ring::Element a {C};
};

TEST_F(ARingCC, Evaluation)
{
  // Unlike CCC, CC's eval reports a failed move only when no error is
  // already pending.

  {
    // Set an error before attempting the move. The failed move must not
    // replace that earlier message.
    SCOPED_TRACE("eval: existing error");
    C.set_from_doubles(a, 1.0, 2.0);
    auto map = coefficientMap(globalZZ);
    ASSERT_NE(map, nullptr);
    ring_elem image;
    ERROR("existing error");

    C.eval(map, a, 0, image);

    EXPECT_TRUE(globalZZ->is_zero(image));
    EXPECT_STREQ(error_message(), "existing error");
  }
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check"
// indent-tabs-mode: nil
// End:
