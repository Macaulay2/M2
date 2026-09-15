#include "basic-rings/aring-CCi.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <initializer_list>
#include <string>

#include "basic-rings/aring-glue.hpp"
#include "matrices/matrix.hpp"
#include "unit-tests/ARingTest.hpp"

namespace {

// Each test gets fresh values, which are cleaned up automatically.
// Trace labels name the case that failed; each case sets its own inputs.
class ARingCCi : public ::testing::Test
{
 protected:
  using Ring = M2::ARingCCi;
  Ring C {100};
  Ring::Element a {C}, b {C}, result {C};

  // Print enough digits to see small differences in a failure message.
  std::string describe(const Ring::ElementType& value) const
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

  // Comparisons alone cannot reject NaN endpoints: MPFR returns zero for them.
  bool hasNumericBounds(const Ring::ElementType& value) const
  {
    return !mpfr_nan_p(&value.re.left) && !mpfr_nan_p(&value.re.right) &&
           !mpfr_nan_p(&value.im.left) && !mpfr_nan_p(&value.im.right);
  }

  // Check for one exact complex value. Both ends of each range must match
  // that value.
  ::testing::AssertionResult hasValue(const Ring::ElementType& value,
                                      double real,
                                      double imaginary) const
  {
    if (hasNumericBounds(value) && mpfr_cmp_d(&value.re.left, real) == 0 &&
        mpfr_cmp_d(&value.re.right, real) == 0 &&
        mpfr_cmp_d(&value.im.left, imaginary) == 0 &&
        mpfr_cmp_d(&value.im.right, imaginary) == 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure()
           << "expected (" << real << ", " << imaginary << "), got "
           << describe(value);
  }

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
    if (hasNumericBounds(value) && mpfr_cmp_si(&value.re.left, reLeft) == 0 &&
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
    if (hasNumericBounds(outer) && hasNumericBounds(inner) &&
        mpfr_cmp(&outer.re.left, &inner.re.left) <= 0 &&
        mpfr_cmp(&outer.re.right, &inner.re.right) >= 0 &&
        mpfr_cmp(&outer.im.left, &inner.im.left) <= 0 &&
        mpfr_cmp(&outer.im.right, &inner.im.right) >= 0)
      return ::testing::AssertionSuccess();
    return ::testing::AssertionFailure() << "expected " << describe(outer)
                                         << " to contain " << describe(inner);
  }

  // Show the actual high-precision value if it differs from the expected
  // integer.
  ::testing::AssertionResult realEquals(mpfr_srcptr actual, long expected) const
  {
    if (mpfr_number_p(actual) && mpfr_cmp_si(actual, expected) == 0)
      return ::testing::AssertionSuccess();
    char value[128];
    mpfr_snprintf(value, sizeof(value), "%.65Rg", actual);
    return ::testing::AssertionFailure()
           << "expected " << expected << ", got " << value;
  }

  // Build a map for constant numbers. There are no variables to assign, so
  // the list of images is empty.
  const RingMap* coefficientMap(const ::Ring* target) const
  {
    auto images =
        Matrix::zero(target->make_FreeModule(1), target->make_FreeModule(0));
    return RingMap::make(images);
  }
};

TEST_F(ARingCCi, Construction)
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
        EXPECT_EQ(ringName(ring), "ACCi_" + std::to_string(precision));
      }
  }

  {
    // Create the ring without settings. Check the defaults reported to
    // callers.
    SCOPED_TRACE("create: default precision");
    Ring defaultRing;

    EXPECT_EQ(defaultRing.get_precision(), 53);
    EXPECT_EQ(defaultRing.characteristic(), 0);
    EXPECT_EQ(ringName(defaultRing), "ACCi_53");
  }
}

TEST_F(ARingCCi, Storage)
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
    // Copy a rectangle, then clear the original. The copy must keep all
    // four original bounds.
    SCOPED_TRACE("set: mutable rectangle");
    setBounds(a, 1, 3, -4, -2);

    ASSERT_TRUE(C.set(result.value(), a.value()));
    C.set_zero(a);

    EXPECT_TRUE(hasBounds(result, 1, 3, -4, -2));
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

TEST_F(ARingCCi, Conversions)
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
    // Keep both ends of the real range when copying it. The imaginary range
    // must become exactly zero.
    SCOPED_TRACE("set: real interval");
    M2::ARingRRi R(100);
    M2::ARingRRi::Element source(R);
    mpfi_interv_si(&source.value(), 1, 3);
    C.set(a, 9.0, 8.0);

    ASSERT_TRUE(C.set(a, &source.value()));

    EXPECT_TRUE(hasBounds(a, 1, 3, 0, 0));
  }

  {
    // Turn one complex number into a rectangle with no width. Each pair of
    // bounds must equal the corresponding part of the number.
    SCOPED_TRACE("set: big complex point");
    M2::ARingCCC sourceRing(100);
    M2::ARingCCC::Element source(sourceRing);
    sourceRing.set(source, 2.0, -3.0);

    ASSERT_TRUE(C.set(result, &source.value()));

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

    ASSERT_TRUE(C.set(result, &source));

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

TEST_F(ARingCCi, Comparisons)
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
    C.set(b, 2.0, 0.0);
    EXPECT_TRUE(C.is_subset(b, a)) << "interior point";

    C.set(b, 3.0, 1.0);
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
    R.set(value, 2);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    R.set(value, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    R.set(value, 4);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    R.set(value, 2);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
  }

  {
    // Try the same inside, edge, and outside checks using a real interval
    // containing one value. Moving the imaginary range away from zero must
    // reject the real value.
    SCOPED_TRACE("is member: real interval");
    M2::ARingRRi R(100);
    M2::ARingRRi::Element value(R);
    R.set(value, 2);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    R.set(value, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    R.set(value, 4);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    R.set(value, 2);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
  }

  {
    // Try the same inside, edge, and outside checks using a complex number
    // with zero imaginary part. Moving the imaginary range away from zero
    // must reject the real value.
    SCOPED_TRACE("is member: complex point");
    M2::ARingCCC R(100);
    M2::ARingCCC::Element value(R);
    R.set(value, 2);
    setBounds(a, 1, 3, -1, 1);

    EXPECT_TRUE(C.is_member(value, a)) << "interior point";
    R.set(value, 1);
    EXPECT_TRUE(C.is_member(value, a)) << "real boundary point";
    R.set(value, 4);
    EXPECT_FALSE(C.is_member(value, a)) << "outside real interval";

    setBounds(a, 1, 3, 2, 3);
    R.set(value, 2);
    EXPECT_FALSE(C.is_member(value, a)) << "imaginary interval excludes zero";
  }
}

TEST_F(ARingCCi, Arithmetic)
{
  // Check calculations on single values and on rectangles of possible
  // values. Rectangle answers may grow wider, but must still include the
  // expected values.

  {
    // Use a = 3 + 2i and b = 1 - 2i. Their answers are exact, so these checks
    // need no rounding allowance.
    SCOPED_TRACE("arithmetic: exact mixed points");
    C.set(a, 3.0, 2.0);
    C.set(b, 1.0, -2.0);

    C.negate(result, a);
    EXPECT_TRUE(hasValue(result, -3, -2)) << "negation";
    C.negate(result, b);
    EXPECT_TRUE(hasValue(result, -1, 2))
        << "negation with negative imaginary part";
    C.add(result, a, b);
    EXPECT_TRUE(hasValue(result, 4, 0)) << "addition";
    C.subtract(result, a, b);
    EXPECT_TRUE(hasValue(result, 2, 4)) << "subtraction";
    C.mult(result, a, b);
    EXPECT_TRUE(hasValue(result, 7, -4)) << "multiplication";
  }

  {
    // Start with 1 + i already saved in result. Adding and then removing a*b
    // should recover that starting value.
    SCOPED_TRACE("accumulate: mixed points");
    C.set(a, 3.0, 2.0);
    C.set(b, 1.0, -2.0);
    C.set(result, 1.0, 1.0);
    C.addMultipleTo(result, a, b);
    EXPECT_TRUE(hasValue(result, 8, -3)) << "add product to accumulator";
    C.set(result, 8.0, -3.0);
    C.subtract_multiple(result, a, b);
    EXPECT_TRUE(hasValue(result, 1, 1)) << "subtract product from accumulator";
  }

  {
    // Reset both operands before storing the answer over an input.
    SCOPED_TRACE("arithmetic: output aliases left input");
    C.set(a, 3.0, 2.0);
    C.set(b, 1.0, -2.0);
    C.set(result, a);

    C.mult(result, result, b);

    EXPECT_TRUE(hasValue(result, 7, -4));
  }

  {
    // Reset both operands before storing the answer over an input.
    SCOPED_TRACE("arithmetic: output aliases right input");
    C.set(a, 3.0, 2.0);
    C.set(b, 1.0, -2.0);
    C.set(result, b);

    C.mult(result, a, result);

    EXPECT_TRUE(hasValue(result, 7, -4));
  }

  {
    // Reset both operands before storing the answer over an input.
    SCOPED_TRACE("arithmetic: output aliases both inputs");
    C.set(a, 3.0, 2.0);
    C.set(b, 1.0, -2.0);
    C.set(result, a);

    C.mult(result, result, result);

    EXPECT_TRUE(hasValue(result, 5, 12));
  }

  {
    // Reset both operands before storing the answer over an input.
    SCOPED_TRACE("arithmetic: output aliases accumulator input");
    C.set(a, 3.0, 2.0);
    C.set(b, 1.0, -2.0);
    C.set(result, a);

    C.addMultipleTo(result, result, b);

    EXPECT_TRUE(hasValue(result, 10, -2));
  }

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
      EXPECT_TRUE(
          hasValue(result, sample.inverseReal, sample.inverseImaginary));

      C.set(a, sample.real, sample.imaginary);
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
      EXPECT_TRUE(
          hasValue(result, sample.quotientReal, sample.quotientImaginary));

      C.set(a, 3.0, 2.0);
      C.divide(a, a, b);
      EXPECT_TRUE(hasValue(a, sample.quotientReal, sample.quotientImaginary))
          << "output aliases numerator";
    }

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
    C.set(one, 1);
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
  // Known powers exercise both exponent interfaces and their supported ranges.
  // Each row gives the base, exponent, and exact real and imaginary answer.
  struct PowerCase
  {
    const char* name;
    double baseReal, baseImaginary;
    int exponent;
    double real, imaginary;
  };
  const PowerCase cases[] = {
      {"i: zero exponent", 0, 1, 0, 1, 0},
      {"i: first power", 0, 1, 1, 0, 1},
      {"i: square", 0, 1, 2, -1, 0},
      {"i: cube", 0, 1, 3, 0, -1},
      {"i: full period", 0, 1, 4, 1, 0},
      {"i: repeated period", 0, 1, 9, 0, 1},
      {"mixed base: cube", 3, 2, 3, -9, 46},
  };
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      C.set(a, sample.baseReal, sample.baseImaginary);
      mpz_t exponent;
      mpz_init_set_si(exponent, sample.exponent);

      C.power(result, a, sample.exponent);
      EXPECT_TRUE(hasValue(result, sample.real, sample.imaginary))
          << "int exponent";
      C.power_mpz(result, a, exponent);
      EXPECT_TRUE(hasValue(result, sample.real, sample.imaginary))
          << "mpz exponent";
      mpz_clear(exponent);
    }

  {
    // Interval powers reject negative exponents through both entry points.
    SCOPED_TRACE("power: negative exponent unsupported");
    C.set(a, 0.0, 1.0);
    mpz_t exponent;
    mpz_init_set_si(exponent, -1);

    EXPECT_THROW(C.power(result, a, -1), int);
    EXPECT_THROW(C.power_mpz(result, a, exponent), int);
    mpz_clear(exponent);
  }
}

TEST_F(ARingCCi, Syzygy)
{
  // Find multipliers x and y that make x*a + y*b equal zero. Check both the
  // chosen multipliers and the final sum to help locate a wrong answer.

  {
    // Choose nonzero inputs with a known cancelling pair. First check x and
    // y, then check that the two products add to zero.
    SCOPED_TRACE("syzygy: both operands nonzero");
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
}

TEST_F(ARingCCi, zeroInputSyzygy)
{
  // The syzygy contract explicitly requires both inputs to be nonzero.
  GTEST_SKIP() << "zero operands are outside syzygy's documented precondition";
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
    C.set(a, 3.0, 4.0);

    C.abs(result, a);

    EXPECT_TRUE(hasValue(result, 5, 0));
  }

  {
    // The squared distance for 3 + 4i is 25. This distinguishes it from the
    // ordinary distance of 5.
    SCOPED_TRACE("abs squared: three plus four i");
    C.set(a, 3.0, 4.0);

    C.abs_squared(result, a);

    EXPECT_TRUE(hasValue(result, 25, 0));
  }

  {
    // Store the distance back in the input itself. The calculation must
    // finish using the old value before replacing it.
    SCOPED_TRACE("abs: output aliases input");
    C.set(a, 3.0, 4.0);

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
        C.set(a, sample.real, sample.imaginary);
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
        C.set(a, sample.real, sample.imaginary);
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
        C.set(a, sample.real, sample.imaginary);
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
        C.set(a, sample.real, sample.imaginary);
        SCOPED_TRACE(sample.name);
        buffer out;
        C.elem_text_out(out, a, true, false, true);
        EXPECT_EQ(std::string(out.str()), sample.expected);
      }
  }
}

TEST_F(ARingCCi, Evaluation)
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
    EXPECT_STRNE(error_message(),
                 "");  // consume the error before the next test
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

        EXPECT_TRUE(hasNumericBounds(a));
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
  C.set(a, 1.0, 2.0);
  C.set(b, 1.0, 2.0);
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
