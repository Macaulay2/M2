// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-QQ-gmp.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <string>

#include <cfloat>
#include <cmath>
#include <iomanip>

#include "unit-tests/ARingTest.hpp"

typedef M2::ARingQQGMP RingType;

template <>
void getElement<RingType>(const RingType& R,
                          int index,
                          RingType::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    {
      R.random(result);
    }
}

#include "unit-tests/ARingQQTest.hpp"

INSTANTIATE_TYPED_TEST_SUITE_P(ARingQQGMP, ARingQQ, ::testing::Types<RingType>);

namespace {

// Cases specific to the GMP implementation.  Everything shared with
// ARingQQFlint is in the typed suite above.
class ARingQQGMP : public ::testing::Test
{
 protected:
  RingType R;
};

TEST_F(ARingQQGMP, Construction)
{
  // Backend identity must agree with the printed name.
  // static_cast avoids odr-using ringID, which has no out-of-line definition
  EXPECT_EQ(static_cast<int>(RingType::ringID), static_cast<int>(M2::ring_QQ));
  EXPECT_EQ(ringName(R), "QQGMP");
}

TEST_F(ARingQQGMP, Formatting)
{
  // A worked fraction checks numerator/denominator order without generator
  // assumptions.

  RingType::Element a(R), b(R);
  buffer o;

  R.set(a, 24);
  R.set(b, 23);
  R.divide(a, a, b);
  R.elem_text_out(o, a, true, false, false);
  EXPECT_EQ(std::string(o.str()), "24/23");
}

TEST_F(ARingQQGMP, Conversions)
{
  // lift_to_mpz, set(double) and set(gmp_RR) are GMP-only.
  {
    // A fractional denominator distinguishes values that cannot lift to ZZ.
    SCOPED_TRACE("lift_to_mpz: succeeds exactly on integers");

    RingType::Element a(R);
    mpz_t result;
    mpz_init(result);

    // integers lift
    R.set(a, -12);
    EXPECT_TRUE(R.lift_to_mpz(result, a));
    EXPECT_EQ(mpz_cmp_si(result, -12), 0);

    R.set_zero(a);
    EXPECT_TRUE(R.lift_to_mpz(result, a));
    EXPECT_EQ(mpz_sgn(result), 0);

    // genuine fractions do not
    mpq_t q;
    mpq_init(q);
    mpq_set_si(q, 3, 4);
    mpq_canonicalize(q);
    EXPECT_TRUE(R.set(a, q));
    EXPECT_FALSE(R.lift_to_mpz(result, a));
    mpq_clear(q);

    mpz_clear(result);
  }

  {
    // Known fractions exercise continued fractions and exact binary fallbacks.
    SCOPED_TRACE("set: from double");

    RingType::Element a(R), expected(R);

    struct
    {
      const char* name;
      double value;
      int num;
      int den;
    } cases[] = {
        {"zero", 0.0, 0, 1},
        {"negative zero", -0.0, 0, 1},
        {"one", 1.0, 1, 1},
        {"minus one", -1.0, -1, 1},
        {"one half", 0.5, 1, 2},
        {"minus one half", -0.5, -1, 2},
        {"three quarters", 0.75, 3, 4},
        {"large integer", 1048576.0, 1048576, 1},
        // 1/3 is not a double, but it is the simplest rational rounding
        // to the nearest double to 1/3
        {"one third", 1.0 / 3.0, 1, 3},
        {"minus one third", -1.0 / 3.0, -1, 3},
        {"two sevenths", 2.0 / 7.0, 2, 7},
        {"twenty two sevenths", 22.0 / 7.0, 22, 7},
        // a fraction whose denominator is already a power of two
        {"three over 2^21", 3.0 / 2097152.0, 3, 2097152},
    };

    for (auto& c : cases)
      {
        SCOPED_TRACE(c.name);
        EXPECT_TRUE(R.set(a, c.value));
        mpq_t q;
        mpq_init(q);
        mpq_set_si(q, c.num, c.den);
        mpq_canonicalize(q);
        EXPECT_TRUE(R.set(expected, q));
        EXPECT_TRUE(R.is_equal(a, expected));
        mpq_clear(q);
      }

    // DBL_MIN has no simpler approximation than its exact binary value.
    EXPECT_TRUE(R.set(a, DBL_MIN));
    mpq_t exact;
    mpq_init(exact);
    mpq_set_d(exact, DBL_MIN);
    EXPECT_TRUE(R.set(expected, exact));
    EXPECT_TRUE(R.is_equal(a, expected));
    EXPECT_EQ(mpq_get_d(&a.value()), DBL_MIN);
    mpq_clear(exact);

    // non-numbers are rejected
    EXPECT_FALSE(R.set(a, std::nan("")));
    EXPECT_FALSE(R.set(a, HUGE_VAL));
    EXPECT_FALSE(R.set(a, -HUGE_VAL));
  }

  {
    // Higher precision must preserve the rounding interval of each source.
    SCOPED_TRACE("set: from gmp_RR");

    RingType::Element a(R), expected(R);
    mpfr_t x;
    mpfr_init2(x, 100);

    mpfr_set_zero(x, 1);
    EXPECT_TRUE(R.set(a, x));
    EXPECT_TRUE(R.is_zero(a));

    mpfr_set_si(x, -7, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, x));
    R.set(expected, -7);
    EXPECT_TRUE(R.is_equal(a, expected));

    // 1/3 at 100 bits rounds back from 1/3
    mpfr_set_si(x, 1, MPFR_RNDN);
    mpfr_div_si(x, x, 3, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, x));
    mpq_t q;
    mpq_init(q);
    mpq_set_si(q, 1, 3);
    EXPECT_TRUE(R.set(expected, q));
    EXPECT_TRUE(R.is_equal(a, expected));

    mpfr_set_si(x, -22, MPFR_RNDN);
    mpfr_div_si(x, x, 7, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, x));
    mpq_set_si(q, -22, 7);
    mpq_canonicalize(q);
    EXPECT_TRUE(R.set(expected, q));
    EXPECT_TRUE(R.is_equal(a, expected));

    // A fractional part above 1/2 rounds up, so the remainder goes
    // negative and the convergents come out with a negative denominator,
    // which set() has to normalize.
    mpfr_set_si(x, 9, MPFR_RNDN);
    mpfr_div_si(x, x, 10, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, x));
    mpq_set_si(q, 9, 10);
    mpq_canonicalize(q);
    EXPECT_TRUE(R.set(expected, q));
    EXPECT_TRUE(R.is_equal(a, expected));

    mpfr_set_si(x, -8, MPFR_RNDN);
    mpfr_div_si(x, x, 5, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, x));
    mpq_set_si(q, -8, 5);
    mpq_canonicalize(q);
    EXPECT_TRUE(R.set(expected, q));
    EXPECT_TRUE(R.is_equal(a, expected));

    // an irrational: the result must round back to x
    mpfr_const_pi(x, MPFR_RNDN);
    EXPECT_TRUE(R.set(a, x));
    mpfr_t y;
    mpfr_init2(y, 100);
    mpfr_set_q(y, &a.value(), MPFR_RNDN);
    EXPECT_EQ(mpfr_cmp(x, y), 0);
    mpfr_clear(y);

    // non-numbers are rejected
    mpfr_set_nan(x);
    EXPECT_FALSE(R.set(a, x));
    mpfr_set_inf(x, 1);
    EXPECT_FALSE(R.set(a, x));
    mpfr_set_inf(x, -1);
    EXPECT_FALSE(R.set(a, x));

    mpq_clear(q);
    mpfr_clear(x);
  }
}

TEST_F(ARingQQGMP, doubleRoundTrip)
{
  // Irrational samples must round back to their input double using nearest
  // rounding. mpq_get_d truncates, so it cannot be the oracle for this
  // contract.
  for (int i = 1; i < 200; ++i)
    {
      const double input = std::sqrt(static_cast<double>(i));
      SCOPED_TRACE(::testing::Message() << "set(double): sqrt(" << i << ")="
                                        << std::setprecision(17) << input);
      RingType::Element value(R);
      ASSERT_TRUE(R.set(value, input));
      mpfr_t rounded;
      mpfr_init2(rounded, 53);

      mpfr_set_q(rounded, &value.value(), MPFR_RNDN);

      EXPECT_EQ(mpfr_get_d(rounded, MPFR_RNDN), input);
      mpfr_clear(rounded);
    }
}

TEST_F(ARingQQGMP, fromRingElemConst)
{
  // The const accessor must preserve the value returned by the copying
  // accessor.
  seedRandom(0x5151);
  SCOPED_TRACE("seed 0x5151");
  testFromRingElemConst(R, ntrials);
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
