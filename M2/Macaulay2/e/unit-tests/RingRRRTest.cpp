// Copyright 2013 Michael E. Stillman

#include <gtest/gtest.h>

#include "unit-tests/RingTest.hpp"

#include <memory>
#include <string>

#include "basic-rings/aring-RRR.hpp"
#include "basic-rings/aring-glue.hpp"

typedef M2::ConcreteRing<M2::ARingRRR> RingRRR;

namespace {

// These 100-bit tests retain their operation-specific bit allowances. Report
// the absolute error and reject nonfinite values before comparing magnitudes.
::testing::AssertionResult almostEqual(const RingRRR *R,
                                       int nbits,
                                       ring_elem expected,
                                       ring_elem actual)
{
  mpfr_t epsilon;
  mpfr_init2(epsilon, 100);
  mpfr_set_ui_2exp(epsilon, 1, -nbits, MPFR_RNDN);
  const ring_elem difference = R->subtract(expected, actual);
  const auto error = difference.get_mpfr();
  const bool equal = mpfr_number_p(error) && mpfr_cmpabs(error, epsilon) < 0;
  char details[256];
  mpfr_snprintf(details,
                sizeof(details),
                "error=%.35Rg, absolute tolerance=%.35Rg",
                error,
                epsilon);
  mpfr_clear(epsilon);
  if (equal) return ::testing::AssertionSuccess();
  return ::testing::AssertionFailure()
         << "expected " << RingElem(R, expected) << ", actual "
         << RingElem(R, actual) << ", " << details;
}

}  // namespace

template <>
ring_elem getElement<RingRRR>(const RingRRR &R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  return R.random();
}

TEST(RingRRR, create)
{
  // The ring adapter retains its precision and identifies its coefficient
  // domain.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));
  ASSERT_NE(R, nullptr);

  EXPECT_TRUE(dynamic_cast<const Z_mod *>(R) == nullptr);
  EXPECT_TRUE(dynamic_cast<const RingRRR *>(R) != nullptr);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "ARRR_100");
  EXPECT_EQ(R->get_precision(), 100);
}
TEST(RingRRR, ones)
{
  // Integer coercions agree exactly with the stored zero and units.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingRRR, negate)
{
  // Generated values cancel their additive inverse.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));
  testRingNegate(R, ntrials);
}
TEST(RingRRR, add)
{
  // Addition followed by cancellation recovers the input within two rounding
  // bits.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<RingRRR> gen(*R);

  for (int i = 0; i < ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      ring_elem c = R->add(a, b);
      ring_elem d = R->negate(b);
      ring_elem e = R->add(c, d);  // should be a
      EXPECT_TRUE(almostEqual(R, 98, a, e));
    }
}
TEST(RingRRR, subtract)
{
  // Subtraction followed by addition recovers the input within two rounding
  // bits.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<RingRRR> gen(*R);
  for (int trial = 0; trial < ntrials; ++trial)
    {
      const auto a = gen.nextElement();
      const auto b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      EXPECT_TRUE(almostEqual(R, 98, a, R->add(R->subtract(a, b), b)));
    }
}
TEST(RingRRR, multDivide)
{
  // A product divided by a nonzero factor recovers the other input within six
  // rounding bits.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a*b) // b == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      ring_elem c = R->mult(a, b);
      if (R->is_zero(b))
        EXPECT_TRUE(R->is_zero(c));
      else
        {
          ring_elem d = R->divide(c, b);
          EXPECT_TRUE(almostEqual(R, 94, a, d));
        }
    }
}
TEST(RingRRR, axioms)
{
  // Generated arithmetic satisfies ring identities within the stated rounding
  // allowances.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b) << ", c=" << RingElem(R, c));

      // Swapping operands preserves sums and products.
      {
        SCOPED_TRACE("commutativity");
        ring_elem d = R->add(a, b);
        ring_elem e = R->add(b, a);
        EXPECT_TRUE(R->is_equal(d, e));
        d = R->mult(a, b);
        e = R->mult(b, a);
        EXPECT_TRUE(almostEqual(R, 98, d, e));
      }
      // Regrouping three operations permits accumulated rounding.
      {
        SCOPED_TRACE("associativity");
        ring_elem d = R->add(a, R->add(b, c));
        ring_elem e = R->add(R->add(a, b), c);
        EXPECT_TRUE(almostEqual(R, 94, d, e));
        d = R->mult(a, R->mult(b, c));
        e = R->mult(R->mult(a, b), c);
        EXPECT_TRUE(almostEqual(R, 94, d, e));
      }
      // Expanding a product introduces two products and one addition.
      {
        SCOPED_TRACE("distributivity");
        ring_elem d = R->mult(a, R->add(b, c));
        ring_elem e = R->add(R->mult(a, b), R->mult(a, c));
        EXPECT_TRUE(almostEqual(R, 92, d, e));
      }
    }
}
TEST(RingRRR, power)
{
  // Power interfaces agree; splitting a power into two factors allows
  // accumulated rounding.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));

  mpz_t gmp1;
  mpz_init(gmp1);
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a));
      EXPECT_TRUE(ringEquals(R, R->one(), R->power(a, 0)));
      EXPECT_TRUE(R->is_equal(R->power(a, 1), a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      SCOPED_TRACE(::testing::Message() << "exponents " << e1 << ", " << e2);
      ring_elem b = R->power(a, e1);
      ring_elem c = R->power(a, e2);
      ring_elem d = R->power(a, e1 + e2);
      EXPECT_TRUE(almostEqual(R, 96, R->mult(b, c), d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      ring_elem b1 = R->power(a, gmp1);
      EXPECT_TRUE(R->is_equal(b1, b));
    }
  mpz_clear(gmp1);
}
TEST(RingRRR, syzygy)
{
  // With a nonzero second operand, the returned coefficients cancel the two
  // inputs.
  RingRRR *R = RingRRR::create(std::make_unique<M2::ARingRRR>(100));

  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      if (R->is_zero(b)) continue;

      // A zero first operand yields the trivial unit relation.
      {
        SCOPED_TRACE("syzygy: zero first operand, nonzero second operand");
        ring_elem u, v;
        R->syzygy(R->zero(), b, u, v);
        EXPECT_TRUE(R->is_equal(u, R->one()));
        EXPECT_TRUE(R->is_equal(v, R->zero()));
      }
      // A unit second operand fixes the first coefficient to one.
      {
        SCOPED_TRACE("syzygy: second operand one");
        ring_elem u, v;
        R->syzygy(a, R->one(), u, v);
        EXPECT_TRUE(R->is_equal(u, R->one()));
        EXPECT_TRUE(almostEqual(R, 98, R->negate(a), v));
      }
      // A negative unit changes the sign of the second coefficient.
      {
        SCOPED_TRACE("syzygy: second operand minus one");
        ring_elem u, v;
        R->syzygy(a, R->minus_one(), u, v);
        EXPECT_TRUE(R->is_equal(u, R->one()));
        EXPECT_TRUE(almostEqual(R, 98, a, v));
      }
      // General nonzero divisors must cancel both input products.
      {
        SCOPED_TRACE("syzygy: nonzero second operand");
        ring_elem u, v;
        R->syzygy(a, b, u, v);
        ring_elem result = R->add(R->mult(a, u), R->mult(b, v));
        EXPECT_TRUE(almostEqual(R, 94, R->zero(), result));
      }
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
