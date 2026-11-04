// Copyright 2013 Michael E. Stillman

#include <gtest/gtest.h>

#include "unit-tests/RingTest.hpp"

#include "basic-rings/aring-glue.hpp"
#include "rings/ZZp.hpp"

template <>
ring_elem getElement<RingQQ>(const RingQQ &R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  mpz_t maxH;
  mpz_init_set_str(maxH, "100000000000", 10);
  gmp_QQ a1 = rawRandomQQ(maxH);
  mpz_clear(maxH);
  ring_elem result;
  EXPECT_TRUE(R.from_rational(a1, result));
  return result;
}
TEST(RingQQ, create)
{
  // Construction exposes the expected coefficient type and ring name.
  const Ring *R = globalQQ;
  ASSERT_NE(R, nullptr);

  EXPECT_TRUE(dynamic_cast<const Z_mod *>(R) == nullptr);
  EXPECT_TRUE(dynamic_cast<const RingQQ *>(R) != nullptr);
  EXPECT_EQ(R->coefficient_type(), Ring::COEFF_QQ);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "QQGMP");
}
TEST(RingQQ, ones)
{
  // Integer coercion agrees with the cached zero and unit constants.
  const Ring *R = globalQQ;
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingQQ, negate)
{  // Generated values cancel their additive inverses.
  testRingNegate(globalQQ, ntrials);
}
TEST(RingQQ, add)
{  // Adding and then subtracting the same value recovers the input.
  testRingAdd(globalQQ, ntrials);
}
TEST(RingQQ, subtract)
{  // Subtraction is undone by adding the subtrahend.
  testRingSubtract(globalQQ, ntrials);
}
TEST(RingQQ, multDivide)
{  // Exact multiplication/division recovers the original value.
  testRingDivide(globalQQ, ntrials);
}
TEST(RingQQ, axioms)
{  // Generated inputs satisfy the commutative ring identities.
  testRingAxioms(globalQQ, ntrials);
}
TEST(RingQQ, power)
{  // Machine and GMP exponents agree with the power addition law.
  testRingPower(globalQQ, ntrials);
}
TEST(RingQQ, syzygy)
{  // Syzygy coefficients cancel their inputs when the second input is nonzero.
  testRingSyzygy(globalQQ, ntrials);
}
TEST(RingQQ, divisionErrors)
{
  // Zero divisors raise the documented exception for both zero and nonzero
  // numerators.
  const auto *R = globalQQ;
  ASSERT_NE(R, nullptr);
  for (long numerator : {0L, 1L, -3L})
    {
      SCOPED_TRACE(::testing::Message() << "numerator " << numerator);
      EXPECT_THROW(R->divide(R->from_long(numerator), R->zero()),
                   exc::division_by_zero_error);
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
