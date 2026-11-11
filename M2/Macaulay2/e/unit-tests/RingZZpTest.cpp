// Copyright 2013 Michael E. Stillman

#include <gtest/gtest.h>

#include "unit-tests/RingTest.hpp"
#include "rings/ZZp.hpp"

#include <sstream>

template <>
ring_elem getElement<Z_mod>(const Z_mod& R, int index)
{
  ring_elem a = getElement<RingZZ>(*globalZZ, index);
  return R.from_int(a.get_mpz());
}

TEST(RingZZmod32003, fromStream)
{
  // Signed integers are consumed one at a time, leaving the nonnumeric suffix
  // unread.
  Z_mod* R = Z_mod::create(32003);
  ASSERT_NE(R, nullptr);
  std::istringstream input("+1234 +345 -235*a");
  for (long expected : {1234L, 345L, -235L})
    {
      SCOPED_TRACE(::testing::Message() << "expected " << expected);
      ring_elem actual;
      ASSERT_TRUE(fromStream(input, *R, actual));
      EXPECT_TRUE(ringEquals(R, R->from_long(expected), actual));
    }
  EXPECT_EQ(input.peek(), '*');
  ring_elem unused;
  EXPECT_FALSE(fromStream(input, *R, unused));
}

///////////////////////////////////////////////
TEST(RingZZmod101, create)
{
  // Construction exposes the expected coefficient type and ring name.
  Ring* R = Z_mod::create(101);
  ASSERT_NE(R, nullptr);

  EXPECT_TRUE(dynamic_cast<const Z_mod*>(R) != nullptr);
  EXPECT_EQ(R->coefficient_type(), Ring::COEFF_BASIC);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "ZZ/101");
}
TEST(RingZZmod101, ones)
{
  // Integer coercion agrees with the cached zero and unit constants.
  Z_mod* R = Z_mod::create(101);
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingZZmod101, negate)
{
  // Generated values cancel their additive inverses.
  Z_mod* R = Z_mod::create(101);
  testRingNegate(R, ntrials);
}
TEST(RingZZmod101, add)
{
  // Adding and then subtracting the same value recovers the input.
  Z_mod* R = Z_mod::create(101);
  testRingAdd(R, ntrials);
}
TEST(RingZZmod101, subtract)
{
  // Subtraction is undone by adding the subtrahend.
  Z_mod* R = Z_mod::create(101);
  testRingSubtract(R, ntrials);
}
TEST(RingZZmod101, multDivide)
{
  // Exact multiplication/division recovers the original value.
  Z_mod* R = Z_mod::create(101);
  testRingDivide(R, ntrials);
}
TEST(RingZZmod101, axioms)
{
  // Generated inputs satisfy the commutative ring identities.
  Z_mod* R = Z_mod::create(101);
  testRingAxioms(R, ntrials);
}
TEST(RingZZmod101, power)
{
  // Machine and GMP exponents agree with the power addition law.
  Z_mod* R = Z_mod::create(101);
  testRingPower(R, ntrials);
}
TEST(RingZZmod101, syzygy)
{
  // Syzygy coefficients cancel their inputs when the second input is nonzero.
  Z_mod* R = Z_mod::create(101);
  testRingSyzygy(R, ntrials);
}
//////////////////////////////////////////////////////////
TEST(RingZZmod2, create)
{
  // Construction exposes the expected coefficient type and ring name.
  Ring* R = Z_mod::create(2);
  ASSERT_NE(R, nullptr);

  EXPECT_TRUE(dynamic_cast<const Z_mod*>(R) != nullptr);
  EXPECT_EQ(R->coefficient_type(), Ring::COEFF_BASIC);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "ZZ/2");
}
TEST(RingZZmod2, ones)
{
  // Integer coercion agrees with the cached zero and unit constants.
  Z_mod* R = Z_mod::create(2);
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingZZmod2, negate)
{
  // Generated values cancel their additive inverses.
  Z_mod* R = Z_mod::create(2);
  testRingNegate(R, ntrials);
}
TEST(RingZZmod2, add)
{
  // Adding and then subtracting the same value recovers the input.
  Z_mod* R = Z_mod::create(2);
  testRingAdd(R, ntrials);
}
TEST(RingZZmod2, subtract)
{
  // Subtraction is undone by adding the subtrahend.
  Z_mod* R = Z_mod::create(2);
  testRingSubtract(R, ntrials);
}
TEST(RingZZmod2, multDivide)
{
  // Exact multiplication/division recovers the original value.
  Z_mod* R = Z_mod::create(2);
  testRingDivide(R, ntrials);
}
TEST(RingZZmod2, axioms)
{
  // Generated inputs satisfy the commutative ring identities.
  Z_mod* R = Z_mod::create(2);
  testRingAxioms(R, ntrials);
}
TEST(RingZZmod2, power)
{
  // Machine and GMP exponents agree with the power addition law.
  Z_mod* R = Z_mod::create(2);
  testRingPower(R, ntrials);
}
TEST(RingZZmod2, syzygy)
{
  // Syzygy coefficients cancel their inputs when the second input is nonzero.
  Z_mod* R = Z_mod::create(2);
  testRingSyzygy(R, ntrials);
}
TEST(RingZZmod101, divisionErrors)
{
  // Zero divisors raise the documented exception for both zero and nonzero
  // numerators.
  const auto* R = Z_mod::create(101);
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
