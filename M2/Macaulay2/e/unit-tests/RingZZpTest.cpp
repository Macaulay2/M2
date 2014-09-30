// Copyright 2013 Michael E. Stillman

#include "RingTest.hpp"
#include "ZZp.hpp"

template <>
ring_elem getElement<Z_mod>(const Z_mod&  R, int index)
{
  ring_elem a = getElement<RingZZ>(*globalZZ, index);
  return R.from_int(a.get_mpz());
}

TEST(RingZZmod32003, fromStream)
{
  std::istringstream i("+1234 +345 -235*a");
  Z_mod *R = Z_mod::create(32003);
  ring_elem a;
  while (fromStream(i, *R, a))
    {
      buffer o;
      R->elem_text_out(o, a);
      std::cout << o.str() << " peek: " << "." << static_cast<char>(i.peek()) << "." << std::endl;
    }
}

///////////////////////////////////////////////
TEST(RingZZmod101, create)
{
  Ring *R = Z_mod::create(101);
  EXPECT_TRUE(R != 0);

  EXPECT_TRUE(dynamic_cast< const Z_mod * >(R) != 0);
  EXPECT_EQ(R->coefficient_type(), Ring::COEFF_BASIC);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "ZZ/101");
}
TEST(RingZZmod101, ones)
{
  Z_mod* R = Z_mod::create(101);
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingZZmod101, negate)
{
  Z_mod* R = Z_mod::create(101);
  testRingNegate(R, ntrials);
}
TEST(RingZZmod101, add)
{
  Z_mod* R = Z_mod::create(101);
  testRingAdd(R, ntrials);
}
TEST(RingZZmod101, subtract)
{
  Z_mod* R = Z_mod::create(101);
  testRingSubtract(R, ntrials);
}
TEST(RingZZmod101, multDivide)
{
  Z_mod* R = Z_mod::create(101);
  testRingDivide(R, ntrials);
}
TEST(RingZZmod101, axioms)
{
  Z_mod* R = Z_mod::create(101);
  testRingAxioms(R, ntrials);
}
TEST(RingZZmod101, power)
{
  Z_mod* R = Z_mod::create(101);
  testRingPower(R, ntrials);
}
TEST(RingZZmod101, syzygy)
{
  Z_mod* R = Z_mod::create(101);
  testRingSyzygy(R, ntrials);
}
//////////////////////////////////////////////////////////
TEST(RingZZmod2, create)
{
  Ring *R = Z_mod::create(2);
  EXPECT_TRUE(R != 0);

  EXPECT_TRUE(dynamic_cast< const Z_mod * >(R) != 0);
  EXPECT_EQ(R->coefficient_type(), Ring::COEFF_BASIC);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "ZZ/2");
}
TEST(RingZZmod2, ones)
{
  Z_mod* R = Z_mod::create(2);
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingZZmod2, negate)
{
  Z_mod* R = Z_mod::create(2);
  testRingNegate(R, ntrials);
}
TEST(RingZZmod2, add)
{
  Z_mod* R = Z_mod::create(2);
  testRingAdd(R, ntrials);
}
TEST(RingZZmod2, subtract)
{
  Z_mod* R = Z_mod::create(2);
  testRingSubtract(R, ntrials);
}
TEST(RingZZmod2, multDivide)
{
  Z_mod* R = Z_mod::create(2);
  testRingDivide(R, ntrials);
}
TEST(RingZZmod2, axioms)
{
  Z_mod* R = Z_mod::create(2);
  testRingAxioms(R, ntrials);
}
TEST(RingZZmod2, power)
{
  Z_mod* R = Z_mod::create(2);
  testRingPower(R, ntrials);
}
TEST(RingZZmod2, syzygy)
{
  Z_mod* R = Z_mod::create(2);
  testRingSyzygy(R, ntrials);
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
