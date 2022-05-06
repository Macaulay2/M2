// Copyright 2013 Michael E. Stillman
#include "RingTest.hpp"
#include <limits>

static bool maxH_initialized = false;
static mpz_t maxH;

template <>
ring_elem getElement<RingZZ>(const RingZZ& R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  if (!maxH_initialized)
    {
      maxH_initialized = true;
      mpz_init(maxH);
      mpz_set_str(maxH, "100000000000", 10);
    }
  gmp_ZZ a1 = rawRandomInteger(maxH);
  return R.from_int(a1);
}

/////////////////////////////////////////////////
TEST(RingZZ, create)
{
  EXPECT_TRUE(globalZZ != 0);

  EXPECT_TRUE(dynamic_cast<const RingZZ*>(globalZZ) != 0);
  EXPECT_EQ(globalZZ->coefficient_type(), Ring::COEFF_ZZ);
  EXPECT_TRUE(globalZZ->is_ZZ());
  EXPECT_EQ(ringName(*globalZZ), "ZZ");
}
TEST(RingZZ, ones)
{
  EXPECT_TRUE(globalZZ->is_equal(globalZZ->one(), globalZZ->from_long(1)));
  EXPECT_TRUE(
      globalZZ->is_equal(globalZZ->minus_one(), globalZZ->from_long(-1)));
  EXPECT_TRUE(globalZZ->is_equal(globalZZ->zero(), globalZZ->from_long(0)));
  EXPECT_TRUE(globalZZ->is_zero(globalZZ->from_long(0)));
}
TEST(RingZZ, random)
{
  mpz_t b;
  mpz_init(b);
  mpz_t maxH;
  mpz_init(maxH);
  mpz_set_str(maxH, "100000000000", 10);
  for (int i = 0; i <= 10; i++)
    {
      buffer o;
      ring_elem a =
          globalZZ->random();  // POOR DESIGN!  Need to be able to choose size
      gmp_ZZ a1 = rawRandomInteger(maxH);  // This one is fine
      a = globalZZ->from_int(a1);
      globalZZ->elem_text_out(o, a);
      // std::cout << o.str() << std::endl;
      mpz_set_str(b, o.str(), 10);
      ring_elem c = globalZZ->from_int(b);
      EXPECT_TRUE(globalZZ->is_equal(a, c));
    }
  mpz_clear(maxH);
  mpz_clear(b);
}
TEST(RingZZ, get_si)
{
  mpz_t a;
  mpz_init(a);
  long minint = std::numeric_limits<int>::min();
  long maxint = std::numeric_limits<int>::max();
  long i = minint - 5;
  while (i < maxint + 5)
    {
      mpz_set_si(a, i);
      auto b = RingZZ::get_si(a);
      //      std::cout << "(min,max)=" << minint << "," << maxint
      //                << " " << "i=" << i
      //                << " ok=" << b.first << " result=" << b.second <<
      //                std::endl;
      if (i >= minint and i <= maxint)
        {
          EXPECT_TRUE(b.first);
          EXPECT_EQ(b.second, i);
        }
      else
        {
          EXPECT_FALSE(b.first);
        }
      if (i == minint + 5)
        i = maxint - 5;
      else
        ++i;
    }
  mpz_clear(a);
}
TEST(RingZZ, negate) { testRingNegate(globalZZ, ntrials); }
TEST(RingZZ, add) { testRingAdd(globalZZ, ntrials); }
TEST(RingZZ, subtract) { testRingSubtract(globalZZ, ntrials); }
TEST(RingZZ, multDivide)
{
  testRingDivide(globalZZ, ntrials);

  // I would prefer for 'divide' to be exact division, with a return value of
  // false, if not exactly divisible
  ring_elem a = globalZZ->from_long(5);
  ring_elem b = globalZZ->from_long(2);
  EXPECT_THROW(globalZZ->divide(a, b), exc::engine_error);
  // ring_elem c = globalZZ->divide(a, b);
  // EXPECT_ANY_THROW(globalZZ->is_equal(c, globalZZ->from_long(2)));
}
TEST(RingZZ, axioms) { testRingAxioms(globalZZ, ntrials); }
TEST(RingZZ, power) { testRingPower(globalZZ, ntrials); }
TEST(RingZZ, gcd)
{
  testRingGCD(globalZZ, ntrials);
  const RingZZ* R = globalZZ;

  RingElementGenerator<RingZZ> gen(*globalZZ);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();

      // (a // gcd(a,b) == 0, b // gcd(a,b) == 0,
      ring_elem c = R->gcd(a, b);
      ring_elem u, v;
      ring_elem d = R->gcd_extended(a, b, u, v);

      EXPECT_TRUE(globalZZ->is_positive(c));

      EXPECT_TRUE(R->is_equal(c, d));
      EXPECT_TRUE(R->is_equal(c, R->add(R->mult(a, u), R->mult(b, v))));
      EXPECT_TRUE(R->is_equal(a, R->mult(R->divide(a, c), c)));
    }

  EXPECT_TRUE(R->is_equal(R->zero(), R->gcd(R->zero(), R->zero())));
  EXPECT_TRUE(R->is_equal(R->one(), R->gcd(R->one(), R->minus_one())));
  EXPECT_TRUE(R->is_equal(R->one(), R->gcd(R->minus_one(), R->minus_one())));
}
TEST(RingZZ, remainder) { testRingRemainder(globalZZ, ntrials); }
TEST(RingZZ, syzygy) { testRingSyzygy(globalZZ, ntrials); }
TEST(RingZZ, content)
{
  buffer o;
  ring_elem a = globalZZ->from_long(-10);
  ring_elem b = globalZZ->from_long(-15);
  ring_elem c = globalZZ->from_long(-5);
  ring_elem d = globalZZ->from_long(5);
  globalZZ->is_equal(d, globalZZ->preferred_associate(c));

  ring_elem e = globalZZ->from_long(0);
  bool ret1 = globalZZ->lower_associate_divisor(e, a);
  o << "ret1=" << (ret1 ? "true" : "false") << " e=";
  globalZZ->elem_text_out(o, e);
  o << newline;
  bool ret2 = globalZZ->lower_associate_divisor(e, b);
  o << "ret2=" << (ret2 ? "true" : "false") << " e=";
  globalZZ->elem_text_out(o, e);
  o << newline;
  std::cout << o.str();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
