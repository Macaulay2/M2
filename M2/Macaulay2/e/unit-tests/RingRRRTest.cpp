// Copyright 2013 Michael E. Stillman

#include "RingTest.hpp"
#include "aring-glue.hpp"
#include "aring-RRR.hpp"

typedef M2::ConcreteRing<M2::ARingRRR> RingRRR;

bool almostEqual(const RingRRR *R, int nbits, ring_elem a, ring_elem b)
{
  mpfr_t epsilon;
  mpfr_init2(epsilon, 100);
  mpfr_set_ui_2exp(epsilon, 1, -nbits, MPFR_RNDN);

  ring_elem c = R->subtract(a, b);
  bool ret = mpfr_cmpabs(c.get_mpfr(), epsilon) < 0;

  mpfr_clear(epsilon);
  return ret;
}

template <>
ring_elem getElement<RingRRR>(const RingRRR &R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  return R.random();
}

TEST(RingRRR, create)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));
  EXPECT_TRUE(R != 0);

  EXPECT_TRUE(dynamic_cast<const Z_mod *>(R) == 0);
  EXPECT_TRUE(dynamic_cast<const RingRRR *>(R) != 0);
  EXPECT_FALSE(R->is_ZZ());
  // FIXME: not implemented: EXPECT_TRUE(R->is_RRR());
  // FIXME: string vs char*: EXPECT_EQ(ringName(*R), "RRR_100");
}
TEST(RingRRR, ones)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingRRR, negate)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));
  testRingNegate(R, ntrials);
}
TEST(RingRRR, add)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));
  RingElementGenerator<RingRRR> gen(*R);

  for (int i = 0; i < ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->add(a, b);
      ring_elem d = R->negate(b);
      ring_elem e = R->add(c, d);  // should be a
      EXPECT_TRUE(almostEqual(R, 98, a, e));
    }
}
TEST(RingRRR, subtract)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));
  testRingSubtract(R, ntrials);
}
TEST(RingRRR, multDivide)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));
  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a*b) // b == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->mult(a, b);
      if (R->is_zero(b))
        EXPECT_TRUE(R->is_zero(c));
      else
        {
          ring_elem d = R->divide(c, b);
          EXPECT_TRUE(almostEqual(R, 94, d, a));
        }
    }
}
TEST(RingRRR, axioms)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));
  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = gen.nextElement();

      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      ring_elem d = R->add(a, b);
      ring_elem e = R->add(b, a);
      EXPECT_TRUE(R->is_equal(d, e));
      d = R->mult(a, b);
      e = R->mult(b, a);
      EXPECT_TRUE(almostEqual(R, 98, d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      d = R->add(a, R->add(b, c));
      e = R->add(R->add(a, b), c);
      EXPECT_TRUE(almostEqual(R, 94, d, e));
      d = R->mult(a, R->mult(b, c));
      e = R->mult(R->mult(a, b), c);
      EXPECT_TRUE(almostEqual(R, 94, d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      d = R->mult(a, R->add(b, c));
      e = R->add(R->mult(a, b), R->mult(a, c));
      EXPECT_TRUE(almostEqual(R, 92, d, e));
    }
}
TEST(RingRRR, power)
{
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));

  mpz_t gmp1;
  mpz_init(gmp1);
  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      // TODO: what should the answer here be?
      // EXPECT_TRUE(R->is_equal(R->power(a, 0), R->one())); // 0^0 == 1 too?
      EXPECT_TRUE(R->is_equal(R->power(a, 1), a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      // std::cout << "(" << e1 << "," << e2 << ")" << std::endl;
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
  // NOTE: RingRRR::syzygy, CCC::syzygy are not useful functions.
  // Should we remove these tests, and the corresponding functions?
  RingRRR *R = RingRRR::create(new M2::ARingRRR(100));

  RingElementGenerator<RingRRR> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem u, v;
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      if (R->is_zero(b)) continue;

      // special cases (note: b != 0 for rest of routine)
      // syzygy(0,b) returns (1,0)
      R->syzygy(R->zero(), b, u, v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(R->is_equal(v, R->zero()));
      // syzygy(a,1) returns (1,-a)
      R->syzygy(a, R->one(), u, v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(almostEqual(R, 98, v, R->negate(a)));
      // syzygy(a,-1) returns (1,a)
      R->syzygy(a, R->minus_one(), u, v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(almostEqual(R, 98, v, a));
      R->syzygy(a, b, u, v);
      ring_elem result = R->add(R->mult(a, u), R->mult(b, v));
      EXPECT_TRUE(almostEqual(R, 94, result, R->zero()));
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
