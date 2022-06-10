// Copyright 2013 Michael E. Stillman

#include "RingTest.hpp"

#include "ZZp.hpp"
#include "aring-glue.hpp"

typedef M2::ConcreteRing<M2::ARingCCC> RingCCC;

bool almostEqual(const RingCCC *R, int nbits, ring_elem a, ring_elem b)
{
  mpfr_t epsilon;
  mpfr_init2(epsilon, 100);
  mpfr_set_ui_2exp(epsilon, 1, -nbits, MPFR_RNDN);

  ring_elem f = R->subtract(a, b);
  auto f1 = BIGCC_RE(f);
  bool re_is_zero = (mpfr_cmpabs(f1, epsilon) < 0);
  auto f2 = BIGCC_IM(f);
  bool im_is_zero = (mpfr_cmpabs(f2, epsilon) < 0);

  bool ret = re_is_zero && im_is_zero;
  mpfr_clear(epsilon);
  return ret;
}

template <>
ring_elem getElement<RingCCC>(const RingCCC &R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  return R.random();
}

////////////////////////////////////////////////////////
TEST(RingCCC, create)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));
  EXPECT_TRUE(R != 0);

  EXPECT_TRUE(dynamic_cast<const Z_mod *>(R) == 0);
  EXPECT_TRUE(dynamic_cast<const RingCCC *>(R) != 0);
  EXPECT_FALSE(R->is_ZZ());
  // FIXME: not implemented: EXPECT_TRUE(R->is_CCC());
  // FIXME: string vs char*: EXPECT_EQ(ringName(*R), "CCC_100");
}
TEST(RingCCC, ones)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingCCC, negate)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));
  testRingNegate(R, ntrials);
}
TEST(RingCCC, add)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));
  RingElementGenerator<RingCCC> gen(*R);

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
TEST(RingCCC, subtract)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));
  testRingSubtract(R, ntrials);
}
TEST(RingCCC, multDivide)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));
  RingElementGenerator<RingCCC> gen(*R);
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
TEST(RingCCC, axioms)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));
  RingElementGenerator<RingCCC> gen(*R);
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
#if 0
      mpfr_printf("a=(%.20Rf,%.20Rf)\n",BIGCC_RE(a), BIGCC_IM(a));
      mpfr_printf("b=(%.20Rf,%.20Rf)\n",BIGCC_RE(b), BIGCC_IM(b));
      mpfr_printf("a*(b+c)=(%.20Rf,%.20Rf)\n",BIGCC_RE(d), BIGCC_IM(d));
      mpfr_printf("a*b+a*c=(%.20Rf,%.20Rf)\n",BIGCC_RE(e), BIGCC_IM(e));
#endif
      EXPECT_TRUE(almostEqual(R, 92, d, e));
    }
}
TEST(RingCCC, power)
{
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));

  mpz_t gmp1;
  mpz_init(gmp1);
  RingElementGenerator<RingCCC> gen(*R);
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
#if 0
      ring_elem e = R->mult(b,c);
      mpfr_printf("b=(%.30Rf,%.30Rf)\n",BIGCC_RE(b), BIGCC_IM(b));
      mpfr_printf("c=(%.30Rf,%.30Rf)\n",BIGCC_RE(c), BIGCC_IM(c));
      mpfr_printf("d=(%.30Rf,%.30Rf)\n",BIGCC_RE(d), BIGCC_IM(d));
      mpfr_printf("e=(%.30Rf,%.30Rf)\n",BIGCC_RE(e), BIGCC_IM(e));
#endif
      EXPECT_TRUE(almostEqual(R, 80, R->mult(b, c), d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      ring_elem b1 = R->power(a, gmp1);
      EXPECT_TRUE(R->is_equal(b1, b));
    }
  mpz_clear(gmp1);
}
TEST(RingCCC, syzygy)
{
  // NOTE: RingCCC::syzygy, RingCCC::syzygy are not useful functions.
  // Should we remove these tests, and the corresponding functions?
  RingCCC *R = RingCCC::create(new M2::ARingCCC(100));

  RingElementGenerator<RingCCC> gen(*R);
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
