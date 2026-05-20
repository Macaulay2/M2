// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-RRR.hpp"
#include "ARingTest.hpp"

bool almostEqual(const M2::ARingRRR& R,
                 int nbits,
                 const M2::ARingRRR::ElementType& a,
                 const M2::ARingRRR::ElementType& b)
{
  mpfr_t epsilon;
  mpfr_init2(epsilon, R.get_precision());
  mpfr_set_ui_2exp(epsilon, 1, -nbits, MPFR_RNDN);

  M2::ARingRRR::ElementType c;
  R.init(c);
  R.subtract(c, a, b);
  bool ret = mpfr_cmpabs(&c, epsilon) < 0;

  R.clear(c);
  mpfr_clear(epsilon);
  return ret;
}

template <>
void getElement<M2::ARingRRR>(const M2::ARingRRR& R,
                              int index,
                              M2::ARingRRR::ElementType& result)
{
  if (index < 50)
    R.set_from_long(result, index - 25);
  else
    R.random(result);
}

// void getElementRRR(const M2::ARingRRR&  R, int index,
// M2::ARingRRR::ElementType& result)
//{
//  if (index < 50) R.set_from_long(result, index-25);
//  else R.random(result);
//}

TEST(ARingRRR, create)
{
  M2::ARingRRR R(100);
  EXPECT_EQ(ringName(R), "ARRR_100");
  EXPECT_EQ(R.characteristic(), 0);
}

void testRingNegateRRR(const M2::ARingRRR& R, int ntrials)
{
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (-a) + (a) == 0
      gen.nextElement(a);
      R.negate(b, a);
      R.add(c, a, b);
      EXPECT_TRUE(R.is_zero(c));
    }
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, negate)
{
  M2::ARingRRR R(100);
  testRingNegateRRR(R, ntrials);
}

TEST(ARingRRR, add)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      R.add(c, a, b);
      R.negate(d, b);
      R.add(e, c, d);  // should be a
      EXPECT_TRUE(almostEqual(R, 98, a, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, subtract)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a-b) + (b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      R.subtract(c, a, b);
      R.add(e, c, b);  // should be a
      EXPECT_TRUE(almostEqual(R, 98, a, e));
      R.mult(e, a, b);
      R.subtract_multiple(e, a, b);
      EXPECT_TRUE(R.is_zero(e));
    }
  R.clear(e);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, multDivide)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a*b) // b == a
      gen.nextElement(a);
      gen.nextElement(b);
      R.mult(c, a, b);
      if (R.is_zero(b))
        EXPECT_TRUE(R.is_zero(c));
      else
        {
          R.divide(d, c, b);
          EXPECT_TRUE(almostEqual(R, 94, d, a));
        }
    }
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, axioms)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      R.add(d, a, b);
      R.add(e, b, a);
      EXPECT_TRUE(almostEqual(R, 98, d, e));
      R.mult(d, a, b);
      R.mult(e, b, a);
      EXPECT_TRUE(almostEqual(R, 98, d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(e, b, c);
      R.add(d, a, e);  // a+(b+c)
      R.add(e, a, b);
      R.add(e, e, c);  // (a+b)+c
      EXPECT_TRUE(almostEqual(R, 94, d, e));
      R.mult(e, b, c);
      R.mult(d, a, e);  // a*(b*c)
      R.mult(e, a, b);
      R.mult(e, e, c);  // (a*b)*c
      EXPECT_TRUE(almostEqual(R, 94, d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(e, b, c);
      R.mult(d, a, e);  // a*(b+c)
      R.mult(b, a, b);
      R.mult(c, a, c);
      R.add(e, b, c);  // a*b + a*c
      EXPECT_TRUE(almostEqual(R, 94, d, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, power_and_invert)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  mpz_t gmp1;
  mpz_init(gmp1);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      // TODO: what should the answer here be?
      // EXPECT_TRUE(R->is_equal(R->power(a, 0), R->one())); // 0^0 == 1 too?
      R.power(b, a, 1);
      EXPECT_TRUE(R.is_equal(b, a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      R.power(b, a, e1);
      R.power(c, a, e2);
      R.power(d, a, e1 + e2);
      R.mult(c, b, c);
      EXPECT_TRUE(almostEqual(R, 96, c, d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      R.power_mpz(d, a, gmp1);
      EXPECT_TRUE(R.is_equal(d, b));
    }
  mpz_clear(gmp1);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

// TODO: syzygy?

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
