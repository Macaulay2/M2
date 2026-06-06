// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "ARingTest.hpp"

bool almostEqual(const M2::ARingCC& C,
                 unsigned long nbits,
                 const M2::ARingCC::ElementType& a,
                 const M2::ARingCC::ElementType& b)
{
  M2::ARingRR::ElementType epsilon = pow(2, static_cast<double>(-nbits));
  M2::ARingRR::ElementType d;
  M2::ARingCC::ElementType c;
  C.subtract(c, a, b);
  C.abs(d, c);
  return C.real_ring().compare_elems(d, epsilon) < 0;
}

template <>
void getElement<M2::ARingCC>(const M2::ARingCC& C,
                             int index,
                             M2::ARingCC::ElementType& result)
{
  if (index < 50)
    C.set_from_long(result, index - 25);
  else
    C.random(result);
}

TEST(ARingCC, create)
{
  M2::ARingCC C;
  EXPECT_EQ(ringName(C), "ACC_53");
  EXPECT_EQ(C.characteristic(), 0);
}

void testRingNegateCCC(const M2::ARingCC& C, int ntrials)
{
  ARingElementGenerator<M2::ARingCC> gen(C);
  M2::ARingCC::ElementType a, b, c;
  C.init(a);
  C.init(b);
  C.init(c);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (-a) + (a) == 0
      gen.nextElement(a);
      C.negate(b, a);
      C.add(c, a, b);
      EXPECT_TRUE(C.is_zero(c));
    }
  C.clear(c);
  C.clear(b);
  C.clear(a);
}

TEST(ARingCC, negate)
{
  M2::ARingCC C;
  testRingNegateCCC(C, ntrials);
}

TEST(ARingCC, add)
{
  M2::ARingCC C;
  auto nbits = C.get_precision();
  ARingElementGenerator<M2::ARingCC> gen(C);
  M2::ARingCC::ElementType a, b, c, d, e;
  C.init(a);
  C.init(b);
  C.init(c);
  C.init(d);
  C.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      C.add(c, a, b);
      C.negate(d, b);
      C.add(e, c, d);  // should be a
      EXPECT_TRUE(almostEqual(C, nbits - 2, a, e));
    }
  C.clear(e);
  C.clear(d);
  C.clear(c);
  C.clear(b);
  C.clear(a);
}

TEST(ARingCC, subtract)
{
  M2::ARingCC C;
  auto nbits = C.get_precision();
  ARingElementGenerator<M2::ARingCC> gen(C);
  M2::ARingCC::ElementType a, b, c, e;
  C.init(a);
  C.init(b);
  C.init(c);
  C.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a-b) + (b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      C.subtract(c, a, b);
      C.add(e, c, b);  // should be a
      EXPECT_TRUE(almostEqual(C, nbits - 2, a, e));
      C.mult(e, a, b);
      C.subtract_multiple(e, a, b);
      EXPECT_TRUE(C.is_zero(e));
    }
  C.clear(e);
  C.clear(c);
  C.clear(b);
  C.clear(a);
}

TEST(ARingCC, multDivide)
{
  M2::ARingCC C;
  auto nbits = C.get_precision();
  ARingElementGenerator<M2::ARingCC> gen(C);
  M2::ARingCC::ElementType a, b, c, d;
  C.init(a);
  C.init(b);
  C.init(c);
  C.init(d);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a*b) // b == a
      gen.nextElement(a);
      gen.nextElement(b);
      C.mult(c, a, b);
      if (C.is_zero(b))
        EXPECT_TRUE(C.is_zero(c));
      else
        {
          C.divide(d, c, b);
          EXPECT_TRUE(almostEqual(C, nbits - 6, d, a));
        }
    }
  C.clear(d);
  C.clear(c);
  C.clear(b);
  C.clear(a);
}

TEST(ARingCC, axioms)
{
  M2::ARingCC C;
  auto nbits = C.get_precision();
  ARingElementGenerator<M2::ARingCC> gen(C);
  M2::ARingCC::ElementType a, b, c, d, e;
  C.init(a);
  C.init(b);
  C.init(c);
  C.init(d);
  C.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      C.add(d, a, b);
      C.add(e, b, a);
      EXPECT_TRUE(almostEqual(C, nbits - 2, d, e));
      C.mult(d, a, b);
      C.mult(e, b, a);
      EXPECT_TRUE(almostEqual(C, nbits - 2, d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      C.add(e, b, c);
      C.add(d, a, e);  // a+(b+c)
      C.add(e, a, b);
      C.add(e, e, c);  // (a+b)+c
      EXPECT_TRUE(almostEqual(C, nbits - 6, d, e));
      C.mult(e, b, c);
      C.mult(d, a, e);  // a*(b*c)
      C.mult(e, a, b);
      C.mult(e, e, c);  // (a*b)*c
      EXPECT_TRUE(almostEqual(C, nbits - 6, d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      C.add(e, b, c);
      C.mult(d, a, e);  // a*(b+c)
      C.mult(b, a, b);
      C.mult(c, a, c);
      C.add(e, b, c);  // a*b + a*c
      EXPECT_TRUE(almostEqual(C, nbits - 6, d, e));
    }
  C.clear(e);
  C.clear(d);
  C.clear(c);
  C.clear(b);
  C.clear(a);
}

TEST(ARingCC, power_and_invert)
{
  M2::ARingCC C;
  auto nbits = C.get_precision();
  ARingElementGenerator<M2::ARingCC> gen(C);
  M2::ARingCC::ElementType a, b, c, d;
  C.init(a);
  C.init(b);
  C.init(c);
  C.init(d);
  mpz_t gmp1;
  mpz_init(gmp1);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      // TODO: what should the answer here be?
      // EXPECT_TRUE(R->is_equal(R->power(a, 0), R->one())); // 0^0 == 1 too?
      C.power(b, a, 1);
      EXPECT_TRUE(C.is_equal(b, a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      C.power(b, a, e1);
      C.power(c, a, e2);
      C.power(d, a, e1 + e2);
      C.mult(c, b, c);
      EXPECT_TRUE(almostEqual(C, nbits - 11, c, d)); /* exponentiation gives
                                                  relatively small number
                                                  of correct digits */

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      C.power_mpz(d, a, gmp1);
      EXPECT_TRUE(fabs(d.re - b.re) < 1.e-14);
      EXPECT_TRUE(fabs(d.im - b.im) < 1.e-14);
    }
  mpz_clear(gmp1);
  C.clear(d);
  C.clear(c);
  C.clear(b);
  C.clear(a);
}

// TODO: syzygy?

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
