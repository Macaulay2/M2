#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-RRi.hpp"
#include "aring-RRR.hpp"
#include "aring-glue.hpp"
#include "ARingTest.hpp"

// For debugging purposes, use
//mpfr_printf("a=(%.20Rf,%.20Rf)\n",&(a.left), &(a.right));

bool almostEqual(const M2::ARingRRi& R,
                 int nbits,
                 const M2::ARingRRi::ElementType& a,
                 const M2::ARingRRi::ElementType& b)
{
    mpfr_t epsilon;
    mpfr_init2(epsilon, R.get_precision());
    mpfr_set_ui_2exp(epsilon, 1, -nbits, GMP_RNDN);
    
    mpfr_t c,d;
    mpfr_init2(c, R.get_precision());
    mpfr_init2(d, R.get_precision());
    
    mpfr_sub(c,&(a.left),&(b.left),GMP_RNDN);
    mpfr_sub(d,&(a.right),&(b.right),GMP_RNDN);
    
    bool retL = mpfr_cmpabs(c, epsilon) < 0,
         retR = mpfr_cmpabs(d, epsilon) < 0;
    
    mpfr_clear(d);
    mpfr_clear(c);
    mpfr_clear(epsilon);
    return retL and retR;
}

template <>
void getElement<M2::ARingRRi>(const M2::ARingRRi& R,
                              int index,
                              M2::ARingRRi::ElementType& result)
{
  if (index < 50)
    R.set_from_long(result, index - 25);
  else
    R.random(result);
}

TEST(ARingRRi, create)
{
  M2::ARingRRi R(100);
  EXPECT_EQ(ringName(R), "ARRi_100");
  EXPECT_EQ(R.characteristic(), 0);
}

void testRingNegateRRi(const M2::ARingRRi& R, const M2::ARingRRR& S, int ntrials)
{
    ARingElementGenerator<M2::ARingRRi> gen(R);
    M2::ARingRRi::ElementType a, b, c;
    R.init(a);
    R.init(b);
    R.init(c);
    
    M2::ARingRRR::ElementType d;
    S.init(d);
    for (int i = 0; i < ntrials; i++)
    {
        // test: (-a) + (a) == 0
        gen.nextElement(a);
        R.negate(b,a);
        R.add(c,a,b);
        R.midpoint(d,c);
        EXPECT_TRUE(S.is_zero(d));
    }
    S.clear(d);
    R.clear(c);
    R.clear(b);
    R.clear(a);
}

TEST(ARingRRi, negate)
{
    M2::ARingRRi R(100);
    M2::ARingRRR S(100);
    testRingNegateRRi(R, S, ntrials);
}

TEST(ARingRRi, add)
{
  M2::ARingRRi R(100);
  ARingElementGenerator<M2::ARingRRi> gen(R);
  M2::ARingRRi::ElementType a, b, c, d, e;
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
      EXPECT_TRUE(R.is_subset(a,e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRi, subtract)
{
  M2::ARingRRi R(100);
  M2::ARingRRR S(100);
  ARingElementGenerator<M2::ARingRRi> gen(R);
  M2::ARingRRi::ElementType a, b, c, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(e);
    
  M2::ARingRRR::ElementType f;
  S.init(f);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a-b) + (b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      R.subtract(c, a, b);
      R.add(e, c, b);  // should be a
      EXPECT_TRUE(R.is_subset(a,e));
      R.mult(e, a, b);
      R.subtract_multiple(e, a, b);
      R.midpoint(f,e);
      EXPECT_TRUE(S.is_zero(f));
    }
  S.clear(f);
  R.clear(e);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRi, multDivide)
{
  M2::ARingRRi R(100);
  ARingElementGenerator<M2::ARingRRi> gen(R);
  M2::ARingRRi::ElementType a, b, c, d;
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
      if (R.is_member(0L,b))
          EXPECT_TRUE(R.is_member(0L,c));
      else
        {
          R.divide(d, c, b);
          EXPECT_TRUE(R.is_subset(a,d));
        }
    }
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRi, axioms)
{
  M2::ARingRRi R(100);
  M2::ARingRRR S(100);
  ARingElementGenerator<M2::ARingRRi> gen(R);
  M2::ARingRRi::ElementType a, b, c, d, e;
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
      EXPECT_TRUE(R.is_equal(d,e));
      R.mult(d, a, b);
      R.mult(e, b, a);
      EXPECT_TRUE(R.is_equal(d,e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(e, b, c);
      R.add(d, a, e);  // a+(b+c)
      R.add(e, a, b);
      R.add(e, e, c);  // (a+b)+c
      EXPECT_TRUE(almostEqual(R,-94,d,e));

      R.mult(e, b, c);
      R.mult(d, a, e);  // a*(b*c)
      R.mult(e, a, b);
      R.mult(e, e, c);  // (a*b)*c
        
      EXPECT_TRUE(almostEqual(R,-94,d,e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(e, b, c);
      R.mult(d, a, e);  // a*(b+c)
      R.mult(b, a, b);
      R.mult(c, a, c);
      R.add(e, b, c);  // a*b + a*c
        
      EXPECT_TRUE(almostEqual(R,-94,d,e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRi, power_and_invert)
{
  M2::ARingRRi R(100);
  ARingElementGenerator<M2::ARingRRi> gen(R);
  M2::ARingRRi::ElementType a, b, c, d;
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
      EXPECT_TRUE(almostEqual(R,-94,c,d));

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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
