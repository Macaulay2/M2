#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "basic-rings/aring-RRi.hpp"
#include "basic-rings/aring-RRR.hpp"
#include "basic-rings/aring-glue.hpp"
#include "unit-tests/ARingTest.hpp"
#include "interface/matrix.cpp"

// For debugging purposes, use
//mpfr_printf("a=(%.20Rf,%.20Rf)\n",&(a.left), &(a.right));

bool almostEqual(const M2::ARingRRi& R,
                 int nbits,
                 const M2::ARingRRi::ElementType& a,
                 const M2::ARingRRi::ElementType& b)
{
    mpfr_t epsilon;
    mpfr_init2(epsilon, R.get_precision());
    mpfr_set_ui_2exp(epsilon, 1, -nbits, MPFR_RNDN);
    
    mpfr_t c,d;
    mpfr_init2(c, R.get_precision());
    mpfr_init2(d, R.get_precision());
    
    mpfr_sub(c,&(a.left),&(b.left),MPFR_RNDN);
    mpfr_sub(d,&(a.right),&(b.right),MPFR_RNDN);
    
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
    R.set(result, index - 25);
  else
    R.random(result);
}

TEST(ARingRRi, create)
{
  M2::ARingRRi R(100);
  EXPECT_EQ(ringName(R), "ARRi_100");
  EXPECT_EQ(R.characteristic(), 0);
}

TEST(ARingRRi, comparison)
{
    M2::ARingRRi R(100);
    M2::ARingRRR S(100);
    M2::ARingRRi::ElementType a, b, c, d, e, f, g, h;
    M2::ARingRRR::ElementType m, n;
    gmp_ZZ p = to_gmp_ZZ(2);

    R.init(a);
    R.init(b);
    R.init(c);
    R.init(d);
    R.init(e);
    R.init(f);
    R.init(g);
    R.init(h);

    S.init(m);
    S.init(n);

    R.set_from_doubles(a,0,0);
    R.set_from_doubles(b,0,1);
    R.set_from_doubles(c,0,-1);
    R.set_from_doubles(d,-1,1);

    EXPECT_TRUE(R.is_member((long)0,a));
    EXPECT_FALSE(R.is_unit(a));
    EXPECT_FALSE(R.is_unit(b));
    EXPECT_FALSE(R.is_unit(c));
    EXPECT_FALSE(R.is_unit(d));

    EXPECT_TRUE(R.is_zero(a));
    EXPECT_FALSE(R.is_zero(b));
    EXPECT_FALSE(R.is_zero(c));
    EXPECT_FALSE(R.is_zero(d));

    R.set_from_doubles(a,1,2);
    R.set_from_doubles(b,1,3);
    R.set_from_doubles(c,2,3);
    R.set_from_doubles(d,4,5);
    R.set_from_doubles(e,3,2);

    EXPECT_EQ(R.computeHashValue(c),2622094);
    EXPECT_TRUE(R.is_equal(a,a));
    EXPECT_FALSE(R.is_equal(a,b));
    EXPECT_FALSE(R.is_equal(a,c));
    EXPECT_FALSE(R.is_equal(a,d));
    EXPECT_FALSE(R.is_equal(b,c));
    EXPECT_FALSE(R.is_equal(b,d));
    EXPECT_FALSE(R.is_equal(c,d));
    EXPECT_TRUE(R.is_equal(c,e));

    R.set(f,2);
    R.set(g,2);
    R.set(h,3);

    EXPECT_TRUE(R.is_equal(f,g));
    EXPECT_FALSE(R.is_equal(f,h));

    EXPECT_EQ(R.compare_elems(a,d),-1);
    EXPECT_EQ(R.compare_elems(d,c),1);
    EXPECT_EQ(R.compare_elems(b,c),0);
    EXPECT_EQ(R.compare_elems(f,g),0);
    EXPECT_EQ(R.compare_elems(f,d),-1);
    EXPECT_EQ(R.compare_elems(h,a),1);

    R.set_left(a,3);

    EXPECT_TRUE(R.is_empty(a));
    EXPECT_FALSE(R.is_empty(b));
    EXPECT_FALSE(R.is_empty(h));

    EXPECT_TRUE(R.is_member((long)2,b));
    EXPECT_FALSE(R.is_member((long)4,b));
    EXPECT_TRUE(R.is_member(2.3,e));
    EXPECT_FALSE(R.is_member(1.6,e));

    S.set(m,2.4);
    S.set(n,3.6);

    EXPECT_TRUE(R.is_member(m,e));
    EXPECT_FALSE(R.is_member(n,e));

    R.set(a,b);
    R.copy(d,b);

    EXPECT_TRUE(R.is_equal(a,b));
    EXPECT_TRUE(R.is_equal(b,d));
    EXPECT_FALSE(R.is_equal(a,c));
    EXPECT_FALSE(R.is_equal(d,e));

    R.set_from_doubles(a,1,3);
    R.set_from_doubles(b,2,3);
    R.set_from_doubles(c,1,2);
    R.set_from_doubles(d,-1,0);
    R.set_from_doubles(e,4,5);

    EXPECT_TRUE(R.is_subset(b,a));
    EXPECT_TRUE(R.is_subset(c,a));
    EXPECT_FALSE(R.is_subset(d,a));
    EXPECT_FALSE(R.is_subset(e,a));

    EXPECT_TRUE(R.is_member(p,a));
    EXPECT_TRUE(R.is_member(p,b));
    EXPECT_FALSE(R.is_member(p,e));
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
