// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "ZZ.hpp"
#include "ring-test.hpp"

template <typename RingType>
ring_elem getElement(const RingType& R, int index);

static int ntrials = 200;
static bool maxH_initialized = false;
static mpz_t maxH;

template <>
ring_elem getElement<RingZZ>(const RingZZ&  R, int index)
{
  if (index < 50) return R.from_int(index-25);
  if (!maxH_initialized)
    {
      maxH_initialized = true;
      mpz_init(maxH);
      mpz_set_str(maxH, "100000000000", 10);
    }
  gmp_ZZ a1 = rawRandomInteger(maxH);
  return R.from_int(a1);
}

template <typename RingType>
class RingElementGenerator
{
public:
  RingElementGenerator(const RingType& R) : mRing(R), mNext(0) {}
  
  ring_elem nextElement() {return getElement<RingType>(mRing, ++mNext);}
  void reset() {mNext = 0;}
private:
  const RingType& mRing;
  int mNext;
};
/////////////////////////////////////////////////
template<typename T>
void testRingCoercions(const T* R, int ntrials)
{
  // from int
  // from mpz
  // from rational

}
template<typename T>
void testRingNegate(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      // test: (-a) + (a) == a
      ring_elem a = gen.nextElement();
      ring_elem b = R->negate(a);
      ring_elem c = R->add(a,b);
      EXPECT_TRUE(R->is_zero(c));
    }
}
template<typename T>
void testRingAdd(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->add(a,b);
      ring_elem d = R->negate(b);
      ring_elem e = R->add(c,d); // should be a
      EXPECT_TRUE(R->is_equal(e,a));
    }
}
template<typename T>
void testRingSubtract(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      // test: (a-b) + (b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->subtract(a,b);
      ring_elem e = R->add(c,b);
      EXPECT_TRUE(R->is_equal(e,a));
    }
}
template<typename T>
void testRingDivide(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      // test: (a*b) // b == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->mult(a,b);
      if (R->is_zero(b))
        EXPECT_TRUE(R->is_zero(c));
      else
        {
          ring_elem d = R->divide(c,b);
          EXPECT_TRUE(R->is_equal(d,a));
        }
    }
}
template<typename T>
void testRingAxioms(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = gen.nextElement();

      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      ring_elem d = R->add(a,b);
      ring_elem e = R->add(b,a);
      EXPECT_TRUE(R->is_equal(d,e));
      d = R->mult(a,b);
      e = R->mult(b,a);
      EXPECT_TRUE(R->is_equal(d,e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      d = R->add(a, R->add(b,c));
      e = R->add(R->add(a,b), c);
      EXPECT_TRUE(R->is_equal(d,e));
      d = R->mult(a, R->mult(b,c));
      e = R->mult(R->mult(a,b), c);
      EXPECT_TRUE(R->is_equal(d,e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      d = R->mult(a, R->add(b,c));
      e = R->add(R->mult(a,b), R->mult(a,c));
      EXPECT_TRUE(R->is_equal(d,e));
    }
}
template<typename T>
void testRingPower(const T* R, int ntrials)
{
  mpz_t gmp1;
  mpz_init(gmp1);
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      EXPECT_TRUE(R->is_equal(R->power(a, 0), R->one())); // 0^0 == 1 too?
      EXPECT_TRUE(R->is_equal(R->power(a, 1), a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      std::cout << "(" << e1 << "," << e2 << ")" << std::endl;
      ring_elem b = R->power(a, e1);
      ring_elem c = R->power(a, e2);
      ring_elem d = R->power(a, e1+e2);
      EXPECT_TRUE(R->is_equal(R->mult(b,c),d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      ring_elem b1 = R->power(a, gmp1);
      EXPECT_TRUE(R->is_equal(b1, b));
    }
  mpz_clear(gmp1);
}
template<typename T>
void testRingGCD(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();

      // (a // gcd(a,b) == 0, b // gcd(a,b) == 0, 
      ring_elem c = R->gcd(a,b);
      ring_elem u, v;
      ring_elem d = R->gcd_extended(a,b,u,v);

      EXPECT_TRUE(R->is_equal(c,d));
      EXPECT_TRUE(R->is_equal(c,R->add(R->mult(a,u), R->mult(b,v))));
      EXPECT_TRUE(R->is_equal(a, R->mult(R->divide(a, c), c)));
    }  
}
template<typename T>
void testRingRemainder(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();

#if 0
      ring_elem c = R->remainder(a, R->zero()); // FAILS!!
      EXPECT_TRUE(R->is_equal(c,a));
#endif

      ring_elem r = R->remainder(a,b);
      ring_elem q = R->quotient(a,b);
      ring_elem r1, q1;
      r1 = R->remainderAndQuotient(a,b,q1);

      EXPECT_TRUE(R->is_equal(r,r1));
      EXPECT_TRUE(R->is_equal(q,q1));
      ring_elem a1 = R->add(R->mult(q,b), r);
      EXPECT_TRUE(R->is_equal(a,a1));
    }

}
/////////////////////////////////////////////////
TEST(RingZZ, create)
{
  EXPECT_TRUE(globalZZ != 0);

  EXPECT_TRUE(dynamic_cast< const RingZZ* >(globalZZ) != 0);
  EXPECT_EQ(globalZZ->coefficient_type(), Ring::COEFF_ZZ);
  EXPECT_TRUE(globalZZ->is_ZZ());
  EXPECT_EQ(ringName(*globalZZ), "ZZ");
}
TEST(RingZZ, ones)
{
  EXPECT_TRUE(globalZZ->is_equal(globalZZ->one(), globalZZ->from_int(1)));
  EXPECT_TRUE(globalZZ->is_equal(globalZZ->minus_one(), globalZZ->from_int(-1)));
  EXPECT_TRUE(globalZZ->is_equal(globalZZ->zero(), globalZZ->from_int(0)));
  EXPECT_TRUE(globalZZ->is_zero(globalZZ->from_int(0)));
}
TEST(RingZZ, random)
{
  mpz_t b;
  mpz_init(b);
  mpz_t maxH;
  mpz_init(maxH);
  mpz_set_str(maxH, "100000000000", 10);
  for (int i=0; i<=10; i++)
    {
      buffer o;
      ring_elem a = globalZZ->random(); // POOR DESIGN!  Need to be able to choose size
      gmp_ZZ a1 = rawRandomInteger(maxH); // This one is fine
      a = globalZZ->from_int(a1);
      globalZZ->elem_text_out(o, a);
      std::cout << o.str() << std::endl;
      mpz_set_str(b, o.str(), 10);
      ring_elem c = globalZZ->from_int(b);
      EXPECT_TRUE(globalZZ->is_equal(a, c));
    }
  mpz_clear(maxH);
  mpz_clear(b);
}
TEST(RingZZ, negate)
{
  testRingNegate(globalZZ, ntrials);
}
TEST(RingZZ, add)
{
  testRingAdd(globalZZ, ntrials);
}
TEST(RingZZ, subtract)
{
  testRingSubtract(globalZZ, ntrials);
}
TEST(RingZZ, multDivide)
{
  testRingDivide(globalZZ, ntrials);

  // I would prefer for 'divide' to be exact division, with a return value of false, if not exactly divisible
  ring_elem a = globalZZ->from_int(5);
  ring_elem b = globalZZ->from_int(2);
  ring_elem c = globalZZ->divide(a,b);
  EXPECT_TRUE(globalZZ->is_equal(c, globalZZ->from_int(2)));
}
TEST(RingZZ, axioms)
{
  testRingAxioms(globalZZ, ntrials);
}
TEST(RingZZ, power)
{
  testRingPower(globalZZ, ntrials);
}
TEST(RingZZ, gcd)
{
  testRingGCD(globalZZ, ntrials);
}
TEST(RingZZ, remainder)
{
  testRingRemainder(globalZZ, ntrials);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
