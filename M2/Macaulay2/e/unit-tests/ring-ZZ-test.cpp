// Copyright 2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "ZZ.hpp"
#include "ZZp.hpp"
#include "QQ.hpp"
#include "RRR.hpp"
#include "ring-test.hpp"

template <typename RingType>
ring_elem getElement(const RingType& R, int index);

static int ntrials = 50000;
static bool maxH_initialized = false;
static mpz_t maxH;


bool almostEqual(const RRR* R, int nbits, ring_elem a, ring_elem b)
{
  mpfr_t epsilon;  
  mpfr_init2(epsilon, 100);
  mpfr_set_ui_2exp(epsilon, 1, -nbits, GMP_RNDN);
  
  ring_elem c = R->subtract(a,b);
  bool ret =  mpfr_cmpabs(c.mpfr_val,epsilon) < 0;

  mpfr_clear(epsilon);
  return ret;
}
bool almostEqual(const CCC* R, int nbits, ring_elem a, ring_elem b)
{
  mpfr_t epsilon;  
  mpfr_init2(epsilon, 100);
  mpfr_set_ui_2exp(epsilon, 1, -nbits, GMP_RNDN);
  
  ring_elem f = R->subtract(a,b);
  mpfr_ptr f1 = BIGCC_RE(f);
  bool re_is_zero = (mpfr_cmpabs(f1,epsilon) < 0);
  mpfr_ptr f2 = BIGCC_IM(f);
  bool im_is_zero = (mpfr_cmpabs(f2,epsilon) < 0);

  bool ret = re_is_zero && im_is_zero;
  mpfr_clear(epsilon);
  return ret;
}
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

template <>
ring_elem getElement<QQ>(const QQ&  R, int index)
{
  if (index < 50) return R.from_int(index-25);
  if (!maxH_initialized)
    {
      maxH_initialized = true;
      mpz_init(maxH);
      mpz_set_str(maxH, "100000000000", 10);
    }
  gmp_QQ a1 = rawRandomQQ(maxH);
  return R.from_rational(a1);
}

template <>
ring_elem getElement<Z_mod>(const Z_mod&  R, int index)
{
  ring_elem a = getElement<RingZZ>(*globalZZ, index);
  return R.from_int(a.get_mpz());
}

template <>
ring_elem getElement<RRR>(const RRR&  R, int index)
{
  if (index < 50) return R.from_int(index-25);
  return R.random();
}

template <>
ring_elem getElement<CCC>(const CCC&  R, int index)
{
  if (index < 50) return R.from_int(index-25);
  return R.random();
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
      //TODO: what should the answer here be?
      //EXPECT_TRUE(R->is_equal(R->power(a, 0), R->one())); // 0^0 == 1 too?
      EXPECT_TRUE(R->is_equal(R->power(a, 1), a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      //std::cout << "(" << e1 << "," << e2 << ")" << std::endl;
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
template<typename T>
void testRingSyzygy(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem u, v;
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      if (R->is_zero(b)) continue;

      // special cases (note: b != 0 for rest of routine)
      // syzygy(0,b) returns (1,0)
      R->syzygy(R->zero(),b,u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(R->is_equal(v, R->zero()));
      // syzygy(a,1) returns (1,-a)
      R->syzygy(a,R->one(),u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(R->is_equal(v, R->negate(a)));
      // syzygy(a,-1) returns (1,a)
      R->syzygy(a,R->minus_one(),u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(R->is_equal(v, a));

      R->syzygy(a,b,u,v);
      ring_elem result = R->add(R->mult(a,u), R->mult(b,v));
#if 0
      buffer o;
      o << "a=";
      R->elem_text_out(o,a);
      o << " b=";
      R->elem_text_out(o,b);
      o << " u=";
      R->elem_text_out(o,u);
      o << " v=";
      R->elem_text_out(o,v);
      std::cout << o.str() << std::endl;
#endif
      EXPECT_TRUE(R->is_zero(result));
    }

  // over ZZ:
  // syzygy(a,b) returns (b/c, -a/c), where c = +- gcd(a,b), with same sign as b
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
      //std::cout << o.str() << std::endl;
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
  const RingZZ *R = globalZZ;

  RingElementGenerator<RingZZ> gen(*globalZZ);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();

      // (a // gcd(a,b) == 0, b // gcd(a,b) == 0, 
      ring_elem c = R->gcd(a,b);
      ring_elem u, v;
      ring_elem d = R->gcd_extended(a,b,u,v);

      EXPECT_TRUE(globalZZ->is_positive(c));

      EXPECT_TRUE(R->is_equal(c,d));
      EXPECT_TRUE(R->is_equal(c,R->add(R->mult(a,u), R->mult(b,v))));
      EXPECT_TRUE(R->is_equal(a, R->mult(R->divide(a, c), c)));
    }  

  EXPECT_TRUE(R->is_equal(R->zero(), R->gcd(R->zero(), R->zero())));
  EXPECT_TRUE(R->is_equal(R->one(), R->gcd(R->one(), R->minus_one())));
  EXPECT_TRUE(R->is_equal(R->one(), R->gcd(R->minus_one(), R->minus_one())));
}
TEST(RingZZ, remainder)
{
  testRingRemainder(globalZZ, ntrials);
}
TEST(RingZZ, syzygy)
{
  testRingSyzygy(globalZZ, ntrials);
}
TEST(RingZZ, content)
{
  buffer o;
  ring_elem a = globalZZ->from_int(-10);
  ring_elem b = globalZZ->from_int(-15);
  ring_elem c = globalZZ->from_int(-5);
  ring_elem d = globalZZ->from_int(5);
  globalZZ->is_equal(d, globalZZ->preferred_associate(c));

  ring_elem e = globalZZ->from_int(0);
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
  EXPECT_TRUE(R->is_equal(R->one(), R->from_int(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_int(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_int(0)));
  EXPECT_TRUE(R->is_zero(R->from_int(0)));
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
  EXPECT_TRUE(R->is_equal(R->one(), R->from_int(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_int(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_int(0)));
  EXPECT_TRUE(R->is_zero(R->from_int(0)));
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
////////////////////////////////////////////////////////
TEST(RingQQ, create)
{
  Ring *R = globalQQ;
  EXPECT_TRUE(R != 0);

  EXPECT_TRUE(dynamic_cast< const Z_mod * >(R) == 0);
  EXPECT_TRUE(dynamic_cast< const QQ * >(R) != 0);
  EXPECT_EQ(R->coefficient_type(), Ring::COEFF_QQ);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "QQ");
}
TEST(RingQQ, ones)
{
  Ring *R = globalQQ;
  EXPECT_TRUE(R->is_equal(R->one(), R->from_int(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_int(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_int(0)));
  EXPECT_TRUE(R->is_zero(R->from_int(0)));
}
TEST(RingQQ, negate)
{
  testRingNegate(globalQQ, ntrials);
}
TEST(RingQQ, add)
{
  testRingAdd(globalQQ, ntrials);
}
TEST(RingQQ, subtract)
{
  testRingSubtract(globalQQ, ntrials);
}
TEST(RingQQ, multDivide)
{
  testRingDivide(globalQQ, ntrials);
}
TEST(RingQQ, axioms)
{
  testRingAxioms(globalQQ, ntrials);
}
TEST(RingQQ, power)
{
  testRingPower(globalQQ, ntrials);
}
TEST(RingQQ, syzygy)
{
  testRingSyzygy(globalQQ, ntrials);
}
////////////////////////////////////////////////////////
TEST(RingRRR, create)
{
  Ring *R = RRR::create(100);
  EXPECT_TRUE(R != 0);

  EXPECT_TRUE(dynamic_cast< const Z_mod * >(R) == 0);
  EXPECT_TRUE(dynamic_cast< const RRR * >(R) != 0);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_TRUE(R->is_RRR());
  EXPECT_EQ(ringName(*R), "RRR_100");
}
TEST(RingRRR, ones)
{
  Ring *R = RRR::create(100);
  EXPECT_TRUE(R->is_equal(R->one(), R->from_int(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_int(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_int(0)));
  EXPECT_TRUE(R->is_zero(R->from_int(0)));
}
TEST(RingRRR, negate)
{
  RRR *R = RRR::create(100);
  testRingNegate(R, ntrials);
}
TEST(RingRRR, add)
{
  RRR *R = RRR::create(100);
  RingElementGenerator<RRR> gen(*R);

  for (int i=0; i<ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->add(a,b);
      ring_elem d = R->negate(b);
      ring_elem e = R->add(c,d); // should be a
      EXPECT_TRUE(almostEqual(R,98,a,e));
    }
}
TEST(RingRRR, subtract)
{
  RRR *R = RRR::create(100);
  testRingSubtract(R, ntrials);
}
TEST(RingRRR, multDivide)
{
  RRR *R = RRR::create(100);
  RingElementGenerator<RRR> gen(*R);
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
          EXPECT_TRUE(almostEqual(R,98,d,a));
        }
    }

}
TEST(RingRRR, axioms)
{
  RRR *R = RRR::create(100);
  RingElementGenerator<RRR> gen(*R);
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
      EXPECT_TRUE(almostEqual(R,98,d,e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      d = R->add(a, R->add(b,c));
      e = R->add(R->add(a,b), c);
      EXPECT_TRUE(almostEqual(R,94,d,e));
      d = R->mult(a, R->mult(b,c));
      e = R->mult(R->mult(a,b), c);
      EXPECT_TRUE(almostEqual(R,94,d,e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      d = R->mult(a, R->add(b,c));
      e = R->add(R->mult(a,b), R->mult(a,c));
      EXPECT_TRUE(almostEqual(R,94,d,e));
    }

}
TEST(RingRRR, power)
{
  RRR *R = RRR::create(100);

  mpz_t gmp1;
  mpz_init(gmp1);
  RingElementGenerator<RRR> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      //TODO: what should the answer here be?
      //EXPECT_TRUE(R->is_equal(R->power(a, 0), R->one())); // 0^0 == 1 too?
      EXPECT_TRUE(R->is_equal(R->power(a, 1), a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      //std::cout << "(" << e1 << "," << e2 << ")" << std::endl;
      ring_elem b = R->power(a, e1);
      ring_elem c = R->power(a, e2);
      ring_elem d = R->power(a, e1+e2);
      EXPECT_TRUE(almostEqual(R,96,R->mult(b,c),d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      ring_elem b1 = R->power(a, gmp1);
      EXPECT_TRUE(R->is_equal(b1, b));
    }
  mpz_clear(gmp1);
}
TEST(RingRRR, syzygy)
{
  // NOTE: RRR::syzygy, CCC::syzygy are not useful functions.
  // Should we remove these tests, and the corresponding functions?
  RRR *R = RRR::create(100);

  RingElementGenerator<RRR> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem u, v;
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      if (R->is_zero(b)) continue;

      // special cases (note: b != 0 for rest of routine)
      // syzygy(0,b) returns (1,0)
      R->syzygy(R->zero(),b,u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(R->is_equal(v, R->zero()));
      // syzygy(a,1) returns (1,-a)
      R->syzygy(a,R->one(),u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(almostEqual(R,98,v, R->negate(a)));
      // syzygy(a,-1) returns (1,a)
      R->syzygy(a,R->minus_one(),u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(almostEqual(R,98,v, a));
      R->syzygy(a,b,u,v);
      ring_elem result = R->add(R->mult(a,u), R->mult(b,v));
      EXPECT_TRUE(almostEqual(R, 95, result, R->zero()));
    }
}
////////////////////////////////////////////////////////
TEST(RingCCC, create)
{
  Ring *R = CCC::create(100);
  EXPECT_TRUE(R != 0);

  EXPECT_TRUE(dynamic_cast< const Z_mod * >(R) == 0);
  EXPECT_TRUE(dynamic_cast< const CCC * >(R) != 0);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_TRUE(R->is_CCC());
  EXPECT_EQ(ringName(*R), "CCC_100");
}
TEST(RingCCC, ones)
{
  Ring *R = CCC::create(100);
  EXPECT_TRUE(R->is_equal(R->one(), R->from_int(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_int(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_int(0)));
  EXPECT_TRUE(R->is_zero(R->from_int(0)));
}
TEST(RingCCC, negate)
{
  CCC *R = CCC::create(100);
  testRingNegate(R, ntrials);
}
TEST(RingCCC, add)
{
  CCC *R = CCC::create(100);
  RingElementGenerator<CCC> gen(*R);

  for (int i=0; i<ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->add(a,b);
      ring_elem d = R->negate(b);
      ring_elem e = R->add(c,d); // should be a
      EXPECT_TRUE(almostEqual(R,98,a,e));
    }
}
TEST(RingCCC, subtract)
{
  CCC *R = CCC::create(100);
  testRingSubtract(R, ntrials);
}
TEST(RingCCC, multDivide)
{
  CCC *R = CCC::create(100);
  RingElementGenerator<CCC> gen(*R);
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
          EXPECT_TRUE(almostEqual(R,94,d,a));
        }
    }

}
TEST(RingCCC, axioms)
{
  CCC *R = CCC::create(100);
  RingElementGenerator<CCC> gen(*R);
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
      EXPECT_TRUE(almostEqual(R,98,d,e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      d = R->add(a, R->add(b,c));
      e = R->add(R->add(a,b), c);
      EXPECT_TRUE(almostEqual(R,94,d,e));
      d = R->mult(a, R->mult(b,c));
      e = R->mult(R->mult(a,b), c);
      EXPECT_TRUE(almostEqual(R,94,d,e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      d = R->mult(a, R->add(b,c));
      e = R->add(R->mult(a,b), R->mult(a,c));
#if 0
      mpfr_printf("a=(%.20Rf,%.20Rf)\n",BIGCC_RE(a), BIGCC_IM(a));
      mpfr_printf("b=(%.20Rf,%.20Rf)\n",BIGCC_RE(b), BIGCC_IM(b));
      mpfr_printf("a*(b+c)=(%.20Rf,%.20Rf)\n",BIGCC_RE(d), BIGCC_IM(d));
      mpfr_printf("a*b+a*c=(%.20Rf,%.20Rf)\n",BIGCC_RE(e), BIGCC_IM(e));
#endif
      EXPECT_TRUE(almostEqual(R,92,d,e));
    }

}
TEST(RingCCC, power)
{
  CCC *R = CCC::create(100);

  mpz_t gmp1;
  mpz_init(gmp1);
  RingElementGenerator<CCC> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      //TODO: what should the answer here be?
      //EXPECT_TRUE(R->is_equal(R->power(a, 0), R->one())); // 0^0 == 1 too?
      EXPECT_TRUE(R->is_equal(R->power(a, 1), a));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      //std::cout << "(" << e1 << "," << e2 << ")" << std::endl;
      ring_elem b = R->power(a, e1);
      ring_elem c = R->power(a, e2);
      ring_elem d = R->power(a, e1+e2);
      ring_elem e = R->mult(b,c);
#if 0
      mpfr_printf("b=(%.30Rf,%.30Rf)\n",BIGCC_RE(b), BIGCC_IM(b));
      mpfr_printf("c=(%.30Rf,%.30Rf)\n",BIGCC_RE(c), BIGCC_IM(c));
      mpfr_printf("d=(%.30Rf,%.30Rf)\n",BIGCC_RE(d), BIGCC_IM(d));
      mpfr_printf("e=(%.30Rf,%.30Rf)\n",BIGCC_RE(e), BIGCC_IM(e));
#endif
      EXPECT_TRUE(almostEqual(R,80,R->mult(b,c),d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      ring_elem b1 = R->power(a, gmp1);
      EXPECT_TRUE(R->is_equal(b1, b));
    }
  mpz_clear(gmp1);
}
TEST(RingCCC, syzygy)
{
  // NOTE: CCC::syzygy, CCC::syzygy are not useful functions.
  // Should we remove these tests, and the corresponding functions?
  CCC *R = CCC::create(100);

  RingElementGenerator<CCC> gen(*R);
  for (int i=0; i<ntrials; i++)
    {
      ring_elem u, v;
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      if (R->is_zero(b)) continue;

      // special cases (note: b != 0 for rest of routine)
      // syzygy(0,b) returns (1,0)
      R->syzygy(R->zero(),b,u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(R->is_equal(v, R->zero()));
      // syzygy(a,1) returns (1,-a)
      R->syzygy(a,R->one(),u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(almostEqual(R,98,v, R->negate(a)));
      // syzygy(a,-1) returns (1,a)
      R->syzygy(a,R->minus_one(),u,v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(almostEqual(R,98,v, a));
      R->syzygy(a,b,u,v);
      ring_elem result = R->add(R->mult(a,u), R->mult(b,v));
      EXPECT_TRUE(almostEqual(R, 94, result, R->zero()));
    }
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
