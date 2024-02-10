// Copyright 2013 Michael E. Stillman

#ifndef __ring_test_hpp__
#define __ring_test_hpp__

#include <cstdio>
#include <string>
#include <iostream>
//#include <sstream>
#include <memory>
#include <gtest/gtest.h>
//#include <mpfr.h>

#include "interface/random.h"
#include "ZZ.hpp"
#include "exceptions.hpp"

const int ntrials = 100;  // 5000

template <typename T>
std::istream& fromStream(std::istream& i,
                         const T& R,
                         typename T::ElementType& result);

template <typename T>
bool fromStream(std::istream& i, const T& R, ring_elem& result);

template <typename T>
std::string ringName(const T& R)
{
  buffer o;
  R.text_out(o);
  std::string result = o.str();
  return result;
}

template <typename RingType>
ring_elem getElement(const RingType& R, int index);

template <typename RingType>
class RingElementGenerator
{
 public:
  RingElementGenerator(const RingType& R) : mRing(R), mNext(0) {}
  ring_elem nextElement() { return getElement<RingType>(mRing, ++mNext); }
  void reset() { mNext = 0; }
 private:
  const RingType& mRing;
  int mNext;
};

template <typename T>
void testRingCoercions(const T* R, int ntrials)
{
  // from int
  // from mpz
  // from rational
}
template <typename T>
void testRingNegate(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (-a) + (a) == a
      ring_elem a = gen.nextElement();
      ring_elem b = R->negate(a);
      ring_elem c = R->add(a, b);
      EXPECT_TRUE(R->is_zero(c));
    }
}
template <typename T>
void testRingAdd(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->add(a, b);
      ring_elem d = R->negate(b);
      ring_elem e = R->add(c, d);  // should be a
      EXPECT_TRUE(R->is_equal(e, a));
    }
}
template <typename T>
void testRingSubtract(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a-b) + (b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = R->subtract(a, b);
      ring_elem e = R->add(c, b);
      EXPECT_TRUE(R->is_equal(e, a));
    }
}
template <typename T>
void testRingDivide(const T* R, int ntrials)
{
  auto zero = R->zero();
  auto a = R->from_long(3);
  EXPECT_ANY_THROW(R->divide(a, zero));

  RingElementGenerator<T> gen(*R);
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
          EXPECT_TRUE(R->is_equal(d, a));
        }
    }
}

template <typename T>
void testRingAxioms(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
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
      EXPECT_TRUE(R->is_equal(d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      d = R->add(a, R->add(b, c));
      e = R->add(R->add(a, b), c);
      EXPECT_TRUE(R->is_equal(d, e));
      d = R->mult(a, R->mult(b, c));
      e = R->mult(R->mult(a, b), c);
      EXPECT_TRUE(R->is_equal(d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      d = R->mult(a, R->add(b, c));
      e = R->add(R->mult(a, b), R->mult(a, c));
      EXPECT_TRUE(R->is_equal(d, e));
    }
}
template <typename T>
void testRingPower(const T* R, int ntrials)
{
  mpz_t gmp1;
  mpz_init(gmp1);
  RingElementGenerator<T> gen(*R);
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
      EXPECT_TRUE(R->is_equal(R->mult(b, c), d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      ring_elem b1 = R->power(a, gmp1);
      EXPECT_TRUE(R->is_equal(b1, b));
    }
  mpz_clear(gmp1);
}
template <typename T>
void testRingGCD(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();

      // (a // gcd(a,b) == 0, b // gcd(a,b) == 0,
      ring_elem c = R->gcd(a, b);
      ring_elem u, v;
      ring_elem d = R->gcd_extended(a, b, u, v);

      EXPECT_TRUE(R->is_equal(c, d));
      EXPECT_TRUE(R->is_equal(c, R->add(R->mult(a, u), R->mult(b, v))));
      EXPECT_TRUE(R->is_equal(a, R->mult(R->divide(a, c), c)));
    }
}
template <typename T>
void testRingRemainder(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();

#if 0
      ring_elem c = R->remainder(a, R->zero()); // FAILS!!
      EXPECT_TRUE(R->is_equal(c,a));
#endif

      ring_elem r = R->remainder(a, b);
      ring_elem q = R->quotient(a, b);
      ring_elem r1, q1;
      r1 = R->remainderAndQuotient(a, b, q1);

      EXPECT_TRUE(R->is_equal(r, r1));
      EXPECT_TRUE(R->is_equal(q, q1));
      ring_elem a1 = R->add(R->mult(q, b), r);
      EXPECT_TRUE(R->is_equal(a, a1));
    }
}
template <typename T>
void testRingSyzygy(const T* R, int ntrials)
{
  RingElementGenerator<T> gen(*R);
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
      EXPECT_TRUE(R->is_equal(v, R->negate(a)));
      // syzygy(a,-1) returns (1,a)
      R->syzygy(a, R->minus_one(), u, v);
      EXPECT_TRUE(R->is_equal(u, R->one()));
      EXPECT_TRUE(R->is_equal(v, a));

      R->syzygy(a, b, u, v);
      ring_elem result = R->add(R->mult(a, u), R->mult(b, v));
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

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
