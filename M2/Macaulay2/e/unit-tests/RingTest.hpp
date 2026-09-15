// Copyright 2013 Michael E. Stillman

#ifndef M2_UNITTESTS__RING_TEST_HPP__
#  define M2_UNITTESTS__RING_TEST_HPP__

#  include <gtest/gtest.h>

#  include <istream>
#  include <string>

#  include "unit-tests/RingElem.hpp"

#  include "interface/random.h"
#  include "rings/ZZ.hpp"
#  include "exceptions.hpp"

const int ntrials = 100;

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

// Keep each property independent of the order in which GoogleTest runs it.
inline void seedRingRandom(unsigned long seed = 0x52494e47)
{
  mpz_t value;
  mpz_init_set_ui(value, seed);
  rawSetRandomSeed(value);
  mpz_clear(value);
}

template <typename T>
::testing::AssertionResult ringEquals(const T* R,
                                      ring_elem expected,
                                      ring_elem actual)
{
  if (R->is_equal(expected, actual)) return ::testing::AssertionSuccess();
  return ::testing::AssertionFailure() << "expected " << RingElem(R, expected)
                                       << ", got " << RingElem(R, actual);
}

template <typename T>
void testRingNegate(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // Negation must cancel the original input.
      ring_elem a = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a));
      ring_elem b = R->negate(a);
      ring_elem c = R->add(a, b);
      EXPECT_TRUE(R->is_zero(c));
    }
}
template <typename T>
void testRingAdd(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a+b) + (-b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      ring_elem c = R->add(a, b);
      ring_elem d = R->negate(b);
      ring_elem e = R->add(c, d);  // should be a
      EXPECT_TRUE(ringEquals(R, a, e));
    }
}
template <typename T>
void testRingSubtract(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a-b) + (b) == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      ring_elem c = R->subtract(a, b);
      ring_elem e = R->add(c, b);
      EXPECT_TRUE(ringEquals(R, a, e));
    }
}
template <typename T>
void testRingDivide(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a*b) // b == a
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      ring_elem c = R->mult(a, b);
      if (R->is_zero(b))
        EXPECT_TRUE(R->is_zero(c));
      else
        {
          ring_elem d = R->divide(c, b);
          EXPECT_TRUE(ringEquals(R, a, d));
        }
    }
}

template <typename T>
void testRingAxioms(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      ring_elem c = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b) << ", c=" << RingElem(R, c));

      // Swapping operands preserves sums and products.
      {
        SCOPED_TRACE("commutativity");
        ring_elem d = R->add(a, b);
        ring_elem e = R->add(b, a);
        EXPECT_TRUE(ringEquals(R, d, e));
        d = R->mult(a, b);
        e = R->mult(b, a);
        EXPECT_TRUE(ringEquals(R, d, e));
      }
      // Grouping cannot change exact ring arithmetic.
      {
        SCOPED_TRACE("associativity");
        ring_elem d = R->add(a, R->add(b, c));
        ring_elem e = R->add(R->add(a, b), c);
        EXPECT_TRUE(ringEquals(R, d, e));
        d = R->mult(a, R->mult(b, c));
        e = R->mult(R->mult(a, b), c);
        EXPECT_TRUE(ringEquals(R, d, e));
      }
      // Expanding a product agrees with the sum of products.
      {
        SCOPED_TRACE("distributivity");
        ring_elem d = R->mult(a, R->add(b, c));
        ring_elem e = R->add(R->mult(a, b), R->mult(a, c));
        EXPECT_TRUE(ringEquals(R, d, e));
      }
    }
}
template <typename T>
void testRingPower(const T* R, int ntrials)
{
  mpz_t gmp1;
  mpz_init(gmp1);
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a));
      EXPECT_TRUE(ringEquals(R, R->one(), R->power(a, 0)));
      EXPECT_TRUE(ringEquals(R, a, R->power(a, 1)));

      int e1 = rawRandomInt(10) + 1;
      int e2 = rawRandomInt(10) + 1;
      SCOPED_TRACE(::testing::Message() << "exponents " << e1 << ", " << e2);
      ring_elem b = R->power(a, e1);
      ring_elem c = R->power(a, e2);
      ring_elem d = R->power(a, e1 + e2);
      EXPECT_TRUE(ringEquals(R, R->mult(b, c), d));

      // Make sure that powers via mpz work (at least for small exponents)
      mpz_set_si(gmp1, e1);
      ring_elem b1 = R->power(a, gmp1);
      EXPECT_TRUE(ringEquals(R, b1, b));
    }
  mpz_clear(gmp1);
}
template <typename T>
void testRingGCD(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));

      // The gcd divides each input and the Bezout coefficients reconstruct it.
      ring_elem c = R->gcd(a, b);
      ring_elem u, v;
      ring_elem d = R->gcd_extended(a, b, u, v);

      EXPECT_TRUE(ringEquals(R, c, d));
      EXPECT_TRUE(ringEquals(R, c, R->add(R->mult(a, u), R->mult(b, v))));
      if (!R->is_zero(c))
        {
          EXPECT_TRUE(ringEquals(R, a, R->mult(R->divide(a, c), c)));
          EXPECT_TRUE(ringEquals(R, b, R->mult(R->divide(b, c), c)));
        }
    }
}
template <typename T>
void testRingRemainder(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));

      if (R->is_zero(b)) continue;  // Remainder requires a nonzero divisor.
      ring_elem r = R->remainder(a, b);
      ring_elem q = R->quotient(a, b);
      ring_elem r1, q1;
      r1 = R->remainderAndQuotient(a, b, q1);

      EXPECT_TRUE(ringEquals(R, r, r1));
      EXPECT_TRUE(ringEquals(R, q, q1));
      ring_elem a1 = R->add(R->mult(q, b), r);
      EXPECT_TRUE(ringEquals(R, a, a1));
    }
}
template <typename T>
void testRingSyzygy(const T* R, int ntrials)
{
  seedRingRandom();
  SCOPED_TRACE("seed 0x52494e47");
  RingElementGenerator<T> gen(*R);
  for (int i = 0; i < ntrials; i++)
    {
      ring_elem a = gen.nextElement();
      ring_elem b = gen.nextElement();
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ", a=" << RingElem(R, a)
                   << ", b=" << RingElem(R, b));
      if (R->is_zero(b)) continue;

      // A zero first operand yields the trivial unit relation.
      {
        SCOPED_TRACE("syzygy: zero first operand, nonzero second operand");
        ring_elem u, v;
        R->syzygy(R->zero(), b, u, v);
        EXPECT_TRUE(ringEquals(R, u, R->one()));
        EXPECT_TRUE(ringEquals(R, v, R->zero()));
      }
      // A unit second operand fixes the first coefficient to one.
      {
        SCOPED_TRACE("syzygy: second operand one");
        ring_elem u, v;
        R->syzygy(a, R->one(), u, v);
        EXPECT_TRUE(ringEquals(R, u, R->one()));
        EXPECT_TRUE(ringEquals(R, v, R->negate(a)));
      }
      // A negative unit changes the sign of the second coefficient.
      {
        SCOPED_TRACE("syzygy: second operand minus one");
        ring_elem u, v;
        R->syzygy(a, R->minus_one(), u, v);
        EXPECT_TRUE(ringEquals(R, u, R->one()));
        EXPECT_TRUE(ringEquals(R, v, a));
      }
      // General nonzero divisors must cancel both input products.
      {
        SCOPED_TRACE("syzygy: nonzero second operand");
        ring_elem u, v;
        R->syzygy(a, b, u, v);
        ring_elem result = R->add(R->mult(a, u), R->mult(b, v));
        EXPECT_TRUE(R->is_zero(result));
      }
    }

  // over ZZ:
  // syzygy(a,b) returns (b/c, -a/c), where c = +- gcd(a,b), with same sign as b
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
