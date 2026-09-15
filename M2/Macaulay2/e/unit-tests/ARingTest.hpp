#ifndef M2_UNITTESTS__RING_TEST_HPP__
#define M2_UNITTESTS__RING_TEST_HPP__

#include "interface/random.h"
#include "exceptions.hpp"  // for exc::division_by_zero_error

#include "exceptions.hpp"
#include "matrices/matrix-con.hpp"
#include "matrices/matrix.hpp"
#include "ringmap.hpp"
#include "free-modules/freemod.hpp"
#include "rings/ring.hpp"

const int ntrials = 1000;
// const int ntrials = 1000000; // not good for the ssd - system swaps
// memory....

// Give generated cases a repeatable GMP random stream. The caller traces the seed.
inline void seedRandom(unsigned long seed)
{
  mpz_t value;
  mpz_init_set_ui(value, seed);
  rawSetRandomSeed(value);
  mpz_clear(value);
}

template <typename T>
std::string describeElement(const T& R, const typename T::ElementType& value)
{
  buffer out;
  R.elem_text_out(out, value, true, false, false);
  return out.str();
}

template <typename RingType>
void getElement(const RingType& R,
                int index,
                typename RingType::ElementType& result);

template <typename RingType>
class ARingElementGenerator
{
 public:
  ARingElementGenerator(const RingType& R) : mRing(R), mNext(0) {}
  void nextElement(typename RingType::ElementType& result)
  {
    getElement<RingType>(mRing, ++mNext, result);
  }
  void reset() { mNext = 0; }
 private:
  const RingType& mRing;
  int mNext;
};

template <typename T>
std::istream& fromStream(std::istream& i,
                         const T& R,
                         typename T::ElementType& result);

template <typename T>
void testSomeMore(const T& R)
{
  typename T::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);

  R.set(a, 27);
  R.set(b, static_cast<int>(R.characteristic()) - 11);
  R.set(c, 16);
  R.add(d, a, b);

  buffer o;
  o << "a=";
  R.elem_text_out(o, a, true);
  o << " b=";
  R.elem_text_out(o, b, true);
  o << " c=";
  R.elem_text_out(o, c, true);
  o << " d=a+b=";
  R.elem_text_out(o, d, true);
  std::cout << o.str() << std::endl;

  EXPECT_TRUE(R.is_equal(c, d));

  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template <typename T>
void testNegate(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b;
  R.init(a);
  R.init(b);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));
      R.negate(b, a);
      R.add(b, a, b);
      EXPECT_TRUE(R.is_zero(b));  // test: (-a) + a == 0
    }
  R.clear(a);
  R.clear(b);
}

template <typename T>
std::string ringName(const T& R)
{
  buffer o;
  R.text_out(o);
  std::string result = o.str();
  return result;
}

template <typename T>
void testCoercions(const T& R)
{
  typename T::ElementType a, b, c;
  mpz_t m, base;
  mpq_t n1;
  R.init(a);
  R.init(b);
  R.init(c);
  mpz_init(m);
  mpz_init(base);
  mpq_init(n1);

  // set
  mpz_set_str(base, "2131236127486324783264782364", 10);
  R.set(c, base);
  for (int i = -1000; i < 1000; i++)
    {
      mpz_set_si(m, i);
      mpz_add(m, m, base);   // m = base + i
      R.set(a, m);  // a = (base + i) mod charac
      R.set(b, i);
      R.add(b, c, b);                 // b = (base mod charac) + (i mod charac)
      EXPECT_TRUE(R.is_equal(a, b));  // a, b should be equal
    }

  // set
  for (int i = 1; i < 300; i++)
    {
      mpq_set_si(n1, 43999, i);
      mpq_canonicalize(n1);  // n1 = 43999/i

      // check that (43999 mod charac)/(i mod charac) == n1 mod charac
      // if (i mod charac) is not zero.
      if (R.characteristic() == 0 or (i % R.characteristic()) == 0) continue;
      bool ok = R.set(a, n1);
      EXPECT_TRUE(ok);
      R.set(b, 43999);
      R.set(c, i);
      if (!R.is_zero(c))
        {
          R.divide(c, b, c);
          EXPECT_TRUE(R.is_equal(a, c));
        }
    }

  R.clear(a);
  R.clear(b);
  R.clear(c);
  mpz_clear(m);
  mpz_clear(base);
  mpq_clear(n1);
}

template <typename T>
void testAxioms(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b, c, d, e, f;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(e);
  R.init(f);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b) << ", c=" << describeElement(R, c));

      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      R.add(d, a, b);
      R.add(e, b, a);
      EXPECT_TRUE(R.is_equal(d, e));
      R.mult(d, a, b);
      R.mult(e, b, a);
      EXPECT_TRUE(R.is_equal(d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(d, b, c);
      R.add(d, a, d);
      R.add(e, a, b);
      R.add(e, e, c);
      EXPECT_TRUE(R.is_equal(d, e));
      R.mult(d, b, c);
      R.mult(d, a, d);
      R.mult(e, a, b);
      R.mult(e, e, c);
      EXPECT_TRUE(R.is_equal(d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(d, b, c);
      R.mult(d, a, d);
      R.mult(e, a, b);
      R.mult(f, a, c);
      R.add(e, e, f);
      EXPECT_TRUE(R.is_equal(d, e));

      // test: (b+c)*a == b*a + c*a
      R.add(d, b, c);
      R.mult(d, d, a);
      R.mult(e, b, a);
      R.mult(f, c, a);
      R.add(e, e, f);
      EXPECT_TRUE(R.is_equal(d, e));

      // Test identities
      // test: a+0 == a, a*1 == a, a*0 == 0
      R.set_zero(f);
      R.add(d, a, f);
      EXPECT_TRUE(R.is_equal(d, a));
      R.mult(d, a, f);
      EXPECT_TRUE(R.is_zero(d));
      R.set(f, 1);
      R.mult(d, a, f);
      EXPECT_TRUE(R.is_equal(d, a));

      // test: a-a == 0
      R.subtract(d, a, a);
      EXPECT_TRUE(R.is_zero(d));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
  R.clear(e);
  R.clear(f);
}

template <typename T>
void testAdd(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b));
      R.add(c, a, b);  // c = a+b
      R.negate(d, b);  // d = -b

#if 0
      buffer o;
      o << "a=";
      R.elem_text_out(o, a , true, false, false);
      o << " b=";
      R.elem_text_out(o, b , true, false, false);
      o << " a+b=";
      R.elem_text_out(o, c , true, false, false);
      o << " -b=";
      R.elem_text_out(o, d , true, false, false);
#endif

      R.add(d, c, d);  // d = (a+b) + (-b)

#if 0
      o << " a=";
      R.elem_text_out(o, d , true, false, false);
      std::cout << o.str() << std::endl;
#endif
      EXPECT_TRUE(R.is_equal(d, a));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template <typename T>
void testSubtract(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      gen.nextElement(d);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b) << ", c=" << describeElement(R, c) << ", d=" << describeElement(R, d));
      R.add(c, a, b);       // c = a+b
      R.subtract(d, c, b);  // d = (a+b) - b
      EXPECT_TRUE(R.is_equal(d, a));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template <typename T>
void testMultiply(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b, c, d, zero, one;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(zero);
  R.init(one);
  R.set(zero, 0);
  R.set(one, 1);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b));

      R.mult(c, a, zero);
      EXPECT_TRUE(R.is_equal(c, zero));

      // a*1 == a
      R.mult(c, a, one);
      EXPECT_TRUE(R.is_equal(c, a));

      // multiplying by 2 and 3 agrees with repeated addition
      R.add(c, a, a);
      R.set(d, 2);
      R.mult(d, a, d);
      EXPECT_TRUE(R.is_equal(c, d));

      R.add(c, c, a);
      R.set(d, 3);
      R.mult(d, a, d);
      EXPECT_TRUE(R.is_equal(c, d));

      // a*(-b) == -(a*b)
      R.negate(c, b);
      R.mult(c, a, c);
      R.mult(d, a, b);
      R.negate(d, d);
      EXPECT_TRUE(R.is_equal(c, d));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
  R.clear(zero);
  R.clear(one);
}

template <typename T>
void testDivide(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b, c, d, zero;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(zero);
  R.set(zero, 0);
  for (int i = 0; i < ntrials; i++)
    {
      // c = a*b
      // c//a == b
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      gen.nextElement(d);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b) << ", c=" << describeElement(R, c) << ", d=" << describeElement(R, d));
      if (R.is_zero(a)) continue;
      R.mult(c, a, b);
      R.divide(d, c, a);
      EXPECT_TRUE(R.is_equal(b, d));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
  R.clear(zero);
}

template <typename T>
void testReciprocal(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b, c, one;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(one);
  R.set(one, 1);
  for (int i = 0; i < ntrials; i++)
    {
      // c = 1/a
      // 1/a * a == 1
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));
      if (R.is_zero(a)) continue;
      R.invert(b, a);
      R.mult(c, b, a);
      EXPECT_TRUE(R.is_equal(c, one));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(one);
}

template <typename T>
void testPower(const T& R, int ntrials)
{
  // test the following: (x=generator of the finite field, q = card of field)
  // check: x^i != x, for 2 <= i <= characteristic-??
  // x^q == x
  // x^(q-1) == 1
  // x^(-1) * x == 1
  // x^(-2) * x^2 == 1

  // a^2 == a*a, for various a
  // a^3 == a*a*a
  // a^0 == 1, what if a == 0?
  // 1^n == 1, various n
  // q goes through power_mpz: the cardinality need not fit in power()'s
  // exponent argument (int here, int32_t for ffpack, long for flint).
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, c, d, one;
  mpz_t q, qminus1;
  mpz_init(q);
  mpz_init(qminus1);
  mpz_set_ui(q, static_cast<unsigned long>(R.cardinality()));
  mpz_sub_ui(qminus1, q, 1);

  R.init(one);
  R.init(a);
  R.init(c);
  R.init(d);
  R.set(one, 1);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));

      R.power_mpz(c, a, q);
      EXPECT_TRUE(R.is_equal(c, a));  // test a^q == a

      R.power(c, a, 0);  // test a^0 == 1, including a == 0
      EXPECT_TRUE(R.is_equal(c, one));

      R.power(c, one, i);  // test 1^n == 1
      EXPECT_TRUE(R.is_equal(c, one));

      R.power(c, a, 2);  // test a^2 == a*a
      R.mult(d, a, a);
      EXPECT_TRUE(R.is_equal(c, d));

      R.power(c, a, 3);  // test a^3 == a*a*a
      R.mult(d, d, a);
      EXPECT_TRUE(R.is_equal(c, d));

      if (R.is_zero(a)) continue;

      R.power_mpz(c, a, qminus1);
      EXPECT_TRUE(R.is_equal(c, one));  // test a^(q-1) == 1

      R.power(c, a, -1);  // test a^-1 * a == 1
      R.mult(c, a, c);
      EXPECT_TRUE(R.is_equal(c, one));

      R.power(c, a, -2);  // test a^-2 * a^3 == a
      R.power(d, a, 3);
      R.mult(d, c, d);
      EXPECT_TRUE(R.is_equal(d, a));
    }
  R.clear(a);
  R.clear(c);
  R.clear(d);
  R.clear(one);
  mpz_clear(q);
  mpz_clear(qminus1);
}

// Division by zero throws, and every nonzero element of a field is a unit.
template <typename T>
void testFieldDivideByZero(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, c, zero, one;
  R.init(a);
  R.init(c);
  R.init(zero);
  R.init(one);
  R.set_zero(zero);
  R.set(one, 1);

  EXPECT_THROW(R.invert(c, zero), exc::division_by_zero_error);
  EXPECT_FALSE(R.is_unit(zero));

  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));
      EXPECT_THROW(R.divide(c, a, zero), exc::division_by_zero_error);
      if (R.is_zero(a)) continue;

      EXPECT_TRUE(R.is_unit(a));
      R.divide(c, zero, a);  // 0/a == 0
      EXPECT_TRUE(R.is_zero(c));
      R.divide(c, a, a);  // a/a == 1
      EXPECT_TRUE(R.is_equal(c, one));
    }
  R.clear(a);
  R.clear(c);
  R.clear(zero);
  R.clear(one);
}

template <typename T>
void testFiniteField(const T& R, int ntrials)
{
  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);       // fails in char 2, ffpack (negating 1 gives -1)...
  testSubtract(R, ntrials);  // fails in char 2, ffpack
  testMultiply(R, ntrials);
  testDivide(R, ntrials);  // fails in char 2, ffpack
  testReciprocal(R, ntrials);
  testPower(R, ntrials);  // fails?
  testAxioms(R, ntrials);
  testFieldDivideByZero(R, ntrials);

  // TODO: test promote, lift, syzygy(?), (ringmaps)
  // test random number generation?
  // get generator
}

////////////////////////////////////////////////////////////////////
// Generic ARing contracts, for any ring with a getElement<>
// specialization.  Call these rather than restating them.
////////////////////////////////////////////////////////////////////

// set/copy/init_set are deep copies, set_zero zeroes, swap exchanges.
// The only helper that copies with set(); the rest use copy(), since a
// ring with an integral ElementType has no set(e, e) overload.
template <typename T>
void testStorage(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R), b(R), saved(R), zero(R);
  R.set_zero(zero);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));
      R.set(saved, a);

      R.set(b, a);
      EXPECT_TRUE(R.is_equal(b, saved));
      R.set_zero(a);
      EXPECT_TRUE(R.is_zero(a));
      EXPECT_TRUE(R.is_equal(b, saved)) << "set: shares storage with source";

      R.set(a, saved);
      R.copy(b, a);
      EXPECT_TRUE(R.is_equal(b, saved));
      R.set_zero(a);
      EXPECT_TRUE(R.is_equal(b, saved)) << "copy: shares storage with source";

      // init_set, via Element(R, a)
      R.set(a, saved);
      {
        typename T::Element c(R, a);
        EXPECT_TRUE(R.is_equal(c, saved));
        R.set_zero(a);
        EXPECT_TRUE(R.is_equal(c, saved)) << "init_set: shares storage";
      }

      R.set(a, saved);
      R.set(b, zero);
      R.swap(a, b);
      EXPECT_TRUE(R.is_zero(a));
      EXPECT_TRUE(R.is_equal(b, saved));
    }
}

// is_equal, compare_elems and computeHashValue agree with each other.
template <typename T>
void testComparisons(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R), b(R), c(R);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b));
      R.copy(c, a);

      EXPECT_TRUE(R.is_equal(a, a));
      EXPECT_EQ(R.compare_elems(a, a), 0);
      EXPECT_TRUE(R.is_equal(a, c));
      EXPECT_EQ(R.compare_elems(a, c), 0);
      EXPECT_EQ(R.is_equal(a, b), R.compare_elems(a, b) == 0);

      int cmp = R.compare_elems(a, b);
      EXPECT_EQ(cmp, -R.compare_elems(b, a));
      EXPECT_GE(cmp, -1);
      EXPECT_LE(cmp, 1);

      EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(c));
    }
}

// The output of an arithmetic operation may alias its inputs.
template <typename T>
void testAliasing(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R), b(R), expected(R), result(R);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b));

      R.add(expected, a, b);
      R.copy(result, a);
      R.add(result, result, b);
      EXPECT_TRUE(R.is_equal(result, expected)) << "add: result aliases a";
      R.copy(result, b);
      R.add(result, a, result);
      EXPECT_TRUE(R.is_equal(result, expected)) << "add: result aliases b";

      R.subtract(expected, a, b);
      R.copy(result, a);
      R.subtract(result, result, b);
      EXPECT_TRUE(R.is_equal(result, expected)) << "subtract: result aliases a";
      R.copy(result, b);
      R.subtract(result, a, result);
      EXPECT_TRUE(R.is_equal(result, expected)) << "subtract: result aliases b";

      R.mult(expected, a, b);
      R.copy(result, a);
      R.mult(result, result, b);
      EXPECT_TRUE(R.is_equal(result, expected)) << "mult: result aliases a";
      R.copy(result, b);
      R.mult(result, a, result);
      EXPECT_TRUE(R.is_equal(result, expected)) << "mult: result aliases b";

      R.mult(expected, a, a);
      R.copy(result, a);
      R.mult(result, result, result);
      EXPECT_TRUE(R.is_equal(result, expected)) << "mult: all three alias";

      R.negate(expected, a);
      R.copy(result, a);
      R.negate(result, result);
      EXPECT_TRUE(R.is_equal(result, expected)) << "negate: result aliases a";

      // subtract_multiple accumulates: result -= a*b
      R.mult(expected, a, b);
      R.negate(expected, expected);
      R.add(expected, a, expected);
      R.copy(result, a);
      R.subtract_multiple(result, a, b);
      EXPECT_TRUE(R.is_equal(result, expected)) << "subtract_multiple: a - a*b";
    }
}

// subtract_multiple accumulates result -= a*b rather than overwriting.
template <typename T>
void testSubtractMultiple(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R), b(R), c(R), expected(R), result(R);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b) << ", c=" << describeElement(R, c));

      R.mult(expected, a, b);
      R.subtract(expected, c, expected);
      R.copy(result, c);
      R.subtract_multiple(result, a, b);
      EXPECT_TRUE(R.is_equal(result, expected));

      // subtracting a zero multiple is a no-op
      R.copy(result, c);
      R.set_zero(a);
      R.subtract_multiple(result, a, b);
      EXPECT_TRUE(R.is_equal(result, c));
    }
}

// For a coefficient ring every variable maps to 1.
template <typename T>
void testCoefficientRingSetVar(const T& R)
{
  typename T::Element a(R), one(R);
  R.set(one, 1);
  for (int v = 0; v < 5; v++)
    {
      SCOPED_TRACE(v);
      R.set_var(a, v);
      EXPECT_TRUE(R.is_equal(a, one));
    }
}

// power and power_mpz agree, and a^n is the n-fold product.  power's
// exponent type varies by ring, so stay within [0, maxExponent].
template <typename T>
void testPowerAgreement(const T& R, int ntrials, int maxExponent = 16)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R), b(R), c(R), d(R), one(R);
  R.set(one, 1);
  mpz_t n;
  mpz_init(n);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));
      R.copy(d, one);
      for (int e = 0; e <= maxExponent; e++)
        {
          SCOPED_TRACE(e);
          R.power(b, a, e);
          mpz_set_si(n, e);
          R.power_mpz(c, a, n);
          EXPECT_TRUE(R.is_equal(b, c)) << "power and power_mpz disagree";
          EXPECT_TRUE(R.is_equal(b, d)) << "a^n is not the n-fold product";
          R.mult(d, d, a);
        }
      R.power(b, a, 0);
      EXPECT_TRUE(R.is_equal(b, one)) << "a^0 != 1";
    }
  mpz_clear(n);
}

// power_mpz rejects exponents that do not fit in an int.
template <typename T>
void testPowerMpzOutOfRange(const T& R)
{
  typename T::Element a(R), b(R);
  R.set(a, 2);
  mpz_t n;
  mpz_init(n);

  mpz_set_str(n, "4294967296", 10);  // 2^32, too large for an int
  EXPECT_THROW(R.power_mpz(b, a, n), exc::engine_error);

  mpz_set_str(n, "-4294967296", 10);
  EXPECT_THROW(R.power_mpz(b, a, n), exc::engine_error);

  mpz_clear(n);
}

// to_ring_elem and from_ring_elem are inverse to each other.
template <typename T>
void testRingElemRoundTrip(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R), b(R);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));
      ring_elem f;
      R.to_ring_elem(f, a);
      R.from_ring_elem(b, f);
      EXPECT_TRUE(R.is_equal(a, b));
    }
}

// from_ring_elem_const must agree with from_ring_elem.  ASSERT, not
// EXPECT: a ring that gets this wrong gets it wrong on every element.
template <typename T>
void testFromRingElemConst(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a));
      ring_elem f;
      R.to_ring_elem(f, a);
      ASSERT_TRUE(R.is_equal(a, R.from_ring_elem_const(f)));
      ASSERT_EQ(R.compare_elems(a, R.from_ring_elem_const(f)), 0);
    }
}

// syzygy returns (x, y) with a*x + b*y == 0 and x, y not both zero.
template <typename T>
void testSyzygy(const T& R, int ntrials)
{
  ARingElementGenerator<T> gen(R);
  typename T::Element a(R), b(R), x(R), y(R), u(R), v(R);
  for (int i = 0; i < ntrials; i++)
    {
      SCOPED_TRACE(i);
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message() << "trial " << i << ", a=" << describeElement(R, a) << ", b=" << describeElement(R, b));
      if (R.is_zero(b)) continue;  // syzygy asserts b is nonzero
      R.syzygy(a, b, x, y);
      R.mult(u, a, x);
      R.mult(v, b, y);
      R.add(u, u, v);
      EXPECT_TRUE(R.is_zero(u)) << "a*x + b*y != 0";
      EXPECT_FALSE(R.is_zero(x) and R.is_zero(y)) << "trivial syzygy";
    }
}

// A ring map with the given target.  ARing::eval only consults
// map->get_ring(), so a 1x1 identity map is enough to drive it.
inline const RingMap* identityRingMap(const Ring* R)
{
  FreeModule* F = R->make_FreeModule(1);
  MatrixConstructor mat(F, 1);
  mat.set_entry(0, 0, R->one());
  return RingMap::make(mat.to_matrix());
}

template <typename T>
void testARingInterface(const T& R)
{
  // this test makes sure that all of the interface functions required
  // actually exist.

  const M2::RingID rid = R.ringID;
  std::cout << "ring ID: " << rid << std::endl;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
