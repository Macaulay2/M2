#ifndef __ring_test_hpp__
#define __ring_test_hpp__

#include "interface/random.h"

const int ntrials = 1000;
// const int ntrials = 1000000; // not good for the ssd - system swaps
// memory....

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

  R.set_from_long(a, 27);
  R.set_from_long(b, static_cast<int>(R.characteristic()) - 11);
  R.set_from_long(c, 16);
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

  // set_from_mpz
  mpz_set_str(base, "2131236127486324783264782364", 10);
  R.set_from_mpz(c, base);
  for (int i = -1000; i < 1000; i++)
    {
      mpz_set_si(m, i);
      mpz_add(m, m, base);   // m = base + i
      R.set_from_mpz(a, m);  // a = (base + i) mod charac
      R.set_from_long(b, i);
      R.add(b, c, b);                 // b = (base mod charac) + (i mod charac)
      EXPECT_TRUE(R.is_equal(a, b));  // a, b should be equal
    }

  // set_from_mpq
  for (int i = 1; i < 300; i++)
    {
      mpq_set_si(n1, 43999, i);
      mpq_canonicalize(n1);  // n1 = 43999/i

      // check that (43999 mod charac)/(i mod charac) == n1 mod charac
      // if (i mod charac) is not zero.
      if (R.characteristic() == 0 or (i % R.characteristic()) == 0) continue;
      bool ok = R.set_from_mpq(a, n1);
      EXPECT_TRUE(ok);
      R.set_from_long(b, 43999);
      R.set_from_long(c, i);
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
  typename T::ElementType a, b, c, d, zero;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(zero);
  R.set_from_long(zero, 0);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      gen.nextElement(d);
      R.mult(c, a, zero);
      EXPECT_TRUE(R.is_equal(c, zero));
      // TODO: finish this with more tests
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
  R.clear(zero);
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
  R.set_from_long(zero, 0);
  for (int i = 0; i < ntrials; i++)
    {
      // c = a*b
      // c//a == b
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(c);
      gen.nextElement(d);
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
  R.set_from_long(one, 1);
  for (int i = 0; i < ntrials; i++)
    {
      // c = 1/a
      // 1/a * a == 1
      gen.nextElement(a);
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
  ARingElementGenerator<T> gen(R);
  typename T::ElementType a, b, c, d, one;
  int q = static_cast<int>(R.cardinality());
  R.init(one);
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.set_from_long(one, 1);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);

      R.power(c, a, q);
      EXPECT_TRUE(R.is_equal(c, a));  // test a^q == q

      if (R.is_zero(a)) continue;

      R.power(c, a, q - 1);
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
  R.clear(b);
  R.clear(c);
  R.clear(d);
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

  // TODO: test promote, lift, syzygy(?), (ringmaps)
  // test random number generation?
  // get generator
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
