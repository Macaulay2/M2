// Unit tests for ARingRRdd — MultiFloats double-double (~106-bit) approximate real field.
// Mirrors the structure of ARingRRTest.cpp.

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-RRdd.hpp"
#include "ARingTest.hpp"

// |a - b| < 2^(-nbits) ? — ElementType is a 16-byte struct, so build epsilon
// and zero explicitly rather than relying on int → ElementType implicit conv.
bool almostEqual(const M2::ARingRRdd& R,
                 unsigned long nbits,
                 const M2::ARingRRdd::ElementType& a,
                 const M2::ARingRRdd::ElementType& b)
{
  M2::ARingRRdd::ElementType epsilon{pow(2, -static_cast<double>(nbits)), 0.0};
  M2::ARingRRdd::ElementType c;
  R.subtract(c, a, b);
  R.abs(c, c);
  return R.compare_elems(c, epsilon) < 0;
}

template <>
void getElement<M2::ARingRRdd>(const M2::ARingRRdd& R,
                               int index,
                               M2::ARingRRdd::ElementType& result)
{
  if (index < 50)
    R.set_from_long(result, index - 25);
  else
    R.random(result);
}

TEST(ARingRRdd, create)
{
  M2::ARingRRdd R;
  EXPECT_EQ(R.characteristic(), 0u);
  EXPECT_EQ(R.get_precision(), 106u);
}

TEST(ARingRRdd, negate)
{
  M2::ARingRRdd R;
  ARingElementGenerator<M2::ARingRRdd> gen(R);
  M2::ARingRRdd::ElementType a, b, c;
  R.init(a); R.init(b); R.init(c);
  for (int i = 0; i < ntrials; i++)
  {
    gen.nextElement(a);
    R.negate(b, a);
    R.add(c, a, b);
    EXPECT_TRUE(R.is_zero(c));   // exact: -a + a == 0
  }
  R.clear(c); R.clear(b); R.clear(a);
}

TEST(ARingRRdd, add)
{
  M2::ARingRRdd R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRRdd> gen(R);
  M2::ARingRRdd::ElementType a, b, c, d, e;
  R.init(a); R.init(b); R.init(c); R.init(d); R.init(e);
  for (int i = 0; i < ntrials; i++)
  {
    // (a + b) + (-b) == a (to ~nbits-2)
    gen.nextElement(a); gen.nextElement(b);
    R.add(c, a, b);
    R.negate(d, b);
    R.add(e, c, d);
    EXPECT_TRUE(almostEqual(R, nbits - 2, a, e));
  }
  R.clear(e); R.clear(d); R.clear(c); R.clear(b); R.clear(a);
}

TEST(ARingRRdd, subtract)
{
  M2::ARingRRdd R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRRdd> gen(R);
  M2::ARingRRdd::ElementType a, b, c, e, zero;
  R.init(a); R.init(b); R.init(c); R.init(e); R.init(zero);
  R.set_zero(zero);
  for (int i = 0; i < ntrials; i++)
  {
    gen.nextElement(a); gen.nextElement(b);
    // (a - b) + b == a
    R.subtract(c, a, b);
    R.add(e, c, b);
    EXPECT_TRUE(almostEqual(R, nbits - 2, a, e));
    // subtract_multiple: e := a*b; then e -= a*b; should be ~0
    R.mult(e, a, b);
    R.subtract_multiple(e, a, b);
    EXPECT_TRUE(almostEqual(R, nbits - 2, e, zero));
  }
  R.clear(zero); R.clear(e); R.clear(c); R.clear(b); R.clear(a);
}

TEST(ARingRRdd, multDivide)
{
  M2::ARingRRdd R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRRdd> gen(R);
  M2::ARingRRdd::ElementType a, b, c, d;
  R.init(a); R.init(b); R.init(c); R.init(d);
  for (int i = 0; i < ntrials; i++)
  {
    gen.nextElement(a); gen.nextElement(b);
    R.mult(c, a, b);
    if (R.is_zero(b))
      EXPECT_TRUE(R.is_zero(c));
    else
    {
      R.divide(d, c, b);             // (a*b)/b == a
      EXPECT_TRUE(almostEqual(R, nbits - 2, d, a));
    }
  }
  R.clear(d); R.clear(c); R.clear(b); R.clear(a);
}

TEST(ARingRRdd, axioms)
{
  M2::ARingRRdd R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRRdd> gen(R);
  M2::ARingRRdd::ElementType a, b, c, d, e;
  R.init(a); R.init(b); R.init(c); R.init(d); R.init(e);
  for (int i = 0; i < ntrials; i++)
  {
    gen.nextElement(a); gen.nextElement(b); gen.nextElement(c);

    // commutativity
    R.add(d, a, b); R.add(e, b, a);
    EXPECT_TRUE(almostEqual(R, nbits - 2, d, e));
    R.mult(d, a, b); R.mult(e, b, a);
    EXPECT_TRUE(almostEqual(R, nbits - 2, d, e));

    // associativity
    R.add(e, b, c); R.add(d, a, e);   // a + (b + c)
    R.add(e, a, b); R.add(e, e, c);   // (a + b) + c
    EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));
    R.mult(e, b, c); R.mult(d, a, e); // a * (b * c)
    R.mult(e, a, b); R.mult(e, e, c); // (a * b) * c
    EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));

    // distributivity: a * (b + c) == a*b + a*c
    R.add(e, b, c); R.mult(d, a, e);
    R.mult(b, a, b); R.mult(c, a, c); R.add(e, b, c);
    EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));
  }
  R.clear(e); R.clear(d); R.clear(c); R.clear(b); R.clear(a);
}

TEST(ARingRRdd, power_and_invert)
{
  M2::ARingRRdd R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRRdd> gen(R);
  M2::ARingRRdd::ElementType a, b, c, d;
  R.init(a); R.init(b); R.init(c); R.init(d);
  mpz_t gmp1; mpz_init(gmp1);
  for (int i = 0; i < ntrials; i++)
  {
    gen.nextElement(a);
    R.power(b, a, 1);
    EXPECT_TRUE(R.is_equal(b, a));            // a^1 == a (exact)

    int e1 = rawRandomInt(10) + 1;
    int e2 = rawRandomInt(10) + 1;
    R.power(b, a, e1);
    R.power(c, a, e2);
    R.power(d, a, e1 + e2);
    R.mult(c, b, c);                          // a^e1 * a^e2 == a^(e1+e2)
    EXPECT_TRUE(almostEqual(R, nbits - 4, c, d));

    mpz_set_si(gmp1, e1);
    R.power_mpz(d, a, gmp1);
    EXPECT_TRUE(R.is_equal(d, b));            // power_mpz matches int power
  }
  mpz_clear(gmp1);
  R.clear(d); R.clear(c); R.clear(b); R.clear(a);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
