// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "basic-rings/aring-RRR.hpp"
#include "unit-tests/ARingTest.hpp"

bool almostEqual(const M2::ARingRRR& R,
                 int nbits,
                 const M2::ARingRRR::ElementType& a,
                 const M2::ARingRRR::ElementType& b)
{
  mpfr_t epsilon;
  mpfr_init2(epsilon, R.get_precision());
  mpfr_set_ui_2exp(epsilon, 1, -nbits, MPFR_RNDN);

  M2::ARingRRR::ElementType c;
  R.init(c);
  R.subtract(c, a, b);
  bool ret = mpfr_cmpabs(&c, epsilon) < 0;

  R.clear(c);
  mpfr_clear(epsilon);
  return ret;
}

template <>
void getElement<M2::ARingRRR>(const M2::ARingRRR& R,
                              int index,
                              M2::ARingRRR::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

// void getElementRRR(const M2::ARingRRR&  R, int index,
// M2::ARingRRR::ElementType& result)
//{
//  if (index < 50) R.set(result, index-25);
//  else R.random(result);
//}

TEST(ARingRRR, create)
{
  M2::ARingRRR R(100);
  EXPECT_EQ(ringName(R), "ARRR_100");
  EXPECT_EQ(R.characteristic(), 0);
}

void testRingNegateRRR(const M2::ARingRRR& R, int ntrials)
{
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (-a) + (a) == 0
      gen.nextElement(a);
      R.negate(b, a);
      R.add(c, a, b);
      EXPECT_TRUE(R.is_zero(c));
    }
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, negate)
{
  M2::ARingRRR R(100);
  testRingNegateRRR(R, ntrials);
}

TEST(ARingRRR, add)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d, e;
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
      EXPECT_TRUE(almostEqual(R, 98, a, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, subtract)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(e);
  for (int i = 0; i < ntrials; i++)
    {
      // test: (a-b) + (b) == a
      gen.nextElement(a);
      gen.nextElement(b);
      R.subtract(c, a, b);
      R.add(e, c, b);  // should be a
      EXPECT_TRUE(almostEqual(R, 98, a, e));
      R.mult(e, a, b);
      R.subtract_multiple(e, a, b);
      EXPECT_TRUE(R.is_zero(e));
    }
  R.clear(e);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, multDivide)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d;
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
      if (R.is_zero(b))
        EXPECT_TRUE(R.is_zero(c));
      else
        {
          R.divide(d, c, b);
          EXPECT_TRUE(almostEqual(R, 94, d, a));
        }
    }
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, axioms)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d, e;
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
      EXPECT_TRUE(almostEqual(R, 98, d, e));
      R.mult(d, a, b);
      R.mult(e, b, a);
      EXPECT_TRUE(almostEqual(R, 98, d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(e, b, c);
      R.add(d, a, e);  // a+(b+c)
      R.add(e, a, b);
      R.add(e, e, c);  // (a+b)+c
      EXPECT_TRUE(almostEqual(R, 94, d, e));
      R.mult(e, b, c);
      R.mult(d, a, e);  // a*(b*c)
      R.mult(e, a, b);
      R.mult(e, e, c);  // (a*b)*c
      EXPECT_TRUE(almostEqual(R, 94, d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(e, b, c);
      R.mult(d, a, e);  // a*(b+c)
      R.mult(b, a, b);
      R.mult(c, a, c);
      R.add(e, b, c);  // a*b + a*c
      EXPECT_TRUE(almostEqual(R, 94, d, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, power_and_invert)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, d;
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
      EXPECT_TRUE(almostEqual(R, 96, c, d));

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

TEST(ARingRRR, get_precision)
{
  M2::ARingRRR R(100);
  EXPECT_EQ(R.get_precision(), 100);
  M2::ARingRRR S(200);
  EXPECT_EQ(S.get_precision(), 200);
  EXPECT_EQ(ringName(S), "ARRR_200");

  // elements are initialized at the ring's precision
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  EXPECT_EQ(mpfr_get_prec(&a), 100);
  S.init_set(b, a);
  EXPECT_EQ(mpfr_get_prec(&b), 200);
  R.clear(a);
  S.clear(b);
}

TEST(ARingRRR, init_is_zero)
{
  // BUG? (unclear contract): ARingRRR::init is a bare mpfr_init2, which
  // leaves the element as NaN.  ARingRR::init (and e.g. mpz/mpq based rings)
  // give 0.  Combined with is_zero(NaN) being true (see compare_elems_nan),
  // an uninitialized-value mistake looks like a zero.  These expectations
  // fail until init sets the value to 0, or we decide init need not do so.
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a;
  R.init(a);
  EXPECT_FALSE(mpfr_nan_p(&a));
  EXPECT_EQ(mpfr_zero_p(&a), 1);
  R.clear(a);
}

TEST(ARingRRR, precision_matters)
{
  // 1/3 at 100 bits differs from 1/3 at 53 bits, and from 1/3 at 200 bits
  M2::ARingRRR R(100);
  M2::ARingRRR S(200);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  R.set(a, 1);
  R.set(b, 3);
  R.divide(c, a, b);  // c = 1/3 at 100 bits
  EXPECT_NE(mpfr_cmp_d(&c, 1.0 / 3.0), 0);

  M2::ARingRRR::ElementType d;
  S.init(d);
  mpfr_set_si(&d, 1, MPFR_RNDN);
  mpfr_div_si(&d, &d, 3, MPFR_RNDN);  // d = 1/3 at 200 bits
  EXPECT_NE(mpfr_cmp(&c, &d), 0);
  // but they agree to about 100 bits
  mpfr_sub(&d, &d, &c, MPFR_RNDN);
  EXPECT_LT(mpfr_cmp_ui_2exp(&d, 1, -98), 0);
  S.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, coercions)
{
  M2::ARingRRR R(100);
  testCoercions(R);
}

TEST(ARingRRR, set_coercions)
{
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);

  // set(long), set(int)
  R.set(a, 12345L);
  EXPECT_EQ(mpfr_cmp_si(&a, 12345), 0);
  R.set(a, -7);
  EXPECT_EQ(mpfr_cmp_si(&a, -7), 0);

  // set(mpz): 2^80 + 1 is exact at 100 bits, but not as a double
  mpz_t z;
  mpz_init(z);
  mpz_ui_pow_ui(z, 2, 80);
  mpz_add_ui(z, z, 1);
  R.set(a, z);
  EXPECT_EQ(mpfr_cmp_z(&a, z), 0);
  mpz_clear(z);

  // set(mpq)
  mpq_t q;
  mpq_init(q);
  mpq_set_si(q, -7, 4);
  EXPECT_TRUE(R.set(a, q));
  EXPECT_EQ(mpfr_cmp_d(&a, -1.75), 0);
  mpq_set_si(q, 1, 3);
  EXPECT_TRUE(R.set(a, q));
  R.set(b, 1);
  R.set(c, 3);
  R.divide(b, b, c);  // 1/3 at 100 bits
  EXPECT_TRUE(R.is_equal(a, b));
  mpq_clear(q);

  // set(double)
  EXPECT_TRUE(R.set(a, -0.125));
  EXPECT_EQ(mpfr_cmp_d(&a, -0.125), 0);

  // set(gmp_RR): a higher-precision value is rounded to the ring precision
  mpfr_t f;
  mpfr_init2(f, 300);
  mpfr_set_si(f, 1, MPFR_RNDN);
  mpfr_div_si(f, f, 3, MPFR_RNDN);
  EXPECT_TRUE(R.set(a, f));
  EXPECT_EQ(mpfr_get_prec(&a), 100);
  EXPECT_NE(mpfr_cmp(&a, f), 0);
  mpfr_set_prec(f, 100);
  mpfr_set_si(f, 1, MPFR_RNDN);
  mpfr_div_si(f, f, 3, MPFR_RNDN);
  EXPECT_EQ(mpfr_cmp(&a, f), 0);
  mpfr_clear(f);

  // set(ElementType)
  R.set(b, a);
  EXPECT_TRUE(R.is_equal(a, b));

  // set_var always gives 1
  R.set_var(a, 0);
  EXPECT_EQ(mpfr_cmp_si(&a, 1), 0);
  R.set_var(a, 3);
  EXPECT_EQ(mpfr_cmp_si(&a, 1), 0);

  // set_zero
  R.set(a, 3.5);
  R.set_zero(a);
  EXPECT_TRUE(R.is_zero(a));

  // copy, init_set
  R.set(a, 6.25);
  R.copy(b, a);
  EXPECT_TRUE(R.is_equal(a, b));
  M2::ARingRRR::ElementType e;
  R.init_set(e, a);
  EXPECT_TRUE(R.is_equal(a, e));
  R.clear(e);

  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, ring_elem_roundtrip)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  ring_elem r;
  for (int i = 0; i < 100; i++)
    {
      gen.nextElement(a);
      R.to_ring_elem(r, a);
      // to_ring_elem makes a copy: changing a afterwards doesn't change r
      R.from_ring_elem(b, r);
      EXPECT_TRUE(R.is_equal(a, b));
      EXPECT_TRUE(R.is_equal(R.from_ring_elem_const(r), a));
      EXPECT_EQ(mpfr_get_prec(r.get_mpfr()), 100);
      R.set(a, 12345);
      EXPECT_TRUE(R.is_equal(R.from_ring_elem_const(r), b));
    }
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, addMultipleTo)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, r, s, t;
  R.init(a);
  R.init(b);
  R.init(r);
  R.init(s);
  R.init(t);
  for (int i = 0; i < ntrials; i++)
    {
      // test: r += a*b agrees with s = r + (a*b)
      gen.nextElement(a);
      gen.nextElement(b);
      gen.nextElement(r);
      R.mult(t, a, b);
      R.add(s, r, t);
      R.addMultipleTo(r, a, b);
      EXPECT_TRUE(almostEqual(R, 96, r, s));
    }
  R.set(r, 1);
  R.set(a, 2);
  R.set(b, 3);
  R.addMultipleTo(r, a, b);
  EXPECT_EQ(mpfr_cmp_si(&r, 7), 0);
  R.clear(t);
  R.clear(s);
  R.clear(r);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, abs_and_abs_squared)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  R.set(a, -2.5);
  R.abs(b, a);
  EXPECT_EQ(mpfr_cmp_d(&b, 2.5), 0);
  R.set(a, 2.5);
  R.abs(b, a);
  EXPECT_EQ(mpfr_cmp_d(&b, 2.5), 0);
  R.abs(a, a);  // in place
  EXPECT_EQ(mpfr_cmp_d(&a, 2.5), 0);
  R.set(a, -3);
  R.abs(a, a);  // in place, negative
  EXPECT_EQ(mpfr_cmp_si(&a, 3), 0);

  R.set(a, -3);
  R.abs_squared(b, a);
  EXPECT_EQ(mpfr_cmp_si(&b, 9), 0);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      R.abs_squared(b, a);
      R.mult(c, a, a);
      EXPECT_TRUE(R.is_equal(b, c));
      EXPECT_GE(mpfr_sgn(&b), 0);
    }
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, compare_elems)
{
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.set(a, 0.0);
  R.set(b, -0.0);
  R.set(c, -0.01);
  R.set(d, 0.01);
  EXPECT_EQ(R.compare_elems(a, b), 0);
  EXPECT_EQ(R.compare_elems(c, d), -1);
  EXPECT_EQ(R.compare_elems(d, a), 1);

  // values that differ only beyond 53 bits still compare correctly
  R.set(a, 1);
  mpfr_set_ui_2exp(&b, 1, -80, MPFR_RNDN);
  R.add(b, a, b);  // b = 1 + 2^-80
  EXPECT_EQ(R.compare_elems(a, b), -1);
  EXPECT_EQ(R.compare_elems(b, a), 1);
  EXPECT_FALSE(R.is_equal(a, b));

  // infinities
  mpfr_set_inf(&a, -1);
  mpfr_set_inf(&b, 1);
  R.set(c, 1);
  EXPECT_EQ(R.compare_elems(a, c), -1);
  EXPECT_EQ(R.compare_elems(b, c), 1);
  EXPECT_EQ(R.compare_elems(a, b), -1);
  EXPECT_EQ(R.compare_elems(b, b), 0);

  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, compare_elems_nan)
{
  // BUG: mpfr_cmp returns 0 when either argument is NaN (and sets the erange
  // flag), so compare_elems and is_equal treat NaN as equal to every
  // element, and is_zero(NaN) is true.  The convention for NaN has not been
  // decided; at a minimum NaN should not be equal to 1, nor be zero.  These
  // expectations fail until that is fixed.
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType nan, one;
  R.init(nan);
  R.init(one);
  mpfr_set_nan(&nan);
  R.set(one, 1);
  EXPECT_NE(R.compare_elems(nan, one), 0);
  EXPECT_NE(R.compare_elems(one, nan), 0);
  EXPECT_FALSE(R.is_equal(nan, one));
  EXPECT_FALSE(R.is_zero(nan));
  R.clear(one);
  R.clear(nan);
}

TEST(ARingRRR, is_unit)
{
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  R.set(a, 0);  // init leaves a as NaN, so set it explicitly
  R.set(b, 0.5);
  EXPECT_TRUE(R.is_unit(b));
  EXPECT_FALSE(R.is_unit(a));
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, swap)
{
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  R.set(a, 1.5);
  R.set(b, -4.0);
  R.swap(a, b);
  EXPECT_EQ(mpfr_cmp_d(&a, -4.0), 0);
  EXPECT_EQ(mpfr_cmp_d(&b, 1.5), 0);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, invert)
{
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  R.set(b, 2);
  R.invert(c, b);
  EXPECT_EQ(mpfr_cmp_d(&c, 0.5), 0);
  R.invert(b, b);  // in place
  EXPECT_EQ(mpfr_cmp_d(&b, 0.5), 0);
  // BUG: invert(0) currently returns inf rather than throwing.  The intended
  // behavior is to throw, so these expectations fail until ARingRRR::invert
  // is fixed.
  R.set(a, 0);  // init leaves a as NaN, so set it explicitly
  EXPECT_THROW(R.invert(c, a), std::runtime_error);
  R.set(a, -0.0);
  EXPECT_THROW(R.invert(c, a), std::runtime_error);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, power_edge_cases)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, c, one;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(one);
  R.set(one, 1);

  // exponent 0 gives 1, including 0^0
  R.set(a, 0);
  R.power(b, a, 0);
  EXPECT_TRUE(R.is_equal(b, one));
  R.set(a, -3.5);
  R.power(b, a, 0);
  EXPECT_TRUE(R.is_equal(b, one));

  R.set(a, 2);
  R.power(b, a, -3);
  EXPECT_EQ(mpfr_cmp_d(&b, 0.125), 0);

  for (int i = 0; i < ntrials; i++)
    {
      // test: a^-2 * a^2 == 1
      gen.nextElement(a);
      if (R.is_zero(a)) continue;
      R.power(b, a, -2);
      R.power(c, a, 2);
      R.mult(c, b, c);
      EXPECT_TRUE(almostEqual(R, 94, c, one));
    }

  // power_mpz handles exponents that do not fit in an int
  mpz_t n;
  mpz_init(n);
  mpz_set_si(n, -2);
  R.set(a, 2);
  R.power_mpz(b, a, n);
  EXPECT_EQ(mpfr_cmp_d(&b, 0.25), 0);
  mpz_ui_pow_ui(n, 2, 40);
  mpz_add_ui(n, n, 1);  // n = 2^40 + 1, odd
  R.set(a, -1);
  R.power_mpz(b, a, n);
  EXPECT_EQ(mpfr_cmp_si(&b, -1), 0);
  R.set(a, 2);
  R.power_mpz(b, a, n);  // overflows
  EXPECT_TRUE(mpfr_inf_p(&b));
  mpz_neg(n, n);
  R.power_mpz(b, a, n);  // underflows
  EXPECT_TRUE(R.is_zero(b));
  mpz_clear(n);

  R.clear(one);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

static std::string elemString(const M2::ARingRRR& R,
                              double d,
                              bool p_one,
                              bool p_plus)
{
  M2::ARingRRR::ElementType a;
  R.init(a);
  R.set(a, d);
  buffer o;
  R.elem_text_out(o, a, p_one, p_plus, false);
  R.clear(a);
  return o.str();
}

TEST(ARingRRR, elem_text_out)
{
  M2::ARingRRR R(100);
  EXPECT_EQ(elemString(R, 2.5, true, false), "2.5");
  EXPECT_EQ(elemString(R, 2.5, true, true), "+2.5");
  EXPECT_EQ(elemString(R, -2.5, true, false), "-2.5");
  EXPECT_EQ(elemString(R, -2.5, true, true), "-2.5");
  EXPECT_EQ(elemString(R, 0.0, true, true), "0");

  // p_one: whether to print a coefficient of 1 or -1
  EXPECT_EQ(elemString(R, 1.0, true, false), "1");
  EXPECT_EQ(elemString(R, 1.0, false, false), "");
  EXPECT_EQ(elemString(R, 1.0, false, true), "+");
  EXPECT_EQ(elemString(R, -1.0, true, false), "-1");
  EXPECT_EQ(elemString(R, -1.0, false, false), "-");
  EXPECT_EQ(elemString(R, -1.0, false, true), "-");
  EXPECT_EQ(elemString(R, 2.5, false, false), "2.5");
}

TEST(ARingRRR, syzygy)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b, x, y, c, d;
  R.init(a);
  R.init(b);
  R.init(x);
  R.init(y);
  R.init(c);
  R.init(d);
  mpfr_t eps;
  mpfr_init2(eps, 53);
  mpfr_set_ui_2exp(eps, 1, -96, MPFR_RNDN);
  for (int i = 0; i < ntrials; i++)
    {
      // test: x*a + y*b == 0, with x == 1
      gen.nextElement(a);
      gen.nextElement(b);
      if (R.is_zero(a) or R.is_zero(b)) continue;
      R.syzygy(a, b, x, y);
      EXPECT_EQ(mpfr_cmp_si(&x, 1), 0);
      R.mult(c, x, a);
      R.mult(d, y, b);
      R.add(c, c, d);
      // the relation holds relative to the size of a
      mpfr_div(&c, &c, &a, MPFR_RNDN);
      EXPECT_LT(mpfr_cmpabs(&c, eps), 0);
    }
  mpfr_clear(eps);
  R.clear(d);
  R.clear(c);
  R.clear(y);
  R.clear(x);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, syzygy_b_zero)
{
  // BUG (or unchecked precondition): the header says syzygy need not handle
  // b == 0, but when b == 0 it sets x = 1 and leaves y untouched, so the
  // returned x, y do not satisfy x*a + y*b == 0.  These expectations fail
  // until syzygy either handles b == 0 (e.g. x = 0, y = 1) or rejects it.
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b, x, y, c, d;
  R.init(a);
  R.init(b);
  R.init(x);
  R.init(y);
  R.init(c);
  R.init(d);
  R.set(a, 3);
  R.set(b, 0);  // init leaves b as NaN, so set it explicitly
  R.set(y, 42);
  R.syzygy(a, b, x, y);
  R.mult(c, x, a);
  R.mult(d, y, b);
  R.add(c, c, d);
  EXPECT_TRUE(R.is_zero(c));
  EXPECT_NE(mpfr_cmp_si(&y, 42), 0);  // y should have been set
  R.clear(d);
  R.clear(c);
  R.clear(y);
  R.clear(x);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, zeroize_tiny)
{
  mpfr_t eps;
  mpfr_init2(eps, 53);
  mpfr_set_str(eps, "1e-7", 10, MPFR_RNDN);

  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  R.set(a, 0.00000009);
  R.set(b, 0.00000011);
  R.set(c, -0.00000009);

  R.zeroize_tiny(eps, a);
  R.zeroize_tiny(eps, b);
  R.zeroize_tiny(eps, c);
  EXPECT_TRUE(R.is_zero(a));
  EXPECT_FALSE(R.is_zero(b));
  EXPECT_TRUE(R.is_zero(c));

  R.clear(c);
  R.clear(b);
  R.clear(a);
  mpfr_clear(eps);
}

TEST(ARingRRR, increase_norm)
{
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a;
  R.init(a);
  mpfr_t norm;
  mpfr_init2(norm, 100);
  mpfr_set_si(norm, 0, MPFR_RNDN);

  R.set(a, -3.5);
  R.increase_norm(norm, a);
  EXPECT_EQ(mpfr_cmp_d(norm, 3.5), 0);
  R.set(a, 2.0);  // smaller: no change
  R.increase_norm(norm, a);
  EXPECT_EQ(mpfr_cmp_d(norm, 3.5), 0);
  R.set(a, -3.5);  // equal: no change
  R.increase_norm(norm, a);
  EXPECT_EQ(mpfr_cmp_d(norm, 3.5), 0);
  R.set(a, 10.0);
  R.increase_norm(norm, a);
  EXPECT_EQ(mpfr_cmp_d(norm, 10.0), 0);

  mpfr_clear(norm);
  R.clear(a);
}

TEST(ARingRRR, coerceToDouble)
{
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a;
  R.init(a);
  R.set(a, -2.75);
  EXPECT_EQ(R.coerceToDouble(a), -2.75);
  R.set(a, 3);
  mpfr_si_div(&a, 1, &a, MPFR_RNDN);  // 1/3 at 100 bits rounds to the double 1/3
  EXPECT_EQ(R.coerceToDouble(a), 1.0 / 3.0);
  R.clear(a);
}

TEST(ARingRRR, computeHashValue)
{
  M2::ARingRRR R(100);
  ARingElementGenerator<M2::ARingRRR> gen(R);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  for (int i = 0; i < 100; i++)
    {
      // equal values hash equal
      gen.nextElement(a);
      R.copy(b, a);
      EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
    }
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, computeHashValue_negative)
{
  // BUG: computeHashValue converts to double and then casts directly to
  // unsigned int.  For negative values or values >= 2^32 this conversion is
  // undefined behavior.  These expectations fail until the hash is fixed.
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a, b;
  R.init(a);
  R.init(b);
  R.set(a, -1.0);
  R.set(b, 0.0);
  EXPECT_NE(R.computeHashValue(a), R.computeHashValue(b));
  R.set(b, -2.0);
  EXPECT_NE(R.computeHashValue(a), R.computeHashValue(b));
  R.set(a, 1e20);
  R.set(b, 2e20);
  EXPECT_NE(R.computeHashValue(a), R.computeHashValue(b));
  R.clear(b);
  R.clear(a);
}

TEST(ARingRRR, random)
{
  // randomMpfr is documented to return a value in [0, 1]
  M2::ARingRRR R(100);
  M2::ARingRRR::ElementType a;
  R.init(a);
  for (int i = 0; i < ntrials; i++)
    {
      R.random(a);
      EXPECT_GE(mpfr_sgn(&a), 0);
      EXPECT_LE(mpfr_cmp_si(&a, 1), 0);
      EXPECT_EQ(mpfr_get_prec(&a), 100);
    }
  R.clear(a);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
