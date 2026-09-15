// Copyright 2012-2013 Michael E. Stillman

#include <cmath>
#include <cstdio>
#include <stdexcept>
#include <string>
#include <iostream>
#include <limits>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "basic-rings/aring-RR.hpp"
#include "unit-tests/ARingTest.hpp"

// bool almostEqual(const M2::ARingRR& R,
//                  unsigned long nbits,
//                  const M2::ARingRR::ElementType& a,
//                  const M2::ARingRR::ElementType& b)
// {
//   M2::ARingRR::ElementType epsilon = pow(2, static_cast<double>(-nbits));
//   M2::ARingRR::ElementType c;
//   R.subtract(c, a, b);
//   // std::cout << "a = " << a << ", b = " << b << ", c = " << c << ", a-b = " <<
//   // a-b;
//   R.abs(c, c);
//   // std::cout << ", |c| = " << c << ", epsilon = " << epsilon  << std::endl;
//   return R.compare_elems(c, epsilon) < 0;
// }

bool almostEqual(const M2::ARingRR& R,
                 unsigned long nbits,
                 const M2::ARingRR::ElementType& a,
                 const M2::ARingRR::ElementType& b)
{
  M2::ARingRR::ElementType epsilon = std::ldexp(1.0, -static_cast<int>(nbits));
  double c = 0.0;
  R.subtract(c, a, b);
  R.abs(c, c);
  return R.compare_elems(c, epsilon) < 0;
}

// Use relative error when an identity involves products larger than one.
static double relativeTolerance(unsigned long nbits, double a, double b)
{
  return std::ldexp(std::max({1.0, std::fabs(a), std::fabs(b)}),
                    -static_cast<int>(nbits));
}

TEST(ARingRR, almostEqual_tolerance)
{
  M2::ARingRR R;
  auto nbits = R.get_precision() - 2;
  EXPECT_TRUE(almostEqual(R, nbits, 1.0, 1.0));
  EXPECT_TRUE(almostEqual(R, nbits, 1.0, std::nextafter(1.0, 2.0)));
  EXPECT_FALSE(almostEqual(R, nbits, 1.0, 2.0));
  EXPECT_FALSE(almostEqual(R, nbits, 2.0, 1.0));
  EXPECT_EQ(relativeTolerance(nbits, 0.0, 0.5), std::ldexp(1.0, -51));
  EXPECT_EQ(relativeTolerance(nbits, -32.0, 16.0), std::ldexp(32.0, -51));
}





template <>
void getElement<M2::ARingRR>(const M2::ARingRR& R,
                             int index,
                             M2::ARingRR::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

// void getElementRR(const M2::ARingRR&  R, int index, M2::ARingRR::ElementType&
// result)
//{
//  if (index < 50) R.set(result, index-25);
//  else R.random(result);
//}

TEST(ARingRR, create)
{
  M2::ARingRR R;
  EXPECT_EQ(ringName(R), "ARR_53");
  EXPECT_EQ(R.characteristic(), 0);
}

void testRingNegateRR(const M2::ARingRR& R, int ntrials)
{
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c;
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

TEST(ARingRR, compare_elems) 
{
  M2::ARingRR R;
  M2::ARingRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.set(a, 0.0);
  R.set(b, -0.0);
  R.set(c, -0.01);
  R.set(d, 0.01);
  EXPECT_EQ(R.compare_elems(a,b),0);
  EXPECT_EQ(R.compare_elems(c,d),-1);
  EXPECT_EQ(R.compare_elems(d,a),1);

}

TEST(ARingRR, negate)
{
  M2::ARingRR R;
  testRingNegateRR(R, ntrials);
}

TEST(ARingRR, add)
{
  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d, e;
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
      EXPECT_TRUE(almostEqual(R, R.get_precision() - 2, a, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, subtract)
{
  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, e;
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
      EXPECT_TRUE(almostEqual(R, nbits - 2, a, e));
      R.mult(e, a, b);
      // std::cout << e-a*b << " " << e << " " << a << " " << b << std::endl;
      R.subtract_multiple(e, a, b);
      // EXPECT_TRUE(R.is_zero(e)); // this is not necessarily zero (it is with
      // MPFR)
      EXPECT_TRUE(almostEqual(R, nbits - 2, e, 0));
    }
  R.clear(e);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, multDivide)
{
  std::cout.precision(30);
  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d;
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
          // std::cout << a << " " << b << " " << c << " " << d << " " << d-a <<
          // std::endl;
          EXPECT_TRUE(almostEqual(R, nbits - 2, d, a));
        }
    }
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, axioms)
{
  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d, e;
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
      EXPECT_TRUE(almostEqual(R, nbits - 2, d, e));
      R.mult(d, a, b);
      R.mult(e, b, a);
      EXPECT_TRUE(almostEqual(R, nbits - 2, d, e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(e, b, c);
      R.add(d, a, e);  // a+(b+c)
      R.add(e, a, b);
      R.add(e, e, c);  // (a+b)+c
      EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));
      R.mult(e, b, c);
      R.mult(d, a, e);  // a*(b*c)
      R.mult(e, a, b);
      R.mult(e, e, c);  // (a*b)*c
      EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(e, b, c);
      R.mult(d, a, e);  // a*(b+c)
      R.mult(b, a, b);
      R.mult(c, a, c);
      R.add(e, b, c);  // a*b + a*c
      EXPECT_TRUE(almostEqual(R, nbits - 6, d, e));
    }
  R.clear(e);
  R.clear(d);
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, power_and_invert)
{
  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c, d;
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
      EXPECT_TRUE(almostEqual(R, nbits - 4, c, d));

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

TEST(ARingRR, invert)
{
  M2::ARingRR R;
  M2::ARingRR::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.set(b, 2.0);
  R.set(c, 1.0);
  R.set(d, 0.5);
  R.invert(b,b);
  EXPECT_TRUE(b == d);
  // BUG: invert(0.0) currently returns inf rather than throwing.  The
  // intended behavior is to throw, so these expectations fail until
  // ARingRR::invert is fixed.
  EXPECT_THROW(R.invert(a,a), std::runtime_error);
  R.set(a, -0.0);
  EXPECT_THROW(R.invert(a,a), std::runtime_error);
}

TEST(ARingRR, zeroize_tiny) 
{
  mpfr_t eps;
  mpfr_init2(eps,53);
  mpfr_set_str(eps, "1e-7", 10, MPFR_RNDN);
  
  M2::ARingRR R;
  M2::ARingRR::ElementType a, b;
  R.init_set(a, 0.00000009);
  R.init_set(b, 0.00000011);

  R.zeroize_tiny(eps, a);
  R.zeroize_tiny(eps, b);
  EXPECT_EQ(0, a);
  EXPECT_NE(0, b);

  mpfr_clear(eps);
}

TEST(ARingRR, is_unit)
{
  M2::ARingRR R;
  M2::ARingRR::ElementType a, b;
  R.init(a);
  R.init_set(b, 0.5);
  EXPECT_TRUE(R.is_unit(b));
  EXPECT_FALSE(R.is_unit(a));
}

TEST(ARingRR, get_precision)
{
  M2::ARingRR R;
  EXPECT_EQ(R.get_precision(), 53);
}

TEST(ARingRR, set_coercions)
{
  M2::ARingRR R;
  M2::ARingRR::ElementType a, b;
  R.init(a);
  R.init(b);

  // set(long)
  R.set(a, 12345L);
  EXPECT_EQ(a, 12345.0);
  R.set(a, -7L);
  EXPECT_EQ(a, -7.0);
  long big = (1L << 53);
  R.set(a, big);
  EXPECT_EQ(a, 9007199254740992.0);

  // set(mpz)
  mpz_t z;
  mpz_init(z);
  mpz_set_si(z, -42);
  R.set(a, z);
  EXPECT_EQ(a, -42.0);
  mpz_ui_pow_ui(z, 2, 100);
  R.set(a, z);
  EXPECT_EQ(a, std::ldexp(1.0, 100));
  mpz_clear(z);

  // set(mpq)
  mpq_t q;
  mpq_init(q);
  mpq_set_si(q, 1, 3);
  EXPECT_TRUE(R.set(a, q));
  EXPECT_EQ(a, 1.0 / 3.0);
  mpq_set_si(q, -7, 4);
  EXPECT_TRUE(R.set(a, q));
  EXPECT_EQ(a, -1.75);
  mpq_clear(q);

  // set(gmp_RR)
  mpfr_t f;
  mpfr_init2(f, 53);
  mpfr_set_d(f, 2.5, MPFR_RNDN);
  EXPECT_TRUE(R.set(a, f));
  EXPECT_EQ(a, 2.5);
  mpfr_set_prec(f, 200);
  mpfr_set_si(f, 1, MPFR_RNDN);
  mpfr_div_si(f, f, 3, MPFR_RNDN);
  EXPECT_TRUE(R.set(a, f));
  EXPECT_EQ(a, 1.0 / 3.0);
  mpfr_clear(f);

  // set(double)
  EXPECT_TRUE(R.set(a, -0.125));
  EXPECT_EQ(a, -0.125);

  // set_var always gives 1
  R.set_var(a, 0);
  EXPECT_EQ(a, 1.0);
  R.set_var(a, 3);
  EXPECT_EQ(a, 1.0);

  // set_zero
  R.set(a, 3.5);
  R.set_zero(a);
  EXPECT_TRUE(R.is_zero(a));

  // copy, init_set
  R.set(a, 6.25);
  R.copy(b, a);
  EXPECT_EQ(b, 6.25);
  M2::ARingRR::ElementType c;
  R.init_set(c, a);
  EXPECT_EQ(c, 6.25);
  R.clear(c);

  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, ring_elem_roundtrip)
{
  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b;
  R.init(a);
  R.init(b);
  ring_elem r;
  for (int i = 0; i < 100; i++)
    {
      gen.nextElement(a);
      R.to_ring_elem(r, a);
      R.from_ring_elem(b, r);
      EXPECT_EQ(a, b);
      EXPECT_EQ(R.from_ring_elem_const(r), a);
    }
  R.set(a, -0.0);
  R.to_ring_elem(r, a);
  R.from_ring_elem(b, r);
  EXPECT_TRUE(std::signbit(b));
  EXPECT_TRUE(R.is_zero(b));
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, addMultipleTo)
{
  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, r, s, t;
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
      EXPECT_TRUE(almostEqual(R, nbits - 4, r, s));
    }
  R.set(r, 1.0);
  R.addMultipleTo(r, 2.0, 3.0);
  EXPECT_EQ(r, 7.0);
  R.clear(t);
  R.clear(s);
  R.clear(r);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, abs_and_abs_squared)
{
  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);
  R.set(a, -2.5);
  R.abs(b, a);
  EXPECT_EQ(b, 2.5);
  R.set(a, 2.5);
  R.abs(b, a);
  EXPECT_EQ(b, 2.5);
  R.set(a, -0.0);
  R.abs(b, a);
  EXPECT_TRUE(R.is_zero(b));
  EXPECT_FALSE(std::signbit(b));

  R.set(a, -3.0);
  R.abs_squared(b, a);
  EXPECT_EQ(b, 9.0);
  for (int i = 0; i < ntrials; i++)
    {
      gen.nextElement(a);
      R.abs_squared(b, a);
      R.mult(c, a, a);
      EXPECT_EQ(b, c);
      EXPECT_GE(b, 0.0);
    }
  R.clear(c);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, swap)
{
  M2::ARingRR R;
  M2::ARingRR::ElementType a, b;
  R.init_set(a, 1.5);
  R.init_set(b, -4.0);
  R.swap(a, b);
  EXPECT_EQ(a, -4.0);
  EXPECT_EQ(b, 1.5);
}

TEST(ARingRR, power_edge_cases)
{
  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, c;
  R.init(a);
  R.init(b);
  R.init(c);

  // exponent 0 gives 1, including 0^0 (matching std::pow)
  R.set(a, 0.0);
  R.power(b, a, 0);
  EXPECT_EQ(b, 1.0);
  R.set(a, -3.5);
  R.power(b, a, 0);
  EXPECT_EQ(b, 1.0);

  R.set(a, 2.0);
  R.power(b, a, -3);
  EXPECT_EQ(b, 0.125);

  for (int i = 0; i < ntrials; i++)
    {
      // test: a^-2 * a^2 == 1
      gen.nextElement(a);
      if (R.is_zero(a)) continue;
      R.power(b, a, -2);
      R.power(c, a, 2);
      R.mult(c, b, c);
      EXPECT_TRUE(almostEqual(R, nbits - 4, c, 1.0));
    }

  // power_mpz: exponents that don't fit in an int throw
  mpz_t n;
  mpz_init(n);
  mpz_set_si(n, -2);
  R.set(a, 2.0);
  R.power_mpz(b, a, n);
  EXPECT_EQ(b, 0.25);
  mpz_ui_pow_ui(n, 2, 40);
  EXPECT_THROW(R.power_mpz(b, a, n), exc::engine_error);
  mpz_neg(n, n);
  EXPECT_THROW(R.power_mpz(b, a, n), exc::engine_error);
  mpz_clear(n);

  R.clear(c);
  R.clear(b);
  R.clear(a);
}

static std::string elemString(const M2::ARingRR& R,
                              const M2::ARingRR::ElementType& a,
                              bool p_one,
                              bool p_plus)
{
  buffer o;
  R.elem_text_out(o, a, p_one, p_plus, false);
  return o.str();
}

TEST(ARingRR, elem_text_out)
{
  M2::ARingRR R;
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

TEST(ARingRR, syzygy)
{
  M2::ARingRR R;
  auto nbits = R.get_precision();
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b, x, y, c, d;
  R.init(a);
  R.init(b);
  R.init(x);
  R.init(y);
  R.init(c);
  R.init(d);
  for (int i = 0; i < ntrials; i++)
    {
      // test: x*a + y*b == 0, with x == 1
      gen.nextElement(a);
      gen.nextElement(b);
      if (R.is_zero(a) or R.is_zero(b)) continue;
      R.syzygy(a, b, x, y);
      EXPECT_EQ(x, 1.0);
      R.mult(c, x, a);
      R.mult(d, y, b);
      R.add(c, c, d);
      R.abs(d, a);
      // the relation holds relative to the size of a
      EXPECT_LE(std::fabs(c), d * std::ldexp(1.0, 2 - static_cast<int>(nbits)));
    }
  R.clear(d);
  R.clear(c);
  R.clear(y);
  R.clear(x);
  R.clear(b);
  R.clear(a);
}

TEST(ARingRR, syzygy_b_zero)
{
  // BUG (or unchecked precondition): the header says syzygy need not handle
  // b == 0, but when b == 0 it sets x = 1 and leaves y untouched, so the
  // returned x, y do not satisfy x*a + y*b == 0.  These expectations fail
  // until syzygy either handles b == 0 (e.g. x = 0, y = 1) or rejects it.
  M2::ARingRR R;
  M2::ARingRR::ElementType a, b, x, y;
  R.init_set(a, 3.0);
  R.init_set(b, 0.0);
  R.init(x);
  R.init_set(y, 42.0);
  R.syzygy(a, b, x, y);
  EXPECT_EQ(x * a + y * b, 0.0);
  EXPECT_NE(y, 42.0);  // y should have been set
}

TEST(ARingRR, increase_norm)
{
  M2::ARingRR R;
  mpfr_t norm;
  mpfr_init2(norm, 53);
  mpfr_set_d(norm, 0.0, MPFR_RNDN);

  R.increase_norm(norm, -3.5);
  EXPECT_EQ(mpfr_get_d(norm, MPFR_RNDN), 3.5);
  R.increase_norm(norm, 2.0);  // smaller: no change
  EXPECT_EQ(mpfr_get_d(norm, MPFR_RNDN), 3.5);
  R.increase_norm(norm, -3.5);  // equal: no change
  EXPECT_EQ(mpfr_get_d(norm, MPFR_RNDN), 3.5);
  R.increase_norm(norm, 10.0);
  EXPECT_EQ(mpfr_get_d(norm, MPFR_RNDN), 10.0);

  mpfr_clear(norm);
}

TEST(ARingRR, coerceToDouble)
{
  M2::ARingRR R;
  EXPECT_EQ(R.coerceToDouble(-2.75), -2.75);
  EXPECT_EQ(R.coerceToDouble(0.0), 0.0);
  EXPECT_EQ(R.coerceToDouble(1e300), 1e300);
}

TEST(ARingRR, computeHashValue)
{
  M2::ARingRR R;
  ARingElementGenerator<M2::ARingRR> gen(R);
  M2::ARingRR::ElementType a, b;
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

TEST(ARingRR, computeHashValue_negative)
{
  // BUG: computeHashValue casts a double directly to unsigned int.  For
  // negative values or values >= 2^32 this conversion is undefined behavior
  // (observed with AppleClang: unrelated values such as -1.0 and 1e20 both
  // hash to 0, and the results are not even stable).  These expectations
  // fail until the hash is fixed.
  M2::ARingRR R;
  EXPECT_NE(R.computeHashValue(-1.0), R.computeHashValue(0.0));
  EXPECT_NE(R.computeHashValue(-1.0), R.computeHashValue(-2.0));
  EXPECT_NE(R.computeHashValue(1e20), R.computeHashValue(2e20));
}

TEST(ARingRR, compare_elems_infinity)
{
  M2::ARingRR R;
  double inf = std::numeric_limits<double>::infinity();
  EXPECT_EQ(R.compare_elems(-inf, 1.0), -1);
  EXPECT_EQ(R.compare_elems(inf, 1.0), 1);
  EXPECT_EQ(R.compare_elems(-inf, inf), -1);
  EXPECT_EQ(R.compare_elems(1e308, inf), -1);
}

TEST(ARingRR, compare_elems_nan)
{
  // BUG: compare_elems computes f - g and returns 0 when the difference is
  // neither < 0 nor > 0.  With a NaN argument the difference is NaN, so NaN
  // compares "equal" to every element.  The convention for NaN has not been
  // decided; at a minimum it should not compare equal to 1.0.  These
  // expectations fail until compare_elems is fixed.
  M2::ARingRR R;
  double nan = std::numeric_limits<double>::quiet_NaN();
  EXPECT_NE(R.compare_elems(nan, 1.0), 0);
  EXPECT_NE(R.compare_elems(1.0, nan), 0);
  // inf - inf is also NaN, but inf == inf should still compare equal
  double inf = std::numeric_limits<double>::infinity();
  EXPECT_EQ(R.compare_elems(inf, inf), 0);
}

TEST(ARingRR, random)
{
  // randomDouble() is documented to return a value in [0, 1]
  M2::ARingRR R;
  M2::ARingRR::ElementType a;
  R.init(a);
  for (int i = 0; i < ntrials; i++)
    {
      R.random(a);
      EXPECT_GE(a, 0.0);
      EXPECT_LE(a, 1.0);
    }
  R.clear(a);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
