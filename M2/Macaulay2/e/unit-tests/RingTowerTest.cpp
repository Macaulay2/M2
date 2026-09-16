// Copyright 2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "unit-tests/RingTest.hpp"
#include "rings/tower.hpp"
#include "util.hpp"
#include "interface/ring.h"
#include "exceptions.hpp"
#include "unit-tests/util-polyring-creation.hpp"

// First: we need a routine to read a polynomial from a string.
// Format:  variables are a..zA..Z, and then [1], [2], ...
// Need both input and output routines for reading/writing polynomials in this
// format.
// coefficients: (+ or - or nothing) (number) (optional: . or /, followed by
// another (number)
// for GF, do we mix the a^r in?

template <>
ring_elem getElement<Tower>(const Tower& R, int index)
{
  return R.random();
}

//////////////////////////////////////////////////
TEST(RingTower, create)
{
  std::vector<std::string> vars = {"a", "b"};
  M2_ArrayString varnames = stdvector_to_M2_ArrayString(vars);
  const Tower* R = Tower::create(101, varnames);
  EXPECT_TRUE(R != nullptr);
  EXPECT_EQ(ringName(*R), "Tower[ZZ/101[a,b]]");
  EXPECT_EQ(R->n_vars(), 2);
  for (int i = 1; i < 1; i++)
    {
      ring_elem f = R->random();
      buffer o;
      o << "f = ";
      R->elem_text_out(o, f);
      std::cout << o.str() << std::endl;
    }
}

TEST(RingTower, elems)
{
  std::vector<std::string> vars = {"a", "b"};
  M2_ArrayString varnames = stdvector_to_M2_ArrayString(vars);
  const Tower* R = Tower::create(101, varnames);

  ring_elem a = R->var(0);
  ring_elem b = R->var(1);

  buffer o;
  o << "a=";
  R->elem_text_out(o, a);
  o << " b=";
  R->elem_text_out(o, b);
  ring_elem c = R->add(a, R->from_long(2));
  c = R->add(c, b);
  o << " c=";
  R->elem_text_out(o, c);
  ring_elem d = R->power(c, 2);
  o << " d=";
  R->elem_text_out(o, d);
  o << newline;

  std::cout << o.str();
}

// DPoly::add_term takes the outermost variable at index 0, the reverse of the
// variable-indexed order varpower::to_expvector produces, so makeTerm has to
// reverse.  x^2*y^3 must not come back as x^3*y^2.
TEST(RingTower, makeTerm)
{
  std::vector<std::string> vars = {"x", "y"};
  M2_ArrayString varnames = stdvector_to_M2_ArrayString(vars);
  const Tower* R = Tower::create(101, varnames);
  ASSERT_NE(R, nullptr);
  const Ring* kk = IM2_Ring_ZZp(101);
  ring_elem one = kk->from_long(1);

  std::vector<int> vp = varpowerOf({{1, 3}, {0, 2}});
  EXPECT_TRUE(R->is_equal(R->makeTerm(kk, one, vp.data()),
                          monomialOf(R, {{0, 2}, {1, 3}})));

  // one variable at a time, to pin down which is which
  std::vector<int> justX = varpowerOf({{0, 1}});
  EXPECT_TRUE(R->is_equal(R->makeTerm(kk, one, justX.data()), R->var(0)));
  std::vector<int> justY = varpowerOf({{1, 1}});
  EXPECT_TRUE(R->is_equal(R->makeTerm(kk, one, justY.data()), R->var(1)));

  // the coefficient is carried
  EXPECT_TRUE(R->is_equal(R->makeTerm(kk, kk->from_long(5), justX.data()),
                          R->mult(R->from_long(5), R->var(0))));
}

TEST(RingTower, makeTermRejectsBadInput)
{
  std::vector<std::string> vars = {"x", "y"};
  M2_ArrayString varnames = stdvector_to_M2_ArrayString(vars);
  const Tower* R = Tower::create(101, varnames);
  ASSERT_NE(R, nullptr);
  const Ring* kk = IM2_Ring_ZZp(101);
  ring_elem one = kk->from_long(1);

  // exponents index a dense array, so negatives are never legal here
  std::vector<int> negative = varpowerOf({{0, -1}});
  EXPECT_THROW(R->makeTerm(kk, one, negative.data()), exc::engine_error);

  // a variable the ring does not have
  std::vector<int> tooMany = varpowerOf({{2, 1}});
  EXPECT_THROW(R->makeTerm(kk, one, tooMany.data()), exc::engine_error);

  // the coefficient ring must be the prime field of the same characteristic
  const Ring* wrong = IM2_Ring_ZZp(7);
  std::vector<int> vp = varpowerOf({{0, 1}});
  EXPECT_THROW(R->makeTerm(wrong, wrong->from_long(1), vp.data()),
               exc::engine_error);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
