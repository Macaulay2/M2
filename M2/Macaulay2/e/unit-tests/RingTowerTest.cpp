// Copyright 2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "RingTest.hpp"
#include "tower.hpp"
#include "../util.hpp"

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
  M2_ArrayString varnames = toM2ArrayString(vars);
  const Tower* R = Tower::create(101, varnames);
  EXPECT_TRUE(R != 0);
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
  M2_ArrayString varnames = toM2ArrayString(vars);
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
