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

// First: we need a routne to read a polynomial from a string.
// Format:  variables are a..zA..Z, and then [1], [2], ...
// Need both input and output routines for reading/writing polynomials in this format.
// coefficients: (+ or - or nothing) (number) (optional: . or /, followed by another (number)
// for GF, do we mix the a^r in?

M2_ArrayString toM2ArrayString(std::vector<std::string> &strs)
{
  int n = static_cast<int>(strs.size()); // needed since M2_ArrayString len field is int
  int i;
  M2_ArrayString a = getmemarraytype(M2_ArrayString,n);
  a->len = n;
  for (i=0; i<n; i++) a->array[i] = M2_tostring(strs[i].c_str());
  return a;
}

//////////////////////////////////////////////////
TEST(RingTower, create)
{
  std::vector<std::string> vars = {"a", "b"};
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
