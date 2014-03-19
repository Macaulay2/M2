// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-qq-gmp.hpp"

#include "ARingTest.hpp"

extern gmp_ZZ getRandomInteger();

template<>
void getElement<M2::ARingQQGMP>(const M2::ARingQQGMP& R, int index, M2::ARingQQGMP::ElementType& result)
{
  if (index < 50) 
    R.set_from_long(result, index-25);
  else
    {
      R.random(result);
    }
}

TEST(ARingQQGMP, create) {
  M2::ARingQQGMP R;

  M2::ARingQQGMP::ElementType a;
  buffer o;

  ARingElementGenerator<M2::ARingQQGMP> gen(R);
  R.init(a);
  gen.nextElement(a);

  EXPECT_EQ(ringName(R), "QQGMP");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
  R.clear(a);
}

TEST(ARingQQGMP, display) {
  M2::ARingQQGMP R;

  M2::ARingQQGMP::ElementType a, b;
  buffer o;

  ARingElementGenerator<M2::ARingQQGMP> gen(R);
  R.init(a);
  R.init(b);
  gen.nextElement(a);
  gen.nextElement(b);
  R.divide(a,a,b);
  R.elem_text_out(o, a, true, false, false);
  EXPECT_TRUE(strcmp(o.str(), "24/23") == 0);
  std::cout << "a = ." << o.str() << "." << std::endl;

  EXPECT_EQ(ringName(R), "QQGMP");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
  R.clear(a);
}

TEST(ARingQQGMP, arithmetic) {
  M2::ARingQQGMP R;

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  //  testPower(R, ntrials);  // this test can't work, as it expects a finite field
  testAxioms(R, ntrials);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
