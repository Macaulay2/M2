// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-zz-flint.hpp"

#include "ARingTest.hpp"

extern gmp_ZZ getRandomInteger();

template<>
void getElement<M2::ARingZZ>(const M2::ARingZZ& R, int index, M2::ARingZZ::ElementType& result)
{
  if (index < 50) 
    R.set_from_long(result, index-25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set_from_mpz(result, a);
    }
}

TEST(ARingZZ, create) {
  M2::ARingZZ R;

  M2::ARingZZ::ElementType a;
  buffer o;

  ARingElementGenerator<M2::ARingZZ> gen(R);
  R.init(a);
  gen.nextElement(a);

  EXPECT_EQ(ringName(R), "ZZFlint");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
  R.clear(a);
}

TEST(ARingZZ, arithmetic) {
  M2::ARingZZ R;

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  //  testReciprocal(R, ntrials); // this test is not applicable, as this is not a field
  //  testPower(R, ntrials);  // this test can't work, as it expects a finite field
  testAxioms(R, ntrials);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
