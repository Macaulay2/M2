// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <gmp.h>
#include <flint/fmpz.h>

#include "basic-rings/aring-ZZ-flint.hpp"
#include "basic-rings/aring-QQ-gmp.hpp"
#include "unit-tests/ARingTest.hpp"

extern gmp_ZZ getRandomInteger();

template <>
void getElement<M2::ARingZZ>(const M2::ARingZZ& R,
                             int index,
                             M2::ARingZZ::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set(result, a);
    }
}

TEST(ARingZZ, create)
{
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

TEST(ARingZZ, arithmetic)
{
  M2::ARingZZ R;

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  //  testReciprocal(R, ntrials); // this test is not applicable, as this is not
  //  a field
  //  testPower(R, ntrials);  // this test can't work, as it expects a finite
  //  field
  testAxioms(R, ntrials);
}

TEST(ARingZZ, is_unit)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a;

  R.init(a);

  R.set(a, 1);
  EXPECT_TRUE(R.is_unit(a));

  R.set(a, -1);
  EXPECT_TRUE(R.is_unit(a));

  R.set(a, 2);
  EXPECT_FALSE(R.is_unit(a));

  fmpz_set_str(&a, "36893488147419103232", 10);
  EXPECT_FALSE(R.is_unit(a));
}

TEST(ARingZZ, compare_elems)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b;

  R.init(a);
  R.init(b);

  R.set_zero(a);
  R.set(b, 1);
  EXPECT_EQ(R.compare_elems(a, b), -1);
  EXPECT_EQ(R.compare_elems(b, a), 1);
  EXPECT_EQ(R.compare_elems(a, a), 0);

  fmpz_set_str(&a, "36893488147419103232", 10);
  fmpz_set_str(&b, "36893488147419103233", 10);
  EXPECT_EQ(R.compare_elems(a, b), -1);
  EXPECT_EQ(R.compare_elems(b, a), 1);
  EXPECT_EQ(R.compare_elems(a, a), 0);
}

TEST(ARingZZ, init_set)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b;

  R.init(a);
  fmpz_set_str(&a, "36893488147419103232", 10);
  R.init_set(b, a);

  EXPECT_EQ(R.compare_elems(a, b), 0);
}

TEST(ARingZZ, set)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b;

  R.init(a);
  R.init(b);
  fmpz_set_str(&a, "36893488147419103232", 10);
  R.set(b, a);

  EXPECT_EQ(R.compare_elems(a, b), 0);

  M2::ARingQQGMP S;
  M2::ARingQQGMP::ElementType c,d,e;
  
  S.init(c);
  S.set(c,57);
  R.set(a,&c);
  R.set(b,57);
  EXPECT_EQ(R.compare_elems(a, b), 0);

  S.init(d);
  S.set(d,2);
  S.init(e);
  S.divide(e,c,d);
  EXPECT_FALSE(R.set(a,&e));
}

TEST(ARingZZ, set_var)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b;

  R.init(a);
  R.init(b);

  R.set_zero(a);
  R.set(b, 1);

  R.set_var(a,5);
  
  EXPECT_EQ(R.compare_elems(a,b), 0);

  R.set(a,57);
  R.set_var(a,3);
  
  EXPECT_EQ(R.compare_elems(a, b), 0);
}


TEST(ARingZZ, invert)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, c;

  R.init(a);
  R.init(b);
  R.init(c);
  
  R.set(b, 1);
  R.set(c, 1);

  R.invert(a,b);
  EXPECT_EQ(R.compare_elems(a,c), 0);
  
  R.set(b,-1);
  R.set(c,-1);
  R.invert(a,b);

  EXPECT_EQ(R.compare_elems(a,c), 0);

  R.set(b,57);
  R.set(c,0);
  R.invert(a,b);
  
  EXPECT_EQ(R.compare_elems(a,c), 0);
}

// Divide is getting tested in the divisible case up above
//


TEST(ARingZZ, dividewhendivisible)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, c, d;

  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  
  R.set(a, 8);
  R.set(b, 4);
  R.divide(c,a,b);
  R.set(d, 2);

  EXPECT_EQ(R.compare_elems(c,d), 0);
}


TEST(ARingZZ, dividenondivisible)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, c;

  R.init(a);
  R.init(b);
  R.init(c);

  R.set(a, 2);
  R.set(b, 3);

  //EXPECT_THROW(R.divide(c,a,b),exc::engine_error);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:

