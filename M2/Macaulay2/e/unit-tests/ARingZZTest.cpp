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
#include "basic-rings/aring-ZZ-gmp.hpp"
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


TEST(ARingZZ, divide)
{
  
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, c, d;

  // First do an example where a is divisible by b
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  
  R.set(a, 8);
  R.set(b, 4);
  R.divide(c,a,b);
  R.set(d, 2);

  EXPECT_EQ(R.compare_elems(c,d), 0);

  // Now do an example where a is not divisible by b   
  // Currently the behavior in aring-ZZ-flint.hpp
  // is that this should return false
  // But we think this should return an error instead

  R.set(a, 2);
  R.set(b, 3);

  EXPECT_THROW(R.divide(c,a,b),exc::engine_error);
}


TEST(ARingZZ, power)
{

  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, c;

  R.init(a);
  R.init(b);
  R.init(c);

  R.set(a, 2);
  
  R.power(b,a,3);

  R.set(c,8);
  EXPECT_EQ(R.compare_elems(b,c), 0);
  
}


TEST(ARingZZ, power_mpz)
{

  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, c;

  M2::ARingZZGMP S;
  M2::ARingZZGMP::ElementType d, e, f;
  
  R.init(a);
  R.init(b);
  R.set(a,2);

  // Case 1: if the exponent is negative should get error
  S.init(d);
  S.set(d,-3);

  EXPECT_THROW(R.power_mpz(b,a,&d),exc::engine_error);

  // Case 2: exponent ok
  S.set(d,31);
  R.set(c,2147483648);
  R.power_mpz(b,a,&d);

  EXPECT_EQ(R.compare_elems(b,c), 0);
  
  // Case 3: exponent too big
  mpz_set_str(&d, "37778931862957161709568", 10);
  EXPECT_THROW(R.power_mpz(b,a,&d),exc::engine_error);
}

TEST(ARingZZ, swap)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, c, d;

  R.init(a);
  R.init(b);
  R.init(c);

  R.set(a, 57);
  R.set(b, 2);
  R.set(c, 2);
  R.set(d, 57);

  R.swap(a,b);
  EXPECT_EQ(R.compare_elems(a,c), 0);
  EXPECT_EQ(R.compare_elems(b,d), 0);

}


TEST(ARingZZ, random)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a;

  R.init(a);

  EXPECT_NO_THROW(R.random(a));

}



TEST(ARingZZ, display)
{
  M2::ARingZZ R;

  M2::ARingZZ::ElementType a;
  buffer o1, o2, o3, o4;

  R.init(a);
  
  R.set(a,24);

  R.elem_text_out(o1, a, false, false, false);
  EXPECT_STREQ(o1.str(), "24");

  R.elem_text_out(o2, a, true, true, false);
  EXPECT_STREQ(o2.str(), "+24");

  R.set(a,-1);
  R.elem_text_out(o3, a, true, false, false);
  EXPECT_STREQ(o3.str(), "-1");

  R.elem_text_out(o4, a, false, false, false);
  EXPECT_STREQ(o4.str(), "-");  

  o3.reset();
  o4.reset();

  R.set(a,1);
  R.elem_text_out(o3, a, true, false, false);
  EXPECT_STREQ(o3.str(), "1");

  R.elem_text_out(o4, a, false, false, false);
  EXPECT_STREQ(o4.str(), "");
}




TEST(ARingZZ, syzygy)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a, b, x1, y1, x2, y2;

  R.init(a);
  R.init(b);
  R.init(x1);
  R.init(y1);
  R.init(x2);
  R.init(y2);

  // Case 1: a is 0
  R.set(a,0);
  R.set(b,1);
  R.syzygy(a,b,x1,y1);
  R.set(x2,1);
  R.set(y2,0);

  EXPECT_EQ(R.compare_elems(x1,x2), 0);
  EXPECT_EQ(R.compare_elems(y1,y2), 0);

  // Case 2: b is 1                                                                                                                                            
  R.set(a,5);
  R.set(b,1);
  R.syzygy(a,b,x1,y1);
  R.set(x2,1);
  R.set(y2,-5);

  EXPECT_EQ(R.compare_elems(x1,x2), 0);
  EXPECT_EQ(R.compare_elems(y1,y2), 0);


  // Case 3: b is -1                                                                                                                                          
                                                                                                                                                              
  R.set(a,5);
  R.set(b,-1);
  R.syzygy(a,b,x1,y1);
  R.set(x2,1);
  R.set(y2,5);

  EXPECT_EQ(R.compare_elems(x1,x2), 0);
  EXPECT_EQ(R.compare_elems(y1,y2), 0);  
  
  // Case 4a: general, b is positive
  
  R.set(a, 6);
  R.set(b, 8);
  R.syzygy(a,b,x1,y1);
  R.set(x2,4);
  R.set(y2,-3);

  EXPECT_EQ(R.compare_elems(x1,x2), 0);
  EXPECT_EQ(R.compare_elems(y1,y2), 0);

  // Case 4b: general, b is negative

  R.set(a, 6);
  R.set(b, -8);
  R.syzygy(a,b,x1,y1);
  R.set(x2,4);
  R.set(y2,3);

  EXPECT_EQ(R.compare_elems(x1,x2), 0);
  EXPECT_EQ(R.compare_elems(y1,y2), 0);
  
}


TEST(ARingZZ, computeHashValue)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a;

  R.init(a);
  R.set(a,5);

  EXPECT_EQ(R.compare_elems(R.computeHashValue(a),5),0);

}


TEST(ARingZZ, coerceToLongInteger)
{
  M2::ARingZZ R;
  M2::ARingZZ::ElementType a;
  long b=0;
  
  R.init(a);
  
  fmpz_set_str(&a, "1208925819614629174706176", 10);
  
  EXPECT_FALSE(R.coerceToLongInteger(b,a));

  R.set(a,1048576);
  EXPECT_TRUE(R.coerceToLongInteger(b,a));

  EXPECT_EQ(b,1048576);
  
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:


