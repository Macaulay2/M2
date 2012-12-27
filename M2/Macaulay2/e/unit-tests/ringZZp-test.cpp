// Copyright 2011 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "ZZp.hpp"
#include "aring-ffpack.hpp"

TEST(RingZZp, create) {
  const Z_mod *R = Z_mod::create(101);
  EXPECT_FALSE(R == 0);
  buffer o;
  o << "Ring being tested: ";
  R->text_out(o);
  fprintf(stdout, "%s\n", o.str());
}

template<typename T>
void testSomeMore(const T &R)
{
  typename T::ElementType a,b,c,d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.set_from_int(a, 20);
  R.set_from_int(b, R.characteristic() - 10);
  R.set_from_int(c, 10);
  R.add(d,a,b);
  EXPECT_TRUE(R.is_equal(c,d));
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template<typename T>
bool testRingName(const T &R, const std::string &ans)
{
  buffer o;
  R.text_out(o);
  std::string result = o.str();
  return result == ans;
}

template<typename T>
std::string ringName(const T &R)
{
  buffer o;
  R.text_out(o);
  std::string result = o.str();
  return result;
}

template <typename T>
void testArithmeticSmallField(const T& R)
{
  M2::ARingZZpFFPACK::ElementType a,b,c,d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  for (int i=0; i<R.characteristic(); i++)
    for (int j=0; j<R.characteristic(); j++)
      {
        R.set_from_int(a, i);
        R.set_from_int(b, j);
        R.init(c);
        
        // addition
        R.add(c,a,b);
        int ans_plus = i+j;
        //if (ans_plus >= R.characteristic()) ans_plus -= R.characteristic();
        R.set_from_int(d, ans_plus);
        EXPECT_TRUE(R.is_equal(c,d));
        
        // subtraction
        int ans_minus = i-j;
        
        // multiplication
        // test that a*b == (a*b % characteristic).
        
        // division
        // test that (a/b) * b == a (if b is non-zero)
      }
  // test promote and lift?
  
  // test random number generation?
  
  // get generator
  
  // clean up: for this ring type, this should do nothing
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template <typename T>
void testArithmeticBigField(const T& R)
{
  EXPECT_TRUE(false);
}

#if defined(HAVE_FFLAS_FFPACK)
  TEST(ARingZZpFFPACK, create) {
    M2::ARingZZpFFPACK R(101);
      
    EXPECT_EQ(ringName(R), "ZZpFPACK(101,1)");
    if (R.characteristic() < 500)
      testArithmeticSmallField(R);
    else
      testArithmeticBigField(R);

    testSomeMore(R);
  }

#endif 

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
