// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "ZZp.hpp"
#include "aring-ffpack.hpp"

const int nelements = 200;
long randomVals[nelements] = {
  2666036, 85344567, 71531106, 8755168, 53852118, 88167705, 22475268, 41297550, 91010248, 44033907, 
  33751711, 95042283, 701143, 62443598, 55048281, 17723663, 85240587, 25979106, 47819861, 90244528, 
  11275250, 22798387, 68996632, 76613634, 48760025, 24733508, 27522895, 56224559, 66749385, 49236242, 
  42314935, 56483458, 17870101, 84017847, 4627176, 94565033, 93275532, 76351828, 90611837, 77643518, 
  59276468, 26344784, 73201185, 91083238, 85501801, 85345398, 83160965, 89062810, 37815670, 8227378, 
  19118805, 63640713, 84232050, 24326912, 73922619, 70743421, 19090900, 59358457, 17634169, 86322817, 
  37134114, 97886677, 82613353, 13603908, 61119326, 40173934, 24107001, 90616769, 72088106, 47299394, 
  97111628, 22494627, 6547534, 68935260, 57921076, 3521860, 45351552, 87950309, 87755396, 43573207, 
  77361055, 97023982, 36678347, 66937610, 3708760, 36009386, 84861416, 84983584, 99073162, 25592499, 
  55121036, 217404, 45246974, 87694278, 18631417, 97176622, 66230759, 70817866, 48984802, 99776399, 
  11107075, 83804819, 1210456, 72894434, 36929177, 57482178, 26142753, 88954986, 53609557, 80607781, 
  98680672, 2989714, 89692551, 96341984, 70402005, 50025094, 3450389, 24030230, 94134112, 85324655, 
  35404622, 74519773, 85307471, 87613072, 80434774, 55494037, 34945135, 36300411, 44911039, 42734386, 
  69273889, 6537915, 18710819, 33093559, 68308135, 29544635, 65475048, 51852879, 85499965, 65398813, 
  61670710, 98961531, 70981648, 24941547, 8504747, 80452330, 45113018, 60607115, 65165225, 73257098, 
  2992669, 51802181, 65378474, 2825238, 78916325, 8474068, 2695455, 53942610, 94297201, 37662100, 
  45567374, 30141840, 32491957, 91837138, 28261048, 71359280, 11933270, 16587656, 64093661, 12235770, 
  16195573, 99396594, 35549941, 98074540, 45023021, 15205552, 3304979, 34666480, 89209262, 10261916, 
  35340937, 98935118, 77343644, 78522496, 46395773, 35429063, 54767177, 14130046, 2726640, 44257782, 
  31615869, 83095327, 15062803, 92772905, 25189126, 86464567, 43372313, 24240507, 96790882, 99639739};

template <typename T>
bool getElement(const T& R, int index, typename T::ElementType& result);

template<>
bool getElement<M2::ARingZZpFFPACK>(const M2::ARingZZpFFPACK& R, int index, M2::ARingZZpFFPACK::ElementType& result)
{
  if (index >= nelements) return false;
  //  int idx = index % R.cardinality();
  R.power(result, R.getGenerator(), randomVals[index]);
  return true;
}

TEST(RingZZp, create) {
  const Z_mod *R = Z_mod::create(101);
  EXPECT_FALSE(R == 0);
  buffer o;
  o << "Ring being tested: ";
  R->text_out(o);
  fprintf(stdout, "%s\n", o.str());
}

template<typename T>
void testSomeMore(const T& R)
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
std::string ringName(const T &R)
{
  buffer o;
  R.text_out(o);
  std::string result = o.str();
  return result;
}

template <typename T>
void testNegate(const T& R)
{
  typename T::ElementType a, b;
  R.init(a);
  R.init(b);
  for (int i=0; i<nelements; i++)
    {
      if (!getElement(R, i, a)) break;
      R.negate(b,a);
      R.add(b,a,b);
      EXPECT_TRUE(R.is_zero(b)); // test: (-a) + a == 0
    }
  R.clear(a);
  R.clear(b);
}

template <typename T>
void testAxioms(const T& R)
{
  typename T::ElementType a, b, c, d, e, f;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(e);
  R.init(f);
  int next = 0;
  while(true)
    {
      if (!getElement(R, ++next, a)) break;
      if (!getElement(R, ++next, b)) break;
      if (!getElement(R, ++next, c)) break;

      // Test commutativity
      // test: a*b = b*a
      // test: a+b == b+a
      R.add(d,a,b);
      R.add(e,b,a);
      EXPECT_TRUE(R.is_equal(d,e));
      R.mult(d,a,b);
      R.mult(e,b,a);
      EXPECT_TRUE(R.is_equal(d,e));

      // Test associativity
      // test: a+(b+c) == (a+b)+c
      // test: a*(b*c) == (a*b)*c
      R.add(d,b,c);
      R.add(d,a,d);
      R.add(e,a,b);
      R.add(e,e,c);
      EXPECT_TRUE(R.is_equal(d,e));
      R.mult(d,b,c);
      R.mult(d,a,d);
      R.mult(e,a,b);
      R.mult(e,e,c);
      EXPECT_TRUE(R.is_equal(d,e));

      // Test distributivity
      // test: a*(b+c) == a*b + a*c
      R.add(d,b,c);
      R.mult(d,a,d);
      R.mult(e,a,b);
      R.mult(f,a,c);
      R.add(e,e,f);
      EXPECT_TRUE(R.is_equal(d,e));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
  R.clear(e);
  R.clear(f);
}

template <typename T>
void testAdd(const T& R)
{
  typename T::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  int next = 0;
  while (true)
    {
      if (!getElement(R, ++next, a)) break;
      if (!getElement(R, ++next, b)) break;
      R.add(c,a,b); // c = a+b
      R.negate(d,b); // d = -b
      R.add(d,c,d);  // d = (a+b) + (-b)
      EXPECT_TRUE(R.is_equal(d,a));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template <typename T>
void testSubtract(const T& R)
{
  typename T::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  int next = 0;
  while (true)
    {
      if (!getElement(R, ++next, a)) break;
      if (!getElement(R, ++next, b)) break;
      R.add(c,a,b); // c = a+b
      R.subtract(d,c,b);  // d = (a+b) - b
      EXPECT_TRUE(R.is_equal(d,a));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template <typename T>
void testMultiply(const T& R)
{
  typename T::ElementType a, b, c, d;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  int next = 0;
  while (true)
    {
      if (!getElement(R, ++next, a)) break;
      if (!getElement(R, ++next, b)) break;
      R.mult(c,a,R.zero());
      EXPECT_TRUE(R.is_equal(c, R.zero()));
      //TODO: finish this with more tests
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
}

template <typename T>
void testPower(const T& R)
{
  // test the following: (x=generator of the finite field, q = card of field)
  // check: x^i != x, for 2 <= i <= characteristic-??
  // x^q == x
  // x^(q-1) == 1
  // x^(-1) * x == 1
  // x^(-2) * x^2 == 1

  // a^2 == a*a, for various a
  // a^3 == a*a*a
  // a^0 == 1, what if a == 0?
  // 1^n == 1, various n
  typename T::ElementType a, b, c, d, one;
  long q = 101; // R.cardinality();
  R.init(one);
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.set_from_int(one, 1);
  int next = 0;
  while (true)
    {
      if (!getElement(R, ++next, a)) break;
      if (!getElement(R, ++next, b)) break;

      R.power(c, a, q); 
      EXPECT_TRUE(R.is_equal(c,a)); // test a^q == q

      if (R.is_zero(a)) continue;

      R.power(c, a, q-1);
      EXPECT_TRUE(R.is_equal(c,one)); // test a^(q-1) == 1

      R.power(c, a, -1);  // test a^-1 * a == 1
      R.mult(c,a,c); 
      EXPECT_TRUE(R.is_equal(c, one));

      R.power(c, a, -2);  // test a^-1 * a == 1
      R.power(d, a, 3); 
      R.mult(d, c, d);
      EXPECT_TRUE(R.is_equal(d, a));
    }
  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
  R.clear(one);
}

#if defined(HAVE_FFLAS_FFPACK)
  TEST(ARingZZpFFPACK, create) {
    M2::ARingZZpFFPACK R(101);
      
    EXPECT_EQ(ringName(R), "ZZpFPACK(101,1)");
    testSomeMore(R);
  }

TEST(ARingZZpFFPACK, arithmetic) {
  M2::ARingZZpFFPACK R(101);
  testNegate(R);
  testAdd(R);
  testSubtract(R);
  //  testMultiply(R);
  //  testDivide(R);
  //  testReciprical(R)
  testPower(R);
  testAxioms(R);

  //TODO: test promote, lift, syzygy(?), (ringmaps)
  // test random number generation?
  // get generator

}

#endif 

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
