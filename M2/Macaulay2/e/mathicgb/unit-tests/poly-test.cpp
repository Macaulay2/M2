// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"
#include "mathicgb/Poly.hpp"

#include "mathicgb/Basis.hpp"
#include "mathicgb/ModuleMonoSet.hpp"
#include "mathicgb/io-util.hpp"
#include "mathicgb/SigPolyBasis.hpp"
#include "mathicgb/SignatureGB.hpp"
#include "mathicgb/MathicIO.hpp"
#include <gtest/gtest.h>
#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>

using namespace mgb;

std::string ideal1 =
"32003 4 1 1 1 1 1 \
2 \
ab+3d2+2d \
c3+a-b+1 \
";

std::string ideal2 = " \
32003 4 \
1 1 1 1 1 \
7 \
bc2-ad2 \
abc-d3 \
b3-acd \
a2d2-cd3 \
ac3d-ab2d2 \
a2c2d-b2d3 \
c3d3-b2d4 \
";

bool testPolyParse(PolyRing* R, std::string s, bool withComponent)
{
  // parse poly, then see if it matches the orig string
  std::istringstream i(s);
  Scanner in(i);
  auto f = MathicIO<>().readPolyDoNotOrder(*R, withComponent, in);
  std::ostringstream o;
  MathicIO<>().writePoly(f, withComponent, o);
  //  std::cout << "orig = " << s << std::endl;
  //  std::cout << "f    = " << o.str() << std::endl;
  return o.str() == s;
}
bool testPolyParse2(PolyRing* R, std::string s, std::string answer)
{
  // parse poly, then see if it matches the orig string
  std::istringstream i(s);
  Scanner scanner(i);
  auto f = MathicIO<>().readPolyDoNotOrder(*R, false, scanner);
  std::ostringstream o;
  MathicIO<>().writePoly(f, false, o);
  //  std::cout << "orig = " << s << std::endl;
  //  std::cout << "f    = " << o.str() << std::endl;
  return o.str() == answer;
}

TEST(Poly,parse) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  EXPECT_TRUE(testPolyParse(R.get(), "3a<1>+1<0>", true));
  EXPECT_TRUE(testPolyParse(R.get(), "3a+1", false));
  EXPECT_TRUE(testPolyParse(R.get(), "3a<1>+13af3<0>+14cde<0>", true));
  EXPECT_TRUE(testPolyParse(R.get(), "1<1>+13af3<0>+14cde<0>", true));
}

bool testMonomialParse(PolyRing* R, std::string s)
{
  Monomial m = stringToMonomial(R, s);
  std::string str2 = monomialToString(R,m);
  return s == str2;
}

TEST(Monomial, parse) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  EXPECT_TRUE(testMonomialParse(R.get(), "ab2d<2>"));
  EXPECT_TRUE(testMonomialParse(R.get(), "ab2d<0>"));
  EXPECT_TRUE(testMonomialParse(R.get(), "<13>"));
  EXPECT_TRUE(testMonomialParse(R.get(), "abcdef<0>"));
  EXPECT_TRUE(testMonomialParse(R.get(), "a10b3d4<0>"));
}

TEST(Monomial,compare) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  
  Monomial mone = stringToMonomial(R.get(), "<0>");
  Monomial mone2 = stringToMonomial(R.get(), "1");
  Monomial m1 = stringToMonomial(R.get(), "ab2<0>");
  Monomial m2 = stringToMonomial(R.get(), "a2b<0>");

  EXPECT_TRUE(R->monomialEQ(mone, mone2));

  //  monomial mone = monomialFromString(R, "0 0");
  //  monomial m1 = monomialFromString(R, "0 2 1 2 0 1");
  //  monomial m2 = monomialFromString(R, "0 2 1 1 0 2");

  bool a = R->monomialLT(m1,m2);
  EXPECT_TRUE(a);
  EXPECT_FALSE(R->monomialEQ(m1,m2));
  EXPECT_EQ(LT, R->monomialCompare(m1,m2));

  a = R->monomialLT(mone,m1);
  EXPECT_TRUE(a);
  EXPECT_FALSE(R->monomialEQ(mone,m1));
  EXPECT_EQ(GT, R->monomialCompare(m1,mone));

  a = R->monomialLT(mone,mone);
  EXPECT_FALSE(a);
  EXPECT_TRUE(R->monomialEQ(mone,mone));
  EXPECT_EQ(EQ, R->monomialCompare(mone,mone));

  Monomial b = stringToMonomial(R.get(), "b<0>");
  Monomial c = stringToMonomial(R.get(), "c<0>");

  a = R->monomialLT(b,c);
  EXPECT_FALSE(a);
}

TEST(Monomial,mult) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial m1 = stringToMonomial(R.get(), "ab2<0>");
  Monomial m2 = stringToMonomial(R.get(), "a2b<0>");
  Monomial m3ans = stringToMonomial(R.get(), "a3b3<0>");

  Monomial m3 = R->allocMonomial();
  R->monomialMult(m1,m2,m3);
  EXPECT_TRUE(R->monomialEQ(m3ans,m3));

  R->freeMonomial(m1);
  R->freeMonomial(m2);
  R->freeMonomial(m3);
  R->freeMonomial(m3ans);
}

TEST(Monomial,multTo) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial m1 = stringToMonomial(R.get(), "ab2<0>");
  Monomial m2 = stringToMonomial(R.get(), "a2b<0>");
  Monomial m3ans = stringToMonomial(R.get(), "a3b3<0>");
  R->monomialMultTo(m1,m2);
  EXPECT_TRUE(R->monomialEQ(m3ans,m1));

  R->freeMonomial(m1);
  R->freeMonomial(m2);
  R->freeMonomial(m3ans);
}

TEST(Monomial, divide) {
  // test of monomialDivide, monomialIsDivisibleBy, monomialQuotientAndMult
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial m1 = stringToMonomial(R.get(), "ab2<0>");
  Monomial m2 = stringToMonomial(R.get(), "a2b<0>");
  Monomial m3ans = stringToMonomial(R.get(), "a3b3<0>");
  Monomial m3 = R->allocMonomial();
  Monomial m1a = R->allocMonomial();

  R->monomialMult(m1,m2,m3);
  EXPECT_TRUE(R->monomialIsDivisibleBy(m3,m2));
  EXPECT_FALSE(R->monomialIsDivisibleBy(m2,m3));
  R->monomialDivide(m3,m2,m1a);
  EXPECT_TRUE(R->monomialEQ(m1,m1a));

  R->freeMonomial(m1);
  R->freeMonomial(m2);
  R->freeMonomial(m3);
  R->freeMonomial(m3ans);
  R->freeMonomial(m1a);
}

TEST(Monomial, monomialQuotientAndMult) {
  // test of monomialQuotientAndMult
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial m1 = stringToMonomial(R.get(), "ab2f2<0>");
  Monomial m2 = stringToMonomial(R.get(), "af<0>");
  Monomial m3 = stringToMonomial(R.get(), "<2>");

  Monomial n = R->allocMonomial();
  Monomial n1 = R->allocMonomial();
  Monomial na = R->allocMonomial();

  R->monomialQuotientAndMult(m1,m2,m3,n);
  R->monomialDivide(m1,m2,n1);  // m1//m2
  R->monomialMult(n1,m3,na); // m1//m2 * m3  should be n

  EXPECT_TRUE(R->monomialEQ(n, na));

  R->freeMonomial(m1);
  R->freeMonomial(m2);
  R->freeMonomial(m3);
  R->freeMonomial(n);
  R->freeMonomial(n1);
  R->freeMonomial(na);
}

void testMonomialOps(const PolyRing* R, std::string s1, std::string s2)
{
  Monomial m1 = stringToMonomial(R, s1);
  Monomial m2 = stringToMonomial(R, s2);
  Monomial m3 = stringToMonomial(R, "abcdef<0>");

 
  Monomial m4 = R->allocMonomial();
  Monomial lcm = R->allocMonomial();
  Monomial m8 = R->allocMonomial();
  Monomial m1a = R->allocMonomial();
  Monomial m2a = R->allocMonomial();
  Monomial m1b = R->allocMonomial();
  Monomial m2b = R->allocMonomial();

  R->monomialMult(m1,m2,m4);
  R->monomialLeastCommonMultiple(m1,m2,lcm);

  // lcm(m1,m2)/m1, lcm(m1,m2)/m2:  relatively prime
  EXPECT_TRUE(R->monomialIsDivisibleBy(lcm, m1));
  EXPECT_TRUE(R->monomialIsDivisibleBy(lcm, m2));
  R->monomialDivide(lcm, m1, m1a);
  R->monomialDivide(lcm, m2, m2a);
  EXPECT_TRUE(R->monomialRelativelyPrime(m1a,m2a));

  EXPECT_TRUE(R->monomialIsDivisibleBy(lcm, m1a));
  EXPECT_TRUE(R->monomialIsDivisibleBy(lcm, m2a));
  R->monomialDivide(lcm, m1a, m1b);
  R->monomialDivide(lcm, m2a, m2b);
  EXPECT_TRUE(R->monomialEQ(m1, m1b));
  EXPECT_TRUE(R->monomialEQ(m2, m2b));
  R->monomialMult(m1a, m2a, m8);

  size_t supp1 = R->monomialSizeOfSupport(m1a);
  size_t supp2 = R->monomialSizeOfSupport(m2a);
  size_t supp = R->monomialSizeOfSupport(m8);
  EXPECT_EQ(supp1+supp2, supp)
    << monomialToString(R,m1) << " " << monomialToString(R,m2) << "\n"
    << monomialToString(R,m1a) << " " << monomialToString(R,m2a) << " "
    << monomialToString(R,m8);
}

TEST(Monomial, ops)
{
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  testMonomialOps(R.get(), "ab2f2<0>", "bc2df3<0>");
  testMonomialOps(R.get(), "ab2f2<0>", "<0>");
  testMonomialOps(R.get(), "<0>", "<0>");
  testMonomialOps(R.get(), "a<0>", "a<0>");
  testMonomialOps(R.get(), "a<0>", "b<0>");
  testMonomialOps(R.get(), "a10b10c10d10e10f10<0>", "b2f5<0>");
}

TEST(Monomial, ei)
{
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial m1 = stringToMonomial(R.get(), "<1>");
  Monomial m1a = R->allocMonomial();
  R->monomialEi(1, m1a);
  EXPECT_TRUE(R->monomialEQ(m1,m1a));


  m1 = stringToMonomial(R.get(), "<0>");
  R->monomialEi(0, m1a);
  EXPECT_TRUE(R->monomialEQ(m1,m1a));

  m1 = stringToMonomial(R.get(), "<1>");
  R->monomialEi(1, m1a);
  EXPECT_TRUE(R->monomialEQ(m1,m1a));

  m1 = stringToMonomial(R.get(), "<10000>");
  R->monomialEi(10000, m1a);
  EXPECT_TRUE(R->monomialEQ(m1,m1a));
}

TEST(Monomial, strict)
{
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial m1 = stringToMonomial(R.get(), "ab2c3d4e");
  Monomial m2 = stringToMonomial(R.get(), "ab2c3d4");
  Monomial m3 = stringToMonomial(R.get(), "ab2c3d4");
  Monomial m4 = stringToMonomial(R.get(), "ab2c3d3e");

  EXPECT_TRUE(R->monomialHasStrictlyLargerExponent(m1,m2,m3));
  EXPECT_FALSE(R->monomialHasStrictlyLargerExponent(m1,m2,m4));
}

TEST(Monomial, divideToNegative)
{
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial m1 = stringToMonomial(R.get(), "ab100<0>");
  Monomial m2 = stringToMonomial(R.get(), "ab2c3d4<0>");
  Monomial m3 = R->allocMonomial();
  Monomial m4 = R->allocMonomial();
  Monomial m5 = R->allocMonomial();
  Monomial mone = stringToMonomial(R.get(), "<0>");

  R->monomialDivideToNegative(m1,m2,m3);
  R->monomialDivideToNegative(m2,m1,m4);
  R->monomialMult(m3,m4,m5);

  EXPECT_TRUE(R->monomialEQ(m5,mone));

  m3 = stringToMonomial(R.get(), "ab2c3d4");
  m4 = stringToMonomial(R.get(), "ab2c3d3e");

  EXPECT_TRUE(R->monomialHasStrictlyLargerExponent(m1,m2,m3));
  EXPECT_FALSE(R->monomialHasStrictlyLargerExponent(m2,m2,m4));
}

TEST(Monomial, findSignature)
{
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));

  Monomial v1 = stringToMonomial(R.get(), "abef");
  Monomial v2 = stringToMonomial(R.get(), "acdf2");
  Monomial u1 = stringToMonomial(R.get(), "f5<13>");
  Monomial t1 = R->allocMonomial();
  Monomial t1ans = stringToMonomial(R.get(), "cdf6<13>");

  R->monomialFindSignature(v1,v2,u1,t1);
  EXPECT_TRUE(R->monomialEQ(t1,t1ans));
}

//#warning "remove this code"
#if 0
bool testMonomialOldParse(PolyRing *R, std::string s)
{
  monomial m = monomialParseFromString(R, s);
  std::string str2 = monomialDisplay(R,m);
  return s == str2;
}

TEST(OldMonomial, parse) {
  PolyRing *R = ringFromString("32003 6 1\n1 1 1 1 1 1");
  EXPECT_TRUE(testMonomialOldParse(R, "ab2d<2>"));
  EXPECT_TRUE(testMonomialOldParse(R, "ab2d<0>"));
  EXPECT_TRUE(testMonomialOldParse(R, "<13>"));
  EXPECT_TRUE(testMonomialOldParse(R, "abcdef<0>"));
  EXPECT_TRUE(testMonomialOldParse(R, "a10b3d4<0>"));
}

TEST(OldMonomial,compare) {
  PolyRing *R = ringFromString("32003 6 1\n1 1 1 1 1 1");
  monomial mone = monomialFromString(R, "0 0");
  monomial m1 = monomialFromString(R, "0 2 1 2 0 1");
  monomial m2 = monomialFromString(R, "0 2 1 1 0 2");

  bool a = R->monomialLT(m1,m2);
  EXPECT_TRUE(a);
  EXPECT_FALSE(R->monomialEQ(m1,m2));
  EXPECT_EQ(LT, R->monomialCompare(m1,m2));

  a = R->monomialLT(mone,m1);
  EXPECT_TRUE(a);
  EXPECT_FALSE(R->monomialEQ(mone,m1));
  EXPECT_EQ(GT, R->monomialCompare(m1,mone));

  a = R->monomialLT(mone,mone);
  EXPECT_FALSE(a);
  EXPECT_TRUE(R->monomialEQ(mone,mone));
  EXPECT_EQ(EQ, R->monomialCompare(mone,mone));

  monomial b = monomialFromString(R, "0 1 1 1");
  monomial c = monomialFromString(R, "0 1 2 1");
  a = R->monomialLT(b,c);
  EXPECT_FALSE(a);
}

TEST(OldMonomial,mult) {
  PolyRing *R = ringFromString("32003 6 1\n1 1 1 1 1 1");
  monomial m1 = monomialFromString(R, "0 2 1 2 0 1"); // ab2
  monomial m2 = monomialFromString(R, "0 2 1 1 0 2"); // a2b
  // std::cout << "m1 is " << monomialToString(R,m1) << std::endl;
  //std::cout << "m2 is " << monomialToString(R,m2) << std::endl;
  monomial m3 = new int[R->maxMonomialSize()];
  R->monomialMult(m1,m2,m3);
  monomial m3ans = monomialFromString(R, "0 2 1 3 0 3"); // a3b3
  //std::cout << "answer is " << monomialToString(R,m3) << std::endl;
  EXPECT_TRUE(R->monomialEQ(m3ans,m3));
}

TEST(OldMonomial,multTo) {
  PolyRing *R = ringFromString("32003 6 1\n1 1 1 1 1 1");
  monomial m1 = monomialFromString(R, "0 3 5 2 1 2 0 1"); // ab2
  monomial m2 = monomialFromString(R, "0 2 1 1 0 2"); // a2b
  R->monomialMultTo(m1,m2);
  monomial m3ans = monomialFromString(R, "0 3 5 2 1 3 0 3"); // a3b3
  EXPECT_TRUE(R->monomialEQ(m3ans,m1));
}

TEST(OldMonomial, divide) {
  // test of monomialDivide, monomialIsDivisibleBy, monomialQuotientAndMult
  PolyRing *R = ringFromString("32003 6 1\n1 1 1 1 1 1");
  monomial m1 = monomialFromString(R, "0 3 5 2 1 2 0 1"); // ab2
  monomial m2 = monomialFromString(R, "0 2 1 1 0 2"); // a2b
  monomial m3 = new int[R->maxMonomialSize()];
  monomial m1a = new int[R->maxMonomialSize()];
  R->monomialMult(m1,m2,m3);
  EXPECT_TRUE(R->monomialIsDivisibleBy(m3,m2));
  EXPECT_FALSE(R->monomialIsDivisibleBy(m2,m3));
  R->monomialDivide(m3,m2,m1a);
  EXPECT_TRUE(R->monomialEQ(m1,m1a));
}

TEST(OldMonomial, monomialQuotientAndMult) {
  // test of monomialQuotientAndMult
  PolyRing *R = ringFromString("32003 6 1\n1 1 1 1 1 1");
  monomial m1 = monomialFromString(R, "0 3 5 2 1 2 0 1"); // ab2f2
  monomial m2 = monomialFromString(R, "0 2 5 1 0 1"); // af
  monomial m3 = monomialFromString(R, "2 0"); // e_2
  monomial n = new int[R->maxMonomialSize()];
  monomial n1 = new int[R->maxMonomialSize()];
  monomial na = new int[R->maxMonomialSize()];
  R->monomialQuotientAndMult(m1,m2,m3,n);
  R->monomialDivide(m1,m2,n1);  // m1//m2
  R->monomialMult(n1,m3,na); // m1//m2 * m3  should be n
  //  std::cout << "n is " << monomialToString(R,n) << std::endl;
  //  std::cout << "na is " << monomialToString(R,na) << std::endl;

  EXPECT_TRUE(R->monomialEQ(n, na));
}
#endif


TEST(Coeff, reciprocal) {
  std::unique_ptr<PolyRing> R(ringFromString("11 6 1\n1 1 1 1 1 1"));
  coefficient vals[10] = {1,2,3,4,5,6,7,8,9,10};
  coefficient ans[10] = {1,6,4,3,9,2,8,7,5,10};
  for (int i=0; i<10; i++)
    {
      coefficient a = vals[i];
      R->coefficientReciprocalTo(a);
      EXPECT_EQ(ans[i], a);
    }
}

TEST(Coeff, reciprocal2) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  for (int i=1; i<32002; i++)
    {
      coefficient a1;
      coefficient a = i;
      R->coefficientReciprocalTo(a);
      R->coefficientDivide(1,i,a1);
      EXPECT_EQ(a,a1);
      R->coefficientMultTo(a,i);
      EXPECT_TRUE(a > 0);
      EXPECT_TRUE(a < 32003);
      EXPECT_TRUE(a == 1);
    }
}

TEST(Coeff, negate) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  for (int i=1; i<32002; i++)
    {
      coefficient a1 = i;
      coefficient a = i;
      R->coefficientNegateTo(a);
      EXPECT_TRUE(a > 0);
      EXPECT_TRUE(a < 32003);
      R->coefficientAddTo(a1,a);
      EXPECT_EQ(0,a1);
    }
}

TEST(Coeff, addone) {
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  for (int i=0; i<32002; i++)
    {
      coefficient a1 = i;
      coefficient a = i;
      R->coefficientAddOneTo(a);
      EXPECT_TRUE(a > 0);
      EXPECT_TRUE(a < 32003);
      R->coefficientAddTo(a1,1);
      EXPECT_EQ(a,a1);
    }
}

TEST(MTArray,DivList1) {
  // We create a table here
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  auto M = ModuleMonoSet::make(R->monoid(), 1, 6, false);
  std::string mons[2] = {
    "abc<1>",
    "a2d<1>"
  };
  for (int i=0; i<2; i++)
    {
      monomial m = monomialParseFromString(R.get(), mons[i]);
      M->insert(m);
    }
  //  M.display(std::cout);

  // Now we test membership
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "abc4d<1>")));
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "a2d2<1>")));
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "abc<1>")));
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "a2d<1>")));
  EXPECT_FALSE(M->member(monomialParseFromString(R.get(), "a2d<2>")));
  EXPECT_FALSE(M->member(monomialParseFromString(R.get(), "ad<1>")));
}

TEST(MTArray,KDTree1) {
  // We create a table here
  std::unique_ptr<PolyRing> R(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  std::unique_ptr<ModuleMonoSet> M(ModuleMonoSet::make(R->monoid(), 2, 6, false));
  std::string mons[2] = {
    "abc<1>",
    "a2d<1>"
  };
  for (int i=0; i<2; i++)
    {
      monomial m = monomialParseFromString(R.get(), mons[i]);
      M->insert(m);
    }
  //  M.display(std::cout);

  // Now we test membership
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "abc4d<1>")));
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "a2d2<1>")));
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "abc<1>")));
  EXPECT_TRUE(M->member(monomialParseFromString(R.get(), "a2d<1>")));
  EXPECT_FALSE(M->member(monomialParseFromString(R.get(), "a2d<2>")));
  EXPECT_FALSE(M->member(monomialParseFromString(R.get(), "ad<1>")));
}

//#warning "remove this code"
#if 0
bool test_find_signatures(const PolyRing *R, 
			  const_monomial u1, 
			  const_monomial u2, 
			  const_monomial v1, 
			  const_monomial v2)
{
  monomial g = new int[R->maxMonomialSize()];
  monomial t1 = new int[R->maxMonomialSize()];
  monomial t2 = new int[R->maxMonomialSize()];
  monomial x1 = new int[R->maxMonomialSize()];
  monomial x2 = new int[R->maxMonomialSize()];
  monomial y1 = new int[R->maxMonomialSize()];
  monomial y2 = new int[R->maxMonomialSize()];
  monomial v1v2 = new int[R->maxMonomialSize()];
  monomial x1g = new int[R->maxMonomialSize()];
  monomial p = new int[R->maxMonomialSize()];
  monomial m = new int[R->maxMonomialSize()];
  

  R->monomialFindSignature(v1,v2,u1,t1);
  R->monomialFindSignature(v2,v1,u2,t2);

  R->monomialMult(v1, v2, p);
  R->monomialLeastCommonMultiple(v1, v2, m);
  R->monomialDivide(p, m, g);
  //R->monomialGreatestCommonDivisor(v1, v2, g);

  // check that v1*t1 == v2*t2
  // v1*v2 == g * (v1*t1)
  R->monomialDivide(t1,u1,y1);
  R->monomialDivide(t2,u2,y2); // remove mult by signatures
  R->monomialMult(v1,y1,x1);
  R->monomialMult(v2,y2,x2);
  R->monomialMult(v1,v2,v1v2);
  R->monomialMult(x1,g,x1g);
#if 0
  std::cout << "v1 is " << monomialToString(R,v1) << std::endl;
  std::cout << "v2 is " << monomialToString(R,v2) << std::endl;
  std::cout << "u1 is " << monomialToString(R,u1) << std::endl;
  std::cout << "u2 is " << monomialToString(R,u2) << std::endl;
  std::cout << "t1 is " << monomialToString(R,t1) << std::endl;
  std::cout << "22 is " << monomialToString(R,t2) << std::endl;
  std::cout << "x1 is " << monomialToString(R,x1) << std::endl;
  std::cout << "x2 is " << monomialToString(R,x2) << std::endl;
  std::cout << "y1 is " << monomialToString(R,y1) << std::endl;
  std::cout << "y2 is " << monomialToString(R,y2) << std::endl;
  std::cout << "g is " << monomialToString(R,g) << std::endl;
  std::cout << "v1v2 is " << monomialToString(R,v1v2) << std::endl;
  std::cout << "x1g is " << monomialToString(R,x1g) << std::endl;
#endif
  if (!R->monomialEQ(x1,x2)) return false;
  if (!R->monomialEQ(v1v2,x1g)) return false;

  return true;
}

TEST(OldMonomial, findSignatures) {
  PolyRing *R = ringFromString("32003 6 1\n1 1 1 1 1 1");
  monomial v1 = monomialFromString(R, "0 3  5 2  1 2  0 1"); // ab2f2
  monomial v2 = monomialFromString(R, "0 3  2 3  1 1  0 1"); // abc3
  monomial u1 = monomialFromString(R, "2 3  4 1  1 2  0 1"); // 
  monomial u2 = monomialFromString(R, "3 2  1 1  0 2"); // 
  EXPECT_TRUE(test_find_signatures(R,u1,u2,v1,v2));
}
#endif

TEST(Ideal,readwrite) {
  // This also tests Poly::iterator
  std::unique_ptr<Basis> I = basisParseFromString(ideal1);
  size_t ngens = I->viewGenerators().size();
  EXPECT_TRUE(2 == ngens);

  // now read and write each generator
  for (size_t i=0; i<ngens; i++)
    {
      const Poly *f = I->getPoly(i);
      std::ostringstream o;
      MathicIO<>().writePoly(*f, false, o);
      std::stringstream ifil(o.str());
      Scanner scanner(ifil);
      EXPECT_TRUE(MathicIO<>().readPoly(f->ring(), false, scanner) == *f);
    }
}

TEST(Poly,lead) {
  // This also tests Poly::iterator, Poly::read, Poly::write
  std::unique_ptr<Basis> I = basisParseFromString(ideal1);
  std::unique_ptr<const PolyRing> R(I->getPolyRing());
  const auto& monoid = R->monoid();
  monomial lm = stringToMonomial(R.get(), "ab");
  EXPECT_TRUE(monoid.equal(lm, I->getPoly(0)->leadMono()));
  EXPECT_EQ(1, I->getPoly(0)->leadCoef());
  EXPECT_EQ(0, monoid.component(I->getPoly(0)->leadMono()));
  R->freeMonomial(lm);
}
