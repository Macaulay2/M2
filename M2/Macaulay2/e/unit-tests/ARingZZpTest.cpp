// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "reader.hpp"
#include "ZZp.hpp"
#include "aring-ffpack.hpp"
#include "aring-zzp.hpp"
#include "ARingTest.hpp"

static bool maxH_initialized = false;
static mpz_t maxH;

gmp_ZZ getRandomInteger()
{
  if (!maxH_initialized)
    {
      maxH_initialized = true;
      mpz_init(maxH);
      mpz_set_str(maxH, "100000000000", 10);
    }
  return rawRandomInteger(maxH);
}

template<>
void getElement<M2::ARingZZp>(const M2::ARingZZp& R, int index, M2::ARingZZp::ElementType& result)
{
  if (index < 50) 
    R.set_from_int(result, index-25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set_from_mpz(result, a);
    }
}

TEST(RingZZp, create) {
  const Z_mod *R = Z_mod::create(101);
  EXPECT_FALSE(R == 0);
  buffer o;
  o << "Ring being tested: ";
  R->text_out(o);
  fprintf(stdout, "%s\n", o.str());
}

TEST(ARingZZp, create) {
  M2::ARingZZp R(101);


  M2::ARingZZp::ElementType a;
  buffer o;

  ARingElementGenerator<M2::ARingZZp> gen(R);
  R.init(a);
  gen.nextElement(a);

  EXPECT_EQ(ringName(R), "AZZ/101");
  EXPECT_EQ(R.cardinality(), 101);
  EXPECT_EQ(R.characteristic(), 101);
  // Now check what the generator is, as an integer
  R.init(a);
  R.set_var(a, 0);
  R.elem_text_out(o, a, true, true, false);
  std::cout << "generator is " << o.str() << std::endl;
  R.clear(a);

}

TEST(ARingZZp, arithmetic101) {
  M2::ARingZZp R(101);
  testFiniteField(R, ntrials);
}

TEST(ARingZZp, arithmetic2) {
  M2::ARingZZp R(2);
  testFiniteField(R, ntrials);
}

TEST(ARingZZp, arithmetic3) {
  M2::ARingZZp R(3);
  testFiniteField(R, ntrials);
}

TEST(ARingZZp, fromStream)
{
  std::istringstream i("+1234 +345 -235*a");
  M2::ARingZZp R(32003);
  M2::ARingZZp::ElementType a;
  R.init(a);
  while (true)
    {
      while (isspace(i.peek()))
        i.get();

      if (!isdigit(i.peek()) && i.peek() != '+' && i.peek() != '-')
        break;

      fromStream(i, R, a);

      buffer o;
      R.elem_text_out(o, a);
      std::cout << o.str() << " peek: " << i.peek() << std::endl;
    }
  R.clear(a);
}

#if defined(HAVE_FFLAS_FFPACK)
template<>
void getElement<M2::ARingZZpFFPACK>(const M2::ARingZZpFFPACK& R, int index, M2::ARingZZpFFPACK::ElementType& result)
{
  if (index < 50) 
    R.set_from_int(result, index-25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set_from_mpz(result, a);
    }
}

TEST(ARingZZpFFPACK, create) {
  M2::ARingZZpFFPACK R(101);
  
  EXPECT_EQ(ringName(R), "ZZpFPACK(101,1)");
  testSomeMore(R);

  std::cout << "max modulus for ffpack zzp: " << M2::ARingZZpFFPACK::getMaxModulus() << std::endl;

  M2::ARingZZpFFPACK::ElementType a;
  R.init(a);
  R.set_from_int(a, 99);
  R.set_from_int(a, 101);
  R.set_from_int(a, 103);
  R.clear(a);
}

TEST(ARingZZpFFPACK, arithmetic101) {
  M2::ARingZZpFFPACK R(101);
  testFiniteField(R, ntrials);
}

//TODO: commented out because it appears wrong.  Perhaps p=2 isn't allowed here?
// strange: does not fail on my thinkpad...
TEST(ARingZZpFFPACK, arithmetic2) {
  M2::ARingZZpFFPACK R(2);
  testFiniteField(R,ntrials);
}

TEST(ARingZZpFFPACK, arithmetic3) {
  M2::ARingZZpFFPACK R(3);
  testFiniteField(R, ntrials);
}

TEST(ARingZZpFFPACK, arithmetic66000007) {
  M2::ARingZZpFFPACK R(66000007);

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  testPower(R, ntrials);
  testAxioms(R, ntrials);
}

//TODO: commented out because it takes too long:
// Actually: now this characteristic seems too big?!
TEST(ARingZZpFFPACK, arithmetic67108859) {
  M2::ARingZZpFFPACK R(67108859);  

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  testPower(R, ntrials);
  testAxioms(R, ntrials);
}

TEST(ARingZZpFFPACK, arithmetic33500479) {
  M2::ARingZZpFFPACK R(33500479);  

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  testPower(R, ntrials);
  testAxioms(R, ntrials);
}

TEST(ARingZZp, read) {
  std::string a = "-42378489327498312749c3";
  std::istringstream i(a);

  M2::ARingZZp R(101);
  M2::Reader<M2::ARingZZp> reader(R);
  M2::ARingZZp::ElementType b, c;
  R.init(b);
  R.init(c);
  reader.read(i, b);
  R.set_from_int(c, 3);

  EXPECT_TRUE(R.is_equal(b,c));
}
#endif 

////////////////////////////
// Flint ZZ/p arithmetic ///
////////////////////////////
#if defined(HAVE_FLINT)
#include "../aring-zzp-flint.hpp"
template<>
void getElement<M2::ARingZZpFlint>(const M2::ARingZZpFlint& R, int index, M2::ARingZZpFlint::ElementType& result)
{
  if (index < 50) 
    R.set_from_int(result, index-25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set_from_mpz(result, a);
    }
}

TEST(ARingZZpFlint, create) {
  M2::ARingZZpFlint R(101);
  
  EXPECT_EQ(ringName(R), "AZZFlint/101");
  testSomeMore(R);

  M2::ARingZZpFlint::ElementType a;
  R.init(a);
  R.set_from_int(a, 99);
  R.set_from_int(a, 101);
  R.set_from_int(a, 103);
  R.clear(a);
}

TEST(ARingZZpFlint, arithmetic101) {
  M2::ARingZZpFlint R(101);
  testFiniteField(R, ntrials);
}

//TODO: commented out because it appears wrong.  Perhaps p=2 isn't allowed here?
// strange: does not fail on my thinkpad...
TEST(ARingZZpFlint, arithmetic2) {
  M2::ARingZZpFlint R(2);
  testFiniteField(R,ntrials);
}

TEST(ARingZZpFlint, arithmetic3) {
  M2::ARingZZpFlint R(3);
  testFiniteField(R, ntrials);
}

TEST(ARingZZpFlint, arithmetic66000007) {
  M2::ARingZZpFlint R(66000007);

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  testPower(R, ntrials);
  testAxioms(R, ntrials);
}

//TODO: commented out because it takes too long:
// Actually: now this characteristic seems too big?!
TEST(ARingZZpFlint, arithmetic67108859) {
  M2::ARingZZpFlint R(67108859);  

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  testPower(R, ntrials);
  testAxioms(R, ntrials);
}

TEST(ARingZZpFlint, arithmetic33500479) {
  M2::ARingZZpFlint R(33500479);  

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  testPower(R, ntrials);
  testAxioms(R, ntrials);
}

TEST(ARingZZpFlint, arithmetic18446744073709551557) {
  // largest prime < 2^64
  M2::ARingZZpFlint R(18446744073709551557UL);  

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  //  testPower(R, ntrials);  // this test fails: as it expects the characteristic to fit into an int.
  testAxioms(R, ntrials);
}

#endif 


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
