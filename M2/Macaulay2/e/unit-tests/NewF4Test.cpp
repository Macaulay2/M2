// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include <iostream>
#include <memory>
#include <bitset>
#include <gtest/gtest.h>

#include "polyring.hpp"
#include "util-polyring-creation.hpp"
#include "gb-f4/MonomialHashTable.hpp"
#include "gb-f4/MonomialLookupTable.hpp"
#include "VectorArithmetic.hpp"
#include "BasicPolyListParser.hpp"
#include "gb-f4/PolynomialList.hpp"

TEST(NewF4, hashstats)
{
  newf4::HashTableStats stats;

  stats.dump();
}

newf4::HashInt hashFunction(const newf4::MonomialView& m)
{
  newf4::HashInt hash = 0;
  //for (auto a = m.cbegin(); a != m.cend(); ++a) { hash += (a.var() + a.power()); }
  for (auto a : m) { hash += a.first + a.second; } 
  return hash;
}

TEST(NewF4, hashtable)
{
  newf4::MonomialHashTable hashtab(5);
  // std::vector<int32_t> mdata{5, 1, 2, 2, 5};
  // newf4::Monomial m(mdata);
  // newf4::MonomialIndex m1 = hashtab.find(m, 7342643);
  // newf4::MonomialIndex m2 = hashtab.find(m, 7342643);
  // std::cout << "m1 = " << m1 << std::endl;
  // EXPECT_EQ(m1, m2);

  MemoryBlock B;
  for (int i=0; i<100000; ++i)
    {
      newf4::MonomialView m({5, 1, 2, i, 3}, B);
      /*newf4::MonomialIndex m1 = */ hashtab.find(m, hashFunction(m));
    }
  for (int i=0; i<10000; ++i)
    {
      newf4::MonomialView m({5, 1, 2, i, 3}, B);
      /*newf4::MonomialIndex m1 = */ hashtab.find(m, hashFunction(m));
    }
  std::cout << std::endl;
  hashtab.dump();

}

TEST(NewF4, matrixstream)
{
  const PolynomialRing* R = simplePolynomialRing(1235952427, {"x", "y", "z"});
  const std::string polys = R"(1*x^1+2*y^1+2*z^1+1235952426
y^1*z^1+494380972*z^2+370785728*y^1+247190485*z^1
y^2+988761941*z^2+741571456*y^1+494380971*z^1
1*z^3+924021576*z^2+700373042*y^1+653289140*z^1
)";


  const Ring *K = R->getCoefficients();
  auto VA = new VectorArithmetic(K);

  BasicPolyList B = parseBasicPolyListFromString(polys, {"x", "y", "z"});
  newf4::MonomialHashTable monHashTable;
  newf4::PolynomialList L(*VA, monHashTable);
  newf4::PolynomialListStreamCollector S(1235952427, 3, 1, L);
  toStream(B, S);
  std::cout << "Number of monomials: " << monHashTable.size() << std::endl;
  monHashTable.dump();
}

TEST(NewF4MonomialLookupTable, mask_creation)
{
  MemoryBlock B;
  for (int i=1; i<10; ++i)
    {
      newf4::MonomialView m({5, 0, 2, i, 3}, B);
      auto mask = newf4::MonomialLookupTable::createMask(m);
      std::cout << std::bitset<64>(mask) << std::endl;
    }

  newf4::MonomialView ab({5,0,1,1,1}, B);
  newf4::MonomialView ac({5,0,1,2,1}, B);
  newf4::MonomialView ab2({5,0,1,1,2}, B);
  auto abMask = newf4::MonomialLookupTable::createMask(ab);
  auto ab2Mask = newf4::MonomialLookupTable::createMask(ab2);
  auto acMask = newf4::MonomialLookupTable::createMask(ac);
  EXPECT_FALSE(newf4::MonomialLookupTable::maskDivides(abMask,acMask));
  EXPECT_TRUE(newf4::MonomialLookupTable::maskDivides(abMask,ab2Mask));

  std::cout << std::endl;
}

TEST(NewF4MonomialLookupTable, monomialDivides)
{
  MemoryBlock B;
  newf4::MonomialView ab({5,0,1,1,1}, B);
  newf4::MonomialView ac({5,0,1,2,1}, B);
  newf4::MonomialView ab2({5,0,1,1,2}, B);
  newf4::MonomialView unit({1},B);
  newf4::MonomialView ab2f({7,0,1,1,2,5,1},B);
  newf4::MonomialView bd({5,1,1,3,1},B);
  newf4::MonomialView abdf({9,0,1,1,1,3,1,5,1},B);
  EXPECT_FALSE(newf4::MonomialView::monomialDivides(ab,ac));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(ab,ab2));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(unit,unit));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(unit,ab2));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(bd,abdf));
  EXPECT_FALSE(newf4::MonomialView::monomialDivides(ab2,abdf));
}

TEST(NewF4MonomialLookupTable, monomialOperations)
{
  MemoryBlock B;
  newf4::MonomialView ab({5,0,1,1,1}, B);
  newf4::MonomialView ac({5,0,1,2,1}, B);
  newf4::MonomialView abc({7,0,1,1,1,2,1}, B);
  newf4::MonomialView a2bc({7,0,2,1,1,2,1}, B);
  newf4::MonomialView b({3,1,1}, B);
  newf4::MonomialView lcm = newf4::MonomialView::lcm(ab,ac,B);
  newf4::MonomialView product = newf4::MonomialView::product(ab,ac,B);
  newf4::MonomialView quotient = newf4::MonomialView::quotient(ab,ac,B);

  std::cout << lcm.size() << std::endl;
  for (auto i = lcm.begin(); i != lcm.end(); ++i)
  {
    std::cout << i.var() << " ";
    std::cout << i.power() << " ";
  }
  std::cout << std::endl;

  std::cout << product.size() << std::endl;
  for (auto i = product.begin(); i != product.end(); ++i)
  {
    std::cout << i.var() << " ";
    std::cout << i.power() << " ";
  }
  std::cout << std::endl;

  std::cout << quotient.size() << std::endl;
  for (auto i = quotient.begin(); i != quotient.end(); ++i)
  {
    std::cout << i.var() << " ";
    std::cout << i.power() << " ";
  }
  std::cout << std::endl;

  EXPECT_TRUE(newf4::MonomialView::monomialDivides(lcm,abc));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(abc,lcm));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(product,a2bc));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(a2bc,product));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(quotient,b));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(b,quotient));
}

  // TODO: need a function to get variable names from a PolynomialRing, Monoid (as a std::vector...)
// TODO: need readMatrix(PolynomialRing, PolyList) -> const Matrix*
// TODO: need a polynomialListFromStream function.
// TODO: maybe even a (String, MatrixStream) -> PolyList, PolynomialList.
// TODO: need a sort function for monomials in a polynomial (put a Polynomial into order)
//   this is more complicated: needs to take (component, MonomialView)'s.  And allow Schreyer orders too...

// Stream functions:
//   Stream objects: these are collectors.  They often have other arguments needed to construct their objects?
//     MatrixStream
//     BasicPolyListStream
//     PolynomialListStream
//   Outputting to a string

//   PolynomialListStream -- a stream class generator?
//   fromStream(Stream) -> PolynomialList, PolyList.
//   typeAtoB(typeA. makeB) -> B // typeA generates stream calls for A, and makeB is a stream collector
//   PolyListGeneratorStream // function whose argument is a MatrixStream
//   PolyListFromStream // class which implements the MatrixStream interface.
//   PolyListToStream...
//   
//   MatrixStreamGenerator: this takes a Collector, and Matrix and sends the Matrix to the Collector.
//   MatrixStreamCollector: this implements the functions in the MatrixStream interface: ideal_done(), ...
//
//   BasicPolyListCollector(some details about the ring, variables?) -> BasicPolyListStream
//   BasicPolyListGenerator(BasicPolyList, Stream)
//   BasicPolyListParser : (std::vector<std::string> varnames, String) -> BasicPolyList
//   BasicPolyListParserGenerator : (std::vector<std::string> varnames, String, Stream)


//   const Matrix* M = readMatrix(R, polys);

  // Now let's make these into object in gb-f4...


  // Start with a string of polynomials.
  // Read it in with the parsing functions.
  // Use PolynomialListStream to get it to this code.

  // What tests to make here?

  // Write it out to a Matrix.
  // Read it from a Matrix.
