// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "polyring.hpp"
#include "util-polyring-creation.hpp"
#include "gb-f4/MonomialHashTable.hpp"
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
  for (auto a : m) { hash += a; }
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
      newf4::MonomialIndex m1 = hashtab.find(m, hashFunction(m));
    }
  for (int i=0; i<10000; ++i)
    {
      newf4::MonomialView m({5, 1, 2, i, 3}, B);
      newf4::MonomialIndex m1 = hashtab.find(m, hashFunction(m));
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
  newf4::PolynomialList L(*VA);
  newf4::PolynomialListStreamCollector S(1235952427, 3, 1, L);
  toStream(B, S);
  std::cout << "Number of monomials: " << L.monomialHashTable().size() << std::endl;
  L.monomialHashTable().dump();
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
}
