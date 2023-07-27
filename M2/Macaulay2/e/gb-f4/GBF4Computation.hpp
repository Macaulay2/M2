#pragma once

#include "Basis.hpp"
#include "MacaulayMatrix.hpp"
#include "MonomialHashTable.hpp"
#include "MonomialLookupTable.hpp"
#include "PolynomialList.hpp"
#include "SPairs.hpp"
#include "../VectorArithmetic.hpp"

namespace newf4 {

enum class Strategy;
  
class GBF4Computation
{
 private:
  // Core Data:
  MonomialHashTable mBasisMonomials;     // table for monomials in the basis
  PolynomialList mInput; // as a MonomialHashTable.
  Basis mGBSoFar; // same hash table here.

  MonomialLookupTable mMonomialLookup;   // put this in Basis or not?
  MonomialHashTable mSPairMonomials;     // table for the LCMs of spairs
  SPairSet mSPairs;
  const VectorArithmetic& mVectorArithmetic;
  MacaulayMatrix mMacaulayMatrix;

  // Support Data:
  // GB of base ring (for GBs over a quotient)
  // Hilbert function hint information
  // how to get spairs with exterior/skew variables
public:
  GBF4Computation(const VectorArithmetic& vectorArithmetic,
                  const std::vector<int>& variableWeights,
                  Strategy strategy);
};

  // Steps:
  //  1. "Read in" the example (uses MatrixStream stuff)
  //     Set the ring information (variables names, #vars, charac, ??)
  //     Reads into a PolynomialList, which is in the Basis.
  //  2.    
// a separate computation class will contain the necessary stuff for
// computations over QQ, rational reconstruction, etc

} // end newf4 namespace

// Local Variables:
// indent-tabs-mode: nil
// End:
