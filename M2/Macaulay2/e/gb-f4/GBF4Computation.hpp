#pragma once

#include "Basis.hpp"
#include "MacaulayMatrix.hpp"
#include "MonomialHashTable.hpp"
#include "MonomialLookupTable.hpp"
#include "SPairs.hpp"
#include "../VectorArithmetic.hpp"

namespace newf4 {

class GBF4Computation
{
 private:
  // Core Data:
  Basis mGBSoFar;
  MonomialLookupTable mMonomialLookup;   // put this in Basis or not?
  MonomialHashTable mBasisMonomials;     // table for monomials in the basis
  MonomialHashTable mSPairMonomials;     // table for the LCMs of spairs
  SPairSet mSPairs;
  VectorArithmetic mVectorArithmetic;
  MacaulayMatrix mMacaulayMatrix;

  // Support Data:
  // GB of base ring (for GBs over a quotient)
  // Hilbert function hint information
  // how to get spairs with exterior/skew variables
};

// a separate computation class will contain the necessary stuff for
// computations over QQ, rational reconstruction, etc

} // end newf4 namespace

// Local Variables:
// indent-tabs-mode: nil
// End: