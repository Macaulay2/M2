#pragma once

/**
 * @file gb-f4/GBF4Computation.hpp
 * @brief `newf4::GBF4Computation` --- top-level driver for the refactored F4 Gröbner-basis engine.
 *
 * Declares the class that owns all of the new F4's state: a
 * `MonomialHashTable mBasisMonomials` for basis monomials and a
 * separate `mSPairMonomials` for S-pair LCMs, a
 * `PolynomialList mInput` of original generators, a
 * `Basis mGBSoFar` holding the evolving GB and its
 * `GBPolyStatus` flags, a `MonomialLookupTable mMonomialLookup`
 * for fast divisor queries, an `SPairSet mSPairs` of pending
 * overlaps, a `MacaulayMatrix mMacaulayMatrix` reused each
 * degree, and a reference to the user-selected
 * `VectorArithmetic& mVectorArithmetic` backend.
 * `initializeWithMatrix(const Matrix*)` and
 * `initializeWithBasicPolyList(const BasicPolyList&)` are the
 * two ingestion entry points; the remaining public surface is
 * `show*` / `dump*` debug helpers --- the actual GB-construction
 * methods have not been added yet, matching the refactor-in-progress
 * status of this subdir.
 *
 * `GBF4Computation` does not inherit from `GBComputation`; the
 * legacy `GBComputation` virtuals are exposed through
 * `GBF4Interface` (a `GBComputation` subclass) which holds a
 * `unique_ptr<GBF4Computation>` and forwards. All `gb-f4/`
 * classes live in `namespace newf4` so their names don't
 * collide with the older F4 in `f4/`. The `Strategy` enum is
 * forward-declared here but **defined** in `GBF4Interface.hpp`
 * (`enum class Strategy { Normal };`) with a single value at
 * present --- the variant-selector switch is reserved space,
 * not a live mechanism. Modern counterpart of the legacy
 * `f4/f4-computation.hpp`: same algorithmic shape (per-degree
 * S-pair selection, build the Macaulay matrix, row-reduce,
 * promote new pivots, prune subsumed S-pairs) but with each
 * concern in its own class instead of being interleaved.
 *
 * @see Basis.hpp
 * @see MacaulayMatrix.hpp
 * @see MonomialHashTable.hpp
 * @see MonomialLookupTable.hpp
 * @see PolynomialList.hpp
 * @see SPairs.hpp
 * @see GBF4Interface.hpp
 */

#include "Basis.hpp"
#include "MacaulayMatrix.hpp"
#include "MonomialHashTable.hpp"
#include "MonomialLookupTable.hpp"
#include "PolynomialList.hpp"
#include "SPairs.hpp"
#include "../VectorArithmetic.hpp"

class FreeModule;

namespace newf4 {

enum class Strategy;
  
class GBF4Computation
{
 private:
  // Core Data:
  MonomialHashTable mBasisMonomials;     // table for monomials in the basis
  PolynomialList mInput; // as a MonomialHashTable.
  Basis mGBSoFar; // same hash table here.
  
  const FreeModule* mFreeModule;  // for debugging information *only*

  MonomialLookupTable mMonomialLookup;   // put this in Basis or not?
  MonomialHashTable mSPairMonomials;     // table for the LCMs of spairs
  SPairSet mSPairs;
  const VectorArithmetic& mVectorArithmetic;
  MacaulayMatrix mMacaulayMatrix;

  std::vector<int> mVariableWeights;

  // Support Data:
  // GB of base ring (for GBs over a quotient)
  // Hilbert function hint information
  // how to get spairs with exterior/skew variables
public:
  GBF4Computation(const VectorArithmetic& vectorArithmetic,
                  const FreeModule* freeModule,
                  const std::vector<int>& variableWeights,
                  Strategy strategy);

  void initializeWithMatrix(const Matrix* M);
  void initializeWithBasicPolyList(const BasicPolyList& basicPolyList);

  void dumpBasisMonomials() const;

  const PolynomialList& getInput() const { return mInput; }

  // these are all for debugging only -- copies made, etc.
  void showInput() const;
  void showGBStatusArray() const;
  void showMinimalBasis() const;
  void showFullBasis() const;

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
