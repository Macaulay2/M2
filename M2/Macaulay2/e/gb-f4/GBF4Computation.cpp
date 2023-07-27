#include "GBF4Computation.hpp"

newf4::GBF4Computation::GBF4Computation(
    const VectorArithmetic& vectorArithmetic,
    const std::vector<int>& variableWeights,
    newf4::Strategy strategy)
  : mBasisMonomials(),
      mInput(vectorArithmetic, mBasisMonomials),
      mGBSoFar(vectorArithmetic, mBasisMonomials),
      mMonomialLookup(),
      mSPairMonomials(),
      mSPairs(),
      mVectorArithmetic(vectorArithmetic),
      mMacaulayMatrix()
{
}

// Local Variables:
// indent-tabs-mode: nil
// End:
