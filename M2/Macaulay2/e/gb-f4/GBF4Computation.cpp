#include "GBF4Computation.hpp"
#include "../matrix-stream.hpp"
#include <iostream>

namespace newf4 {

GBF4Computation::GBF4Computation(
    const VectorArithmetic& vectorArithmetic,
    const std::vector<int>& variableWeights,
    Strategy strategy)
  : mBasisMonomials(),
      mInput(vectorArithmetic, mBasisMonomials),
      mGBSoFar(vectorArithmetic, mBasisMonomials),
      mMonomialLookup(),
      mSPairMonomials(),
      mSPairs(),
      mVectorArithmetic(vectorArithmetic),
      mMacaulayMatrix(),
      mVariableWeights(variableWeights)
{
}

void GBF4Computation::initializeWithMatrix(const Matrix* M)
{
   PolynomialListStreamCollector S(mVectorArithmetic.ring()->characteristic(),
                                   mVariableWeights.size(),  // TODO: is this correct?
                                   1,  // TODO: is this correct??
                                   mInput);
   matrixToStream(M,S);
}

void GBF4Computation::initializeWithBasicPolyList(const BasicPolyList& basicPolyList)
{
   PolynomialListStreamCollector S(mVectorArithmetic.ring()->characteristic(),
                                   mVariableWeights.size(),  // TODO: is this correct?
                                   1,  // TODO: is this correct??
                                   mInput);
   toStream(basicPolyList,S);
}

void GBF4Computation::dumpBasisMonomials() const
{
   std::cout << "Number of monomials: " << mBasisMonomials.size() << std::endl;
   mBasisMonomials.dump();
}

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
