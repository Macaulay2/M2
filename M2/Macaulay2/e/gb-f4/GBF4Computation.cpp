#include "GBF4Interface.hpp"
#include "GBF4Computation.hpp"
#include "../matrix-stream.hpp"
#include <iostream>

namespace newf4 {

GBF4Computation::GBF4Computation(
    const VectorArithmetic& vectorArithmetic,
    const FreeModule* freeModule,
    const std::vector<int>& variableWeights,
    Strategy strategy)
  : mBasisMonomials(),
      mInput(vectorArithmetic, mBasisMonomials),
      mGBSoFar(vectorArithmetic, mBasisMonomials),
      mFreeModule(freeModule),
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

void GBF4Computation::showInput() const
{
   const Matrix* M = toMatrix(mFreeModule, mInput);
   buffer o;
   M->text_out(o);
   std::cout << "--- Input Polynomials ---" << std::endl; 
   std::cout << o.str() << std::endl;
}

void GBF4Computation::showGBStatusArray() const
{
   auto gbStatus = mGBSoFar.getGBStatusList();
   std::cout << "--- GB Status Array ---" << std::endl; 
   for (auto i = 0; i < gbStatus.size(); ++i) 
   {
      std::cout << i << ": " << toString(gbStatus[i]) << std::endl;
   }
}

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
