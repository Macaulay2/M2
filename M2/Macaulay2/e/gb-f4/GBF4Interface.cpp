#include "GBF4Interface.hpp"

#include "PolynomialList.hpp"

//////////////////////////////////////////////////////////////////////////
// getting polynomials/ideals/submodules to/from this code to M2, files //
//////////////////////////////////////////////////////////////////////////

// TODO: Fix int/Strategy discrepancy.  Make a ComputationStrategy type in util.hpp or comp-gb.hpp?
auto createGBF4Interface(const Matrix *inputMatrix,
                         const std::vector<int>& variableWeights, // what is this, do we need it?
                         int strategy, // do we need this?
                         int numThreads
                         ) -> GBComputation*
{
  return newf4::createGBF4Interface(inputMatrix, variableWeights, newf4::Strategy::Normal, numThreads);  
}

namespace newf4 {

auto createGBF4Interface(const Matrix *inputMatrix,
                         const std::vector<int>& variableWeights, // what is this, do we need it?
                         Strategy strategy, // do we need this?
                         int numThreads
                         ) -> GBComputation*
{
  const PolynomialRing* R = inputMatrix->get_ring()->cast_to_PolynomialRing();
  if (R == nullptr)
    throw exc::engine_error("expected polynomial ring");

  auto C = new GBF4Interface(R,
                             inputMatrix,
                             variableWeights,
                             strategy,
                             numThreads);
  return C;
}

  
GBF4Interface::GBF4Interface(const PolynomialRing* originalRing,
                             const Matrix* inputMatrix,
                             const std::vector<int>& variableWeights,
                             Strategy strategy,
                             int numThreads
                             )
    : mOriginalRing(originalRing),
      mFreeModule(inputMatrix->rows()),
      mVectorArithmetic(std::make_unique<VectorArithmetic>(mOriginalRing->getCoefficients())),
      mComputation(std::make_unique<GBF4Computation>(*mVectorArithmetic,
                                                     mFreeModule,
                                                     variableWeights,
                                                     strategy))
{
    mComputation->initializeWithMatrix(inputMatrix);
    mComputation->dumpBasisMonomials();
    mComputation->showInput();
}

GBF4Interface::GBF4Interface(const PolynomialRing* originalRing,
                             const FreeModule* freeModule,
                             const BasicPolyList& basicPolyList,
                             const std::vector<int>& variableWeights,
                             Strategy strategy,
                             int numThreads
                             )
    : mOriginalRing(originalRing),
      mFreeModule(freeModule),
      mVectorArithmetic(std::make_unique<VectorArithmetic>(mOriginalRing->getCoefficients())),
      mComputation(std::make_unique<GBF4Computation>(*mVectorArithmetic,
                                                     mFreeModule,
                                                     variableWeights,
                                                     strategy))
{
    mComputation->initializeWithBasicPolyList(basicPolyList);
}

GBF4Interface::~GBF4Interface()
{
  // TODO: Clean up properly
}

}; // namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
