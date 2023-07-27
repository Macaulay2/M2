#include "GBF4Interface.hpp"

#include "../matrix-stream.hpp"
#include "PolynomialList.hpp"

//////////////////////////////////////////////////////////////////////////
// getting polynomials/ideals/submodules to/from this code to M2, files //
//////////////////////////////////////////////////////////////////////////

namespace newf4 {

auto createGBF4Interface(
                    const Matrix *inputMatrix,
                    const std::vector<int>& variableWeights, // what is this, do we need it?
                    Strategy strategy // do we need this?
                         ) -> GBComputation*
{
  const PolynomialRing* R = inputMatrix->get_ring()->cast_to_PolynomialRing();
  if (R == nullptr)
    throw exc::engine_error("expected polynomial ring");

  auto C = new GBF4Interface(R,
                             inputMatrix,
                             variableWeights,
                             strategy);
}

  GBF4Interface::GBF4Interface(const PolynomialRing* originalRing,
                               const Matrix* inputMatrix,
                               const std::vector<int>& variableWeights,
                               Strategy strategy
                               )
    : mOriginalRing(originalRing),
      mFreeModule(inputMatrix->rows()),
      mVectorArithmetic(std::make_unique<VectorArithmetic>(mOriginalRing->getCoefficients())),
      mComputation(std::make_unique<GBF4Computation>(*mVectorArithmetic, variableWeights, strategy))
  {
    // populate the input in mComputation
    // make matrix stream from inputMatrix
    // call the 
  }
  
  void populateComputation(const Matrix* M, GBF4Computation& C)
  {
  }

  const Matrix* toMatrix(const FreeModule *target, const PolynomialList& Fs)
{
  MatrixStream S(target);
  toStream(Fs, S);
  return S.value();
}


}; // namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
