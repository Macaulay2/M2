#pragma once

#include "BasicPolyList.hpp"
#include "GBF4Computation.hpp"
#include "PolynomialList.hpp"
#include "../e/comp-gb.hpp"
#include "../matrix-stream.hpp"

class Matrix;

auto createGBF4Interface(const Matrix *inputMatrix,
                         const std::vector<int>& variableWeights, // what is this, do we need it?
                         int strategy,
                         int numThreads
                         ) -> GBComputation*;

namespace newf4 {

class GBF4Computation;
enum class Strategy { Normal };


auto createGBF4Interface(const Matrix *inputMatrix,
                         const std::vector<int>& variableWeights, // what is this, do we need it?
                         Strategy strategy, // do we need this?
                         int numThreads
                         ) -> GBComputation*;
void populateComputation(const Matrix* M, GBF4Computation& C);
  
class GBF4Interface : public GBComputation
{
private:
  // Upward facing data fields
  const PolynomialRing *mOriginalRing;
  const FreeModule * mFreeModule;  // determines whether the monomial order is a
  std::unique_ptr<VectorArithmetic> mVectorArithmetic;

  // TODO: Add a MonoidData class and store information about the monoid underlying
  //        the polynomial ring without all the cruft of the usual Monoid

  // F4 computation itself
  std::unique_ptr<GBF4Computation> mComputation;

public:
  GBF4Interface(const PolynomialRing* originalRing,
                const Matrix* inputMatrix,
                const std::vector<int>& variableWeights,
                Strategy strategy,
                int numThreads
                );

  GBF4Interface(const PolynomialRing* originalRing,
                const FreeModule* freeModule,
                const BasicPolyList& basicPolyList,
                const std::vector<int>& variableWeights,
                Strategy strategy,
                int numThreads
                );

  ~GBF4Interface() override;
  
  void remove_gb() override { }

  enum ComputationStatusCode computation_is_complete();

  bool stop_conditions_ok() override { return true; }

  void start_computation() override { };

  const PolynomialRing *get_ring() const override { return mOriginalRing; }

  Computation /* or null */ *set_hilbert_function(const RingElement *h) override { return nullptr; }

  const Matrix /* or null */ *get_gb() override { return nullptr; }

  const Matrix /* or null */ *get_mingens() override { return nullptr; }

  const Matrix /* or null */ *get_change() override { return nullptr; }

  const Matrix /* or null */ *get_syzygies() override { return nullptr; }

  const Matrix /* or null */ *get_initial(int nparts) override { return nullptr; }

  const Matrix /* or null */ *matrix_remainder(const Matrix *m) override { return nullptr; }

  M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient) override { return false; }

  int contains(const Matrix *m) override { return 0; }

  void text_out(buffer &o) const override { }
  /* This displays statistical information, and depends on the
     M2_gbTrace value */

  int complete_thru_degree() const override { return 0; }
  // The computation is complete up through this degree.

  void show() const override { }  // debug display

  const GBF4Computation& computation() const { return *mComputation; }
};

// utility function for show functions and returning the result
inline const Matrix* toMatrix(const FreeModule *target, const PolynomialList& Fs)
{
  MatrixStream S(target);
  toStream(Fs, S);
  return S.value();
}

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
