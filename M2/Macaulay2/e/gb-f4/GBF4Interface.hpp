#pragma once

#include "BasicPolyList.hpp"
#include "GBF4Computation.hpp"
#include "PolynomialList.hpp"
#include "../e/comp-gb.hpp"

class Matrix;

namespace newf4 {

class GBF4Computation;
enum class Strategy {Normal};

auto createGBF4Interface(
                    const Matrix *inputMatrix,
                    const std::vector<int>& variableWeights, // what is this, do we need it?
                    Strategy strategy // do we need this?
                    ) -> GBComputation*;
void populateComputation(const Matrix* M, GBF4Computation& C);
  
class GBF4Interface : public GBComputation
{
private:
  // Upward facing data fields
  const PolynomialRing *mOriginalRing;
  const FreeModule * mFreeModule;  // determines whether the monomial order is a
  std::unique_ptr<VectorArithmetic> mVectorArithmetic;

  // F4 computation itself
  std::unique_ptr<GBF4Computation> mComputation; // pointer 
public:
  GBF4Interface(const PolynomialRing* originalRing,
                const Matrix* inputMatrix,
                const std::vector<int>& variableWeights,
                Strategy strategy
                );

  ~GBF4Interface() override;
  
  void remove_gb() override { }

  enum ComputationStatusCode computation_is_complete();

  bool stop_conditions_ok() override { return true; }

  void start_computation() override;

  const PolynomialRing *get_ring() const override { return mOriginalRing; }

  Computation /* or null */ *set_hilbert_function(const RingElement *h) override;

  const Matrix /* or null */ *get_gb() override;

  const Matrix /* or null */ *get_mingens() override;

  const Matrix /* or null */ *get_change() override;

  const Matrix /* or null */ *get_syzygies() override;

  const Matrix /* or null */ *get_initial(int nparts) override;

  const Matrix /* or null */ *matrix_remainder(const Matrix *m) override;

  M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient) override;

  int contains(const Matrix *m) override;

  void text_out(buffer &o) const override;
  /* This displays statistical information, and depends on the
     M2_gbTrace value */

  int complete_thru_degree() const override;
  // The computation is complete up through this degree.

  void show() const override;  // debug display

};



  
} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
