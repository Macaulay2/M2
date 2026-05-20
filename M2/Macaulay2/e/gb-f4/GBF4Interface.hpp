#pragma once

/**
 * @file gb-f4/GBF4Interface.hpp
 * @brief Legacy-to-new-F4 adapter exposing `GBF4Computation` through the engine's `GBComputation` API.
 *
 * Declares the `GBComputation` subclass the M2 interpreter
 * sees plus two `createGBF4Interface` factory overloads (one
 * at global scope taking `int strategy`, one in
 * `namespace newf4` taking `Strategy strategy`) and the
 * `populateComputation(const Matrix*, GBF4Computation&)` helper.
 * `GBF4Interface` holds borrowed pointers to the
 * `PolynomialRing` and `FreeModule`, plus owning
 * `unique_ptr<VectorArithmetic>` and
 * `unique_ptr<GBF4Computation>` members carrying the algorithm
 * state. Two constructors cover ingestion from a legacy
 * `Matrix` or from a `BasicPolyList` --- the latter being the
 * representation produced by `BasicPolyListParser` from the
 * MSolve text format and similar engine-neutral inputs.
 *
 * This adapter is currently a **non-functional placeholder**:
 * `start_computation()` has an empty body, and every
 * `GBComputation` reporter override (`get_gb`, `get_mingens`,
 * `get_change`, `get_syzygies`, `get_initial`,
 * `matrix_remainder`, `matrix_lift`, `set_hilbert_function`,
 * `contains`) currently returns `nullptr`, `false`, or `0`.
 * The embedded `GBF4Computation` likewise has no GB-construction
 * methods yet --- ingestion is the only live path. Separating
 * "interface" from "computation" is the eventual design so the
 * templated core stays free of `Matrix` / `Computation` glue.
 * The `toMatrix` helper at the bottom converts a
 * `PolynomialList` back through `MatrixStream` for `show*`
 * paths and is the one piece that is wired up.
 *
 * @see GBF4Computation.hpp
 * @see PolynomialList.hpp
 * @see comp-gb.hpp
 * @see matrix-stream.hpp
 */

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

  Computation /* or null */ *set_hilbert_function(const RingElement *h) override
  {
    (void) h;
    return nullptr;
  }

  const Matrix /* or null */ *get_gb() override { return nullptr; }

  const Matrix /* or null */ *get_mingens() override { return nullptr; }

  const Matrix /* or null */ *get_change() override { return nullptr; }

  const Matrix /* or null */ *get_syzygies() override { return nullptr; }

  const Matrix /* or null */ *get_initial(int nparts) override {
    (void) nparts;
    return nullptr;
  }

  const Matrix /* or null */ *matrix_remainder(const Matrix *m) override
  {
    (void) m;
    return nullptr;
  }

  M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient) override
  {
    (void) m;
    (void) result_remainder;
    (void) result_quotient;
    return false;
  }

  int contains(const Matrix *m) override
  {
    (void) m;
    return 0;
  }

  void text_out(buffer &o) const override
  {
    (void) o;
  }
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
