/* Copyright 2005 - 2021, Michael E. Stillman */

#ifndef _F4Computation_h_
#define _F4Computation_h_

/**
 * @file f4/f4-computation.hpp
 * @brief `F4Computation` --- `GBComputation` adapter around the F4 inner-loop engine.
 *
 * Declares `F4Computation`, the `GBComputation` subclass the
 * interpreter sees when F4 is selected. The class holds borrowed
 * pointers to the user's `PolynomialRing` and `FreeModule`
 * (the latter determines whether the order is Schreyer and the
 * degrees of free-module generators), a `VectorArithmetic*`
 * pinned at construction (the per-coefficient-ring fast path
 * picked from the `aring-zzp-ffpack.hpp` / `aring-zzp-flint.hpp`
 * / generic variants the engine has built), a `MonomialInfo*`
 * describing the F4-specific packed monomial layout, and the
 * owned `F4GB* mF4GB` instance that runs the actual algorithm
 * (deleted in `remove_gb`). Coefficient-ring polymorphism is
 * runtime through `VectorArithmetic`, **not** through `F4GB`
 * template instantiation --- `F4GB` is monomorphic.
 *
 * Standard `GBComputation` virtuals (`start_computation`,
 * `get_gb`, `get_mingens`, `get_change`, `get_syzygies`,
 * `get_initial`, `matrix_remainder`, `matrix_lift`,
 * `contains`, `set_hilbert_function`, `complete_thru_degree`)
 * read state out of the running `F4GB` and re-wrap it as engine
 * `Matrix` values; the `vec` <-> `GBF4Polynomial` translation
 * is delegated to `f4-m2-interface.hpp`'s `F4toM2Interface`.
 * `stop_conditions_ok()` unconditionally returns `true`. The
 * `createF4GB(m, collect_syz, n_rows_to_keep, gb_weights,
 * strategy, use_max_degree, max_degree, numThreads)` free
 * factory below is the actual entry point used by
 * `comp-gb.cpp` when the algorithm strategy selects F4.
 *
 * @see f4.hpp
 * @see f4-m2-interface.hpp
 * @see comp-gb.hpp
 */

#include "comp-gb.hpp"              // for GBComputation
#include "interface/m2-types.h"     // for M2_bool, M2_arrayint
#include "f4/f4.hpp"                // for F4GB
#include "interface/computation.h"  // for ComputationStatusCode
#include "polyring.hpp"             // for PolynomialRing
class Computation;
class FreeModule;
class Matrix;
class MonomialInfo;
class RingElement;
class VectorArithmetic;
class buffer;

/**
 * @brief `GBComputation` subclass that drives an `F4GB` engine instance
 * from the engine-side computation API.
 *
 * @details Owns the `F4GB`, the source `PolynomialRing`, the `FreeModule`
 * the GB lives in, and the optional `RingElement*` Hilbert
 * function used for early termination. Implements
 * `start_computation` / `stop_conditions_ok` /
 * `complete_thru_degree` by delegating to `F4GB`'s
 * degree-by-degree driver, and exposes the standard
 * `get_matrix` / `get_gb` / `get_change` / `get_syzygies`
 * accessors the front end polls when done.
 */
class F4Computation : public GBComputation
{
  // Interface to the F4 linear algebra GB computation
  // Handles: translation of polynomials to/from the correct form
  //          different possible template instantiations

  const PolynomialRing *mOriginalRing;
  const FreeModule * mFreeModule;  // determines whether the monomial order is a
                                   // Schreyer order.
                                   // Also determines degrees of elements in F.
  const VectorArithmetic* mVectorArithmetic;
  MonomialInfo *mMonoid;
  F4GB *mF4GB;
 public:
  F4Computation(const VectorArithmetic* VA,
                const Matrix *m,
                M2_bool collect_syz,
                int n_rows_to_keep,
                M2_arrayint gb_weights,
                int strategy,
                M2_bool use_max_degree,
                int max_degree,
                int numThreads);

  ~F4Computation() override;

  void remove_gb() override { delete mF4GB; }

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

GBComputation *createF4GB(const Matrix *m,
                          M2_bool collect_syz,
                          int n_rows_to_keep,
                          M2_arrayint gb_weights,
                          int strategy,
                          M2_bool use_max_degree,
                          int max_degree,
                          int numThreads);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
