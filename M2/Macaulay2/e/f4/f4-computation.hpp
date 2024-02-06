/* Copyright 2005 - 2021, Michael E. Stillman */

#ifndef _F4Computation_h_
#define _F4Computation_h_

#include "comp-gb.hpp"              // for GBComputation
#include "engine-exports.h"         // for M2_bool, M2_arrayint
#include "f4/f4.hpp"                // for F4GB
#include "interface/computation.h"  // for ComputationStatusCode
#include "polyring.hpp"             // for PolynomialRing
class Computation;
class F4Mem;
class FreeModule;
class Matrix;
class MonomialInfo;
class RingElement;
class VectorArithmetic;
class buffer;

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
  F4Mem *mMemoryBlock;
  F4GB *mF4GB;
 public:
  F4Computation(const VectorArithmetic* VA,
                F4Mem *Mem,
                const Matrix *m,
                M2_bool collect_syz,
                int n_rows_to_keep,
                M2_arrayint gb_weights,
                int strategy,
                M2_bool use_max_degree,
                int max_degree);

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
                          int max_degree);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
