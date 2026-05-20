// Copyright 2004 Michael E. Stillman.

#ifndef _comp_gb_hpp_
#define _comp_gb_hpp_

/**
 * @file comp-gb.hpp
 * @brief `GBComputation` --- abstract base of every Groebner-basis algorithm in the engine.
 *
 * `GBComputation` extends `Computation` with the GB-specific
 * vocabulary the interpreter requests once a basis has been
 * (partially) built: `get_gb`, `get_mingens`, `get_change` (the
 * change-of-basis matrix), `get_syzygies`, `get_initial`
 * (leading terms), and `complete_thru_degree`. Each accessor is
 * virtual because the concrete subclasses keep different
 * internal representations --- `gbA` (`gb-default.hpp`) is the
 * classical default, `GB_comp` (`gb-homog2.hpp`) the homogeneous
 * specialisation, `GBinhom_comp` the inhomogeneous variant,
 * the F4 driver in `f4/f4-computation.hpp` and the new-F4 in
 * `gb-f4/`, plus `GaussElimComputation`, `HermiteComputation`,
 * `binomialGB_comp` (`gb-toric.hpp`), `GBWalker` (`gb-walk.hpp`),
 * `GBDeclared` (`comp-gb-declared.hpp`), `GBProxy`
 * (`comp-gb-proxy.hpp`), and `ReducedGB`.
 *
 * The factory `GBComputation::choose_gb(...)` takes an
 * `algorithm` integer plus a `strategy` flag and the input
 * matrix shape, and `switch`es on `algorithm` to instantiate
 * the right subclass --- most user code never constructs one
 * directly. The header also exposes the `SPAIR_*` return codes
 * (`SPAIR_DONE` / `_GB` / `_SYZ` / `_ZERO` / `_MINGEN` /
 * `_GEN` / `_PAIR` / `_RING` / `_REMOVED` / `_DEFERRED`) used
 * by the classical s-pair step. Companion variants
 * `comp-gb-declared.cpp` (basis supplied by the user, no
 * computation) and `comp-gb-proxy.cpp` (forwards every virtual
 * to a held-pointer `GBComputation*` and lets it be hot-swapped)
 * reuse the same interface.
 *
 * @see comp.hpp
 * @see comp-res.hpp
 * @see gbring.hpp
 */

#include "comp.hpp"
class buffer;

// The following are the return values from s_pair_step,
// These are used in GB_comp, GBinhom_comp
const int SPAIR_DONE = 0;
const int SPAIR_GB = 1;
const int SPAIR_SYZ = 2;
const int SPAIR_ZERO = 3;
const int SPAIR_MINGEN = 4;
const int SPAIR_GEN = 5;
const int SPAIR_PAIR = 6;
const int SPAIR_RING = 7;
const int SPAIR_REMOVED = 8;
const int SPAIR_DEFERRED = 9;

/**
    @ingroup gb

    @brief base class for Groebner basis computations.
*/
class GBComputation : public Computation
// This is the base type for all Groebner basis and syzygy computations
// Note: some abstract methods in Computation need also to be defined.
{
 protected:
  friend class GBProxy;
  GBComputation() {}
  ////  bool stop_conditions_ok() override = 0;
  // If the stop conditions in _Stop are inappropriate,
  // return false, and use ERROR(...) to provide an error message.

 public:
  virtual ~GBComputation();

  virtual void remove_gb() = 0;  // Should free all space associated with GB

  GBComputation *cast_to_GBComputation() override { return this; }
  static GBComputation *choose_gb(const Matrix *m,
                                  M2_bool collect_syz,
                                  int n_rows_to_keep,
                                  M2_arrayint gb_weights,
                                  M2_bool use_max_degree,
                                  int max_degree,
                                  int algorithm,
                                  int strategy,
                                  int numThreads,
                                  int max_reduction_count = 10);
  // Values for algorithm and strategy are documented in engine.h
  // Returns NULL if an error occurs

  virtual const Ring *get_ring() const = 0;

  virtual Computation /* or null */ *set_hilbert_function(const RingElement *h);
  // The default version returns an error saying that Hilbert functions cannot
  // be used.

  void start_computation() override = 0;

  int complete_thru_degree() const override = 0;
  // The computation is complete up through this degree.

  // Recall that the status of the computation is maintained by the Computation
  // class,

  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const Matrix /* or null */ *get_gb() = 0;

  virtual const Matrix /* or null */ *get_mingens() = 0;

  virtual const Matrix /* or null */ *get_change() = 0;

  virtual const Matrix /* or null */ *get_syzygies() = 0;

  virtual const Matrix /* or null */ *get_initial(int nparts) = 0;

  virtual const Matrix /* or null */ *get_parallel_lead_terms(M2_arrayint w);

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m) = 0;

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient) = 0;

  virtual int contains(const Matrix *m) = 0;

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  void text_out(buffer &o) const override;
  // This displays statistical information, and depends on the
  // M2_gbTrace value.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
