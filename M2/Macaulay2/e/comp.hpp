// Copyright 2004 Michael E. Stillman

#ifndef _comp_hpp_
#define _comp_hpp_

/**
 * @file comp.hpp
 * @brief Abstract `Computation` base class --- stop-condition machinery for incremental engine work.
 *
 * `Computation` is the pure-virtual root (inheriting from
 * `MutableEngineObject`) of every long-running engine task ---
 * Groebner basis, resolution, Hilbert series, ... . It carries
 * a `ComputationStatusCode` (`COMP_DONE`, `COMP_INTERRUPTED`,
 * `COMP_DONE_DEGREE_LIMIT`, `COMP_DONE_GB_LIMIT`,
 * `COMP_OVERFLOWED`, ...) and a `StopConditions` struct that
 * bundles the user-facing limit vocabulary: `degree_limit`,
 * `basis_element_limit`, `syzygy_limit`, `pair_limit`,
 * `codim_limit`, `subring_limit`, `just_min_gens`,
 * `length_limit`, and the `always_stop` debug knob. M2
 * statements like `gb(I, DegreeLimit => 5)` map to constructing
 * a concrete subclass, calling `set_stop_conditions`, then
 * `start_computation`. The pure-virtual surface every subclass
 * fills in is `stop_conditions_ok`, `complete_thru_degree`,
 * `start_computation`; the typed downcasts
 * `cast_to_GBComputation` / `cast_to_ResolutionComputation`
 * default to `nullptr` so callers can branch on the concrete
 * flavour without RTTI.
 *
 * The incremental design is what makes the engine usable on
 * hour-or-day computations: users can interrupt cleanly (status
 * flips to `COMP_INTERRUPTED`), inspect the partial state via
 * `complete_thru_degree`, raise a limit, and call
 * `start_computation` again to resume where the previous run
 * stopped. `text_out` and `show` are the two diagnostic
 * hooks.
 *
 * @see comp-gb.hpp
 * @see comp-res.hpp
 * @see interface/computation.h
 */

#include "interface/computation.h"
#include "hash.hpp"

class GBComputation;
class ResolutionComputation;

class buffer;

/**
 * @brief Abstract base for long-running, resumable engine computations
 * (`GBComputation`, `ResolutionComputation`, `MutableComplex`,
 * `GBKernelComputation`, ...).
 *
 * @details Manages a `ComputationStatusCode` state machine plus a
 * `StopConditions` record so callers can ask for partial progress
 * (degree limit, basis-element limit, syzygy limit, wall-clock
 * deadline, ...) without losing what has already been computed.
 * Subclasses implement `stop_conditions_ok()` to vet the request
 * and `start_computation()` to do the work; `set_status()` is the
 * single chokepoint that updates the visible state.
 *
 * @ingroup computations
 */
class Computation : public MutableEngineObject
{
 private:
  enum ComputationStatusCode computation_status;

 protected:
  StopConditions stop_;

  Computation();

  enum ComputationStatusCode set_status(enum ComputationStatusCode);

  virtual bool stop_conditions_ok() = 0;
  // If the stop conditions in stop_ are inappropriate,
  // return false, and use ERROR(...) to provide an error message.

  virtual ~Computation();

 public:
  Computation /* or null */ *set_stop_conditions(M2_bool always_stop,
                                                 M2_arrayint degree_limit,
                                                 int basis_element_limit,
                                                 int syzygy_limit,
                                                 int pair_limit,
                                                 int codim_limit,
                                                 int subring_limit,
                                                 M2_bool just_min_gens,
                                                 M2_arrayint length_limit);
  // returns NULL if there is a general problem with one of the stop
  // conditions.

  enum ComputationStatusCode status() const { return computation_status; }
  virtual int complete_thru_degree() const = 0;
  // This is computation specific information.  However, for homogeneous
  // GB's, the GB coincides with the actual GB in degrees <= the returned value.
  // For resolutions of homogeneous modules, the resolution
  // coincides with the actual one in (slanted) degrees <= the returned value.

  virtual void start_computation() = 0;
  // Do the computation as specified by the stop conditions.
  // This routine should set the status of the computation.

  virtual GBComputation *cast_to_GBComputation() { return nullptr; }
  virtual ResolutionComputation *cast_to_ResolutionComputation() { return nullptr; }
  virtual void text_out(buffer &o) const;

  virtual void show() const;  // debug display of some computations
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
