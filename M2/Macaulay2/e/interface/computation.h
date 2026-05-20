#ifndef _computation_h_
#  define _computation_h_

/**
 * @file interface/computation.h
 * @brief `ComputationStatusCode` / `StopConditions` / `StrategyValues` / `Algorithms` / `gbTraceValues` --- engine-to-interpreter vocabulary for long-running computations.
 *
 * Declares the C-side enums and struct the M2 interpreter
 * shares with the engine for long-running computations.
 * `ComputationStatusCode` (18 values: `COMP_DONE`,
 * `COMP_INTERRUPTED`, `COMP_DONE_DEGREE_LIMIT`,
 * `COMP_DONE_GB_LIMIT`, `COMP_OVERFLOWED`, ...) is what every
 * `start_computation()` returns, and the `StopConditions`
 * struct is what the interpreter populates to tell a
 * `Computation` when to pause: `always_stop`,
 * `stop_after_degree` + `degree_limit`, `basis_element_limit`,
 * `syzygy_limit`, `pair_limit`, `use_codim_limit` +
 * `codim_limit`, `subring_limit`, `just_min_gens`, and
 * `length_limit` (commented "ignored for GB computations").
 * Three further bitfield-style enums live in this header but
 * carry a TODO asking whether they should move elsewhere:
 * `StrategyValues` (`STRATEGY_LONGPOLYNOMIALS`, `_SORT`,
 * `_USE_HILB`, `_USE_SYZ`), `Algorithms` (`GB_polyring_field`,
 * `GB_polyring_field_homog`), and `gbTraceValues`
 * (`PRINT_SPAIR_TRACKING`). Concrete computation classes (GB,
 * resolution, Hilbert, ...) build their own interface headers
 * on this vocabulary.
 *
 * `ComputationStatusCode` is the single source of truth for
 * the engine/M2 boundary --- the header carries an explicit
 * "Keep this enum in sync with RawStatusCodes in
 * Macaulay2/m2/gb.m2" comment, and adding a value here without
 * updating the M2-side mirror silently breaks status
 * reporting. Wrapped in `extern "C"` so it crosses the
 * generated-C side of the `.dd` pipeline.
 *
 * @see comp-gb.hpp
 * @see comp-res.hpp
 * @see engine-includes.hpp
 */

#  include "engine-includes.hpp"

#  if defined(__cplusplus)
extern "C" {
#  endif

/**
   Definitions of the stop conditions and of the status return values
*/
enum ComputationStatusCode {
  /* Keep this enum in sync with RawStatusCodes in Macaulay2/m2/gb.m2 */
  COMP_NEED_RESIZE = 1,         /* need resize */
  COMP_ERROR = 2,               /* error */
  COMP_INTERRUPTED = 3,         /* interrupted */
  COMP_NOT_STARTED = 4,         /* not started */
  COMP_INITIAL_STOP = 5,        /* StopBeforeComputation */
  COMP_DONE = 6,                /* done */
  COMP_DONE_DEGREE_LIMIT = 7,   /* DegreeLimit */
  COMP_DONE_LENGTH_LIMIT = 8,   /* LengthLimit */
  COMP_DONE_SYZYGY_LIMIT = 9,   /* SyzygyLimit */
  COMP_DONE_PAIR_LIMIT = 10,    /* PairLimit */
  COMP_DONE_GB_LIMIT = 11,      /* BasisElementLimit */
  COMP_DONE_SYZ_LIMIT = 12,     /* SyzygyLimit */
  COMP_DONE_CODIM = 13,         /* CodimensionLimit */
  COMP_DONE_MIN_GENS = 14,      /* StopWithMinimalGenerators */
  COMP_DONE_STEPS = 15,         /* StepLimit */
  COMP_DONE_SUBRING_LIMIT = 16, /* SubringLimit */
  COMP_COMPUTING = 17,          /* computing */
  COMP_OVERFLOWED = 18,         /* overflowed */
};

/**
 * @brief Bundle of optional early-termination knobs the front end can
 * attach to a long-running `Computation`.
 *
 * @details Each `*_limit` is consulted by the driver after every increment
 * of the matching counter (basis element, syzygy, S-pair, codim,
 * subring, step); `degree_limit` / `length_limit` cap the
 * homological / degree reach for GB and resolution computations;
 * `always_stop` short-circuits everything; `just_min_gens` asks
 * the driver to halt as soon as a minimal generating set is
 * available. `M2_bool` flags gate which limits are active.
 */
struct StopConditions
{
  M2_bool always_stop;
  M2_bool stop_after_degree;
  M2_arrayint degree_limit;  // Stop after completing this 'slanted' degree
  unsigned int basis_element_limit;  // Number of gb elements
  unsigned int syzygy_limit;
  unsigned int pair_limit;
  M2_bool use_codim_limit;
  unsigned int codim_limit;
  unsigned int subring_limit;
  M2_bool just_min_gens;
  M2_arrayint length_limit;  // ignored for GB computations
};

// TODO: should these move elsewhere?
enum StrategyValues {
  STRATEGY_LONGPOLYNOMIALS = 1,
  STRATEGY_SORT = 2,
  STRATEGY_USE_HILB = 4,
  STRATEGY_USE_SYZ = 8
};

enum Algorithms {
  GB_polyring_field = 1, /* The main GB algorithm to use */
  GB_polyring_field_homog = 2
};

enum gbTraceValues {
  /* The printlevel flags */
  PRINT_SPAIR_TRACKING = 1024
};

#  if defined(__cplusplus)
}
#  endif

#endif /* _computation_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
