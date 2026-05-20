#ifndef _computation_h_
#  define _computation_h_

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
