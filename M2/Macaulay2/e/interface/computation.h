#ifndef _computation_h_
#define _computation_h_

#if defined(__cplusplus)
extern "C" {
#endif

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

#if defined(__cplusplus)
}
#endif

#endif /* _computation_h_ */
