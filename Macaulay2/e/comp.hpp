// (c) 1994 Michael E. Stillman.

#ifndef _comp_hh_
#define _comp_hh_

#include "hash.hpp"

extern char system_interrupted;
extern int gbTrace;


#if 0
enum comp_return_value
{
  CALC_COMPUTING   = -2, // Not a possible return value
  COMP_INTERRUPTED = -1,
  COMP_DONE = 0,
  COMP_DONE_DEGREE_LIMIT = 1,
  COMP_DONE_LENGTH_LIMIT = 2,
  COMP_DONE_SYZYGY_LIMIT = 3,
  COMP_DONE_PAIR_LIMIT = 4,
  COMP_RESIZE = 5,
  COMP_COMPUTING = 6,

  CALC_DONE        = -4,
  CALC_INTERRUPTED = -3,
  CALC_DONE_DEGREE = 6,
  CALC_DONE_GB       = 7,
  CALC_DONE_SYZ      = 8,
  CALC_DONE_PAIRS    = 9,
  CALC_DONE_CODIM    = 10,
  CALC_DONE_MIN_GENS = 11,
  CALC_DONE_STEPS    = 12,  // Possible Hilbert function return value
  CALC_NEED_RESIZE   = 13,
  COMP_ERROR         = 14
};

const int interrupted = -1;
const int done        = 0;
const int done_this   = 4;

const int reduc_pairs = 1;
const int reduc_gens  = 2;
const int auto_reduc  = 3;

const int zero_elem   = 5;
const int nonzero_elem= 6;

const int internal_step = 7;


// Other states for resolutions
const int skel        = 2;
const int pairs       = 3;
#endif

class computation : public mutable_object
{
public:
  computation() : mutable_object() {}
  virtual ~computation() {}
  virtual int calc(int nsteps) = 0;

  // The following are only defined for det computations
  // at the moment.
  virtual void discard() {}
  virtual void set_next_minor(const int * /*rows*/, const int * /*cols*/) {}
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
