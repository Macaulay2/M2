// Copyright 2004 Michael E. Stillman

#ifndef _computation_hpp_
#define _computation_hpp_

extern "C" char system_interrupted;
extern "C" int gbTrace;

#include "engine.h"
/* engine.h includes definitions of the stop conditions
   and of the status return values */
#include "hash.hpp"

class GBComputation;
class ResolutionComputation;
class MinorsComputation;
class buffer;

class Computation : public mutable_object
{
private:
  enum ComputationStatusCode computation_status;
protected:
  struct StopConditions
  {
    bool always_stop;
    bool stop_after_degree;
    M2_arrayint degree_limit; // Stop after completing this 'slanted' degree
    unsigned int basis_element_limit; // Number of gb elements
    unsigned int syzygy_limit;
    unsigned int pair_limit;
    bool use_codim_limit;
    unsigned int codim_limit;
    unsigned int subring_limit;
    M2_bool just_min_gens;
    M2_arrayint length_limit; // ignored for GB computations
  } _Stop;

  Computation();

  ComputationStatusCode set_status(enum ComputationStatusCode c);
  
  virtual bool stop_conditions_ok() = 0;
  // If the stop conditions in _Stop are inappropriate,
  // return false, and use ERROR(...) to provide an error message.

  virtual ~Computation();
public:
  ComputationOrNull *
  set_stop_conditions(M2_bool always_stop,
		      M2_bool stop_after_degree,
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

  enum ComputationStatusCode status() { return computation_status; }

  virtual void start_computation() = 0;
  // Do the computation as specified by the stop conditions.
  // This routine should set the status of the computation.

  virtual GBComputation * cast_to_GBComputation() { return 0; }
  virtual MinorsComputation * cast_to_MinorsComputation() { return 0; }
  virtual ResolutionComputation * cast_to_ResolutionComputation() { return 0; }

  virtual void text_out(buffer &o);
};

/* computations which cannot be continued if interrupted */
class HilbertComputation; /* ?? */
class LLLComputation;
class FactorizationComputation;
class FFGaussComputation;
class SmithComputation;
class MaximalIndepSetsComputation;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
