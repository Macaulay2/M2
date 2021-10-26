// Copyright 2004 Michael E. Stillman

#ifndef _comp_hpp_
#define _comp_hpp_

#include "interface/computation.h"
#include "hash.hpp"

class GBComputation;
class ResolutionComputation;

class buffer;

/**
    @ingroup computations
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
