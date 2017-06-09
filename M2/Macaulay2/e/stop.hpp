// Copyright 2005 Michael E. Stillman

#ifndef _stop_hpp_
#define _stop_hpp_

#include "engine-includes.hpp"

struct StopConditions
{
  bool always_stop;
  bool stop_after_degree;
  M2_arrayint degree_limit;  // Stop after completing this 'slanted' degree
  unsigned int basis_element_limit;  // Number of gb elements
  unsigned int syzygy_limit;
  unsigned int pair_limit;
  bool use_codim_limit;
  unsigned int codim_limit;
  unsigned int subring_limit;
  M2_bool just_min_gens;
  M2_arrayint length_limit;  // ignored for GB computations
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
