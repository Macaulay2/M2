// Copyright 2004 Michael E. Stillman

#include "comp.hpp"

#include <stdio.h>

#include "buffer.hpp"
#include "exceptions.hpp"
#include "finalize.hpp"

Computation /* or null */ *Computation::set_stop_conditions(
    M2_bool always_stop,
    M2_arrayint degree_limit,
    int basis_element_limit,
    int syzygy_limit,
    int pair_limit,
    int codim_limit,
    int subring_limit,
    M2_bool just_min_gens,
    M2_arrayint length_limit)
{
  stop_.always_stop = always_stop;
  stop_.stop_after_degree = (degree_limit != 0 && degree_limit->len > 0);
  stop_.degree_limit = degree_limit;
  stop_.basis_element_limit = basis_element_limit;
  stop_.syzygy_limit = syzygy_limit;
  stop_.pair_limit = pair_limit;
  stop_.use_codim_limit = (codim_limit >= 0);
  stop_.codim_limit = codim_limit;
  stop_.subring_limit = subring_limit;
  stop_.just_min_gens = just_min_gens;
  stop_.length_limit = length_limit;

  if (stop_conditions_ok())
    return this;
  else
    return 0;
}

Computation::Computation()
{
  computation_status = COMP_NOT_STARTED;

  stop_.always_stop = false;
  stop_.stop_after_degree = false;
  stop_.degree_limit = 0;
  stop_.basis_element_limit = 0;
  stop_.syzygy_limit = 0;
  stop_.pair_limit = 0;
  stop_.use_codim_limit = false;
  stop_.codim_limit = 0;
  stop_.subring_limit = 0;
  stop_.just_min_gens = false;
  stop_.length_limit = 0;
}

Computation::~Computation() {}

void Computation::text_out(buffer &o) const { o << "-- computation --"; }

void Computation::show() const
{
  printf("No show method available for this computation type\n");
}

enum ComputationStatusCode Computation::set_status(enum ComputationStatusCode c)
{
  switch (computation_status)
    {
      case COMP_OVERFLOWED:
        // if (computation_status == COMP_NEED_RESIZE) break;
        throw(exc::internal_error(
            "attempted to reset status of a computation that overflowed"));
      default:
        return computation_status = c;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
