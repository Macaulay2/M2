// Copyright 2004 Michael E. Stillman

#include "computation.hpp"

ComputationOrNull *
Computation::set_stop_conditions(M2_bool always_stop,
				 M2_bool stop_after_degree,
				 M2_arrayint degree_limit,
				 int basis_element_limit,
				  int syzygy_limit,
				 int pair_limit,
				 int codim_limit,
				 int subring_limit,
				 M2_bool just_min_gens,
				 M2_arrayint length_limit)
{
  _Stop.always_stop = always_stop;
  _Stop.stop_after_degree = stop_after_degree;
  _Stop.degree_limit = degree_limit;
  _Stop.basis_element_limit = basis_element_limit;
  _Stop.syzygy_limit = syzygy_limit;
  _Stop.pair_limit = pair_limit;
  _Stop.use_codim_limit = (codim_limit >= 0);
  _Stop.codim_limit = codim_limit;
  _Stop.subring_limit = subring_limit;
  _Stop.just_min_gens = just_min_gens;
  _Stop.length_limit = length_limit;
  return this;
}

Computation::Computation()
{
  set_stop_conditions(false,
		      false,
		      0,
		      -1,
		      -1,
		      -1,
		      -1,
		      -1,
		      false,
		      0);
}

Computation::~Computation()
{
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
