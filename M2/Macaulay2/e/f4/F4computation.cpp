// Copyright 2005 Michael E. Stillman.

#include "../comp_gb.hpp"
#include "../poly.hpp"
#include "../coeffrings.hpp"
#include "../matrix.hpp"

#include "F4computation.hpp"
#include "moninfo.hpp"

GBComputation *createF4GB(const Matrix *m,
			  M2_bool collect_syz,
			  int n_rows_to_keep,
			  M2_arrayint gb_weights,
			  int strategy,
			  M2_bool use_max_degree,
			  int max_degree)
{
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  const Ring *K = R->getCoefficients();
  GBComputation *G;

  const Z_mod *KZZp = K->cast_to_Z_mod();
  if (KZZp != 0)
    {
      G = new F4Computation<CoefficientRingZZp,MonomialInfo>(KZZp,
							     m,
							     collect_syz,
							     n_rows_to_keep,
							     gb_weights,
							     use_max_degree,
							     max_degree,
							     strategy);
      return G;
    }
  ERROR("cannot use Strategy=>F4 with this type of coefficient ring");
  return 0;
}

template<typename CoeffRing, typename MonInfo>
F4Computation<CoeffRing,MonInfo>::F4Computation(
						     const RingType *K0,
						     const Matrix *m, 
						     M2_bool collect_syz, 
						     int n_rows_to_keep,
						     M2_arrayint gb_weights,
						     int strategy, 
						     M2_bool use_max_degree,
						     int max_degree)
{
}

template class F4Computation<CoefficientRingZZp,MonomialInfo>;


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
