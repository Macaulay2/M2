// Copyright 2005 Michael E. Stillman.

#include "../comp_gb.hpp"
#include "../z_mod_p.hpp"
#include "../poly.hpp"
#include "../coeffrings.hpp"
#include "../matrix.hpp"
#include "../matrixcon.hpp"

#include "F4computation.hpp"
#include "F4toM2interface.hpp"
#include "F4.hpp"
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
  originalR = m->get_ring()->cast_to_PolynomialRing();
  F = m->rows();
  K = K0;
  coeffK = K->get_CoeffRing();
  MI = new MonInfo(originalR->n_vars());

  f4 = new F4<CoeffRing,MonInfo>(coeffK,
				 MI,
				 collect_syz,
				 n_rows_to_keep,
				 gb_weights,
				 strategy,
				 use_max_degree,
				 max_degree);

  F4toM2Interface<CoeffRing,MonInfo>::from_M2_matrix(coeffK,MI,m,gb_weights,f4->get_generators());
}

template<typename CoeffRing, typename MonInfo>
F4Computation<CoeffRing,MonInfo>::~F4Computation()
{
}


/*************************
 ** Top level interface **
 *************************/

template<typename CoeffRing, typename MonInfo>
void F4Computation<CoeffRing,MonInfo>::start_computation()
{
}

template<typename CoeffRing, typename MonInfo>
ComputationOrNull *
F4Computation<CoeffRing,MonInfo>::set_hilbert_function(const RingElement *hf)
{
  return 0;
}

template<typename CoeffRing, typename MonInfo>
const MatrixOrNull *F4Computation<CoeffRing,MonInfo>::get_gb()
{
  const gb_array &gens = f4->get_generators();
  MatrixConstructor result(F,0);
  for (int i=0; i<gens.size(); i++)
    {
      vec v = F4toM2Interface<CoeffRing,MonInfo>::to_M2_vec(coeffK,MI,gens[i]->f, F);
      result.append(v);
    }
  return result.to_matrix();
}

template<typename CoeffRing, typename MonInfo>
const MatrixOrNull *F4Computation<CoeffRing,MonInfo>::get_mingens()
{
#if 0
  MatrixConstructor mat(_F,0);
  for (VECTOR(gbelem *)::iterator i = gb.begin(); 
       i != gb.end(); 
       i++)
    if ((*i)->minlevel == ELEM_POSSIBLE_MINGEN)
      mat.append(originalR->translate_gbvector_to_vec(_F, (*i)->g.f));
  return mat.to_matrix();
#endif
  return 0;
}

template<typename CoeffRing, typename MonInfo>
const MatrixOrNull *F4Computation<CoeffRing,MonInfo>::get_change()
{
  return 0;
}

template<typename CoeffRing, typename MonInfo>
const MatrixOrNull *F4Computation<CoeffRing,MonInfo>::get_syzygies()
{
  return 0;
}

template<typename CoeffRing, typename MonInfo>
const MatrixOrNull *F4Computation<CoeffRing,MonInfo>::get_initial(int nparts)
{
  return 0;
}

template<typename CoeffRing, typename MonInfo>
const MatrixOrNull *F4Computation<CoeffRing,MonInfo>::matrix_remainder(const Matrix *m)
{
  return 0;
}

template<typename CoeffRing, typename MonInfo>
void F4Computation<CoeffRing,MonInfo>::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
}

template<typename CoeffRing, typename MonInfo>
int F4Computation<CoeffRing,MonInfo>::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  return 0;
}

template<typename CoeffRing, typename MonInfo>
int F4Computation<CoeffRing,MonInfo>::complete_thru_degree() const
  // The computation is complete up through this degree.
{
  return 0;
}

template<typename CoeffRing, typename MonInfo>
void F4Computation<CoeffRing,MonInfo>::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     gbTrace value */
{
}


template class F4Computation<CoefficientRingZZp,MonomialInfo>;



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
