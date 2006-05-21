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
#if EXPERIMENT
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  const Ring *K = R->getCoefficients();
  GBComputation *G;
  const Z_mod *KZZp = K->cast_to_Z_mod();
  if (KZZp != 0)
    {
      G = new F4Computation<CoefficientRingZZp>(KZZp,
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
  return NULL;
#else
  ERROR("not implemented yet: Strategy => F4");
  return NULL;
#endif
}

template<typename CoeffRing>
F4Computation<CoeffRing>::F4Computation(
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
  KK = new Gausser(K0);
  MI = new MonomialInfo(originalR->n_vars());

  f4 = new F4GB<CoeffRing>(KK,
			   MI,
			   collect_syz,
			   n_rows_to_keep,
			   gb_weights,
			   strategy,
			   use_max_degree,
			   max_degree);
  
  F4toM2Interface<CoeffRing>::from_M2_matrix(KK,MI,m,gb_weights,f4->get_generators());
}

template<typename CoeffRing>
F4Computation<CoeffRing>::~F4Computation()
{
}


/*************************
 ** Top level interface **
 *************************/

template<typename CoeffRing>
void F4Computation<CoeffRing>::start_computation()
{
  f4->start_computation(stop_);
}

template<typename CoeffRing>
ComputationOrNull *
F4Computation<CoeffRing>::set_hilbert_function(const RingElement *hf)
{
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *F4Computation<CoeffRing>::get_gb()
{
  const gb_array &gens = f4->get_generators();
  MatrixConstructor result(F,0);
  for (int i=0; i<gens.size(); i++)
    {
      vec v = F4toM2Interface<CoeffRing>::to_M2_vec(KK,MI,gens[i]->f, F);
      result.append(v);
    }
  return result.to_matrix();
}

template<typename CoeffRing>
const MatrixOrNull *F4Computation<CoeffRing>::get_mingens()
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

template<typename CoeffRing>
const MatrixOrNull *F4Computation<CoeffRing>::get_change()
{
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *F4Computation<CoeffRing>::get_syzygies()
{
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *F4Computation<CoeffRing>::get_initial(int nparts)
{
  return 0;
}

template<typename CoeffRing>
const MatrixOrNull *F4Computation<CoeffRing>::matrix_remainder(const Matrix *m)
{
  return 0;
}

template<typename CoeffRing>
void F4Computation<CoeffRing>::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
}

template<typename CoeffRing>
int F4Computation<CoeffRing>::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  return 0;
}

template<typename CoeffRing>
int F4Computation<CoeffRing>::complete_thru_degree() const
  // The computation is complete up through this degree.
{
  return 0;
}

template<typename CoeffRing>
void F4Computation<CoeffRing>::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     gbTrace value */
{
}


template class F4Computation<CoefficientRingZZp>;



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
