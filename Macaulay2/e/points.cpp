// Copyright 2005  Michael E. Stillman

#include "points.hpp"
#include "error.h"
#include "mutablemat.hpp"
#include "matrixcon.hpp"
#include "dmat.hpp"

M2_bool rawIdealOfPoints(const Ring *R,
			 const MutableMatrix *Pts,
			 MatrixOrNull ** result_GB,
			 MatrixOrNull ** result_std_monoms)
{
  // Branch depending on the type of K, the ring of Pts.
  // If Pts is not a DMatrix, make it one.

  // First: check that coeff ring of R is the same as ring of Pts

  // Now branch depending on this type
  const Ring *K = Pts->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing(); 
  if (P == 0 || K != P->getCoefficients())
    {
      ERROR("expected polynomial ring with same coefficient ring");
      return false;
    }
  const Z_mod *KZZp = K->cast_to_Z_mod();
  if (KZZp != 0)
    {
      DMat<CoefficientRingZZp> *Pts1;
      *result_GB = PointsComputation<CoefficientRingZZp>::points
	(P,KZZp,Pts1,*result_std_monoms);
      return true;
    }

  ERROR("not implemented yet");
  return false;
}

class monom_int_list
{
public:
  monom_int_list(const PolynomialRing *R) {}
  ~monom_int_list() {}

  void add(int old, int x, int *vp) {}
  bool remove(int &old, int &x, int * &vp) {}
};

template <typename CoeffRing>
Matrix *PointsComputation<CoeffRing>::points(const PolynomialRing *R,
					     const typename CoeffRing::ring_type *K,
					     const DMat<CoeffRing> *Pts,
					     Matrix * & result_std_monoms)
{
  // Declare and initialize our variables
  int nvars = R->n_vars();
  int npoints = Pts->n_cols();

  MatrixConstructor gbG(R->make_FreeModule(1), 0);
  DMat<CoeffRing> *P = new DMat<CoeffRing>(K, npoints, npoints+1);
  DMat<CoeffRing> *PLU = new DMat<CoeffRing>(K, npoints, npoints+1);
  MonomialIdeal *inG = new MonomialIdeal(R);
  VECTOR(monomial) stdG;
  monom_int_list monoms_todo(R);
  
  int next_col = 0;
  // MES Place the monomials [0,0,vp], ..., [0,nvars-1,vp] onto monom list
  // MES Make the first column of P, PLU all ones.

  // The main loop
#if 0
//   while (monoms_todo.remove(old,x,vp))
//     {
//       // First, see if this monomial is in inG, if so, continue.
// 
//       // Place this monomial as the 'next_col' column of P, PLU.
// 
//       // LU1
// 
//       // 
//     }
#endif  
  return 0;
}



#include "coeffrings.hpp"
template class PointsComputation<CoefficientRingRR>;
template class PointsComputation<CoefficientRingCC>;
template class PointsComputation<CoefficientRingZZp>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
