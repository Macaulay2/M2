#ifndef __eigen_hpp_
#define __eigen_hpp_

#include "dmat.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
using LMatrixRRR = DMat<M2::ARingRRR>;
using LMatrixCCC = DMat<M2::ARingCCC>;

namespace EigenM2 {
  bool SVD(const LMatrixRRR *A,
           LMatrixRRR *Sigma,
           LMatrixRRR *U,
           LMatrixRRR *VT);
  bool SVD(const LMatrixCCC *A,
           LMatrixRRR *Sigma,
           LMatrixCCC *U,
           LMatrixCCC *VT);
  bool eigenvalues(const DMatRRR *A, DMatCCC *eigenvals);
  bool eigenvalues(const DMatCCC *A, DMatCCC *eigenvals);
}

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/

