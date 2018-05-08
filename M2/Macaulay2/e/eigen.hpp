#ifndef __eigen_hpp_
#define __eigen_hpp_

#include "dmat.hpp"
#include "aring-RRR.hpp"
using LMatrixRRR = DMat<M2::ARingRRR>;

namespace EigenM2 {
  bool SVD(const LMatrixRRR *A,
           LMatrixRRR *Sigma,
           LMatrixRRR *U,
           LMatrixRRR *VT);
}

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/

