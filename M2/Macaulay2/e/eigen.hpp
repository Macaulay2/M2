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
  bool SVD_divide_conquer(const LMatrixRRR *A,
           LMatrixRRR *Sigma,
           LMatrixRRR *U,
           LMatrixRRR *VT);
  bool SVD_divide_conquer(const LMatrixCCC *A,
           LMatrixRRR *Sigma,
           LMatrixCCC *U,
           LMatrixCCC *VT);
  bool eigenvalues(const LMatrixRRR *A, LMatrixCCC *eigenvals);
  bool eigenvalues(const LMatrixCCC *A, LMatrixCCC *eigenvals);
  bool eigenvalues_hermitian(const LMatrixRRR *A, LMatrixRRR *eigenvals);
  bool eigenvalues_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals);
  bool eigenvectors(const LMatrixRRR *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs);
  bool eigenvectors(const LMatrixCCC *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs);
  bool eigenvectors_hermitian(const LMatrixRRR *A, LMatrixRRR *eigenvals, LMatrixRRR *eigenvecs);
  bool eigenvectors_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals, LMatrixCCC *eigenvecs);
}

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/

