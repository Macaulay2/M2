#ifndef __eigen_hpp_
#define __eigen_hpp_

#include "dmat.hpp"
#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
using LMatrixRR = DMat<M2::ARingRR>;
using LMatrixCC = DMat<M2::ARingCC>;
using LMatrixRRR = DMat<M2::ARingRRR>;
using LMatrixCCC = DMat<M2::ARingCCC>;

namespace EigenM2 {
  bool SVD(const LMatrixRR *A,
    LMatrixRR *Sigma,
    LMatrixRR *U,
    LMatrixRR *VT);
  bool SVD(const LMatrixCC *A,
    LMatrixRR *Sigma,
    LMatrixCC *U,
    LMatrixCC *VT);
  bool SVD_divide_conquer(const LMatrixRR *A,
    LMatrixRR *Sigma,
    LMatrixRR *U,
    LMatrixRR *VT);
  bool SVD_divide_conquer(const LMatrixCC *A,
    LMatrixRR *Sigma,
    LMatrixCC *U,
    LMatrixCC *VT);
  bool eigenvalues(const LMatrixRR *A, LMatrixCC *eigenvals);
  bool eigenvalues(const LMatrixCC *A, LMatrixCC *eigenvals);
  bool eigenvalues_hermitian(const LMatrixRR *A, LMatrixRR *eigenvals);
  bool eigenvalues_hermitian(const LMatrixCC *A, LMatrixRR *eigenvals);
  bool eigenvectors(const LMatrixRR *A, LMatrixCC *eigenvals, LMatrixCC *eigenvecs);
  bool eigenvectors(const LMatrixCC *A, LMatrixCC *eigenvals, LMatrixCC *eigenvecs);
  bool eigenvectors_hermitian(const LMatrixRR *A, LMatrixRR *eigenvals, LMatrixRR *eigenvecs);
  bool eigenvectors_hermitian(const LMatrixCC *A, LMatrixRR *eigenvals, LMatrixCC *eigenvecs);
  bool least_squares(const LMatrixRR *A, const LMatrixRR *B, LMatrixRR *X);
  bool least_squares(const LMatrixCC *A, const LMatrixCC *B, LMatrixCC *X);
  
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
  bool least_squares(const LMatrixRRR *A, const LMatrixRRR *B, LMatrixRRR *X);
  bool least_squares(const LMatrixCCC *A, const LMatrixCCC *B, LMatrixCCC *X);
}

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/

