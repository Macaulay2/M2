#ifndef __lapack_h_
#define __lapack_h_

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "dmat.hpp"

typedef DMat<M2::ARingRRR> DMatRRR;
typedef DMat<M2::ARingCCC> DMatCCC;
typedef DMat<M2::ARingRR> DMatRR;
typedef DMat<M2::ARingCC> DMatCC;

/* Lapack routines */
/* Compute solutions x to Ax = b for square matrix A and a matrix b */

/* MES, On my mac, 10.12.4, lapack include file is at
  /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/Headers/clapack.h
*/
extern "C" {
int dgesv_(int *n,      // number of rows in A
           int *nrhs,   // number of right hand sides
           double *a,   // n by n matrix A, on exit L&U from A=PLU
           int *lda,    // n
           int *ipiv,   // indices defining permutation P
           double *b,   // right-hand-side, on exit the solution matrix
           int *ldb,    // n
           int *info);  // error info

int dgeev_(char *n,     // whether to compute left eigenvectors
           char *n2,    // whether to compute right eigenvectors
           int *size,   // rows
           double *M,   // input matrix A
           int *size1,  // rows
           double *E,   // real components of eigenvalues
           double *E2,  // imaginar components of eigenvalues
           double *,    // left eigenvectors
           int *,       // rows
           double *,    // right eigenvectors
           int *,       // rows
           double *,    // workspace
           int *,       // size of workspace
           int *);      // error info

int dsyev_(char *n,       // whether to compute eigenvectors
           char *n2,      // how to store A (upper or lower)
           int *size,     // rows
           double *M,     // symmetric matrix A
           int *lda,      // rows
           double *eig,   // becomes eigenvalues
           double *work,  // workspace
           int *wsize,    // size of workspace
           int *info);    // error info

void dgetrf_(const int *rows,  // rows
             const int *cols,  // columns
             double *A,        // input matrix, on exit L & U from A=PLU.
             const int *ld,    // rows
             int *ipiv,        // becomes permutation indices of P
             int *info);       // error info

int dgesvd_(char *jobU,     // amount of U to return
            char *jobV,     // amount of V to return
            int *rows,      // rows
            int *cols,      // columns
            double *A,      // input matrix for SVD
            int *ldA,       // rows
            double *Sigma,  // singular values
            double *U,      // U
            int *ldU,       // rows
            double *VT,     // V transpose
            int *ldVT,      // cols
            double *work,   // workspace
            int *lwork,     // size of workspace
            int *info);     // error info

int dgesdd_(char *jobU,     // amount of U to return
            int *rows,      // rows
            int *cols,      // columns
            double *A,      // input matrix for SVD
            int *ldA,       // rows
            double *Sigma,  // singular values
            double *U,      // U
            int *ldU,       // rows
            double *VT,     // V transpose
            int *ldVT,      // cols
            double *work,   // workspace
            int *lwork,     // size of workspace
            int *iwork,     // integer workspace
            int *info);     // error info

int dgels_(char *job,     // type of least squares problem
           int *rows,     // rows
           int *cols,     // columns
           int *nhrs,     // number right hand sides
           double *A,     // input matrix for least squares
           int *ldA,      // rows
           double *b,     // matrix of right hand side vectors
           int *ldb,      // rows of right hand side
           double *work,  // workspace
           int *lwork,    // size of workspace
           int *info);    // error info

int dgelss_(int *rows,      // rows
            int *cols,      // columns
            int *nhrs,      // number right hand sides
            double *A,      // input matrix for least squares
            int *ldA,       // rows
            double *b,      // matrix of right hand side vectors
            int *ldb,       // rows of right hand side
            double *Sigma,  // singular values
            double *rcond,  // used to determine if singular value is 0
            int *rank,      // rank of the matrix on output
            double *work,   // workspace
            int *lwork,     // size of workspace
            int *info);     // error info

// computes a QR factorization of the matrix A.
// to get optimal work size:
//   1. call this function with lwork == -1, at exit, work[0] contains optimal
//   size of 'work'
//        in number of doubles?  or number of bytes?  probably number of
//        doubles...
//   2. after that, allocate space for that size, recall the function.
// the answer is encoded in the following manner:
//   Q = H(1) H(2) ... H(k), where k = min(m,n).
//   H(i) has the form:  I - tau * v * v'
// where
//   tau is a real scalar
//   v is a real vector with v(1:i-1) = 0, v(i)=1, v(i+1:m) is stored on exit in
//     A(i+1:m,i), and tau in tau(i).
// dgeqrf: QR factorization
//   https://docs.oracle.com/cd/E19422-01/819-3691/dgeqrf.html
// dorgqr: to get Q as a matrix:
//   https://docs.oracle.com/cd/E19422-01/819-3691/dorgqr.html
// dormqr: to instead multiply by this Q without creating it:
//   https://docs.oracle.com/cd/E19422-01/819-3691/dormqr.html

int dgeqrf_(int *m,     // #rows (input)
            int *n,     // #columns (input)
            double *A,  // input matrix, in row or column major order? (inout)
            // on output: the top part of A, min(m,n) x n, is R
            //    under that: A(i+1:m, i) is v_i
            //    actually, vi[1:i-1] = 0, vi[i] = 1, vi[i+1:m) = A(i+1:m, i)
            int *lda,      // leading dimension of A (>= max(1, #rows) (input)
            double *tau,   // scalar factors of elementary reflectors (output),
                           // size: min(m,n).
            double *work,  //
            int *lwork,    // size of workspace?
            int *info);  // error info: ==0: successful, ==-i, i-th argument had
                         // illegal value
int dorgqr_(int *m,      // #rows m >= 0
            int *n,      // #cols of Q, m >= n >= 0
            int *k,      // number of elementary reflectors (n >= k >= 0)
            double *A,   // on input: i-th column contains the v_i defining the
                         // i-th reflector
                         // on output: contains the m by n matrix Q.
            int *lda,    // input.
            double *tau,   // input,
            double *work,  // workspace, size 'lwork'.  optimally, lwork >= n *
                           // nb, where nb is optimal block size.
            int *lwork,    // dimension of 'work'
            int *info);
// as usual, setting lwork to -1, results in work[0] containing the optimal work
// size.
// then allocate space, run again.
// todo:
//  a. add in routines for QR in lapack.cpp (RR,RRR,CC,CCC), lapack.hpp defs too
//        RR: WORKING ON
//  b. in mat-linalg.hpp, add in routines for QR as well.
// DONE    b1. at top of file: default doesn't exist case.
//        b2. 4 routines to add: RR, RRR, CC, CCC
//         RR: DONE
// DONE  c. mat.hpp: add in QR routine
// DONE  d. mutablemat-imp.hpp: add in boiler plate.
// DONE   d1. mutablemat-defs.hpp
// DONE  e. add in routines in x-mutablemat.cpp, also change engine.h
//  f. add in a routine in interface.dd
//  g. add in a routine in m2/mutablemat.m2
//  h. document and test QR, ReturnQR.
int zgeqrf_(int *m,     // #rows (input)
            int *n,     // #columns (input)
            double *A,  // input matrix, in row or column major order? (inout)
            // on output: the top part of A, min(m,n) x n, is R
            //    under that: A(i+1:m, i) is v_i
            //    actually, vi[1:i-1] = 0, vi[i] = 1, vi[i+1:m) = A(i+1:m, i)
            int *lda,      // leading dimension of A (>= max(1, #rows) (input)
            double *tau,   // scalar factors of elementary reflectors (output),
                           // size: min(m,n).
            double *work,  //
            int *lwork,    // size of workspace?
            int *info);  // error info: ==0: successful, ==-i, i-th argument had
                         // illegal value
int zungqr_(int *m,      // #rows m >= 0
            int *n,      // #cols of Q, m >= n >= 0
            int *k,      // number of elementary reflectors (n >= k >= 0)
            double *A,   // on input: i-th column contains the v_i defining the
                         // i-th reflector
                         // on output: contains the m by n matrix Q.
            int *lda,    // input.
            double *tau,   // input,
            double *work,  // workspace, size 'lwork'.  optimally, lwork >= n *
                           // nb, where nb is optimal block size.
            int *lwork,    // dimension of 'work'
            int *info);

#if 0  
  int dormqr_(char *__side,
              char *__trans,
              __CLPK_integer *__m,
              __CLPK_integer *__n,
              __CLPK_integer *__k,
              __CLPK_doublereal *__a,
              __CLPK_integer *__lda,
              __CLPK_doublereal *__tau,
              __CLPK_doublereal *__c__,
              __CLPK_integer *__ldc,
              __CLPK_doublereal *__work,
              __CLPK_integer *__lwork,
              __CLPK_integer *__info);
#endif

#ifndef __FFLASFFPACK_config_blas_H
/* cblas routines */
// computes "ax + y"
void cblas_daxpy(const int n,      // length of vectors
                 const double a,   // scalar alpha
                 const double *x,  // vector x
                 const int incx,   // increment of x
                 double *y,        // vector y
                 const int incy);  // increment of y

// computes "alpha AB + beta C"
// NOTE: first 3 args should formally be ENUMS, not ints.
//       Problem? e.g., what if enums change?
void cblas_dgemm(
    const int Order,     // how matrices are stored, by column or row.
    const int TransA,    // whether to transform A, e.g. take transpose
    const int TransB,    // whether to transform B
    const int M,         // rows of A
    const int N,         // columns of B
    const int K,         // columns of A, which must = rows of B
    const double alpha,  // scalar alpha
    const double *A,     // matrix A
    const int lda,       // rows of A
    const double *B,     // matrix B
    const int ldb,       // rows of B
    const double beta,   // scalar bet
    double *C,           // matrix C; on output, alphaAB+betaC
    const int ldc);      // rows of C
#endif

// computes ax
void cblas_dscal(const int n,      // length of vectors
                 const double a,   // scalar alpha
                 double *x,        // vector x
                 const int incx);  // increment of x

int zgesv_(int *n,      // number of rows in A
           int *nrhs,   // number of right hand sides
           double *a,   // n by n matrix A, on exit L&U from A=PLU
           int *lda,    // n
           int *ipiv,   // indices defining permutation P
           double *b,   // right-hand-side
           int *ldb,    // n
           int *info);  // error info

int zgeev_(char *n,        // whether to compute left eigenvectors
           char *n2,       // whether to compute right eigenvectors
           int *size,      // rows
           double *M,      // n by n input matrix
           int *size1,     // rows
           double *E,      // eigenvalues, on output
           double *l,      // left eigenvectors, on output
           int *lsize,     // rows
           double *r,      // right eigenvectors, on output
           int *rsize,     // rows
           double *w,      // workspace
           int *wsize,     // size of workspace
           double *rwork,  // another workspace
           int *info);     // error info

int zheev_(char *n,        // whether to compute eigenvectors
           char *n2,       // how to store A (upper or lower)
           int *size,      // rows
           double *M,      // hermitian matrix A
           int *lda,       // rows
           double *eig,    // becomes eigenvalues
           double *w,      // workspace
           int *wsize,     // size of workspace
           double *rwork,  // another workspace
           int *info);     // error info

int zgetrf_(int *rows,   // rows
            int *cols,   // columns
            double *M,   // input matrix, on exit L & U from A=PLU.
            int *ld,     // rows
            int *ipiv,   // becomes permutation indices of P
            int *info);  // error info

int zgesvd_(char *jobU,     // amount of U to return
            char *jobV,     // amount of V to return
            int *rows,      // rows
            int *cols,      // columns
            double *A,      // input matrix for SVD
            int *ldA,       // rows
            double *Sigma,  // singular values
            double *U,      // U
            int *ldU,       // rows
            double *VT,     // V transpose
            int *ldVT,      // cols
            double *w,      // workspace
            int *lwork,     // size of workspace
            double *rwork,  // another workspace
            int *info);     // error info

int zgesdd_(char *jobU,     // amount of U to return
            int *rows,      // rows
            int *cols,      // columns
            double *A,      // input matrix for SVD
            int *ldA,       // rows
            double *Sigma,  // singular values
            double *U,      // U
            int *ldU,       // rows
            double *VT,     // V transpose
            int *ldVT,      // cols
            double *w,      // workspace
            int *lwork,     // size of workspace
            double *rwork,  // another workspace
            int *iwork,     // integer workspace
            int *info);     // error info

int zgels_(char *job,     // type of least squares problem
           int *rows,     // rows
           int *cols,     // columns
           int *nhrs,     // number right hand sides
           double *A,     // input matrix for least squares
           int *ldA,      // rows
           double *b,     // matrix of right hand side vectors
           int *ldb,      // rows of right hand side
           double *work,  // workspace
           int *lwork,    // size of workspace
           int *info);    // error info

int zgelss_(int *rows,      // rows
            int *cols,      // columns
            int *nhrs,      // number right hand sides
            double *A,      // input matrix for least squares
            int *ldA,       // rows
            double *b,      // matrix of right hand side vectors
            int *ldb,       // rows of right hand side
            double *Sigma,  // singular values
            double *rcond,  // used to determine if singular value is 0
            int *rank,      // rank of the matrix on output
            double *work,   // workspace
            int *lwork,     // size of workspace
            double *rwork,  // workspace
            int *info);     // error info

#ifndef __FFLASFFPACK_config_blas_H
/* cblas routines */
// computes "ax + y"
void cblas_daxpy(const int n,      // length of vectors
                 const double a,   // scalar alpha
                 const double *x,  // vector x
                 const int incx,   // increment of x
                 double *y,        // vector y
                 const int incy);  // increment of y
#endif

// computes ax
void cblas_dscal(const int n,      // length of vectors
                 const double a,   // scalar alpha
                 double *x,        // vector x
                 const int incx);  // increment of x

// computes "alpha AB + beta C"
// NOTE: first 3 args should formally be ENUMS, not ints.
//       Problem? e.g., what if enums change?
void cblas_zgemm(
    const int Order,    // how matrices are stored, by column or row.
    const int TransA,   // whether to transform A, e.g. take transpose
    const int TransB,   // whether to transform B
    const int M,        // rows of A
    const int N,        // columns of B
    const int K,        // columns of A, which must = rows of B
    const void *alpha,  // scalar alpha
    const void *A,      // matrix A
    const int lda,      // rows of A
    const void *B,      // matrix B
    const int ldb,      // rows of B
    const void *beta,   // scalar bet
    void *C,            // matrix C; on output, alphaAB+betaC
    const int ldc);     // rows of C
};


class Lapack
{
 public:
  ///////////////////////////////////////////
  // Translation to/from RR/CC and RRR/CCC //
  ///////////////////////////////////////////

  ////////////////////////////////
  // Input matrices are real /////
  ////////////////////////////////

  static M2_arrayintOrNull LU(const DMatRRR *A,
                              DMatRRR *L,
                              DMatRRR *U);

  static bool solve_triangular(const DMatRRR *U, const DMatRRR *b, DMatRRR *x);

  static bool solve(const DMatRRR *A, const DMatRRR *b, DMatRRR *x);
  // A and b are not modified.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const DMatRRR *A, DMatCCC *eigenvals);
  // Find the eigenvalues of A.  A is not modified.
  // Result is placed into eigenvals.

  static bool eigenvectors(const DMatRRR *A,
                           DMatCCC *eigenvals,
                           DMatCCC *eigenvecs);

  static bool eigenvalues_symmetric(const DMatRRR *A, DMatRRR *eigenvals);

  static bool eigenvectors_symmetric(const DMatRRR *A,
                                     DMatRRR *eigenvals,
                                     DMatRRR *eigenvecs);

  static bool SVD(const DMatRRR *A,
                  DMatRRR *Sigma,
                  DMatRRR *U,
                  DMatRRR *VT);

  static bool SVD_divide_conquer(const DMatRRR *A,
                                 DMatRRR *Sigma,
                                 DMatRRR *U,
                                 DMatRRR *VT);

  static bool least_squares(const DMatRRR *A,
                            const DMatRRR *b,
                            DMatRRR *x);

  static bool least_squares_deficient(const DMatRRR *A,
                                      const DMatRRR *b,
                                      DMatRRR *x);

  ////////////////////////////////
  // Input matrices are complex //
  ////////////////////////////////

  static M2_arrayintOrNull LU(const DMatCCC *A,
                              DMatCCC *L,
                              DMatCCC *U);

  static bool solve(const DMatCCC *A, const DMatCCC *b, DMatCCC *x);

  // static bool solve(const DMatCCC *A, const DMatCCC *b, DMatCCC *x,
  // const unsigned long precision);
  // A and b are not modified.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const DMatCCC *A, DMatCCC *eigenvals);

  static bool eigenvectors(const DMatCCC *A,
                           DMatCCC *eigenvals,
                           DMatCCC *eigenvecs);

  static bool eigenvalues_hermitian(const DMatCCC *A, DMatRRR *eigenvals);

  static bool eigenvectors_hermitian(const DMatCCC *A,
                                     DMatRRR *eigenvals,
                                     DMatCCC *eigenvecs);

  static bool SVD(const DMatCCC *A,
                  DMatRRR *Sigma,
                  DMatCCC *U,
                  DMatCCC *VT);

  static bool SVD_divide_conquer(const DMatCCC *A,
                                 DMatRRR *Sigma,
                                 DMatCCC *U,
                                 DMatCCC *VT);

  static bool least_squares(const DMatCCC *A,
                            const DMatCCC *b,
                            DMatCCC *x);

  static bool least_squares_deficient(const DMatCCC *A,
                                      const DMatCCC *b,
                                      DMatCCC *x);

  /// xxx////////////////////////////// same for RR/CC
  /// //////////////////////////////////////////////

 public:
  typedef DMat<M2::ARingRR> DMatRR;
  typedef DMat<M2::ARingCC> DMatCC;

  ///////////////////////////////////////////
  // Translation to/from RR/CC and RRR/CCC //
  ///////////////////////////////////////////

  ////////////////////////////////
  // Input matrices are real /////
  ////////////////////////////////

  static M2_arrayintOrNull LU(const DMatRR *A, DMatRR *L, DMatRR *U);

  static bool solve(const DMatRR *A, const DMatRR *b, DMatRR *x);
  // A and b are not modified.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const DMatRR *A, DMatCC *eigenvals);
  // Find the eigenvalues of A.  A is not modified.
  // Result is placed into eigenvals.

  static bool eigenvectors(const DMatRR *A,
                           DMatCC *eigenvals,
                           DMatCC *eigenvecs);

  static bool eigenvalues_symmetric(const DMatRR *A, DMatRR *eigenvals);

  static bool eigenvectors_symmetric(const DMatRR *A,
                                     DMatRR *eigenvals,
                                     DMatRR *eigenvecs);

  static bool SVD(const DMatRR *A,
                  DMatRR *Sigma,
                  DMatRR *U,
                  DMatRR *VT);

  static bool SVD_divide_conquer(const DMatRR *A,
                                 DMatRR *Sigma,
                                 DMatRR *U,
                                 DMatRR *VT);

  static bool least_squares(const DMatRR *A,
                            const DMatRR *b,
                            DMatRR *x);

  static bool least_squares_deficient(const DMatRR *A,
                                      const DMatRR *b,
                                      DMatRR *x);

  static bool QR(const DMatRR *A,
                 DMatRR *Q,
                 DMatRR *R,
                 bool return_QR);

  ////////////////////////////////
  // Input matrices are complex //
  ////////////////////////////////

  static M2_arrayintOrNull LU(const DMatCC *A, DMatCC *L, DMatCC *U);

  static bool solve(const DMatCC *A, const DMatCC *b, DMatCC *x);

  // static bool solve(const DMatCC *A, const DMatCC *b, DMatCC *x,
  // const unsigned long precision);
  // A and b are not modified.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const DMatCC *A, DMatCC *eigenvals);

  static bool eigenvectors(const DMatCC *A,
                           DMatCC *eigenvals,
                           DMatCC *eigenvecs);

  static bool eigenvalues_hermitian(const DMatCC *A, DMatRR *eigenvals);

  static bool eigenvectors_hermitian(const DMatCC *A,
                                     DMatRR *eigenvals,
                                     DMatCC *eigenvecs);

  static bool SVD(const DMatCC *A,
                  DMatRR *Sigma,
                  DMatCC *U,
                  DMatCC *VT);

  static bool SVD_divide_conquer(const DMatCC *A,
                                 DMatRR *Sigma,
                                 DMatCC *U,
                                 DMatCC *VT);

  static bool least_squares(const DMatCC *A,
                            const DMatCC *b,
                            DMatCC *x);

  static bool least_squares_deficient(const DMatCC *A,
                                      const DMatCC *b,
                                      DMatCC *x);

  static bool QR(const DMatCC *A,
                 DMatCC *Q,
                 DMatCC *R,
                 bool return_QR);

  static void freeRaw(__mpfr_struct *start, int size);
};

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
