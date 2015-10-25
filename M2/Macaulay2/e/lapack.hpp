#ifndef __lapack_h_
#define __lapack_h_

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "dmat.hpp"
#ifdef HAVE_MPACK
#include <mpack/mblas_mpfr.h>
#include <mpack/mlapack_mpfr.h>
#endif
#include <mpfr.h>
/* Lapack routines */
/* Compute solutions x to Ax = b for square matrix A and a matrix b */
extern "C" {
int dgesv_(int *n,    // number of rows in A
           int *nrhs, // number of right hand sides
           double *a, // n by n matrix A, on exit L&U from A=PLU
           int *lda,  // n
           int *ipiv, // indices defining permutation P
           double *b, // right-hand-side, on exit the solution matrix
           int *ldb,  // n
           int *info);// error info

int dgeev_(char *n,    // whether to compute left eigenvectors
           char *n2,   // whether to compute right eigenvectors
           int *size,  // rows
           double *M,  // input matrix A
           int *size1, // rows
           double *E,  // real components of eigenvalues
           double *E2, // imaginar components of eigenvalues
           double *,   // left eigenvectors
           int *,      // rows
           double *,   // right eigenvectors
           int *,      // rows
           double *,   // workspace
           int *,      // size of workspace
           int *);     // error info

int dsyev_(char *n,      // whether to compute eigenvectors
           char *n2,     // how to store A (upper or lower)
           int *size,    // rows
           double *M,    // symmetric matrix A
           int *lda,     // rows
           double *eig,  // becomes eigenvalues
           double *work, // workspace
           int *wsize,   // size of workspace
           int *info);   // error info

void dgetrf_(const int *rows, // rows
             const int *cols, // columns
             double *A,       // input matrix, on exit L & U from A=PLU.
             const int *ld,   // rows
             int *ipiv,  // becomes permutation indices of P
             int *info); // error info

int dgesvd_(char* jobU,    // amount of U to return
            char* jobV,    // amount of V to return
            int* rows,     // rows
            int* cols,     // columns
            double *A,     // input matrix for SVD
            int *ldA,      // rows
            double *Sigma, // singular values
            double *U,     // U
            int *ldU,      // rows
            double *VT,    // V transpose
            int *ldVT,     // cols
            double *work,  // workspace
            int *lwork,    // size of workspace
            int *info);    // error info

int dgesdd_(char* jobU,    // amount of U to return
            int* rows,     // rows
            int* cols,     // columns
            double *A,     // input matrix for SVD
            int *ldA,      // rows
            double *Sigma, // singular values
            double *U,     // U
            int *ldU,      // rows
            double *VT,    // V transpose
            int *ldVT,     // cols
            double *work,  // workspace
            int *lwork,    // size of workspace
            int *iwork,    // integer workspace
            int *info);    // error info

int dgels_(char* job,     // type of least squares problem
           int* rows,     // rows
           int* cols,     // columns
           int* nhrs,     // number right hand sides
           double *A,     // input matrix for least squares
           int *ldA,      // rows
           double *b,     // matrix of right hand side vectors
           int *ldb,      // rows of right hand side
           double *work,  // workspace
           int *lwork,    // size of workspace
           int *info);    // error info

int dgelss_(int* rows,     // rows
            int* cols,     // columns
            int* nhrs,     // number right hand sides
            double *A,     // input matrix for least squares
            int *ldA,      // rows
            double *b,     // matrix of right hand side vectors
            int *ldb,      // rows of right hand side
            double *Sigma, // singular values
            double *rcond, // used to determine if singular value is 0
            int *rank,     // rank of the matrix on output
            double *work,  // workspace
            int *lwork,    // size of workspace
            int *info);    // error info

#ifndef __FFLASFFPACK_config_blas_H
/* cblas routines */
// computes "ax + y"
void cblas_daxpy(const int n,     // length of vectors
                 const double a,  // scalar alpha
                 const double* x, // vector x
                 const int incx,  // increment of x
                 double* y,       // vector y
                 const int incy); // increment of y



// computes "alpha AB + beta C"
// NOTE: first 3 args should formally be ENUMS, not ints.
//       Problem? e.g., what if enums change?
void cblas_dgemm(const int Order,     // how matrices are stored, by column or row.
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
void cblas_dscal(const int n,     // length of vectors
                 const double a,  // scalar alpha
                 const double* x, // vector x
                 const int incx); // increment of x


int zgesv_(int *n,    // number of rows in A
           int *nrhs, // number of right hand sides
           double *a, // n by n matrix A, on exit L&U from A=PLU
           int *lda,  // n
           int *ipiv, // indices defining permutation P
           double *b, // right-hand-side
           int *ldb,  // n
           int *info);// error info

int zgeev_(char *n,        // whether to compute left eigenvectors
           char *n2,       // whether to compute right eigenvectors
           int *size,      // rows
           double *M,  // n by n input matrix
           int *size1,     // rows
           double *E,  // eigenvalues, on output
           double *l,  // left eigenvectors, on output
           int *lsize,     // rows
           double *r,  // right eigenvectors, on output
           int *rsize,     // rows
           double *w,  // workspace
           int *wsize,     // size of workspace
           double *rwork,  // another workspace
           int *info);     // error info

int zheev_(char *n,       // whether to compute eigenvectors
           char *n2,      // how to store A (upper or lower)
           int *size,     // rows
           double *M, // hermitian matrix A
           int *lda,      // rows
           double *eig,   // becomes eigenvalues
           double *w, // workspace
           int *wsize,    // size of workspace
           double *rwork, // another workspace
           int *info);    // error info

int zgetrf_(int *rows, // rows
            int *cols, // columns
            double *M, // input matrix, on exit L & U from A=PLU.
            int *ld,   // rows
            int *ipiv, // becomes permutation indices of P
            int *info);// error info

int zgesvd_(char* jobU,     // amount of U to return
            char* jobV,     // amount of V to return
            int* rows,      // rows
            int* cols,      // columns
            double *A,  // input matrix for SVD
            int *ldA,       // rows
            double *Sigma,  // singular values
            double *U,  // U
            int *ldU,       // rows
            double *VT, // V transpose
            int *ldVT,      // cols
            double *w,  // workspace
            int *lwork,     // size of workspace
            double *rwork,  // another workspace
            int *info);     // error info

int zgesdd_(char* jobU,     // amount of U to return
            int* rows,      // rows
            int* cols,      // columns
            double *A,  // input matrix for SVD
            int *ldA,       // rows
            double *Sigma,  // singular values
            double *U,  // U
            int *ldU,       // rows
            double *VT, // V transpose
            int *ldVT,      // cols
            double *w,  // workspace
            int *lwork,     // size of workspace
            double *rwork,  // another workspace
            int *iwork,     // integer workspace
            int *info);     // error info

int zgels_(char* job,     // type of least squares problem
           int* rows,     // rows
           int* cols,     // columns
           int* nhrs,     // number right hand sides
           double *A,     // input matrix for least squares
           int *ldA,      // rows
           double *b,     // matrix of right hand side vectors
           int *ldb,      // rows of right hand side
           double *work,  // workspace
           int *lwork,    // size of workspace
           int *info);    // error info

int zgelss_(int* rows,     // rows
            int* cols,     // columns
            int* nhrs,     // number right hand sides
            double *A,     // input matrix for least squares
            int *ldA,      // rows
            double *b,     // matrix of right hand side vectors
            int *ldb,      // rows of right hand side
            double *Sigma, // singular values
            double *rcond, // used to determine if singular value is 0
            int *rank,     // rank of the matrix on output
            double *work,  // workspace
            int *lwork,    // size of workspace
            double *rwork,  // workspace
            int *info);    // error info

#ifndef __FFLASFFPACK_config_blas_H
/* cblas routines */
// computes "ax + y"
void cblas_daxpy(const int n,     // length of vectors
                 const double a,  // scalar alpha
                 const double* x, // vector x
                 const int incx,  // increment of x
                 double* y,       // vector y
                 const int incy); // increment of y
#endif

// computes ax
void cblas_dscal(const int n,     // length of vectors
                 const double a,  // scalar alpha
                 const double* x, // vector x
                 const int incx); // increment of x


// computes "alpha AB + beta C"
// NOTE: first 3 args should formally be ENUMS, not ints.
//       Problem? e.g., what if enums change?
void cblas_zgemm(const int Order,   // how matrices are stored, by column or row.
                 const int TransA,  // whether to transform A, e.g. take transpose
                 const int TransB,  // whether to transform B
                 const int M,       // rows of A
                 const int N,       // columns of B
                 const int K,       // columns of A, which must = rows of B
                 const void *alpha, // scalar alpha
                 const void *A,     // matrix A
                 const int lda,     // rows of A
                 const void *B,     // matrix B
                 const int ldb,     // rows of B
                 const void *beta,  // scalar bet
                 void *C,           // matrix C; on output, alphaAB+betaC
                 const int ldc);    // rows of C

};

class Lapack {
 public:
  typedef DMat<M2::ARingRRR> LMatrixRRR; 
  typedef DMat<M2::ARingCCC> LMatrixCCC; 

  typedef LMatrixCCC::elem CCelem;
  //typedef CoeffRing::elem CCelem;

  ///////////////////////////////////////////
  // Translation to/from RR/CC and RRR/CCC //
  ///////////////////////////////////////////

  ////////////////////////////////
  // Input matrices are real /////
  ////////////////////////////////

  static M2_arrayintOrNull LU(const LMatrixRRR *A,
                               LMatrixRRR *L,
                               LMatrixRRR *U);

  static bool solve(const LMatrixRRR *A, const LMatrixRRR *b, LMatrixRRR *x);
  // A and b are not modifed.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const LMatrixRRR *A, LMatrixCCC *eigenvals);
  // Find the eigenvalues of A.  A is not modified.
  // Result is placed into eigenvals.

  static bool eigenvectors(const LMatrixRRR *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs);

  static bool eigenvalues_symmetric(const LMatrixRRR *A, LMatrixRRR *eigenvals);

  static bool eigenvectors_symmetric(const LMatrixRRR *A, LMatrixRRR *eigenvals, LMatrixRRR *eigenvecs);

  static bool SVD(const LMatrixRRR *A, LMatrixRRR *Sigma, LMatrixRRR *U, LMatrixRRR *VT);

  static bool SVD_divide_conquer(const LMatrixRRR *A, LMatrixRRR *Sigma, LMatrixRRR *U, LMatrixRRR *VT);

  static bool least_squares(const LMatrixRRR *A, const LMatrixRRR *b, LMatrixRRR *x);

  static bool least_squares_deficient(const LMatrixRRR *A, const LMatrixRRR *b, LMatrixRRR *x);

  ////////////////////////////////
  // Input matrices are complex //
  ////////////////////////////////

  static M2_arrayintOrNull LU(const LMatrixCCC *A,
                               LMatrixCCC *L,
                               LMatrixCCC *U
                               );

  static bool solve(const LMatrixCCC *A, const LMatrixCCC *b, LMatrixCCC *x);

  //static bool solve(const LMatrixCCC *A, const LMatrixCCC *b, LMatrixCCC *x, const unsigned long precision);
  // A and b are not modifed.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const LMatrixCCC *A, LMatrixCCC *eigenvals);

  static bool eigenvectors(const LMatrixCCC *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs);

  static bool eigenvalues_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals);

  static bool eigenvectors_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals, LMatrixCCC *eigenvecs);

  static bool SVD(const LMatrixCCC *A, LMatrixRRR *Sigma, LMatrixCCC *U, LMatrixCCC *VT);

  static bool SVD_divide_conquer(const LMatrixCCC *A, LMatrixRRR *Sigma, LMatrixCCC *U, LMatrixCCC *VT);

  static bool least_squares(const LMatrixCCC *A, const LMatrixCCC *b, LMatrixCCC *x);

  static bool least_squares_deficient(const LMatrixCCC *A, const LMatrixCCC *b, LMatrixCCC *x);

  ///xxx////////////////////////////// same for RR/CC //////////////////////////////////////////////

 public:
  typedef DMat<M2::ARingRR> LMatrixRR; 
  typedef DMat<M2::ARingCC> LMatrixCC; 

  ///////////////////////////////////////////
  // Translation to/from RR/CC and RRR/CCC //
  ///////////////////////////////////////////

  ////////////////////////////////
  // Input matrices are real /////
  ////////////////////////////////

  static M2_arrayintOrNull LU(const LMatrixRR *A,
                               LMatrixRR *L,
                               LMatrixRR *U);

  static bool solve(const LMatrixRR *A, const LMatrixRR *b, LMatrixRR *x);
  // A and b are not modifed.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const LMatrixRR *A, LMatrixCC *eigenvals);
  // Find the eigenvalues of A.  A is not modified.
  // Result is placed into eigenvals.

  static bool eigenvectors(const LMatrixRR *A, LMatrixCC *eigenvals, LMatrixCC *eigenvecs);

  static bool eigenvalues_symmetric(const LMatrixRR *A, LMatrixRR *eigenvals);

  static bool eigenvectors_symmetric(const LMatrixRR *A, LMatrixRR *eigenvals, LMatrixRR *eigenvecs);

  static bool SVD(const LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT);

  static bool SVD_divide_conquer(const LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT);

  static bool least_squares(const LMatrixRR *A, const LMatrixRR *b, LMatrixRR *x);

  static bool least_squares_deficient(const LMatrixRR *A, const LMatrixRR *b, LMatrixRR *x);

  ////////////////////////////////
  // Input matrices are complex //
  ////////////////////////////////

  static M2_arrayintOrNull LU(const LMatrixCC *A,
                               LMatrixCC *L,
                               LMatrixCC *U
                               );

  static bool solve(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x);

  //static bool solve(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x, const unsigned long precision);
  // A and b are not modifed.  The result is placed into x.
  // Returns x s.t. Ax = b
  // A should be non-singular.

  static bool eigenvalues(const LMatrixCC *A, LMatrixCC *eigenvals);

  static bool eigenvectors(const LMatrixCC *A, LMatrixCC *eigenvals, LMatrixCC *eigenvecs);

  static bool eigenvalues_hermitian(const LMatrixCC *A, LMatrixRR *eigenvals);

  static bool eigenvectors_hermitian(const LMatrixCC *A, LMatrixRR *eigenvals, LMatrixCC *eigenvecs);

  static bool SVD(const LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT);

  static bool SVD_divide_conquer(const LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT);

  static bool least_squares(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x);

  static bool least_squares_deficient(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x);


  // should we dump the following.... ?
#ifdef HAVE_MPACK
  static void delete_mpack_array(__mpfr_struct* a, int len);
  static void fill_from_mpack_array(CCelem *elemarray, mpreal *mparray, int cols, int rows);
// can't link... it looks like mpcomplex can't be supported by the current version of MPACK
//  static void fill_from_mpack_array2(CCelem *elemarray, mpcomplex *mparray, int cols, int rows);
#endif
  static void freeRaw(__mpfr_struct *start, int size);
};

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
