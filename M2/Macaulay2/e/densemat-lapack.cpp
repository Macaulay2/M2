#include "densematRR.hpp"

/* Lapack routines */
/* Compute solutions x to Ax = b for square matrix A and a matrix b */
extern "C" void dgesv_(int *n,    // number of rows in A
		       int *nrhs, // number of right hand sides
		       double *a, // n by n matrix A, on exit L&U from A=PLU
		       int *lda,  // n
		       int *ipiv, // indices defining permutation P
		       double *b, // right-hand-side, on exit the solution matrix
		       int *ldb,  // n
		       int *info);// error info

extern "C" void dgeev_(char *n,    // whether to compute left eigenvectors
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


extern "C" void dsyev_(char *n,      // whether to compute eigenvectors
		       char *n2,     // how to store A (upper or lower)
		       int *size,    // rows
		       double *M,    // symmetric matrix A
		       int *lda,     // rows
		       double *eig,  // becomes eigenvalues
		       double *work, // workspace
		       int *wsize,   // size of workspace
		       int *info);   // error info
		       

extern "C" void dgetrf_(int *rows, // rows 
			int *cols, // columns
			double *A, // input matrix, on exit L & U from A=PLU.
			int *ld,   // rows
			int *ipiv, // becomes permutation indices of P
			int *info);// error info


extern "C" void dgesvd_(char* jobU,    // amount of U to return
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

extern "C" void dgesdd_(char* jobU,    // amount of U to return
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

extern "C" void dgels_(char* job,     // type of least squares problem
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

extern "C" void dgelss_(int* rows,     // rows
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

/* cblas routines */
// computes "ax + y"
extern "C" void cblas_daxpy(const int n,     // length of vectors
			    const double a,  // scalar alpha
			    const double* x, // vector x
			    const int incx,  // increment of x
			    double* y,       // vector y
			    const int incy); // increment of y

// computes ax
extern "C" void cblas_dscal(const int n,     // length of vectors
			    const double a,  // scalar alpha
			    const double* x, // vector x
			    const int incx); // increment of x

// computes "alpha AB + beta C"
// NOTE: first 3 args should formally be ENUMS, not ints.
//       Problem? e.g., what if enums change?
extern "C" void cblas_dgemm(const int Order,     // how matrices are stored, by column or row.
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

DenseMutableMatrixRR * DenseMutableMatrixRR::solve(DenseMutableMatrixRR *b, 
						   DenseMutableMatrixRR *x)
{
  MutableMatrix *copythis1 = copy(true /* dense */ );
  DenseMutableMatrixRR *copythis = copythis1->cast_to_DenseMutableMatrixRR();
  int size = n_rows();
  int bsize, info;
  int *permutation = newarray_atomic(int, size);

  /* make sure matrix is square */
  if (n_rows() != n_cols())
    {
      ERROR("expected a square matrix");
      return 0;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->n_rows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return 0;
    }

  bsize = b->n_cols();
  x->initialize(b->n_rows(), n_cols(), b->array_);

  dgesv_(&size, &bsize,
	 copythis->array_, 
	 &size, permutation, 
	 x->array_,
	 &size, &info);

  if (info > 0)       
    {
      ERROR("according to dgesv, matrix is singular");
      return 0;
    }
  else if (info < 0)
    {
      ERROR("argument passed to dgesv had an illegal value");
      return 0;
    }

  return x;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
