#ifndef __lapack_h_
#define __lapack_h_

#if defined(__cplusplus)
class LMatrixRR;
class LMatrixCC;
#else
typedef struct LMatrixRR LMatrixRR;
typedef struct LMatrixCC LMatrixCC;
#endif

typedef LMatrixRR LMatrixRROrNull;
typedef LMatrixCC LMatrixCCOrNull;

typedef struct M2_double_array {
  unsigned int len;
  double array[1];
} M2_double_array;

typedef struct M2_complex_array {
  unsigned int len;
  M2_CC array[1];
} M2_complex_array;

typedef struct LArrayPair_int_double {
  M2_arrayint indices;
  M2_double_array *values;
} LArrayPair_int_double;

typedef struct LArrayPair_int_complex {
  M2_arrayint indices;
  M2_complex_array *values;
} LArrayPair_int_complex;

#if defined(__cplusplus)
extern "C" {
#endif

M2_CC LP_make_M2_Comp(double* w);
M2_CC LP_make_M2_Complex(double re, double im);

LMatrixRR *LP_LMatrixRR_make(int nrows, int ncols); 
  /* Assumed: nrows, ncols >= 0.  Negative values are treated as 0. */

LMatrixCC *LP_LMatrixCC_make(int nrows, int ncols);
  /* Assumed: nrows, ncols >= 0.  Negative values are treated as 0. */

M2_string LP_LMatrixRR_to_string(const LMatrixRR *M);

M2_string LP_LMatrixCC_to_string(const LMatrixCC *M);

int LP_LMatrixRR_nrows(const LMatrixRR *M);
int LP_LMatrixRR_ncols(const LMatrixRR *M);
int LP_LMatrixCC_nrows(const LMatrixCC *M);
int LP_LMatrixCC_ncols(const LMatrixCC *M);
  /* returns number of rows or columns of M */

void LP_LMatrixRR_get_entry(const LMatrixRR *M, int r, int c, double *result);
  /* If (r,c) is out of range, set the result to 0.0 */

void LP_LMatrixCC_get_entry(const LMatrixCC *M, int r, int c, M2_CC result);
  /* If (r,c) is out of range, set the result to 0.0 + 0.0i */

void LP_LMatrixRR_set_entry(LMatrixRR *M, int r, int c, double re);
  /* If (r,c) is out of range, this does nothing */

  /* change to const M2_Complex *val */
void LP_LMatrixCC_set_entry(LMatrixCC *M, int r, int c, M2_CC val);
  /* If (r,c) is out of range, this does nothing */

  /* TO_DO */
LArrayPair_int_double LP_LMatrixRR_get_values(LMatrixRR *M);
  /* Returns two array: an array of integer pairs (stored consecutively), 
     corresponding to indices, and an array of double's, corresponding to
     non-zero entries of M.
  */

  /* TO_DO */
LArrayPair_int_complex LP_LMatrixCC_get_values(LMatrixRR *M);
  /* Returns two array: an array of integer pairs (stored consecutively), 
     corresponding to indices, and an array of complex's, corresponding to
     non-zero entries of M.
  */

void LP_LMatrixRR_set_values(LMatrixRR *M, M2_arrayint rowcols, M2_double_array *vals);
  /* rowcols should be an array [r1,c1,r2,c2,...,rn,cn], and vals should
     be an array of doubles [v1,...,vn].  Set M[ri,ci] = vi, all i=1..n. 
     Any index pairs out of range are ignored.
     Also does nothing if array of rowcols is not twice as long as array of vals.
  */

void LP_LMatrixCC_set_values(LMatrixCC *M, M2_arrayint rowcols, M2_complex_array *vals);
  /* rowcols should be an array [r1,c1,r2,c2,...,rn,cn], and vals should
     be an array of complexes [v1,...,vn].  Set M[ri,ci] = vi, all i=1..n.
     Any index pairs out of range are ignored.
     Also does nothing if array of rowcols is not twice as long as array of vals.
  */

LMatrixRR * LP_LMatrixRR_get_submatrix(LMatrixRR *M, M2_arrayint rows, 
					     M2_arrayint cols);
  /* Returns submatrix of specified rows and columns, in order (i.e.,
     a row or column may be repeated multiple times).
     Ignores any rows or columns which are out of bounds.
  */

LMatrixCC * LP_LMatrixCC_get_submatrix(LMatrixCC *M, M2_arrayint rows, 
					     M2_arrayint cols);
  /* Returns submatrix of specified rows and columns, in order (i.e.,
     a row or column may be repeated multiple times).
     Ignores any rows or columns which are out of bounds.
  */

LMatrixCCOrNull *LP_LMatrixRR_eigenvalues(LMatrixRR *M, LMatrixCC *eigenvalues);
  /* Given a square matrix M, and a previously initialized matrix 'eigenvalues' 
     (it may have be initialized to any size: the size will be changed in necessary)
     Computes eigenvalues of M.  Stores them in 'eigenvalues', also returning 
     'eigenvalues', or NULL if the matrix is not square, or the LAPACK routine
     produces an error.
  */

LMatrixCCOrNull *LP_LMatrixRR_eigenvectors(LMatrixRR *M, LMatrixCC *eigenvalues, 
					   LMatrixCC *eigenvectors);
  /* Given square matrix M, and previously initialized matrices 'eigenvalues' and
     'eigenvectors' (which may have any size: the size will be changed in necessary)
     Computes eigenvalues and eigenvectors of M, and stores them in 'eigenvalues', 
     and 'eigenvectors'.  Returns 'eigenvectors', or NULL if the matrix is not square, 
     or the LAPACK routine produces an error.
  */


LMatrixCCOrNull *LP_LMatrixCC_eigenvalues(LMatrixCC *M, LMatrixCC *eigenvalues);
  /* Given a square matrix M, and a previously initialized matrix 'eigenvalues' 
     (it may have be initialized to any size: the size will be changed in necessary)
     Computes eigenvalues of M.  Stores them in 'eigenvalues', also returning 
     'eigenvalues', or NULL if the matrix is not square, or the LAPACK routine
     produces an error.
  */

LMatrixCCOrNull *LP_LMatrixCC_eigenvectors(LMatrixCC *M, LMatrixCC *eigenvalues, 
					   LMatrixCC *eigenvectors);
  /* Given square matrix M, and previously initialized matrices 'eigenvalues' and
     'eigenvectors' (which may have any size: the size will be changed in necessary)
     Computes eigenvalues and eigenvectors of M, and stores them in 'eigenvalues', 
     and 'eigenvectors'.  Returns 'eigenvectors', or NULL if the matrix is not square, 
     or the LAPACK routine produces an error.
  */

LMatrixRROrNull *LP_LMatrixRR_eigenvalues_symmetric(LMatrixRR *M, LMatrixRR *eigenvalues);
  /* Given a square matrix M, and a previously initialized matrix 'eigenvalues' 
     (it may have be initialized to any size: the size will be changed in necessary)
     M is assumed to be symmetric, by only looking at the upper triangular part.
     Computes eigenvalues of M.  Stores them in 'eigenvalues', also returning 
     'eigenvalues', or NULL if the matrix is not square, or the LAPACK routine
     produces an error.
  */

LMatrixRROrNull *LP_LMatrixRR_eigenvectors_symmetric(LMatrixRR *M, LMatrixRR *eigenvalues, 
						     LMatrixRR *eigenvectors);
  /* Given a symmetric matrix M, and previously initialized matrices 'eigenvalues' and
     'eigenvectors' (which may have any size: the size will be changed in necessary).
     M is assumed to be symmetric, by only looking at the upper triangular part.
     Computes eigenvalues and eigenvectors of M, and stores them in 'eigenvalues', 
     and 'eigenvectors'.  Returns 'eigenvectors', or NULL if the matrix is not square, 
     or the LAPACK routine produces an error.
  */

LMatrixRROrNull *LP_LMatrixCC_eigenvalues_hermitian(LMatrixCC *M, LMatrixRR *eigenvalues);
  /* Given a square matrix M, and a previously initialized matrix 'eigenvalues' 
     (it may have be initialized to any size: the size will be changed in necessary)
     M is assumed to be hermitian, by only looking at the upper triangular part.
     Computes eigenvalues of M.  Stores them in 'eigenvalues', also returning 
     'eigenvalues', or NULL if the matrix is not square, or the LAPACK routine
     produces an error.
  */

LMatrixCCOrNull *LP_LMatrixCC_eigenvectors_hermitian(LMatrixCC *M, LMatrixRR *eigenvalues, 
						     LMatrixCC *eigenvectors);
  /* Given a symmetric matrix M, and previously initialized matrices 'eigenvalues' and
     'eigenvectors' (which may have any size: the size will be changed in necessary).
     M is assumed to be symmetric, by only looking at the upper triangular part.
     Computes eigenvalues and eigenvectors of M, and stores them in 'eigenvalues', 
     and 'eigenvectors'.  Returns 'eigenvectors', or NULL if the matrix is not square, 
     or the LAPACK routine produces an error.
  */

LMatrixRROrNull *LP_LMatrixRR_solve(LMatrixRR *A, LMatrixRR *b, LMatrixRR *x);
  /* Given an invertible n by n matrix A, an n by m matrix b, and a previously 
     initialized matrix x of any size (the size will be changed if necessary).  
     Computes solutions to Ax = b and stores in 'x'.  Returns 'x' or NULL
     if A is not square, A and b are not compatible, or LAPACK routine produces
     an error (matrix is not invertible, or argument had an illegal value).
  */

LMatrixCCOrNull *LP_LMatrixCC_solve(LMatrixCC *A, LMatrixCC *b, LMatrixCC *x);
  /* Given an invertible n by n matrix A, an n by m matrix b, and a previously 
     initialized matrix x of any size (the size will be changed if necessary).  
     Computes solutions to Ax = b and stores in 'x'.  Returns 'x' or NULL
     if A is not square, A and b are not compatible, or LAPACK routine produces
     an error (matrix is not invertible, or argument had an illegal value).
  */


LMatrixRROrNull *LP_LMatrixRR_LU(LMatrixRR *M, LMatrixRR *L,
				 LMatrixRR *U, LMatrixRR *P);
  /* Given a matrix M, and previously initialized matrices L, U, and P of
     any size (the size will be changed if necessary).
     Computes the LU decomposition 'M=PLU', where 'L' is lower triangular with 1's
     on the diagonal, 'U' is upper triangular, and 'P' is a permutation matrix.
     Returns 'U' or NULL if LAPACK routine produces an error.
  */


LMatrixCCOrNull *LP_LMatrixCC_LU(LMatrixCC *M, LMatrixCC *L,
				 LMatrixCC *U, LMatrixRR *P);
  /* Given a matrix M, and previously initialized matrices L, U, and P of
     any size (the size will be changed if necessary).
     Computes the LU decomposition 'M=PLU', where 'L' is lower triangular with 1's
     on the diagonal, 'U' is upper triangular, and 'P' is a permutation matrix.
     Returns 'U' or NULL if LAPACK routine produces an error.
  */

LMatrixRROrNull *LP_LMatrixRR_SVD(LMatrixRR *M, LMatrixRR *Sigma, 
				  LMatrixRR *U, LMatrixRR *VT);
  /* Given a matrix M, and previously initialized matrices U, Sigma, and VT 
     of any size (the size will be changed if necessary).  
     Computes the singular values and stores them in column vector 'Sigma'.  
     Also computes orthogonal matrices 'U' and 'VT' of SVD, 'M =[U][S][VT]', 
     where 'S' is the diagonal matrix with diagonal entries 'Sigma'.
     Returns 'Sigma' or NULL if LAPACK routine produces an error.
  */

LMatrixRROrNull *LP_LMatrixRR_SVD_divide_conquer(LMatrixRR *M, 
					   LMatrixRR *Sigma,
					   LMatrixRR *U, 
					   LMatrixRR *VT);
  /* Same as SVD but uses divide and conquer strategy */

LMatrixRROrNull *LP_LMatrixCC_SVD(LMatrixCC *M, LMatrixRR *Sigma, 
				  LMatrixCC *U, LMatrixCC *VT);
  /* Given a matrix M, and previously initialized matrices U, Sigma, and VT 
     of any size (the size will be changed if necessary).  
     Computes the singular values and stores them in column vector 'Sigma'.  
     Also computes orthogonal matrices 'U' and 'VT' of SVD, 'M =[U][S][VT]', 
     where 'S' is the diagonal matrix with diagonal entries 'Sigma'.
     Returns 'Sigma' or NULL if LAPACK routine produces an error.
     Note that 'Sigma' is a matrix over the reals.
  */

LMatrixRROrNull *LP_LMatrixCC_SVD_divide_conquer(LMatrixCC *M, 
						 LMatrixRR *Sigma,
						 LMatrixCC *U, 
						 LMatrixCC *VT);
  /* Same as SVD but uses divide and conquer strategy */

LMatrixRROrNull *LP_LMatrixRR_least_squares(LMatrixRR *M, LMatrixRR *b, 
					    LMatrixRR *x);
  /* Given a matrix M (which MUST be of FULL RANK), and previously 
     initialized matrices U, Sigma, and VT of any size (they'll be resized).
     Computes the solutions 'x' to the linear least squares problem which 
     minimizes |Ax-b| if A has more rows than columns or which minimizes |x| 
     satisfying Ax=b if A has more columns than rows.
     Returns 'x' or NULL if LAPACK routine produces an error
  */

LMatrixCCOrNull *LP_LMatrixCC_least_squares(LMatrixCC *M, LMatrixCC *b, 
					    LMatrixCC *x);
  /* Given a matrix M (which MUST be of FULL RANK), and previously 
     initialized matrices U, Sigma, and VT of any size (they'll be resized),
     Computes the solutions 'x' to the linear least squares problem which 
     minimizes |Ax-b| if A has more rows than columns or which minimizes |x| 
     satisfying Ax=b if A has more columns than rows.
     Returns 'x' or NULL if LAPACK routine produces an error
  */

LMatrixRROrNull *LP_LMatrixRR_least_squares_deficient(LMatrixRR *M, LMatrixRR *b, 
						      LMatrixRR *x);
  /* Given a matrix M (which can be rank deficient), and previously 
     initialized matrices U, Sigma, and VT of any size (they'll be resized),
     Computes the minimum norm solutions 'x' to the linear least squares problem 
     which minimizes |Ax-b|. 
     Returns 'x' or NULL if LAPACK routine produces an error
  */

LMatrixCCOrNull *LP_LMatrixCC_least_squares_deficient(LMatrixCC *M, LMatrixCC *b, 
						      LMatrixCC *x);
  /* Given a matrix M (which can be rank deficient), and previously 
     initialized matrices U, Sigma, and VT of any size (they'll be resized),
     Computes the minimum norm solutions 'x' to the linear least squares problem 
     which minimizes |Ax-b|. 
     Returns 'x' or NULL if LAPACK routine produces an error
  */

LMatrixRROrNull *LP_LMatrixRR_add(LMatrixRR *M, LMatrixRR *N);
LMatrixCCOrNull *LP_LMatrixRC_add(LMatrixRR *M, LMatrixCC *N);
LMatrixCCOrNull *LP_LMatrixCR_add(LMatrixCC *M, LMatrixRR *N);
LMatrixCCOrNull *LP_LMatrixCC_add(LMatrixCC *M, LMatrixCC *N);

LMatrixRROrNull *LP_LMatrixRR_subtract(LMatrixRR *M, LMatrixRR *N);
LMatrixCCOrNull *LP_LMatrixRC_subtract(LMatrixRR *M, LMatrixCC *N);
LMatrixCCOrNull *LP_LMatrixCR_subtract(LMatrixCC *M, LMatrixRR *N);
LMatrixCCOrNull *LP_LMatrixCC_subtract(LMatrixCC *M, LMatrixCC *N);

LMatrixRR *LP_LMatrixRR_negate(LMatrixRR *M);
LMatrixCC *LP_LMatrixCC_negate(LMatrixCC *M);

LMatrixCCOrNull *LP_LMatrixCC_mult(LMatrixCC *M, LMatrixCC *N);
LMatrixRROrNull *LP_LMatrixRR_mult(LMatrixRR *M, LMatrixRR *N);
LMatrixCCOrNull *LP_LMatrixRC_mult(LMatrixRR *M, LMatrixCC *N);
LMatrixCCOrNull *LP_LMatrixCR_mult(LMatrixCC *M, LMatrixRR *N);

double * LP_LMatrixRR_get_epsilon();
double * LP_LMatrixCC_get_epsilon();
void LP_LMatrixRR_set_epsilon(double epsilon);
void LP_LMatrixCC_set_epsilon(double epsilon);
  /* paramter _epsilon determines when matrices are considered equal */

const M2_bool LP_LMatrixRR_is_equal(const LMatrixRR *M, const LMatrixRR *N);
const M2_bool LP_LMatrixCC_is_equal(const LMatrixCC *M, const LMatrixCC *N);
  /* matrices are equal if real and imaginary parts of all entries
     are within _epsilon of each other */

/* The following routines are not to be placed in the front end interface */
LMatrixCC *LP_LMatrixCC_from_LMatrixRR(LMatrixRR *N);
void LP_LMatrixRR_resize(LMatrixRR *M, int nrows, int ncols);
void LP_LMatrixCC_resize(LMatrixCC *M, int nrows, int ncols);

#if defined(__cplusplus)
}
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
