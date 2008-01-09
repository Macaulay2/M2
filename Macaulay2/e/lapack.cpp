#include "coeffrings.hpp"
#include "lapack.hpp"
#include "config.h"

//typedef DMat<CoefficientRingRR> LMatrixRR;

// lapack arrays are all arrays of doubles.
// these arrays are grabbed via newarray_atomic, or newarray_atomic_clear
// and should be freed via deletearray.

typedef double *LapackDoubles;

M2_arrayint_OrNull Lapack::LU(const LMatrixRR *A,
			      LMatrixRR *L,
			      LMatrixRR *U)
{
#if !LAPACK
  ERROR("lapack not present");
  return NULL;
#else
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = makearrayint(rows);
  int *perm = newarray_atomic(int, min);

  LapackDoubles copyA = A->make_lapack_array();

  L->resize(rows, min);
  U->resize(min, cols);

  dgetrf_(&rows, &cols, copyA,
	  &rows, perm, &info);

  /* set the lower triangular matrix L */
  mpfr_ptr *vals = L->get_array();
  int loc = 0;
  for (int j=0; j<min; j++) {
    for (int i=0; i<rows; i++) {
      assert(vals < L->get_array() + L->n_rows() * L->n_cols());
      if (i > j) {
	mpfr_set_d(*vals++,copyA[loc++], GMP_RNDN);
      } else if (i == j) {
	mpfr_set_si(*vals++, 1, GMP_RNDN);
	loc++;
      } else {
	mpfr_set_si(*vals++, 0, GMP_RNDN);
	loc++;
      }
    }
  }

  /* set the upper triangular matrix U */
  vals = U->get_array();
  loc = 0;
  for (int j=0; j<cols; j++) {
    for (int i=0; i<min; i++) {
      assert(vals < U->get_array() + U->n_rows() * U->n_cols());
      if (i <= j) {
	mpfr_set_d(*vals++, copyA[loc++], GMP_RNDN);
      } else {
	mpfr_set_si(*vals++, 0, GMP_RNDN);
	loc ++;
      }
    }
  }

  
  /* set the permutation array */
  for (int row=1; row<=min; row++) {
    int targ = row;
    for (int i=1; i<=min; i++) {
      if (i == targ)
	targ = perm[i-1];
      else if (perm[i-1] == targ)
	targ = i;
    }
    result->array[row-1] = targ-1;
  }
  for (int i=min; i<rows; i++)
    result->array[i] = i;

  deletearray(copyA);
  deletearray(perm);

  if (info < 0)       
    {
      ERROR("argument passed to dgetrf had an illegal value");
      return 0;
    }

  return result;
#endif
}

bool Lapack::solve(const LMatrixRR *A, /* read only */
		   const LMatrixRR *b, /* read only */
		   LMatrixRR *x) /* output value */
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  int bsize, info;
  int *permutation = newarray_atomic(int, size);

  /* make sure matrix is square */
  if (A->n_rows() != A->n_cols())
    {
      ERROR("expected a square matrix");
      return false;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->n_rows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return false;;
    }

  LapackDoubles copyA = A->make_lapack_array();
  LapackDoubles copyb = b->make_lapack_array();

  bsize = b->n_cols();

  dgesv_(&size, &bsize,
	 copyA, 
	 &size, permutation, 
	 copyb, // also the result
	 &size, &info);

  // Now set x
  x->resize(size, bsize);
  mpfr_ptr *vals = x->get_array();
  long len = size*bsize;
  double *p = copyb;
  for (long i=0; i<len; i++)
    mpfr_set_d(*vals++, *p++, GMP_RNDN);

  if (info > 0)       
    {
      ERROR("according to dgesv, matrix is singular");
      return false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to dgesv had an illegal value");
      return false;
    }

  return true;
#endif
}

bool Lapack::eigenvalues(const LMatrixRR *A, LMatrixCC *eigvals)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  char dont = 'N';
  int wsize = 3*size;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  LMatrixRR * copyA = A->copy();
  LMatrixRR * real = new LMatrixRR(globalRR,size,1); // real components of eigvals
  LMatrixRR * imag = new LMatrixRR(globalRR,size,1); // imaginary components

  dgeev_(&dont, &dont, 
	 &size, copyA->get_lapack_array(), &size,
	 real->get_lapack_array(), 
	 imag->get_lapack_array(),
	 static_cast<double *>(0), &size,  /* left eigenvectors */
	 static_cast<double *>(0), &size,  /* right eigenvectors */
	 workspace, &wsize, &info);

  if (info < 0)       
    {
      ERROR("argument passed to dgeev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      return false;
    }

  eigvals->resize(size, 1);
  M2_CC_struct *elems = eigvals->get_array();
  for (int i = 0; i < size; i++) {
    elems[i].re = real->get_array()[i];
    elems[i].im = imag->get_array()[i];
  }

  return eigvals;
#endif
}

bool Lapack::eigenvectors(const LMatrixRR *A,
			  LMatrixCC *eigvals,
			  LMatrixCC *eigvecs)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  char dont = 'N';
  char doit = 'V';
  int wsize = 4*size;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  LMatrixRR *copyA = A->copy();
  LMatrixRR * real = new LMatrixRR(globalRR,size,1); // real components of eigvals
  LMatrixRR * imag = new LMatrixRR(globalRR,size,1); // imaginary components
  LMatrixRR * eigen = new LMatrixRR(globalRR,size,size); // eigvecs

  dgeev_(&dont, &doit, 
	 &size, copyA->get_array(), &size,
	 real->get_array(), 
	 imag->get_array(),
	 static_cast<double *>(0), &size,  /* left eigvecs */
	 eigen->get_array(), &size,  /* right eigvecs */
	 workspace, &wsize, &info);

  if (info < 0)       
    {
      ERROR("argument passed to dgeev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      return false;
    }

  // Make the complex arrays of eigvals and eigvecs
  eigvals->resize(size, 1);
  eigvecs->resize(size, size);
  M2_CC_struct *elems = eigvecs->get_array();
  for (int j = 0; j < size; j++) {
    eigvals->get_array()[j].re = real->get_array()[j];
    eigvals->get_array()[j].im = imag->get_array()[j];
    int loc = j*size;
    if (imag->get_array()[j] == 0) {
      for (int i = 0; i < size; i++)
	elems[loc+i].re = eigen->get_array()[loc+i];
    } else if (imag->get_array()[j] > 0) {
      for (int i = 0; i < size; i++) {
	elems[loc+i].re = eigen->get_array()[loc+i];
	elems[loc+i].im = eigen->get_array()[loc+size+i];
	elems[loc+size+i].re = eigen->get_array()[loc+i];
	elems[loc+size+i].im = -eigen->get_array()[loc+size+i];
      }
    } 
  }

  return true;
#endif
}

bool Lapack::eigenvalues_symmetric(const LMatrixRR *A, LMatrixRR *eigvals)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return 0;
  }

  char dont = 'N';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix. */

  int wsize = 3*size-1;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  LMatrixRR * copyA = A->copy();
  eigvals->resize(size,1);

  dsyev_(&dont, &triangle, 
	 &size, copyA->get_array(),
	 &size, eigvals->get_array(),
	 workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("dsyev did not converge");
      return false;
    }

  return true;
#endif
}

bool Lapack::eigenvectors_symmetric(const LMatrixRR *A,
				    LMatrixRR *eigvals, 
				    LMatrixRR *eigvecs)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  char doit = 'V';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix */

  int wsize = 3*size-1;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  eigvecs->set_matrix(A);
  eigvals->resize(size,1);

  dsyev_(&doit, &triangle, 
	 &size, eigvecs->get_array(), 
	 &size, eigvals->get_array(),
	 workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("dsyev did not converge");
      return false;
    }

  return true;
#endif
}

bool Lapack::SVD(const LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = (3*min+max >= 5*min) ? 3*min+max : 5*min;
  double *workspace = newarray_atomic(double, wsize);

  LMatrixRR * copyA = A->copy();
  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);
  
  dgesvd_(&doit, &doit, &rows, &cols, 
	  copyA->get_array(), &rows,
	  Sigma->get_array(), 
	  U->get_array(), &rows,
	  VT->get_array(), &cols,
	  workspace, &wsize, 
	  &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesvd had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("dgesvd did not converge");
      return false;
    }
  
  return true;
#endif
}

bool Lapack::SVD_divide_conquer(const LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 4*min*min + max + 9*min;
  double *workspace = newarray_atomic(double,wsize);
  int *iworkspace = newarray_atomic(int, 8*min);

  LMatrixRR * copyA = A->copy();
  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);

  dgesdd_(&doit, &rows, &cols, 
	  copyA->get_array(), &rows,
	  Sigma->get_array(), 
	  U->get_array(), &rows,
	  VT->get_array(), &cols,
	  workspace, &wsize, 
	  iworkspace, &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesdd had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("dgesdd did not converge");
      return false;
    }

  return true;
#endif
}

bool Lapack::least_squares(const LMatrixRR *A, const LMatrixRR *b, LMatrixRR *x)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  LMatrixRR *copyA = A->copy();
  LMatrixRR *copyb = b->copy();
  char job = 'N';
  int rows = A->n_rows();
  int cols = A->n_cols();
  int brows = b->n_rows();
  int bcols = b->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >=  max) ? bcols : max);
  double *workspace = newarray_atomic(double, wsize);

  if (brows != rows) {
    ERROR("expected compatible right hand side");
    return 0;
  }

  if (rows < cols) {
    copyb->resize(cols, bcols);
    // for (int i = 0; i < brows*bcols; i++) copyb->_array[i] = b->_array[i];
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*cols;
      for (int i = 0; i < brows; i++) {
	copyb->get_array()[copyloc++] = b->get_array()[bloc++];
      }
    }
  }
  
  dgels_(&job, &rows, &cols, &bcols,
	 copyA->get_array(), &rows,
	 copyb->get_array(), &max,
	 workspace, &wsize, 
	 &info);

  if (info != 0)
    {
      ERROR("argument passed to dgels had an illegal value");
      return false;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*rows;
      for (int i = 0; i < cols; i++) {
	x->get_array()[xloc++] = copyb->get_array()[copyloc++];
      }
    }
  } else {
    x->set_matrix(copyb);
  }

  return true;
#endif
}

bool Lapack::least_squares_deficient(const LMatrixRR *A, const LMatrixRR *b, LMatrixRR *x)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  LMatrixRR *copyA = A->copy();
  LMatrixRR *copyb = b->copy();
  int rows = A->n_rows();
  int cols = A->n_cols();
  int brows = b->n_rows();
  int bcols = b->n_cols();
  double rcond = -1.0;  // use machine precision
  int rank, info;
  int min = (rows < cols) ? rows : cols;
  int max = (rows > cols) ? rows : cols;
  int tempmax = ((2*min >  max) ? 2*min : max);
  int wsize = 3*min + ((tempmax >  bcols) ? tempmax : bcols);
  double *workspace = newarray_atomic(double,wsize);
  double *sing = newarray_atomic(double,min);

  if (brows != rows) {
    ERROR("expected compatible right hand side");
    return false;
  }

  if (rows < cols) {
    copyb->resize(cols, bcols);
    for (int i = 0; i < brows*bcols; i++)
      copyb->get_array()[i] = b->get_array()[i];
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*cols;
      for (int i = 0; i < brows; i++) {
	copyb->get_array()[copyloc++] = b->get_array()[bloc++];
      }
    }
  }

  dgelss_(&rows, &cols, &bcols,
	  copyA->get_array(), &rows,
	  copyb->get_array(), &max,
	  sing, &rcond, &rank,
	  workspace, &wsize, 
	  &info);

  if (info != 0)
    {
      ERROR("argument passed to dgelss had an illegal value");
      return false;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*rows;
      for (int i = 0; i < cols; i++) {
	x->get_array()[xloc++] = copyb->get_array()[copyloc++];
      }
    }
  } else {
    x->set_matrix(copyb);
  }

  return true;
#endif
}


M2_arrayint_OrNull Lapack::LU(const LMatrixCC *A,
			      LMatrixCC *L,
			      LMatrixCC *U)
{
#if LAPACK
  ERROR("lapack not present");
  return NULL;
#else
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = makearrayint(rows);
  int *perm = newarray_atomic(int, min);

  LMatrixCC *copyA = A->copy();

  L->resize(rows, rows);
  U->resize(rows, cols);

  zgetrf_(&rows, &cols, copyA->get_lapack_array(), 
	  &rows, perm, &info);

  // set the lower triangular matrix L
  double *vals = L->get_lapack_array();
  int loc = 0;
  for (int j=0; j<cols; j++) {
    for (int i=0; i<rows; i++) {
      if (i > j) {
	vals[2*loc] = copyA->get_lapack_array()[2*loc];
	vals[2*loc+1] = copyA->get_lapack_array()[2*loc+1];
      } else if (i == j) {
	vals[2*loc] = 1;
	vals[2*loc+1] = 0;
      } else {
	vals[2*loc] = 0;
	vals[2*loc+1] = 0;
      }
      loc++;
    }
  }

  // set the upper triangular matrix U
  vals = U->get_lapack_array();
  loc = 0;
  for (int j=0; j<cols; j++) {
    for (int i=0; i<rows; i++) {
      if (i <= j) {
	vals[2*loc] = copyA->get_lapack_array()[2*loc];
	vals[2*loc+1] = copyA->get_lapack_array()[2*loc+1];
      } else {
	vals[2*loc] = 0;
	vals[2*loc+1] = 0;
      }
      loc++;
    }
  }

  /* set the permutation array */
  for (int row=1; row<=min; row++) {
    int targ = row;
    for (int i=1; i<=min; i++) {
      if (i == targ)
	targ = perm[i-1];
      else if (perm[i-1] == targ)
	targ = i;
    }
    result->array[row-1] = targ-1;
  }
  for (int i=min; i<rows; i++)
    result->array[i] = i;

  if (info < 0)       
    {
      ERROR("argument passed to zgetrf had an illegal value");
      return 0;
    }
  return result;
#endif
}

bool Lapack::solve(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  LMatrixCC *copyA = A->copy();

  int size = A->n_rows();
  int bsize, info;
  int *permutation = newarray_atomic(int, size);

  /* make sure matrix is square */
  if (size != A->n_cols())
    {
      ERROR("expected a square matrix");
      return false;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->n_rows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return false;
    }

  bsize = b->n_cols();
  x->set_matrix(b);

  zgesv_(&size, &bsize,
	 copyA->get_lapack_array(),
	 &size, permutation, 
	 x->get_lapack_array(),
	 &size, &info);

  if (info > 0)       
    {
      ERROR("according to zgesv, matrix is singular");
      return false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to zgesv had an illegal value");
      return false;
    }

  return true;
#endif
}

bool Lapack::eigenvalues(const LMatrixCC *A, LMatrixCC *eigvals)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  LMatrixCC *copyA = A->copy();

  char dont = 'N';
  int wsize = 4*size;
  int rsize = 2*size;
  double *workspace = newarray_atomic(double, wsize);
  double *rwork = newarray_atomic(double, rsize);

  int info;

  eigvals->resize(size,1);

  zgeev_(&dont, &dont, 
	 &size, copyA->get_lapack_array(),
	 &size, eigvals->get_lapack_array(),
	 static_cast<double *>(0), &size,  /* left eigenvectors */
	 static_cast<double *>(0), &size,  /* right eigenvectors */
	 workspace, &wsize, rwork,
	 &info);

  if (info < 0)       
    {
      ERROR("argument passed to zgeev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      return false;
    }
  return true;
#endif
}

bool Lapack::eigenvectors(const LMatrixCC *A, LMatrixCC *eigvals, LMatrixCC *eigvecs)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size !=A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  LMatrixCC *copyA = A->copy();

  char dont = 'N';
  char doit = 'V';
  int wsize = 4*size;
  int rsize = 2*size;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,rsize);
  int info;

  eigvals->resize(size,1);
  eigvecs->resize(size,size);

  zgeev_(&dont, &doit, 
	 &size, copyA->get_lapack_array(),
	 &size, eigvals->get_lapack_array(),
	 static_cast<double *>(0), &size,  /* left eigvecs */
	 eigvecs->get_lapack_array(), &size,  /* right eigvecs */
	 workspace, &wsize, rwork,
	 &info);

  if (info < 0)       
    {
      ERROR("argument passed to zgeev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      return false;
    }

  return true;
#endif
}

bool Lapack::eigenvalues_hermitian(const LMatrixCC *A, LMatrixRR *eigvals)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  char dont = 'N';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix. */

  int wsize = 4*size-2;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,3*size-2);
  int info;

  LMatrixCC *copyA = A->copy();
  eigvals->resize(size,1);

  zheev_(&dont, &triangle, 
	 &size, copyA->get_lapack_array(),
	 &size, eigvals->get_array(),
	 workspace, &wsize, rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("zheev did not converge");
      return false;
    }

  return true;
#endif
}

bool Lapack::eigenvectors_hermitian(const LMatrixCC *A, LMatrixRR *eigvals, LMatrixCC *eigvecs)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  char doit = 'V';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix */

  int wsize = 4*size-2;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,3*size-2);
  int info;

  eigvecs->set_matrix(A);
  eigvals->resize(size,1);

  zheev_(&doit, &triangle, 
	 &size, eigvecs->get_lapack_array(),
	 &size, eigvals->get_array(),
	 workspace, &wsize, rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("zheev did not converge");
      return false;
    }

  return true;
#endif
}

bool Lapack::SVD(const LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 4*min+2*max;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,5*min);

  LMatrixCC *copyA = A->copy();
  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);
  
  zgesvd_(&doit, &doit, &rows, &cols, 
	  copyA->get_lapack_array(), &rows,
	  Sigma->get_array(), 
	  U->get_lapack_array(), &rows,
	  VT->get_lapack_array(), &cols,
	  workspace, &wsize, 
	  rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesvd had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("zgesvd did not converge");
      return false;
    }
  
  return true;
#endif
}

bool Lapack::SVD_divide_conquer(const LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 2*min*min + 4*min + 2*max;

  double *workspace = newarray_atomic(double, wsize);
  int *iworkspace = newarray_atomic(int,8*min);
  double *rwork = newarray_atomic(double,5*min*min + 7*min);

  LMatrixCC *copyA = A->copy();
  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);

  zgesdd_(&doit, &rows, &cols, 
	  copyA->get_lapack_array(), &rows,
	  Sigma->get_array(), 
	  U->get_lapack_array(), &rows,
	  VT->get_lapack_array(), &cols,
	  workspace, &wsize, rwork, 
	  iworkspace, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesdd had an illegal value");
      return false;
    }
  else if (info > 0) 
    {
      ERROR("zgesdd did not converge");
      return false;
    }

  return true;
#endif
}

bool Lapack::least_squares(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  LMatrixCC *copyA = A->copy();
  LMatrixCC *copyb = b->copy();
  char job = 'N';
  int rows = A->n_rows();
  int cols = A->n_cols();
  int brows = b->n_rows();
  int bcols = b->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >=  max) ? bcols : max);
  double *workspace = newarray_atomic(double, 2*wsize);

  if (brows != rows) {
    ERROR("expected compatible right hand side");
    return false;
  }

  if (rows < cols) {
    copyb->resize(cols, bcols);
    // for (int i = 0; i < brows*bcols; i++) copyb->_array[i] = b->_array[i];
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*cols;
      for (int i = 0; i < brows; i++) {
	copyb->get_array()[copyloc++] = b->get_array()[bloc++];
      }
    }
  }
  
  zgels_(&job, &rows, &cols, &bcols,
	 copyA->get_lapack_array(), &rows,
	 copyb->get_lapack_array(), &max,
	 workspace, &wsize, 
	 &info);

  if (info != 0)
    {
      ERROR("argument passed to zgels had an illegal value");
      return false;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*rows;
      for (int i = 0; i < cols; i++) {
	x->get_array()[xloc++] = copyb->get_array()[copyloc++];
      }
    }
  } else {
    x->set_matrix(copyb);
    /*
    x->resize(cols,bcols);
    for (int j = 0; j < bcols*cols; j++) {
      x->_array[j] = copyb->_array[j];
    }
    */
  }
  return true;
#endif
}

bool Lapack::least_squares_deficient(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x)
{
#if LAPACK
  ERROR("lapack not present");
  return false;
#else
  LMatrixCC *copyA = A->copy();
  LMatrixCC *copyb = b->copy();
  int rows = A->n_rows();
  int cols = A->n_cols();
  int brows = b->n_rows();
  int bcols = b->n_cols();
  double rcond = -1.0;
  int rank, info;
  int min = (rows < cols) ? rows : cols;
  int max = (rows > cols) ? rows : cols;
  int wsize = 2*min + ((bcols >  max) ? bcols : max);

  double *workspace = newarray_atomic(double, 2*wsize);
  double *sing = newarray_atomic(double, min);
  double *rwork = newarray_atomic(double, 5*min);

  if (brows != rows) {
    ERROR("expected compatible right hand side");
    return false;
  }

  if (rows < cols) {
    copyb->resize(cols, bcols);
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*cols;
      for (int i = 0; i < brows; i++) {
	copyb->get_array()[copyloc++] = b->get_array()[bloc++];
      }
    }
  }
  
  zgelss_(&rows, &cols, &bcols,
	  copyA->get_lapack_array(), &rows,
	  copyb->get_lapack_array(), &max,
	  sing, &rcond, &rank,
	  workspace, &wsize, 
	  rwork, &info);

  if (info != 0)
    {
      ERROR("argument passed to zgelss had an illegal value");
      return false;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*rows;
      for (int i = 0; i < cols; i++) {
	x->get_array()[xloc++] = copyb->get_array()[copyloc++];
      }
    }
  } else {
    x->set_matrix(copyb);
    /*
    x->resize(cols,bcols);
    for (int j = 0; j < bcols*cols; j++) {
      x->_array[j] = copyb->_array[j];
    }
    */
  }
  return true;
#endif
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
