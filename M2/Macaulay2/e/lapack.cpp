#include "coeffrings.hpp"
#include "dmat.hpp"
#include "lapack.hpp"

typedef DMat<CoefficientRingRR> LMatrixRR;

extern "C" M2_CC LP_make_M2_Complex(double re, double im)
{
  M2_CC z = newitem_atomic(M2_CC_struct);
  z->re = re;
  z->im = im;
  return z;
}

M2_arrayint_OrNull Lapack::LU(LMatrixRR *M)
{
  int rows = M->n_rows();
  int cols = M->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = makearrayint(rows);
  int *permutation = result->array;

  dgetrf_(&rows, &cols, M->get_lapack_array(),
	  &rows, permutation, &info);

  for (int i=0; i<min; i++)
    permutation[i]--;
  for (int i=min; i<rows; i++)
    permutation[i] = i;

  if (info < 0)       
    {
      ERROR("argument passed to dgetrf had an illegal value");
      return 0;
    }

  return result;
}

bool Lapack::solve(LMatrixRR *A, /* read only */
		   LMatrixRR *b, /* read only */
		   LMatrixRR *x) /* output value */
{
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

  LMatrixRR *copyA = A->copy();
  bsize = b->n_cols();
  x->set_matrix(b);

  dgesv_(&size, &bsize,
	 copyA->get_array(), 
	 &size, permutation, 
	 x->get_array(),
	 &size, &info);

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
}

bool Lapack::eigenvalues(LMatrixRR *A, LMatrixCC *eigvals)
{
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
}

bool Lapack::eigenvectors(LMatrixRR *A,
			  LMatrixCC *eigvals,
			  LMatrixCC *eigvecs)
{
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
}


bool Lapack::eigenvalues_symmetric(LMatrixRR *A, LMatrixRR *eigvals)
{
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
}

bool Lapack::eigenvectors_symmetric(LMatrixRR *A,
				    LMatrixRR *eigvals, 
				    LMatrixRR *eigvecs)
{
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
}

bool Lapack::SVD(LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT)
{
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
}

bool Lapack::SVD_divide_conquer(LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT)
{
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
}

bool Lapack::least_squares(LMatrixRR *A, LMatrixRR *b, LMatrixRR *x)
{
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
}

bool Lapack::least_squares_deficient(LMatrixRR *A, LMatrixRR *b, LMatrixRR *x)
{
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
}


M2_arrayint_OrNull Lapack::LU(LMatrixCC *M)
{
  int rows = M->n_rows();
  int cols = M->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = makearrayint(rows);
  int *permutation = result->array;

  zgetrf_(&rows, &cols, M->get_lapack_array(), 
	  &rows, permutation, &info);

  for (int i=0; i<min; i++)
    permutation[i]--;
  for (int i=min; i<rows; i++)
    permutation[i] = i;

  if (info < 0)       
    {
      ERROR("argument passed to zgetrf had an illegal value");
      return 0;
    }
  return result;
}

bool Lapack::solve(LMatrixCC *A, LMatrixCC *b, LMatrixCC *x)
{
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
}

bool Lapack::eigenvalues(LMatrixCC *A, LMatrixCC *eigvals)
{
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
}

bool Lapack::eigenvectors(LMatrixCC *A, LMatrixCC *eigvals, LMatrixCC *eigvecs)
{
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
}

bool Lapack::eigenvalues_hermitian(LMatrixCC *A, LMatrixRR *eigvals)
{
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
}

bool Lapack::eigenvectors_hermitian(LMatrixCC *A, LMatrixRR *eigvals, LMatrixCC *eigvecs)
{
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
}

bool Lapack::SVD(LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT)
{
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
}

bool Lapack::SVD_divide_conquer(LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT)
{
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
}

bool Lapack::least_squares(LMatrixCC *A, LMatrixCC *b, LMatrixCC *x)
{
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
}

bool Lapack::least_squares_deficient(LMatrixCC *A, LMatrixCC *b, LMatrixCC *x)
{
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
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
