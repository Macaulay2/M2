#include "matrixRR.hpp"
#include "matrixCC.hpp"
#include "stdio.h"
#include "string.h"
#include "../d/M2mem.h"

double LMatrixRR::_epsilon = 0.0;

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


void LMatrixRR::initialize(int nrows0, int ncols0, double *array)
{
  _nrows = nrows0;
  _ncols = ncols0;
  int len = nrows0 * ncols0;
  _array = newarray_atomic(double,len);
  if (array == 0)
    {
      for (int i=0; i<len; i++)
	_array[i] = 0;
    }
  else
    {
      for (int i=0; i<len; i++)
	_array[i] = array[i];
    }
}

LMatrixRR::LMatrixRR(int nrows0, int ncols0)
  : _nrows(nrows0),
    _ncols(ncols0)
{
  initialize(nrows0, ncols0, 0);
  // We assume that nrows0, ncols0 >= 0.
}


void LMatrixRR::text_out(buffer &o) const
{
  char s[1000], t[1000];
  int *field_len = newarray_atomic(int, _ncols);
  int loc = 0;

  for (int j=0; j<_ncols; j++) {
    unsigned int len = 0;
    for (int i=0; i<_nrows; i++) {
      if (_array[loc] == 0)
	_array[loc] = 0; // fixes -0.000 problem
      sprintf(s,"%f ", _array[loc++]);
      if (strlen(s) > len)
	len = strlen(s);
    }
    field_len[j] = len;
  }

  for (int i=0; i<_nrows; i++) {
    for (int j=0; j<_ncols; j++) {
      loc = j * _nrows + i;
      sprintf(s,"%f ", _array[loc]);
      memset(t, ' ', field_len[j]-strlen(s)); // left side space padding
      memcpy(t+field_len[j]-strlen(s), s, 
	     strlen(s)+1); // add 1 to make sure to get '\0'
      o << t;
    }
    o << newline;
  }
}

void LMatrixRR::resize(int new_nrows, int new_ncols)
{
  int new_len = new_nrows * new_ncols;
  if (new_len != _nrows * _ncols)
    initialize(new_nrows, new_ncols, 0);
  else
    for (int i=0; i<new_len; i++)
      _array[i] = 0;
}

LMatrixRR * LMatrixRR::copy() const
{
  LMatrixRR *result = new LMatrixRR;
  result->initialize(_nrows, _ncols, _array);
  return result;
}

void LMatrixRR::get_entry(int r, int c, double *re) const
{
  if (r >= _nrows || c >= _ncols || r < 0 || c < 0)
    return;
  int loc = c * _nrows + r;
  *re = _array[loc];
}

void LMatrixRR::set_entry(int r, int c, double re)
{
  if (r >= _nrows || c >= _ncols || r < 0 || c < 0)
    return;
  int loc = c * _nrows + r;
  _array[loc] = re;
}

void LMatrixRR::set_column(int c, double *vals)
{
  if (c >= _ncols || c < 0)
    return;
  int loc = c * _nrows;
  for (int i=0; i<_nrows; i++)
    {
      _array[loc++] = *vals++;
    }
}

void LMatrixRR::set_row(int r, double *vals)
{
  if (r >= _nrows || r < 0)
    return;
  int loc = r;
  for (int i=0; i<_nrows; i++)
    {
      _array[loc] = *vals++;
      loc += _ncols;
    }
}

void LMatrixRR::set_matrix(double *vals)
{
  for (int i=0; i<_ncols*_nrows; i++)
    {
      _array[i] = *vals++;
    }
}

void LMatrixRR::set_matrix(LMatrixRR *mat)
{
  resize(mat->nrows(), mat->ncols());
  for (int i=0; i<_nrows*_ncols; i++)
    _array[i] = mat->_array[i];
}

void LMatrixRR::set_values(M2_arrayint rowcols, M2_double_array *vals)
{
  if (2*vals->len != rowcols->len)  // put error message here?
    return;
  int r, c, loc;
  for (unsigned int i = 0; i < vals->len; i++) {
    r = rowcols->array[2*i];
    c = rowcols->array[2*i+1];
    if (r >= _nrows || c >= _ncols || r < 0 || c < 0)
      continue;
  loc = c * _nrows + r;
  _array[loc] = vals->array[i];
  }
}

LMatrixRR * LMatrixRR::sub_matrix(const M2_arrayint rows, const M2_arrayint cols) const
{
  int rowsize = 0;
  int colsize = 0;
  for (unsigned int i = 0; i < rows->len; i++) {
    if (rows->array[i] >= _nrows || rows->array[i] < 0) continue;
    rowsize++;
  }
  for (unsigned int i = 0; i < cols->len; i++) {
    if (cols->array[i] >= _ncols || cols->array[i] < 0) continue;
    colsize++;
  }
  LMatrixRR * submat = new LMatrixRR(rowsize, colsize);
  int entry = 0;
  int r, c;
  for (unsigned int j = 0; j < cols->len; j++) {
    c = cols->array[j];
    if (c >= _ncols || c < 0) continue;
    for (unsigned int i = 0; i < rows->len; i++) {
      r = rows->array[i];
      if (r >= _nrows || r < 0) continue;
      submat->_array[entry++] = _array[c*_nrows + r];
    }
  }
  return submat;
}


LMatrixRROrNull * LMatrixRR::operator+(const LMatrixRR *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = _nrows*_ncols;
  LMatrixRR *P = N->copy();

  cblas_daxpy(len, 1, _array, 1, P->_array, 1);
  return P;
}

LMatrixCCOrNull * LMatrixRR::operator+(const LMatrixCC *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = _nrows*_ncols;
  LMatrixCC *P = N->copy();

  // increment P by 2 to only add to real part
  cblas_daxpy(len, 1, _array, 1, P->_array, 2);
  return P;
}

LMatrixRROrNull * LMatrixRR::operator-(const LMatrixRR *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = _nrows*_ncols;
  LMatrixRR *P = copy();

  cblas_daxpy(len, -1, N->_array, 1, P->_array, 1);
  return P;
}

LMatrixCCOrNull * LMatrixRR::operator-(const LMatrixCC *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = _nrows*_ncols;
  LMatrixCC *P = N->copy();

  //compute -P
  cblas_dscal(2*len, -1, P->_array, 1);

  // increment P by 2 to only add to real part
  cblas_daxpy(len, 1, _array, 1, P->_array, 2);
  return P;
}

LMatrixRR * LMatrixRR::operator-() const
{
  int len = _nrows*_ncols;
  LMatrixRR *P = copy();

  //compute -P
  cblas_dscal(len, -1, P->_array, 1);

  return P;
}

LMatrixRROrNull * LMatrixRR::operator*(const LMatrixRR *N) const
{
  if (_ncols != N->nrows()) {
    ERROR("Expected compatible matrices for multiplication");
    return 0;
  }

  LMatrixRR *P = new LMatrixRR(_nrows, N->ncols());

  cblas_dgemm(102, // column major "CblasColMajor"
	      111, 111, // no transpose "CblasNoTrans"
	      _nrows, N->ncols(), _ncols,
	      1, // scalar alpha in "alpha AB + beta C"
	      _array, _nrows,
	      N->_array, N->nrows(), 
	      0, // scalar beta in "alpha AB + beta C"
	      P->_array, _nrows);

  return P;

  /* -- Old version --
  LMatrixRR *P = new LMatrixRR(_nrows, N->ncols());
  double re1, re2, tot;
  for (int i = 0; i < _nrows; i++)
    for (int j = 0; j < N->ncols(); j++) {
      tot = 0;
      for (int k = 0; k < _ncols; k++) {
	re1 = _array[_nrows*k + i];  // entry (i,k)
	re2 = N->_array[(N->_nrows)*j + k];  // entry (k,j) of N
	tot += re1*re2;
      }
      P->set_entry(i, j, tot);
    }

  return P;
  */
}

LMatrixCC * LMatrixRR::operator*(const LMatrixCC *N) const
{
  if (_ncols != N->nrows()) {
    ERROR("Expected compatible matrices for multiplication");
    return 0;
  }

  LMatrixCC *P = new LMatrixCC(copy());
  return (*P) * N;

  /* -- Old version --
  LMatrixCC *P = new LMatrixCC(_nrows, N->ncols());
  double re, im, re_tot, im_tot;
  for (int i = 0; i < _nrows; i++)
    for (int j = 0; j < N->ncols(); j++) {
      re_tot = 0;
      im_tot = 0;
      for (int k = 0; k < _ncols; k++) {
	re = N->_array[(N->_nrows)*j + k].re; // real part of N(k,j)
	im = N->_array[(N->_nrows)*j + k].im; // imag part of N(k,j)
	re_tot += _array[_nrows*k+i]*re;  // mult by entry (i,k)
	im_tot += _array[_nrows*k+i]*im;  // mult by entry (i,k)
      }
      P->_array[(P->_nrows)*j + i].re = re_tot; // set entry (i,j)
      P->_array[(P->_nrows)*j + i].im = im_tot; // set entry (i,j)
    }

  return P;
  */
}

bool LMatrixRR::is_equal(const LMatrixRR &N) const
{
  return is_close(N, _epsilon);
  /* exact equality version:
  if (this == &N) return true;
  if (_nrows != N.nrows() || _ncols != N.ncols())
    return false;
  for (int i = 0; i < _nrows*_ncols; i++)
    if (_array[i] != N._array[i])
      return false;
  return true;
  */
}

bool LMatrixRR::is_close(const LMatrixRR &N, double tolerance) const
{
  if (this == &N) return true;
  if (_nrows != N.nrows() || _ncols != N.ncols())
    return false;
  for (int i = 0; i < _nrows*_ncols; i++)
    if (_array[i] - N._array[i] > tolerance ||
	_array[i] - N._array[i] < -tolerance) 
      return false;
  return true;
}

LMatrixRROrNull * LMatrixRR::solve(LMatrixRR *b, LMatrixRR *x)
{
  LMatrixRR *copythis = copy();

  int size = _nrows;
  int bsize, info;
  int *permutation = newarray_atomic(int, size);

  /* make sure matrix is square */
  if (_nrows != _ncols)
    {
      ERROR("expected a square matrix");
      return 0;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->nrows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return 0;
    }

  bsize = b->ncols();
  x->set_matrix(b);

  dgesv_(&size, &bsize,
	 copythis->_array, 
	 &size, permutation, 
	 x->_array,
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

LMatrixRROrNull * LMatrixRR::LU(LMatrixRR *L, LMatrixRR *U, LMatrixRR *P)
{
  int rows = _nrows;
  int cols = _ncols;
  int info;
  int min = (rows <= cols) ? rows : cols;
  int *permutation = newarray_atomic(int, min);

  LMatrixRR *copythis = copy();

  P->resize(rows, rows);
  L->resize(rows, rows);
  U->resize(rows, cols);

  dgetrf_(&rows, &cols, copythis->_array, 
	  &rows, permutation, &info);

  /* set the lower triangular matrix L */
  double *vals = L->_array;
  int loc = 0;
  for (int j=0; j<cols; j++) {
    for (int i=0; i<rows; i++) {
      if (i > j) {
	*vals++ = copythis->_array[loc++];
      } else if (i == j) {
	*vals++ = 1;
	loc++;
      } else {
	*vals++ = 0;
	loc++;
      }
    }
  }

  /* set the upper triangular matrix U */
  vals = U->_array;
  loc = 0;
  for (int j=0; j<cols; j++) {
    for (int i=0; i<rows; i++) {
      if (i <= j) {
	*vals++ = copythis->_array[loc++];
      } else {
	*vals++ = 0;
	loc ++;
      }
    }
  }

  /* set the permutation matrix P */
  for (int row=1; row<=min; row++) {
    int targ = row;
    for (int i=1; i<=min; i++) {
      if (i == targ)
	targ = permutation[i-1];
      else if (permutation[i-1] == targ)
	targ = i;
    }
    P->set_entry(row-1, targ-1, 1);
  }

  if (info < 0)       
    {
      ERROR("argument passed to dgetrf had an illegal value");
      return 0;
    }
  else if (info > 0) {
    ERROR("Warning: matrix is singular according to dgetrf");
  }

  return U;
}

LMatrixCCOrNull * LMatrixRR::eigenvalues(LMatrixCC *eigvals)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  LMatrixRR *copythis = copy();

  char dont = 'N';
  int size = _nrows;
  int wsize = 3*size;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  LMatrixRR * real = new LMatrixRR(size,1); // real components of eigvals
  LMatrixRR * imag = new LMatrixRR(size,1); // imaginary components

  dgeev_(&dont, &dont, 
	 &size, copythis->_array, &size,
	 real->_array, 
	 imag->_array,
	 static_cast<double *>(0), &size,  /* left eigenvectors */
	 static_cast<double *>(0), &size,  /* right eigenvectors */
	 workspace, &wsize, &info);

  if (info < 0)       
    {
      ERROR("argument passed to dgeev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      return 0;
    }

  eigvals->resize(size, 1);
  for (int i = 0; i < size; i++) {
    eigvals->_array[2*i] = real->_array[i];
    eigvals->_array[2*i+1] = imag->_array[i];
  }

  return eigvals;
  
}

LMatrixCC * LMatrixRR::eigenvectors(LMatrixCC *eigvals,
				    LMatrixCC *eigvecs)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  LMatrixRR *copythis = copy();

  char dont = 'N';
  char doit = 'V';
  int size = _nrows;
  int wsize = 4*size;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  LMatrixRR * real = new LMatrixRR(size,1); // real components of eigvals
  LMatrixRR * imag = new LMatrixRR(size,1); // imaginary components
  LMatrixRR * eigen = new LMatrixRR(size,size); // eigvecs

  dgeev_(&dont, &doit, 
	 &size, copythis->_array, &size,
	 real->_array, 
	 imag->_array,
	 static_cast<double *>(0), &size,  /* left eigvecs */
	 eigen->_array, &size,  /* right eigvecs */
	 workspace, &wsize, &info);

  if (info < 0)       
    {
      ERROR("argument passed to dgeev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      return 0;
    }

  // Make the complex arrays of eigvals and eigvecs
  eigvals->resize(size, 1);
  eigvecs->resize(size, size);
  for (int j = 0; j < size; j++) {
    eigvals->_array[2*j] = real->_array[j];
    eigvals->_array[2*j+1] = imag->_array[j];
    int loc = j*size;
    if (imag->_array[j] == 0) {
      for (int i = 0; i < size; i++)
	eigvecs->_array[2*(loc+i)] = eigen->_array[loc+i];
    } else if (imag->_array[j] > 0) {
      for (int i = 0; i < size; i++) {
	eigvecs->_array[2*(loc+i)] = eigen->_array[loc+i];
	eigvecs->_array[2*(loc+i) + 1] = eigen->_array[loc+size+i];
	eigvecs->_array[2*(loc+size+i)] = eigen->_array[loc+i];
	eigvecs->_array[2*(loc+size+i) + 1] = -eigen->_array[loc+size+i];
      }
    } 
  }

  return eigvecs;
  
}


LMatrixRROrNull * LMatrixRR::eigenvalues_symmetric(LMatrixRR *eigvals)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  LMatrixRR *copythis = copy();

  char dont = 'N';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix. */

  int size = _nrows;
  int wsize = 3*size-1;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  eigvals->resize(size,1);

  dsyev_(&dont, &triangle, 
	 &size, copythis->_array, 
	 &size, eigvals->_array,
	 workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("dsyev did not converge");
      return 0;
    }

  return eigvals;
}

LMatrixRROrNull * LMatrixRR::eigenvectors_symmetric(LMatrixRR *eigvals, 
						    LMatrixRR *eigvecs)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  char doit = 'V';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix */

  int size = _nrows;
  int wsize = 3*size-1;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  eigvecs->set_matrix(this);
  eigvals->resize(size,1);

  dsyev_(&doit, &triangle, 
	 &size, eigvecs->_array, 
	 &size, eigvals->_array,
	 workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("dsyev did not converge");
      return 0;
    }

  return eigvecs;
}



LMatrixRROrNull * LMatrixRR::SVD(LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT)
{
  LMatrixRR *copythis = copy();
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = _nrows;
  int cols = _ncols;
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = (3*min+max >= 5*min) ? 3*min+max : 5*min;
  double *workspace = newarray_atomic(double, wsize);

  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);
  
  dgesvd_(&doit, &doit, &rows, &cols, 
	  copythis->_array, &rows,
	  Sigma->_array, 
	  U->_array, &rows,
	  VT->_array, &cols,
	  workspace, &wsize, 
	  &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesvd had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("dgesvd did not converge");
      return 0;
    }
  
  return Sigma;
}

LMatrixRROrNull * LMatrixRR::SVD_divide_conquer(LMatrixRR *Sigma, 
						LMatrixRR *U, LMatrixRR *VT)
{
  LMatrixRR *copythis = copy();
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = _nrows;
  int cols = _ncols;
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 4*min*min + max + 9*min;
  double *workspace = newarray_atomic(double,wsize);
  int *iworkspace = newarray_atomic(int, 8*min);

  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);

  dgesdd_(&doit, &rows, &cols, 
	  copythis->_array, &rows,
	  Sigma->_array, 
	  U->_array, &rows,
	  VT->_array, &cols,
	  workspace, &wsize, 
	  iworkspace, &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesdd had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("dgesdd did not converge");
      return 0;
    }

  return Sigma;
}


LMatrixRROrNull * LMatrixRR::least_squares(LMatrixRR *b, LMatrixRR *x)
{
  LMatrixRR *copythis = copy();
  LMatrixRR *copyb = b->copy();
  char job = 'N';
  int rows = _nrows;
  int cols = _ncols;
  int brows = b->nrows();
  int bcols = b->ncols();
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
	copyb->_array[copyloc++] = b->_array[bloc++];
      }
    }
  }
  
  dgels_(&job, &rows, &cols, &bcols,
	 copythis->_array, &rows,
	 copyb->_array, &max,
	 workspace, &wsize, 
	 &info);

  if (info != 0)
    {
      ERROR("argument passed to dgels had an illegal value");
      return 0;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*rows;
      for (int i = 0; i < cols; i++) {
	x->_array[xloc++] = copyb->_array[copyloc++];
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

  return x;
}

LMatrixRROrNull * LMatrixRR::least_squares_deficient(LMatrixRR *b, LMatrixRR *x)
{
  LMatrixRR *copythis = copy();
  LMatrixRR *copyb = b->copy();
  int rows = _nrows;
  int cols = _ncols;
  int brows = b->nrows();
  int bcols = b->ncols();
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
    return 0;
  }

  if (rows < cols) {
    copyb->resize(cols, bcols);
    for (int i = 0; i < brows*bcols; i++)
      copyb->_array[i] = b->_array[i];
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*cols;
      for (int i = 0; i < brows; i++) {
	copyb->_array[copyloc++] = b->_array[bloc++];
      }
    }
  }

  dgelss_(&rows, &cols, &bcols,
	  copythis->_array, &rows,
	  copyb->_array, &max,
	  sing, &rcond, &rank,
	  workspace, &wsize, 
	  &info);

  if (info != 0)
    {
      ERROR("argument passed to dgelss had an illegal value");
      return 0;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*rows;
      for (int i = 0; i < cols; i++) {
	x->_array[xloc++] = copyb->_array[copyloc++];
      }
    }
  } else {
    x->set_matrix(copyb);
  }

  return x;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
