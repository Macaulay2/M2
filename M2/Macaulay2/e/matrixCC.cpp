#include "matrixCC.hpp"
#include "matrixRR.hpp"
#include "stdio.h"
#include "string.h"

double LMatrixCC::_epsilon = 0.0;

/* lapack routines */

extern "C" void zgesv_(int *n,    // number of rows in A
		       int *nrhs, // number of right hand sides
		       double *a, // n by n matrix A, on exit L&U from A=PLU
		       int *lda,  // n
		       int *ipiv, // indices defining permutation P
		       double *b, // right-hand-side
		       int *ldb,  // n
		       int *info);// error info

extern "C" void zgeev_(char *n,        // whether to compute left eigenvectors
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

extern "C" void zheev_(char *n,       // whether to compute eigenvectors
		       char *n2,      // how to store A (upper or lower)
		       int *size,     // rows
		       double *M, // hermitian matrix A
		       int *lda,      // rows
		       double *eig,   // becomes eigenvalues
		       double *w, // workspace
		       int *wsize,    // size of workspace
		       double *rwork, // another workspace
		       int *info);    // error info

extern "C" void zgetrf_(int *rows, // rows 
			int *cols, // columns
			double *M, // input matrix, on exit L & U from A=PLU.
			int *ld,   // rows
			int *ipiv, // becomes permutation indices of P
			int *info);// error info

extern "C" void zgesvd_(char* jobU,     // amount of U to return
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

extern "C" void zgesdd_(char* jobU,     // amount of U to return
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

extern "C" void zgels_(char* job,     // type of least squares problem
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

extern "C" void zgelss_(int* rows,     // rows
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
extern "C" void cblas_zgemm(const int Order,   // how matrices are stored, by column or row.
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

void LMatrixCC::initialize(int nrows, int ncols, double *array)
{
  _nrows = nrows;
  _ncols = ncols;
  int len = 2 * nrows * ncols;
  _array = (double *) getmem_atomic(sizeof(double) * len);
  if (array == 0)
    for (int i=0; i<len; i++) {
      _array[i] = 0;
    }
  else
    for (int i=0; i<len; i++) {
      _array[i] = array[i];
    }
}

LMatrixCC::LMatrixCC(int nrows, int ncols)
  : _nrows(nrows),
    _ncols(ncols)
{
  initialize(nrows, ncols, 0);
  // We assume that nrows, ncols >= 0.
}

LMatrixCC::LMatrixCC(LMatrixRR *N)
{
  initialize(N->nrows(), N->ncols(), 0);
  for (int i = 0; i < _nrows*_ncols; i++)
    _array[2*i] = N->_array[i];
}


void LMatrixCC::text_out(buffer &o) const
{
  char s[1000], t[1000];
  int *field_len = (int *) getmem_atomic(sizeof(int) * _ncols);
  int loc = 0;

  for (int j=0; j<_ncols; j++) {
    unsigned int len = 0;
    for (int i=0; i<_nrows; i++) {
      if (_array[loc] == 0)
	_array[loc] = 0; // fixes -0.0000 problem for real part
      if (_array[loc+1] >= 0) {
	if (_array[loc+1] == 0) 
	  _array[loc+1] = 0; // fixes -0.0000 problem
	sprintf(s,"%f+%fi ", _array[loc], _array[loc+1]);
      } else
	sprintf(s,"%f%fi ", _array[loc], _array[loc+1]);
      loc += 2;
      if (strlen(s) > len)
	len = strlen(s);
    }
    field_len[j] = len;
  }

  for (int i=0; i<_nrows; i++) {
    for (int j=0; j<_ncols; j++) {
      loc = j * _nrows + i;
      if (_array[2*loc+1] >= 0)
	sprintf(s,"%f+%fi ", _array[2*loc], _array[2*loc+1]);
      else
	sprintf(s,"%f%fi ", _array[2*loc], _array[2*loc+1]);
      memset(t, ' ', field_len[j]-strlen(s)); // left side space padding
      memcpy(t+field_len[j]-strlen(s), s, 
	     strlen(s)+1); // add 1 to make sure to get '\0'
      o << t;
    }
    o << newline;
  }
}

void LMatrixCC::resize(int new_nrows, int new_ncols)
{
  int new_len = 2 * new_nrows * new_ncols;
  if (new_len != 2 * _nrows * _ncols)
    initialize(new_nrows, new_ncols, 0);
  else
    for (int i=0; i<new_len; i++) {
      _array[i] = 0;
    }
}

LMatrixCC * LMatrixCC::copy() const
{
  LMatrixCC *result = new LMatrixCC;
  result->initialize(_nrows, _ncols, _array);
  return result;
}

void LMatrixCC::get_entry(int r, int c, M2_CC result) const
{
  if (r >= _nrows || c >= _ncols)
    return;
  int loc = c * _nrows + r;
  result->re = _array[2*loc];
  result->im = _array[2*loc+1];
}

void LMatrixCC::set_entry(int r, int c, M2_CC val)
{
  if (r >= _nrows || c >= _ncols)
    return;
  int loc = c * _nrows + r;
  _array[2*loc] = val->re;
  _array[2*loc+1] = val->im;
}

void LMatrixCC::set_column(int c, M2_CC vals)
{
  if (c >= _ncols)
    return;
  int loc = 2 * c * _nrows;
  for (int i=0; i<_nrows; i++)
    {
      _array[loc++] = vals[i].re;
      _array[loc++] = vals[i].im;
    }
}

void LMatrixCC::set_row(int r, M2_CC vals)
{
  if (r >= _nrows)
    return;
  for (int i=0; i<_nrows; i++)
    {
      _array[2*i*_ncols + 2*r] = vals[i].re;
      _array[2*i*_ncols + 2*r + 1] = vals[i].im;
    }
}

void LMatrixCC::set_matrix(M2_CC vals)
{
  for (int i=0; i<_ncols*_nrows; i++)
    {
      _array[2*i] = vals[i].re;
      _array[2*i+1] = vals[i].im;
    }
}

void LMatrixCC::set_matrix(LMatrixCC *mat)
{
  resize(mat->nrows(), mat->ncols());
  for (int i=0; i< 2*_nrows*_ncols; i++) {
    _array[i] = mat->_array[i];
  }
}

void LMatrixCC::set_values(M2_arrayint rowcols, M2_complex_array *vals)
{
  if (2*vals->len != rowcols->len) // put error message here?
    return;
  int r, c, loc;
  for (unsigned int i = 0; i < vals->len; i++) {
    r = rowcols->array[2*i];
    c = rowcols->array[2*i+1];
    if (r >= _nrows || c >= _ncols)
      continue;
  loc = c * _nrows + r;
  _array[2*loc] = vals->array[i]->re;
  _array[2*loc+1] = vals->array[i]->im;
  }
}


void LMatrixCC::get_entry(int r, int c, double &re, double &im) const
{
  if (r >= _nrows || c >= _ncols)
    return;
  int loc = c * _nrows + r;
  re = _array[2*loc];
  im = _array[2*loc+1];
}

void LMatrixCC::set_entry(int r, int c, double re, double im)
{
  if (r >= _nrows || c >= _ncols)
    return;
  int loc = c * _nrows + r;
  _array[2*loc] = re;
  _array[2*loc+1] = im;
}

void LMatrixCC::set_column(int c, double *vals)
{
  if (c >= _ncols)
    return;
  int loc = 2 * c * _nrows;
  for (int i=0; i<_nrows; i++)
    {
      _array[loc++] = *vals++;
      _array[loc++] = *vals++;
    }
}

void LMatrixCC::set_row(int r, double *vals)
{
  if (r >= _nrows)
    return;
  for (int i=0; i<_nrows; i++)
    {
      _array[2*i*_ncols + 2*r] = *vals++;
      _array[2*i*_ncols + 2*r + 1] = *vals++;
    }
}

void LMatrixCC::set_matrix(double *vals)
{
  for (int i=0; i<2*_ncols*_nrows; i++)
    {
      _array[i] = *vals++;
    }
}

LMatrixCC * LMatrixCC::sub_matrix(const M2_arrayint rows, const M2_arrayint cols) const
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
  LMatrixCC * submat = new LMatrixCC(rowsize, colsize);
  int entry = 0;
  int r, c, loc;
  for (unsigned int j = 0; j < cols->len; j++) {
    c = cols->array[j];
    if (c >= _ncols || c < 0) continue;
    for (unsigned int i = 0; i < rows->len; i++) {
      r = rows->array[i];
      if (r >= _nrows || r < 0) continue;
      loc = 2*(c*_nrows + r);
      submat->_array[entry++] = _array[loc];
      submat->_array[entry++] = _array[loc+1];
    }
  }
  return submat;
}

LMatrixCCOrNull * LMatrixCC::operator+(const LMatrixCC *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = 2*_nrows*_ncols;  // pass matrices as type double
  LMatrixCC *P = N->copy();

  cblas_daxpy(len, 1, _array, 1, P->_array, 1);
  return P;
}

LMatrixCCOrNull * LMatrixCC::operator+(const LMatrixRR *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = _nrows*_ncols;
  LMatrixCC *P = copy();

  // increment P by 2 to only add to real component
  cblas_daxpy(len, 1, N->_array, 1, P->_array, 2);
  return P;
}

LMatrixCCOrNull * LMatrixCC::operator-(const LMatrixCC *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = 2*_nrows*_ncols;  // pass matrices as type double
  LMatrixCC *P = copy();

  cblas_daxpy(len, -1, N->_array, 1, P->_array, 1);
  return P;
}

LMatrixCCOrNull * LMatrixCC::operator-(const LMatrixRR *N) const
{
  if (_nrows != N->_nrows && _ncols != N->_ncols) {
    ERROR("Expected compatible matrices for addition");
    return 0;
  }

  int len = _nrows*_ncols;
  LMatrixCC *P = copy();

  // increment P by 2 to only add to real component
  cblas_daxpy(len, -1, N->_array, 1, P->_array, 2);
  return P;
}

LMatrixCC * LMatrixCC::operator-() const
{
  int len = 2*_nrows*_ncols;
  LMatrixCC *P = copy();

  //compute -P
  cblas_dscal(len, -1, P->_array, 1);

  return P;
}

LMatrixCCOrNull * LMatrixCC::operator*(const LMatrixCC *N) const
{
  if (_ncols != N->nrows()) {
    ERROR("Expected compatible matrices for multiplication");
    return 0;
  }

  double *alpha = (double *) getmem_atomic(sizeof(double)*2);
  double *beta = (double *) getmem_atomic(sizeof(double)*2);
  alpha[0] = 1; // real part
  alpha[1] = 0; // imag part
  beta[0] = 0;  // real part
  beta[1] = 0;  // imag part

  LMatrixCC *P = new LMatrixCC(_nrows, N->ncols());  
  cblas_zgemm(102, // column major "CblasColMajor"
	      111, 111, // no transpose "CblasNoTrans"
	      _nrows, N->ncols(), _ncols,
	      alpha, // scalar alpha in "alpha AB + beta C"
	      _array, _nrows,
	      N->_array, N->nrows(), 
	      beta, // scalar beta in "alpha AB + beta C"
	      P->_array, _nrows);

  return P;

  /* -- Old version --
  LMatrixCC *P = new LMatrixCC(_nrows, N->ncols());
  double re1, im1, re2, im2, re_tot, im_tot;
  for (int i = 0; i < _nrows; i++)
    for (int j = 0; j < N->ncols(); j++) {
      re_tot = 0;
      im_tot = 0;
      for (int k = 0; k < _ncols; k++) {
	re1 = _array[k*_nrows + i].re; // entry (i,k) real part
	im1 = _array[k*_nrows + i].im; // entry (i,k) imag part
	re2 = N->_array[j*_nrows + k].re; // entry (k,j) real part
	im2 = N->_array[j*_nrows + k].im; // entry (k,j) imag part
	re_tot += re1*re2-im1*im2;
	im_tot += re1*im2+re2*im1;
      }
      P->_array[j*_nrows + i].re = re_tot; // entry (i,j) real part
      P->_array[j*_nrows + i].im = im_tot; // entry (i,j) imag part
    }

  return P;
  */
}

LMatrixCCOrNull * LMatrixCC::operator*(const LMatrixRR *N) const
{
  if (_ncols != N->nrows()) {
    ERROR("Expected compatible matrices for multiplication");
    return 0;
  }

  LMatrixCC *P = new LMatrixCC(N->copy());  
  return (*this) * P;

  /* -- Old version --
  LMatrixCC *P = new LMatrixCC(_nrows, N->ncols());
  double re_tot, im_tot;
  for (int i = 0; i < _nrows; i++)
    for (int j = 0; j < N->ncols(); j++) {
      re_tot = 0;
      im_tot = 0;
      for (int k = 0; k < _ncols; k++) {
	re_tot += _array[k*_nrows + i].re * (N->_array[j*_nrows + k]);
	im_tot += _array[k*_nrows + i].im * (N->_array[j*_nrows + k]);
      }
      P->_array[j*_nrows + i].re = re_tot; // entry (i,j) real part
      P->_array[j*_nrows + i].im = im_tot; // entry (i,j) imag part
    }

  return P;
  */
}

bool LMatrixCC::is_equal(const LMatrixCC &N) const
{
  return is_close(N, _epsilon);
}

bool LMatrixCC::is_close(const LMatrixCC &N, double tolerance) const
{
  // closeness under the 1-norm
  if (this == &N) return true;
  if (_nrows != N.nrows() || _ncols != N.ncols())
    return false;
  for (int i = 0; i < 2*_nrows*_ncols; i++)
    if (_array[i] - N._array[i] > tolerance || 
	_array[i] - N._array[i] < -tolerance)
      return false;
  return true;
}


LMatrixCCOrNull * LMatrixCC::eigenvalues(LMatrixCC *eigenvalues)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  LMatrixCC *copythis = copy();

  char dont = 'N';
  int size = _nrows;
  int wsize = 4*size;
  int rsize = 2*size;
  double *workspace = (double *) getmem_atomic(sizeof(double) * wsize);
  double *rwork = (double *) getmem_atomic(sizeof(double) * rsize);
  int info;

  eigenvalues->resize(size,1);

  zgeev_(&dont, &dont, 
	 &size, copythis->_array, 
	 &size, eigenvalues->_array,
	 (double *)0, &size,  /* left eigenvectors */
	 (double *)0, &size,  /* right eigenvectors */
	 workspace, &wsize, rwork,
	 &info);

  if (info < 0)       
    {
      ERROR("argument passed to zgeev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigenvalues");
      return 0;
    }

  return eigenvalues;
  
}

LMatrixCCOrNull * LMatrixCC::eigenvectors(LMatrixCC *eigenvalues, LMatrixCC *eigenvectors)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  LMatrixCC *copythis = copy();

  char dont = 'N';
  char doit = 'V';
  int size = _nrows;
  int wsize = 4*size;
  int rsize = 2*size;
  double *workspace = (double *) getmem_atomic(sizeof(double) * wsize);
  double *rwork = (double *) getmem_atomic(sizeof(double) * rsize);
  int info;

  eigenvalues->resize(size,1);
  eigenvectors->resize(size,size);

  zgeev_(&dont, &doit, 
	 &size, copythis->_array, 
	 &size, eigenvalues->_array,
	 (double *)0, &size,  /* left eigenvectors */
	 eigenvectors->_array, &size,  /* right eigenvectors */
	 workspace, &wsize, rwork,
	 &info);

  if (info < 0)       
    {
      ERROR("argument passed to zgeev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigenvalues");
      return 0;
    }

  return eigenvectors;
}

LMatrixCCOrNull * LMatrixCC::solve(LMatrixCC *b, LMatrixCC *x)
{
  LMatrixCC *copythis = copy();

  int size = _nrows;
  int bsize, info;
  int *permutation = (int *) getmem_atomic(sizeof(int) * size);

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

  zgesv_(&size, &bsize,
	 copythis->_array, 
	 &size, permutation, 
	 x->_array,
	 &size, &info);

  if (info > 0)       
    {
      ERROR("according to zgesv, matrix is singular");
      return 0;
    }
  else if (info < 0)
    {
      ERROR("argument passed to zgesv had an illegal value");
      return 0;
    }

  return x;
}

LMatrixCCOrNull * LMatrixCC::LU(LMatrixCC *L, LMatrixCC *U, LMatrixRR *P)
{
  int rows = _nrows;
  int cols = _ncols;
  int info;
  int min = (rows <= cols) ? rows : cols;
  int *permutation = (int *) getmem_atomic(sizeof(int) * min);

  LMatrixCC *copythis = copy();

  P->resize(rows, rows);
  L->resize(rows, rows);
  U->resize(rows, cols);

  zgetrf_(&rows, &cols, copythis->_array, 
	  &rows, permutation, &info);

  // set the lower triangular matrix L
  double *vals = L->_array;
  int loc = 0;
  for (int j=0; j<cols; j++) {
    for (int i=0; i<rows; i++) {
      if (i > j) {
	vals[2*loc] = copythis->_array[2*loc];
	vals[2*loc+1] = copythis->_array[2*loc+1];
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
  vals = U->_array;
  loc = 0;
  for (int j=0; j<cols; j++) {
    for (int i=0; i<rows; i++) {
      if (i <= j) {
	vals[2*loc] = copythis->_array[2*loc];
	vals[2*loc+1] = copythis->_array[2*loc+1];
      } else {
	vals[2*loc] = 0;
	vals[2*loc+1] = 0;
      }
      loc++;
    }
  }

  // set the permutation matrix P
  for (int row=1; row<=min; row++) {
    int targ = row;
    for (int i=1; i<=min; i++) {
      if (i == targ)
	targ = permutation[i-1];
      else if (permutation[i-1] == targ)
	targ = i;
    }
    P->_array[(targ-1)*min + row - 1] = 1;
  }

  if (info < 0)       
    {
      ERROR("argument passed to zgetrf had an illegal value");
      return 0;
    }
  else if (info > 0) {
    ERROR("Warning: matrix is singular according to zgetrf");
  }

  return U;
}

LMatrixRROrNull * LMatrixCC::eigenvalues_hermitian(LMatrixRR *eigenvalues)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  LMatrixCC *copythis = copy();

  char dont = 'N';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix. */

  int size = _nrows;
  int wsize = 4*size-2;
  double *workspace = (double *) getmem_atomic(sizeof(double) * wsize);
  double *rwork = (double *) getmem_atomic(sizeof(double) * (3*size-2));
  int info;

  eigenvalues->resize(size,1);

  zheev_(&dont, &triangle, 
	 &size, copythis->_array, 
	 &size, eigenvalues->_array,
	 workspace, &wsize, rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("zheev did not converge");
      return 0;
    }

  return eigenvalues;
}

LMatrixCCOrNull * LMatrixCC::eigenvectors_hermitian(LMatrixRR *eigenvalues, 
						    LMatrixCC *eigenvectors)
{
  if (_nrows != _ncols) {
    ERROR("expected a square matrix");
    return 0;
  }

  char doit = 'V';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix */

  int size = _nrows;
  int wsize = 4*size-2;
  double *workspace = (double *) getmem_atomic(sizeof(double) * wsize);
  double *rwork = (double *) getmem_atomic(sizeof(double) * (3*size-2));
  int info;

  eigenvectors->set_matrix(this);
  eigenvalues->resize(size,1);

  zheev_(&doit, &triangle, 
	 &size, eigenvectors->_array, 
	 &size, eigenvalues->_array,
	 workspace, &wsize, rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("zheev did not converge");
      return 0;
    }

  return eigenvectors;
}


LMatrixRROrNull * LMatrixCC::SVD(LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT)
{
  LMatrixCC *copythis = copy();
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = _nrows;
  int cols = _ncols;
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 4*min+2*max;
  double *workspace = (double *) getmem_atomic(sizeof(double) * wsize);
  double *rwork = (double *) getmem_atomic(sizeof(double) * 5 * min);

  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);
  
  zgesvd_(&doit, &doit, &rows, &cols, 
	  copythis->_array, &rows,
	  Sigma->_array, 
	  U->_array, &rows,
	  VT->_array, &cols,
	  workspace, &wsize, 
	  rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesvd had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("zgesvd did not converge");
      return 0;
    }
  
  return Sigma;
}

LMatrixRROrNull * LMatrixCC::SVD_divide_conquer(LMatrixRR *Sigma, 
						LMatrixCC *U, LMatrixCC *VT)
{
  LMatrixCC *copythis = copy();
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = _nrows;
  int cols = _ncols;
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 2*min*min + 4*min + 2*max;
  double *workspace = (double *) getmem_atomic(sizeof(double) * wsize);
  int *iworkspace = (int *) getmem_atomic(sizeof(int) * 8 * min);
  double *rwork = (double *) getmem_atomic(sizeof(double) * (5*min*min + 7*min));

  U->resize(rows,rows);
  VT->resize(cols,cols);
  Sigma->resize(min,1);

  zgesdd_(&doit, &rows, &cols, 
	  copythis->_array, &rows,
	  Sigma->_array, 
	  U->_array, &rows,
	  VT->_array, &cols,
	  workspace, &wsize, rwork, 
	  iworkspace, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesdd had an illegal value");
      return 0;
    }
  else if (info > 0) 
    {
      ERROR("zgesdd did not converge");
      return 0;
    }

  return Sigma;
}

LMatrixCCOrNull * LMatrixCC::least_squares(LMatrixCC *b, LMatrixCC *x)
{
  LMatrixCC *copythis = copy();
  LMatrixCC *copyb = b->copy();
  char job = 'N';
  int rows = _nrows;
  int cols = _ncols;
  int brows = b->nrows();
  int bcols = b->ncols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >=  max) ? bcols : max);
  double *workspace = (double *) getmem_atomic(sizeof(double) * 2 * wsize);

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
      copyloc = 2*j*cols;
      for (int i = 0; i < brows; i++) {
	copyb->_array[copyloc++] = b->_array[bloc++];
	copyb->_array[copyloc++] = b->_array[bloc++];
      }
    }
  }
  
  zgels_(&job, &rows, &cols, &bcols,
	 copythis->_array, &rows,
	 copyb->_array, &max,
	 workspace, &wsize, 
	 &info);

  if (info != 0)
    {
      ERROR("argument passed to zgels had an illegal value");
      return 0;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = 2*j*rows;
      for (int i = 0; i < cols; i++) {
	x->_array[xloc++] = copyb->_array[copyloc++];
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

LMatrixCCOrNull * LMatrixCC::least_squares_deficient(LMatrixCC *b, LMatrixCC *x)
{
  LMatrixCC *copythis = copy();
  LMatrixCC *copyb = b->copy();
  int rows = _nrows;
  int cols = _ncols;
  int brows = b->nrows();
  int bcols = b->ncols();
  double rcond = -1.0;
  int rank, info;
  int min = (rows < cols) ? rows : cols;
  int max = (rows > cols) ? rows : cols;
  int wsize = 2*min + ((bcols >  max) ? bcols : max);
  double *workspace = (double *) getmem_atomic(sizeof(double) * 2 * wsize);
  double *sing = (double *) getmem_atomic(sizeof(double) * min);
  double *rwork = (double *) getmem_atomic(sizeof(double) * 5 * min);

  if (brows != rows) {
    ERROR("expected compatible right hand side");
    return 0;
  }

  if (rows < cols) {
    copyb->resize(cols, bcols);
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = 2*j*cols;
      for (int i = 0; i < brows; i++) {
	copyb->_array[copyloc++] = b->_array[bloc++];
	copyb->_array[copyloc++] = b->_array[bloc++];
      }
    }
  }
  
  zgelss_(&rows, &cols, &bcols,
	  copythis->_array, &rows,
	  copyb->_array, &max,
	  sing, &rcond, &rank,
	  workspace, &wsize, 
	  rwork, &info);

  if (info != 0)
    {
      ERROR("argument passed to zgelss had an illegal value");
      return 0;
    }

  if (rows > cols) {
    x->resize(cols,bcols);
    int copyloc = 0;
    int xloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = 2*j*rows;
      for (int i = 0; i < cols; i++) {
	x->_array[xloc++] = copyb->_array[copyloc++];
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
