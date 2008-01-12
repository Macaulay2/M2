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
  __mpfr_struct *vals = L->get_array();
  int loc = 0;
  for (int j=0; j<min; j++) {
    for (int i=0; i<rows; i++) {
      assert(vals < L->get_array() + L->n_rows() * L->n_cols());
      if (i > j) {
	mpfr_set_d(vals++,copyA[loc++], GMP_RNDN);
      } else if (i == j) {
	mpfr_set_si(vals++, 1, GMP_RNDN);
	loc++;
      } else {
	mpfr_set_si(vals++, 0, GMP_RNDN);
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
	mpfr_set_d(vals++, copyA[loc++], GMP_RNDN);
      } else {
	mpfr_set_si(vals++, 0, GMP_RNDN);
	loc ++;
      }
    }
    loc += (rows-min);;
  }

  for (int i=0; i<rows; i++) result->array[i] = i;
  for (int i=0; i<min; i++)
    {
      int thisloc = perm[i]-1;
      int tmp = result->array[thisloc];
      result->array[thisloc] = result->array[i];
      result->array[i] = tmp;
    }

#if 0
  /* MES: I think that this commented out code is FLAWED */
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
#endif

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
  __mpfr_struct *vals = x->get_array();
  long len = size*bsize;
  double *p = copyb;
  for (long i=0; i<len; i++)
    mpfr_set_d(vals++, *p++, GMP_RNDN);

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
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  bool ret = true;
  char dont = 'N';
  int wsize = 3*size;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  double *copyA = A->make_lapack_array();
  double *real = newarray_atomic(double,size); // real components of eigvals
  double *imag = newarray_atomic(double,size); // imaginary components

  dgeev_(&dont, &dont, 
	 &size, copyA, &size,
	 real, 
	 imag,
	 static_cast<double *>(0), &size,  /* left eigenvectors */
	 static_cast<double *>(0), &size,  /* right eigenvectors */
	 workspace, &wsize, &info);

  if (info < 0)       
    {
      ERROR("argument passed to dgeev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      M2_CCC_struct *elems = eigvals->get_array();
      for (int i = 0; i < size; i++) {
	mpfr_set_d(elems[i].re, real[i], GMP_RNDN);
	mpfr_set_d(elems[i].im, imag[i], GMP_RNDN);
      }
    }

  deletearray(copyA);
  deletearray(real);
  deletearray(imag);
  return ret;
#endif
}

bool Lapack::eigenvectors(const LMatrixRR *A,
			  LMatrixCC *eigvals,
			  LMatrixCC *eigvecs)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  bool ret = true;
  char dont = 'N';
  char doit = 'V';
  int wsize = 4*size;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  double *copyA = A->make_lapack_array();
  double *real = newarray_atomic(double,size);  // real components of eigvals
  double *imag = newarray_atomic(double,size); // imaginary components
  double *eigen = newarray_atomic(double,size*size); // eigvecs

  dgeev_(&dont, &doit, 
	 &size, copyA, &size,
	 real, 
	 imag,
	 static_cast<double *>(0), &size,  /* left eigvecs */
	 eigen, &size,  /* right eigvecs */
	 workspace, &wsize, &info);

  if (info < 0)       
    {
      ERROR("argument passed to dgeev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      ret = false;
    }
  else 
    {
      // Make the complex arrays of eigvals and eigvecs
      eigvals->resize(size, 1);
      eigvecs->resize(size, size);
      M2_CCC_struct *elems = eigvecs->get_array();
      for (int j = 0; j < size; j++) {
	mpfr_set_d(eigvals->get_array()[j].re, real[j], GMP_RNDN);
	mpfr_set_d(eigvals->get_array()[j].im, imag[j], GMP_RNDN);
	int loc = j*size;
	if (imag[j] == 0) {
	  for (int i = 0; i < size; i++)
	    mpfr_set_d(elems[loc+i].re, eigen[loc+i], GMP_RNDN);
	} else if (imag[j] > 0) {
	  for (int i = 0; i < size; i++) {
	    mpfr_set_d(elems[loc+i].re, eigen[loc+i], GMP_RNDN);
	    mpfr_set_d(elems[loc+i].im, eigen[loc+size+i], GMP_RNDN);
	    mpfr_set_d(elems[loc+size+i].re, eigen[loc+i], GMP_RNDN);
	    mpfr_set_d(elems[loc+size+i].im, -eigen[loc+size+i], GMP_RNDN);
	  }
	} 
      }
    }

  deletearray(copyA);
  deletearray(workspace);
  deletearray(real);
  deletearray(imag);
  deletearray(eigen);
  return ret;
#endif
}

bool Lapack::eigenvalues_symmetric(const LMatrixRR *A, LMatrixRR *eigvals)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }
  
  bool ret = true;
  char dont = 'N';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix. */
  int info;

  int wsize = 3*size-1;
  double *workspace = newarray_atomic(double, wsize);

  double *copyA = A->make_lapack_array();
  double *evals = newarray_atomic(double,size);

  dsyev_(&dont, &triangle, 
	 &size, copyA,
	 &size, evals,
	 workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("dsyev did not converge");
      ret = false;
    }
  else
    {
      // Copy eigenvalues back to eigvals
      eigvals->resize(size,1);
      eigvals->fill_from_lapack_array(evals);
    }

  deletearray(workspace);
  deletearray(copyA);
  deletearray(evals);

  return ret;
#endif
}

bool Lapack::eigenvectors_symmetric(const LMatrixRR *A,
				    LMatrixRR *eigvals, 
				    LMatrixRR *eigvecs)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  bool ret = true;
  char doit = 'V';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix */

  int wsize = 3*size-1;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  double *evecs = A->make_lapack_array();
  double *evals = newarray_atomic(double, size);

  dsyev_(&doit, &triangle, 
	 &size, evecs, 
	 &size, evals,
	 workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("dsyev did not converge");
      ret = false;
    }
  else
    {
      // Copy results to eigvals, eigvecs
      eigvecs->resize(size,size);
      eigvecs->fill_from_lapack_array(evecs);
      eigvals->resize(size,1);
      eigvals->fill_from_lapack_array(evals);
    }

  deletearray(workspace);
  deletearray(evecs);
  deletearray(evals);

  return ret;
#endif
}

bool Lapack::SVD(const LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = (3*min+max >= 5*min) ? 3*min+max : 5*min;
  double *workspace = newarray_atomic(double, wsize);

  double *copyA = A->make_lapack_array();
  double *u = newarray_atomic(double, rows*rows);
  double *vt = newarray_atomic(double, cols*cols);
  double *sigma = newarray_atomic(double, min);

  
  dgesvd_(&doit, &doit, &rows, &cols, 
	  copyA, &rows,
	  sigma, 
	  u, &rows,
	  vt, &cols,
	  workspace, &wsize, 
	  &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesvd had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("dgesvd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows,rows);
      VT->resize(cols,cols);
      Sigma->resize(min,1);
      U->fill_from_lapack_array(u);
      VT->fill_from_lapack_array(vt);
      Sigma->fill_from_lapack_array(sigma);
    }

  deletearray(workspace);
  deletearray(copyA);
  deletearray(u);
  deletearray(vt);
  deletearray(sigma);

  return ret;
#endif
}

bool Lapack::SVD_divide_conquer(const LMatrixRR *A, LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 4*min*min + max + 9*min;
  double *workspace = newarray_atomic(double,wsize);
  int *iworkspace = newarray_atomic(int, 8*min);

  double *copyA = A->make_lapack_array();
  double *u = newarray_atomic(double, rows*rows);
  double *vt = newarray_atomic(double, cols*cols);
  double *sigma = newarray_atomic(double, min);

  dgesdd_(&doit, &rows, &cols, 
	  copyA, &rows,
	  sigma, 
	  u, &rows,
	  vt, &cols,
	  workspace, &wsize, 
	  iworkspace, &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesdd had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("dgesdd did not converge");
      ret = false;
    }
  else 
    {
      U->resize(rows,rows);
      VT->resize(cols,cols);
      Sigma->resize(min,1);
      U->fill_from_lapack_array(u);
      VT->fill_from_lapack_array(vt);
      Sigma->fill_from_lapack_array(sigma);
    }

  deletearray(workspace);
  deletearray(iworkspace);
  deletearray(copyA);
  deletearray(u);
  deletearray(vt);
  deletearray(sigma);

  return true;
#endif
}

bool Lapack::least_squares(const LMatrixRR *A, const LMatrixRR *b, LMatrixRR *x)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  bool ret = true;
  char job = 'N';
  int info;
  int rows = A->n_rows();
  int cols = A->n_cols();
  int brows = b->n_rows();
  int bcols = b->n_cols();
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >=  max) ? bcols : max);

  if (brows != rows) {
    ERROR("expected compatible right hand side");
    return false;
  }

  double *copyA = A->make_lapack_array();
  double *copyb = b->make_lapack_array();
  double *workspace = newarray_atomic(double, wsize);

  if (rows < cols) {
    // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at the bottom
    double *copyb2 = newarray_atomic_clear(double, cols*bcols);
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*cols;
      for (int i = 0; i < brows; i++) {
	copyb2[copyloc++] = copyb[bloc++];
      }
    }
    deletearray(copyb);
    copyb = copyb2;
  }
  
  dgels_(&job, &rows, &cols, &bcols,
	 copyA, &rows,
	 copyb, &max,
	 workspace, &wsize, 
	 &info);

  if (info != 0)
    {
      ERROR("argument passed to dgels had an illegal value");
      ret = false;
    }
  else
    {
      if (rows > cols) {
	x->resize(cols,bcols);
	int copyloc = 0;
	int xloc = 0;
	for (int j = 0; j < bcols; j++) {
	  copyloc = j*rows;
	  for (int i = 0; i < cols; i++) {
	    mpfr_set_d(&(x->get_array()[xloc++]), copyb[copyloc++], GMP_RNDN);
	  }
	}
      } else {
	x->fill_from_lapack_array(copyb);
      }
    }

  deletearray(copyA);
  deletearray(copyb);
  deletearray(workspace);

  return ret;
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

  double *copyA = A->make_lapack_array();

  L->resize(rows, min);
  U->resize(min, cols);

  zgetrf_(&rows, &cols, copyA, 
	  &rows, perm, &info);

  if (info < 0)       
    {
      ERROR("argument passed to zgetrf had an illegal value");
      deletearray(result);
      result = NULL;
    }
  else
    {
      // set L
      M2_CCC_struct *elemsL = L->get_array();
      int loc = 0;
      for (int j=0; j<min; j++) {
	for (int i=0; i<rows; i++) {
	  M2_CCC_struct *val = elemsL++;
	  if (i > j) {
	    mpfr_set_d(val->re, copyA[loc], GMP_RNDN);
	    mpfr_set_d(val->im, copyA[loc+1], GMP_RNDN);
	  } else if (i == j) {
	    mpfr_set_si(val->re, 1, GMP_RNDN);
	    mpfr_set_si(val->im, 0, GMP_RNDN);
	  } else {
	    mpfr_set_si(val->re, 0, GMP_RNDN);
	    mpfr_set_si(val->im, 0, GMP_RNDN);
	  }
	  loc += 2;
	}
      }

      // set U
      M2_CCC_struct *elemsU = U->get_array();
      loc = 0;
      for (int j=0; j<cols; j++) {
	for (int i=0; i<min; i++) {
	  M2_CCC_struct *val = elemsU++;
	  if (i > j) {
	    mpfr_set_si(val->re, 0, GMP_RNDN);
	    mpfr_set_si(val->im, 0, GMP_RNDN);
	  } else {
	    mpfr_set_d(val->re, copyA[loc], GMP_RNDN);
	    mpfr_set_d(val->im, copyA[loc+1], GMP_RNDN);
	  }
	  loc += 2;
	}
	loc += 2*(rows-min);
      }

      for (int i=0; i<rows; i++) result->array[i] = i;
      for (int i=0; i<min; i++)
	{
	  int thisloc = perm[i]-1;
	  int tmp = result->array[thisloc];
	  result->array[thisloc] = result->array[i];
	  result->array[i] = tmp;
	}

#if 0      
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
#endif
    }

  deletearray(perm);
  deletearray(copyA);

  return result;
#endif
}

bool Lapack::solve(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else

  bool ret = true;
  int size = A->n_rows();
  int bsize, info;

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

  int *permutation = newarray_atomic(int, size);
  double *copyA = A->make_lapack_array();
  double *copyb = b->make_lapack_array();

  bsize = b->n_cols();

  zgesv_(&size, &bsize,
	 copyA,
	 &size, permutation, 
	 copyb,
	 &size, &info);

  if (info > 0)       
    {
      ERROR("according to zgesv, matrix is singular");
      ret = false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to zgesv had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(size,bsize);
      x->fill_from_lapack_array(copyb);
    }

  deletearray(permutation);
  deletearray(copyA);
  deletearray(copyb);

  return ret;
#endif
}

bool Lapack::eigenvalues(const LMatrixCC *A, LMatrixCC *eigvals)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  bool ret = true;
  char dont = 'N';
  int info;
  int wsize = 4*size;
  int rsize = 2*size;
  double *workspace = newarray_atomic(double, wsize);
  double *rwork = newarray_atomic(double, rsize);

  double *copyA = A->make_lapack_array();
  double *evals = newarray_atomic(double,2*size);

  zgeev_(&dont, &dont, 
	 &size, copyA,
	 &size, evals,
	 static_cast<double *>(0), &size,  /* left eigenvectors */
	 static_cast<double *>(0), &size,  /* right eigenvectors */
	 workspace, &wsize, rwork,
	 &info);

  if (info < 0)       
    {
      ERROR("argument passed to zgeev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size,1);
      eigvals->fill_from_lapack_array(evals);
    }

  deletearray(copyA);
  deletearray(evals);
  deletearray(workspace);
  deletearray(rwork);

  return ret;
#endif
}

bool Lapack::eigenvectors(const LMatrixCC *A, LMatrixCC *eigvals, LMatrixCC *eigvecs)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size !=A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  bool ret = true;
  char dont = 'N';
  char doit = 'V';
  int wsize = 4*size;
  int rsize = 2*size;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,rsize);
  int info;

  double *copyA = A->make_lapack_array();
  double *evals = newarray_atomic(double,2*size);
  double *evecs = newarray_atomic(double,2*size*size);

  zgeev_(&dont, &doit, 
	 &size, copyA,
	 &size, evals,
	 static_cast<double *>(0), &size,  /* left eigvecs */
	 evecs, &size,  /* right eigvecs */
	 workspace, &wsize, rwork,
	 &info);

  if (info < 0)       
    {
      ERROR("argument passed to zgeev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size,1);
      eigvals->fill_from_lapack_array(evals);
      eigvecs->resize(size,size);
      eigvecs->fill_from_lapack_array(evecs);
    }

  deletearray(copyA);
  deletearray(evals);
  deletearray(evecs);
  deletearray(workspace);
  deletearray(rwork);

  return ret;
#endif
}

bool Lapack::eigenvalues_hermitian(const LMatrixCC *A, LMatrixRR *eigvals)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  bool ret = true;
  char dont = 'N';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix. */

  int wsize = 4*size-2;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,3*size-2);
  int info;

  double *copyA = A->make_lapack_array();
  double *evals = newarray_atomic(double,size);

  zheev_(&dont, &triangle, 
	 &size, copyA,
	 &size, evals,
	 workspace, &wsize, rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("zheev did not converge");
      ret = false;
    }
  else
    {
      eigvals->resize(size,1);
      eigvals->fill_from_lapack_array(evals);
    }

  deletearray(copyA);
  deletearray(evals);
  deletearray(workspace);
  deletearray(rwork);

  return ret;
#endif
}

bool Lapack::eigenvectors_hermitian(const LMatrixCC *A, LMatrixRR *eigvals, LMatrixCC *eigvecs)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  int size = A->n_rows();
  if (size != A->n_cols()) {
    ERROR("expected a square matrix");
    return false;
  }

  bool ret = true;
  char doit = 'V';
  char triangle = 'U';  /* Upper triangular part makes symmetric matrix */

  int wsize = 4*size-2;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,3*size-2);
  int info;

  double *evecs = A->make_lapack_array();
  double *evals = newarray_atomic(double,size);


  zheev_(&doit, &triangle, 
	 &size, evecs,
	 &size, evals,
	 workspace, &wsize, rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("zheev did not converge");
      ret = false;
    }
  else
    {
      eigvals->resize(size,1);
      eigvals->fill_from_lapack_array(evals);
      eigvecs->resize(size,size);
      eigvecs->fill_from_lapack_array(evecs);
    }

  deletearray(evals);
  deletearray(evecs);
  deletearray(workspace);
  deletearray(rwork);

  return ret;
#endif
}

bool Lapack::SVD(const LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = A->n_rows();
  int cols = A->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = 4*min+2*max;
  double *workspace = newarray_atomic(double,wsize);
  double *rwork = newarray_atomic(double,5*min);

  double *copyA = A->make_lapack_array();
  double *u = newarray_atomic(double,2*rows*rows);
  double *vt = newarray_atomic(double,2*cols*cols);
  double *sigma = newarray_atomic(double,2*min);
  
  zgesvd_(&doit, &doit, &rows, &cols, 
	  copyA, &rows,
	  sigma, 
	  u, &rows,
	  vt, &cols,
	  workspace, &wsize, 
	  rwork, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesvd had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("zgesvd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows,rows);
      U->fill_from_lapack_array(u);
      VT->resize(cols,cols);
      VT->fill_from_lapack_array(vt);
      Sigma->resize(min,1);
      Sigma->fill_from_lapack_array(sigma);
    }

  deletearray(workspace);
  deletearray(rwork);
  deletearray(copyA);
  deletearray(u);
  deletearray(vt);
  deletearray(sigma);

  return ret;
#endif
}

bool Lapack::SVD_divide_conquer(const LMatrixCC *A, LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  bool ret = true;
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

  double *copyA = A->make_lapack_array();
  double *u = newarray_atomic(double,2*rows*rows);
  double *vt = newarray_atomic(double,2*cols*cols);
  double *sigma = newarray_atomic(double,2*min);
  
  zgesdd_(&doit, &rows, &cols, 
	  copyA, &rows,
	  sigma, 
	  u, &rows,
	  vt, &cols,
	  workspace, &wsize, rwork, 
	  iworkspace, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesdd had an illegal value");
      ret = false;
    }
  else if (info > 0) 
    {
      ERROR("zgesdd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows,rows);
      U->fill_from_lapack_array(u);
      VT->resize(cols,cols);
      VT->fill_from_lapack_array(vt);
      Sigma->resize(min,1);
      Sigma->fill_from_lapack_array(sigma);
    }

  deletearray(workspace);
  deletearray(iworkspace);
  deletearray(rwork);
  deletearray(copyA);
  deletearray(u);
  deletearray(vt);
  deletearray(sigma);

  return ret;
#endif
}

bool Lapack::least_squares(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x)
{
#if !LAPACK
  ERROR("lapack not present");
  return false;
#else
  bool ret = true;
  char job = 'N';
  int info;
  int rows = A->n_rows();
  int cols = A->n_cols();
  int brows = b->n_rows();
  int bcols = b->n_cols();
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >=  max) ? bcols : max);

  if (brows != rows) {
    ERROR("expected compatible right hand side");
    return false;
  }

  double *copyA = A->make_lapack_array();
  double *copyb = b->make_lapack_array();
  double *workspace = newarray_atomic(double, 2*wsize);

#warning "the copy logic might be wrong"
  if (rows < cols) {
    // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at the bottom
    double *copyb2 = newarray_atomic_clear(double, 2*cols*bcols);
    int bloc = 0;
    int copyloc = 0;
    for (int j = 0; j < bcols; j++) {
      copyloc = j*cols;
      for (int i = 0; i < 2*brows; i++) {
	copyb2[copyloc++] = copyb[bloc++];
      }
    }
    deletearray(copyb);
    copyb = copyb2;
  }

  zgels_(&job, &rows, &cols, &bcols,
	 copyA, &rows,
	 copyb, &max,
	 workspace, &wsize, 
	 &info);

  if (info != 0)
    {
      ERROR("argument passed to zgels had an illegal value");
      ret = false;
    }
  else
    {
#warning "the copy logic might be wrong"
      if (rows > cols) {
	x->resize(cols,bcols);
	int copyloc = 0;
	int xloc = 0;
	for (int j = 0; j < bcols; j++) {
	  copyloc = j*rows;
	  for (int i = 0; i < cols; i++) {
	    mpfr_set_d((x->get_array()[xloc]).re, copyb[copyloc++], GMP_RNDN);
	    mpfr_set_d((x->get_array()[xloc++]).im, copyb[copyloc++], GMP_RNDN);
	  }
	}
      } else {
	x->fill_from_lapack_array(copyb);
	/*
	  x->resize(cols,bcols);
	  for (int j = 0; j < bcols*cols; j++) {
	  x->_array[j] = copyb->_array[j];
	  }
	*/
      }
    }

  deletearray(copyA);
  deletearray(copyb);
  deletearray(workspace);

  return ret;
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
