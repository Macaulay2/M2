#include "densematRR.hpp"
#include "coeffrings.hpp"
#include "dmat.hpp"
#include "lapack.h"

typedef DMat<CoefficientRingRR> LMatrixRR;

LMatrixRR* LU(LMatrixRR *M, LMatrixRR *P)
{
  int rows = M->n_rows();
  int cols = M->n_cols();
  int info;
  int min = (rows <= cols) ? rows : cols;
  int *permutation = newarray_atomic(int, min);

  P->initialize(rows, rows, 0);

  dgetrf_(&rows, &cols, M->get_array(),
	  &rows, permutation, &info);

  /* set the permutation matrix P */
  for (int row=1; row<=min; row++) {
    int targ = row;
    for (int i=1; i<=min; i++) {
      if (i == targ)
	targ = permutation[i-1];
      else if (permutation[i-1] == targ)
	targ = i;
    }
    P->set_entry(row-1, targ-1, 1.0);
  }

  if (info < 0)       
    {
      ERROR("argument passed to dgetrf had an illegal value");
      return 0;
    }
  else if (info > 0) {
    ERROR("Warning: matrix is singular according to dgetrf");
  }

  return P;
}

LMatrixRR * solve(LMatrixRR *A,
		  LMatrixRR *b, 
		  LMatrixRR *x)
{
#if 0
  LMatrixRR *copythis1 = A->copy(true /* dense */ );
  DenseMutableMatrixRR *copythis = copythis1->cast_to_DenseMutableMatrixRR();
#endif
  int size = A->n_rows();
  int bsize, info;
  int *permutation = newarray_atomic(int, size);

  /* make sure matrix is square */
  if (A->n_rows() != A->n_cols())
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
  x->initialize(b->n_rows(), A->n_cols(), b->get_array());

  dgesv_(&size, &bsize,
	 A->get_array(), 
	 &size, permutation, 
	 x->get_array(),
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
