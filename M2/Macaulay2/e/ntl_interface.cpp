// Copyright 2005, Michael Stillman

#include "ntl_interface.hpp"
#include "sparsemat.hpp"
#include <NTL/ZZ.h>
#include <NTL/mat_ZZ.h>
#include <NTL/LLL.h>

class mat_ZZ;
mat_ZZ *mutableMatrix_to_NTL_mat_ZZ(const MutableMatrix *M)
{
  // Creates the TRANSPOSE of M

  // We asume or check that the ring is ZZ
  const SparseMutableMatrix *A = M->cast_to_SparseMutableMatrix();
  int ncols = A->n_rows();
  int nrows = A->n_cols();

  mat_ZZ *X = makeNTLMatrixZZ(nrows,ncols);
  for (int i=0; i<ncols; i++)
    for (int j=0; j<nrows; j++)
      {
	ring_elem a;
	if (A->get_entry(i,j,a))
	  {
	    mat_ZZ_set_entry(X,j,i,MPZ_VAL(a));
	  }
      }

  return X;
}
MutableMatrix *mutableMatrix_from_NTL_mat_ZZ(const mat_ZZ *A)
{
  // AGAIN: form the TRANSPOSE of A
  int ncols = A->NumRows();
  int nrows = A->NumCols();

  MutableMatrix *B = MutableMatrix::zero_matrix(globalZZ, nrows, ncols, false);

  mpz_t a;
  mpz_init(a);

  for (int i=0; i<ncols; i++)
    for (int j=0; j<nrows; j++)
      {
	if ((*A)(i+1,j+1) != 0)
	  {
	    mat_ZZ_get_entry(A,i,j,a);
	    B->set_entry(j,i,MPZ_RINGELEM(a));
	  }
      }

  return B;
}

bool ntl_LLL(const MutableMatrix *M, long numer, long denom, int strategy)
{
  int nrows = M->n_rows();
  int ncols = M->n_cols();

  ZZ d;

  mat_ZZ *A = mutableMatrix_to_NTL_mat_ZZ(M);
  long rk = LLL(d,*A,numer,denom);

  dntl_matZZ(A);
  /* Put this back into M */
  mpz_t a;
  mpz_init(a);
  
  for (int j=0; j<ncols; j++)
    for (int i=0; i<nrows; i++)
      {
	mat_ZZ_get_entry(A,j,i,a);
	ring_elem b = globalZZ->from_int(a);
	M->set_entry(i,j,b);
      }
  return true;
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

