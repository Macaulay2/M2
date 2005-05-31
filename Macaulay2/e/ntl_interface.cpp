// Copyright 2005, Michael Stillman

#include "ntl_interface.hpp"
#include "mat.hpp"
//#include "sparsemat.hpp"
#include <NTL/ZZ.h>
#include <NTL/mat_ZZ.h>
#include <NTL/LLL.h>

class mat_ZZ;
mat_ZZ *mutableMatrix_to_NTL_mat_ZZ(const MutableMatrixXXX *M)
{
  // Creates the TRANSPOSE of M

  // We asume or check that the ring is ZZ

  //  const SparseMutableMatrix *A = M->cast_to_SparseMutableMatrix();
  const MutableMatrixXXX *A = M;

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
MutableMatrixXXX *mutableMatrix_from_NTL_mat_ZZ(const mat_ZZ *A)
{
  // AGAIN: form the TRANSPOSE of A
  int ncols = A->NumRows();
  int nrows = A->NumCols();

  MutableMatrixXXX *B = MutableMatrixXXX::zero_matrix(globalZZ, nrows, ncols, false);

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

static const int GS = 0;
static const int Givens = 4;
static const int useLLL = 0;
static const int useBKZ = 8;
static const int FP = 0;
static const int QP1 = 16;
static const int QP = 2*16;
static const int XD = 3*16;
static const int RR = 4*16;

bool ntl_LLL(MutableMatrixXXX *M, long numer, long denom, int strategy)
{
  int nrows = M->n_rows();
  int ncols = M->n_cols();

  ZZ d;
  long rk;
  double delta = numer*1.0/denom;

  mat_ZZ *A = mutableMatrix_to_NTL_mat_ZZ(M);

  switch (strategy) {
  case 2:
    rk = LLL(d,*A,numer,denom);
    break;

  case 3+GS+useLLL+FP:
    rk = LLL_FP(*A,delta);
    break;
  case 3+GS+useLLL+QP:
  case 3+GS+useLLL+QP1:
    rk = LLL_QP(*A,delta);
    break;
  case 3+GS+useLLL+XD:
    rk = LLL_XD(*A,delta);
    break;
  case 3+GS+useLLL+RR:
    rk = LLL_RR(*A,delta);
    break;

  case 3+GS+useBKZ+FP:
    rk = BKZ_FP(*A,delta);
    break;
  case 3+GS+useBKZ+QP:
    rk = BKZ_QP(*A,delta);
    break;
  case 3+GS+useBKZ+QP1:
    rk = BKZ_QP1(*A,delta);
    break;
  case 3+GS+useBKZ+XD:
    rk = BKZ_XD(*A,delta);
    break;
  case 3+GS+useBKZ+RR:
    rk = BKZ_RR(*A,delta);
    break;

  case 3+Givens+useLLL+FP:
    rk = G_LLL_FP(*A,delta);
    break;
  case 3+Givens+useLLL+QP:
  case 3+Givens+useLLL+QP1:
    rk = G_LLL_QP(*A,delta);
    break;
  case 3+Givens+useLLL+XD:
    rk = G_LLL_XD(*A,delta);
    break;
  case 3+Givens+useLLL+RR:
    rk = G_LLL_RR(*A,delta);
    break;

  case 3+Givens+useBKZ+FP:
    rk = G_BKZ_FP(*A,delta);
    break;
  case 3+Givens+useBKZ+QP:
    rk = G_BKZ_QP(*A,delta);
    break;
  case 3+Givens+useBKZ+QP1:
    rk = G_BKZ_QP1(*A,delta);
    break;
  case 3+Givens+useBKZ+XD:
    rk = G_BKZ_XD(*A,delta);
    break;
  case 3+Givens+useBKZ+RR:
    rk = G_BKZ_RR(*A,delta);
    break;
  }

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

