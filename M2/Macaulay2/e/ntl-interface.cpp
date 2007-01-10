// Copyright 2005, Michael Stillman

#include "ntl-interface.hpp"
#include "mat.hpp"

mat_ZZ *mutableMatrix_to_NTL_mat_ZZ(const MutableMatrix *M)
{
  // Creates the TRANSPOSE of M

  // We asume or check that the ring is ZZ

  //  const SparseMutableMatrix *A = M->cast_to_SparseMutableMatrix();
  const MutableMatrix *A = M;

  int ncols = A->n_rows();
  int nrows = A->n_cols();

  mat_ZZ *X = makeNTLMatrixZZ(nrows,ncols);
  for (int i=0; i<ncols; i++)
    for (int j=0; j<nrows; j++)
      {
	ring_elem a;
	if (A->get_entry(i,j,a))
	  {
	    mat_ZZ_set_entry(X,j,i,a.get_mpz());
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
	    B->set_entry(j,i,ring_elem(a));
	  }
      }

  return B;
}

static const int useNTL = 2;
static const int GS = 0;
static const int Givens = 4;
static const int useLLL = 0;
static const int useBKZ = 8;
static const int FP = 16;
static const int QP1 = 2*16;
static const int QP = 3*16;
static const int XD = 4*16;
static const int useRR = 5*16;

bool ntl_LLL(MutableMatrix *M, MutableMatrix *U, long numer, long denom, int strategy)
{
  int nrows = M->n_rows();
  int ncols = M->n_cols();

  ZZ d;
  long rk;
  double delta = numer*1.0/denom;

  if (gbTrace >= 10)
    printf("LLL: using strategy %d\n", strategy);
  mat_ZZ *A = mutableMatrix_to_NTL_mat_ZZ(M);
  mat_ZZ *V = (U ? mutableMatrix_to_NTL_mat_ZZ(U) : 0);
    
  switch (strategy) {
  case 2:
    if (!V)
      rk = LLL(d,*A,numer,denom);
    else
      rk = LLL(d,*A,*V,numer,denom);
    break;

  case useNTL+GS+useLLL+FP:
    if (!V)
      rk = LLL_FP(*A,delta);
    else
      rk = LLL_FP(*A,*V,delta);
    break;
  case useNTL+GS+useLLL+QP:
  case useNTL+GS+useLLL+QP1:
    if (!V)
      rk = LLL_QP(*A,delta);
    else
      rk = LLL_QP(*A,*V,delta);
    break;
  case useNTL+GS+useLLL+XD:
    if (!V)
      rk = LLL_XD(*A,delta);
    else
      rk = LLL_XD(*A,*V,delta);
    break;
  case useNTL+GS+useLLL+useRR:
    if (!V)
      rk = LLL_RR(*A,delta);
    else
      rk = LLL_RR(*A,*V,delta);
    break;

  case useNTL+GS+useBKZ+FP:
    if (!V)
      rk = BKZ_FP(*A,delta);
    else
      rk = BKZ_FP(*A,*V,delta);
    break;
  case useNTL+GS+useBKZ+QP:
    if (!V)
      rk = BKZ_QP(*A,delta);
    else
      rk = BKZ_QP(*A,*V,delta);
    break;
  case useNTL+GS+useBKZ+QP1:
    if (!V)
      rk = BKZ_QP1(*A,delta);
    else
      rk = BKZ_QP1(*A,*V,delta);
    break;
  case useNTL+GS+useBKZ+XD:
    if (!V)
      rk = BKZ_XD(*A,delta);
    else
      rk = BKZ_XD(*A,*V,delta);
    break;
  case useNTL+GS+useBKZ+useRR:
    if (!V)
      rk = BKZ_RR(*A,delta);
    else
      rk = BKZ_RR(*A,*V,delta);
    break;

  case useNTL+Givens+useLLL+FP:
    if (!V)
      rk = G_LLL_FP(*A,delta);
    else
      rk = G_LLL_FP(*A,*V,delta);
    break;
  case useNTL+Givens+useLLL+QP:
  case useNTL+Givens+useLLL+QP1:
    if (!V)
      rk = G_LLL_QP(*A,delta);
    else
      rk = G_LLL_QP(*A,*V,delta);
    break;
  case useNTL+Givens+useLLL+XD:
    if (!V)
      rk = G_LLL_XD(*A,delta);
    else
      rk = G_LLL_XD(*A,*V,delta);
    break;
  case useNTL+Givens+useLLL+useRR:
    if (!V)
      rk = G_LLL_RR(*A,delta);
    else
      rk = G_LLL_RR(*A,*V,delta);
    break;

  case useNTL+Givens+useBKZ+FP:
    if (!V)
      rk = G_BKZ_FP(*A,delta);
    else
      rk = G_BKZ_FP(*A,*V,delta);
    break;
  case useNTL+Givens+useBKZ+QP:
    if (!V)
      rk = G_BKZ_QP(*A,delta);
    else
      rk = G_BKZ_QP(*A,*V,delta);
    break;
  case useNTL+Givens+useBKZ+QP1:
    if (!V)
      rk = G_BKZ_QP1(*A,delta);
    else
      rk = G_BKZ_QP1(*A,*V,delta);
    break;
  case useNTL+Givens+useBKZ+XD:
    if (!V)
      rk = G_BKZ_XD(*A,delta);
    else
      rk = G_BKZ_XD(*A,*V,delta);
    break;
  case useNTL+Givens+useBKZ+useRR:
    if (!V)
      rk = G_BKZ_RR(*A,delta);
    else
      rk = G_BKZ_RR(*A,*V,delta);
    break;
  default:
    delete A;
    if (V) delete V;
    ERROR("Strategy option to LLL not understood");
    return false;
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

  if (U)
    {
      for (int j=0; j<ncols; j++)
	for (int i=0; i<ncols; i++)
	  {
	    mat_ZZ_get_entry(V,j,i,a);
	    ring_elem b = globalZZ->from_int(a);
	    U->set_entry(i,j,b);
	  }
    }
  delete A;
  if (V) delete V;
  return true;
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

