// Copyright 2005, Michael Stillman

#include "ntl-interface.hpp"
#include "mat.hpp"

NTL::mat_ZZ *mutableMatrix_to_NTL_mat_ZZ(const MutableMatrix *M)
{
  // Creates the TRANSPOSE of M

  // We assume or check that the ring is ZZ

  //  const SparseMutableMatrix *A = M->cast_to_SparseMutableMatrix();
  const MutableMatrix *A = M;

  int ncols = static_cast<int>(A->n_rows());
  int nrows = static_cast<int>(A->n_cols());

  NTL::mat_ZZ *X = makeNTLMatrixZZ(nrows, ncols);
  for (int i = 0; i < ncols; i++)
    for (int j = 0; j < nrows; j++)
      {
        ring_elem a;
        if (A->get_entry(i, j, a))
          {
            mat_ZZ_set_entry(X, j, i, a.get_mpz());
          }
      }

  return X;
}
MutableMatrix *mutableMatrix_from_NTL_mat_ZZ(const NTL::mat_ZZ *A)
{
  // AGAIN: form the TRANSPOSE of A
  size_t ncols = A->NumRows();
  size_t nrows = A->NumCols();
  MutableMatrix *B = MutableMatrix::zero_matrix(globalZZ, nrows, ncols, false);

  mpz_t a;
  mpz_init(a);

  for (size_t i = 0; i < ncols; i++)
    for (size_t j = 0; j < nrows; j++)
      {
        if ((*A)(i + 1, j + 1) != 0)
          {
            mat_ZZ_get_entry(A, i, j, a);
            B->set_entry(j, i, ring_elem(a));
          }
      }
  mpz_clear(a);
  return B;
}

#ifdef GS  // Unfortunate Solaris macro
#undef GS
#endif

static const int useNTL = 2;
static const int GS = 0;
static const int Givens = 4;
static const int useLLL = 0;
static const int useBKZ = 8;
static const int FP = 16;
static const int QP1 = 2 * 16;
static const int QP = 3 * 16;
static const int XD = 4 * 16;
static const int useRR = 5 * 16;

bool ntl_LLL(MutableMatrix *M,
             MutableMatrix *U,
             long numer,
             long denom,
             int strategy)
{
  int nrows = static_cast<int>(M->n_rows());
  int ncols = static_cast<int>(M->n_cols());

  NTL::ZZ d;
  // Note that the LLL routines all return the rank, but we ignore this return
  // value.
  double delta = static_cast<double>(numer) / static_cast<double>(denom);

  if (M2_gbTrace >= 10) printf("LLL: using strategy %d\n", strategy);
  NTL::mat_ZZ *A = mutableMatrix_to_NTL_mat_ZZ(M);
  NTL::mat_ZZ *V = (U ? mutableMatrix_to_NTL_mat_ZZ(U) : 0);

  switch (strategy)
    {
      case 2:
        if (!V)
          LLL(d, *A, numer, denom);
        else
          LLL(d, *A, *V, numer, denom);
        break;

      case useNTL + GS + useLLL + FP:
        if (!V)
          LLL_FP(*A, delta);
        else
          LLL_FP(*A, *V, delta);
        break;
      case useNTL + GS + useLLL + QP:
      case useNTL + GS + useLLL + QP1:
        if (!V)
          LLL_QP(*A, delta);
        else
          LLL_QP(*A, *V, delta);
        break;
      case useNTL + GS + useLLL + XD:
        if (!V)
          LLL_XD(*A, delta);
        else
          LLL_XD(*A, *V, delta);
        break;
      case useNTL + GS + useLLL + useRR:
        if (!V)
          LLL_RR(*A, delta);
        else
          LLL_RR(*A, *V, delta);
        break;

      case useNTL + GS + useBKZ + FP:
        if (!V)
          BKZ_FP(*A, delta);
        else
          BKZ_FP(*A, *V, delta);
        break;
      case useNTL + GS + useBKZ + QP:
        if (!V)
          BKZ_QP(*A, delta);
        else
          BKZ_QP(*A, *V, delta);
        break;
      case useNTL + GS + useBKZ + QP1:
        if (!V)
          BKZ_QP1(*A, delta);
        else
          BKZ_QP1(*A, *V, delta);
        break;
      case useNTL + GS + useBKZ + XD:
        if (!V)
          BKZ_XD(*A, delta);
        else
          BKZ_XD(*A, *V, delta);
        break;
      case useNTL + GS + useBKZ + useRR:
        if (!V)
          BKZ_RR(*A, delta);
        else
          BKZ_RR(*A, *V, delta);
        break;

      case useNTL + Givens + useLLL + FP:
        if (!V)
          G_LLL_FP(*A, delta);
        else
          G_LLL_FP(*A, *V, delta);
        break;
      case useNTL + Givens + useLLL + QP:
      case useNTL + Givens + useLLL + QP1:
        if (!V)
          G_LLL_QP(*A, delta);
        else
          G_LLL_QP(*A, *V, delta);
        break;
      case useNTL + Givens + useLLL + XD:
        if (!V)
          G_LLL_XD(*A, delta);
        else
          G_LLL_XD(*A, *V, delta);
        break;
      case useNTL + Givens + useLLL + useRR:
        if (!V)
          G_LLL_RR(*A, delta);
        else
          G_LLL_RR(*A, *V, delta);
        break;

      case useNTL + Givens + useBKZ + FP:
        if (!V)
          G_BKZ_FP(*A, delta);
        else
          G_BKZ_FP(*A, *V, delta);
        break;
      case useNTL + Givens + useBKZ + QP:
        if (!V)
          G_BKZ_QP(*A, delta);
        else
          G_BKZ_QP(*A, *V, delta);
        break;
      case useNTL + Givens + useBKZ + QP1:
        if (!V)
          G_BKZ_QP1(*A, delta);
        else
          G_BKZ_QP1(*A, *V, delta);
        break;
      case useNTL + Givens + useBKZ + XD:
        if (!V)
          G_BKZ_XD(*A, delta);
        else
          G_BKZ_XD(*A, *V, delta);
        break;
      case useNTL + Givens + useBKZ + useRR:
        if (!V)
          G_BKZ_RR(*A, delta);
        else
          G_BKZ_RR(*A, *V, delta);
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

  for (int j = 0; j < ncols; j++)
    for (int i = 0; i < nrows; i++)
      {
        mat_ZZ_get_entry(A, j, i, a);
        ring_elem b = globalZZ->from_int(a);
        M->set_entry(i, j, b);
      }

  if (U)
    {
      for (int j = 0; j < ncols; j++)
        for (int i = 0; i < ncols; i++)
          {
            mat_ZZ_get_entry(V, j, i, a);
            ring_elem b = globalZZ->from_int(a);
            U->set_entry(i, j, b);
          }
    }
  delete A;
  if (V) delete V;
  return true;
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
