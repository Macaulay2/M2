// Copyright 2005,2013  Michael E. Stillman

#include "exceptions.hpp"
#include "error.h"

#include "mat-linalg.hpp"

////////////////////////////////////////////////////////////////////////////
// dmat code that might have alternate implementations, depending of type //
////////////////////////////////////////////////////////////////////////////

namespace MatrixOps {
void addMultipleTo(DMatZZpFFPACK& C,
                   const DMatZZpFFPACK::ElementType& a,
                   const DMatZZpFFPACK& A,
                   const DMatZZpFFPACK& B)
{
  // Compute C := C + a*A*B
  // Both DMat, and FFPACK store dense matrices in row major order.
  // Note that the leading dimension in gemm arguments is #columns,
  // as the matrix is in row-major order

  FFLAS::FFLAS_TRANSPOSE tA = FFLAS::FflasNoTrans;
  FFLAS::FFLAS_TRANSPOSE tB = FFLAS::FflasNoTrans;

  size_t m = A.numRows();
  size_t k = A.numColumns();
  assert(A.numColumns() == B.numRows());
  size_t n = B.numColumns();

  assert(C.numRows() == m);
  assert(C.numColumns() == n);

  DMatZZpFFPACK::ElementType b;
  C.ring().init(b);
  C.ring().set_from_long(b, 1);
  FFLAS::fgemm(C.ring().field(),
               tB,
               tA,
               m,
               n,
               k,
               a,
               A.array(),
               A.numColumns(),
               B.array(),
               B.numColumns(),
               b,
               C.array(),
               C.numColumns());
}

void addMultipleTo(DMatZZpFFPACK& C,
                   const DMatZZpFFPACK& A,
                   const DMatZZpFFPACK& B)
{
  DMatZZpFFPACK::ElementType one;
  A.ring().set_from_long(one, 1);

  addMultipleTo(C, one, A, B);
}

void subtractMultipleTo(DMatZZpFFPACK& C,
                        const DMatZZpFFPACK& A,
                        const DMatZZpFFPACK& B)
{
  DMatZZpFFPACK::ElementType minus_one;
  A.ring().set_from_long(minus_one, -1);
  addMultipleTo(C, minus_one, A, B);
}

void mult(const DMatZZpFFPACK& A, const DMatZZpFFPACK& B, DMatZZpFFPACK& C)
{
  // We assume that C is set to the correct size, and is the zero matrix here.
  addMultipleTo(C, A, B);
}
};  // namespace MatrixOps

namespace ffpackInterface {
size_t rank(const DMatZZpFFPACK& mat)
{
  /// @note 1. matrix data (N) is modified by FFPACK
  DMatZZpFFPACK N(mat);  // copy of matrix mat.
  size_t result = FFPACK::Rank(mat.ring().field(),
                               mat.numRows(),
                               mat.numColumns(),
                               N.array(),
                               mat.numColumns());
  return result;
}

void determinant(const DMatZZpFFPACK& mat, ZZpFFPACK::ElementType& result_det)
{
  /// @note 1. matrix data (N) is modified by FFPACK
  if (mat.numRows() == 0)
    {
      // 26 April 2014: this branch is needed as FFPACK gives answer of 0 in
      // this case.
      mat.ring().set_from_long(result_det, 1);
    }
  else
    {
      DMatZZpFFPACK N(mat);
      ZZpFFPACK::ElementType det = 0;
      result_det = FFPACK::Det(mat.ring().field(),
                               det,
                               mat.numRows(),
                               N.array(),
                               mat.numColumns());
    }
}

bool inverse(const DMatZZpFFPACK& mat, DMatZZpFFPACK& result_inv)
{
  assert(mat.numRows() == mat.numColumns());
  result_inv.resize(mat.numRows(), mat.numRows());

  assert(result_inv.numRows() == mat.numRows());
  assert(result_inv.numColumns() == mat.numRows());

  if (mat.numRows() == 0)
    {
      // 26 April 2014: this branch is needed as FFPACK gives answer of 0 in
      // this case.
      return true;
    }

  DMatZZpFFPACK N(mat);
  size_t n = mat.numRows();
  int nullspacedim;
  FFPACK::Invert2(
      mat.ring().field(), n, N.array(), n, result_inv.array(), n, nullspacedim);
  return (nullspacedim == 0);
}

size_t nullSpace(const DMatZZpFFPACK& mat, DMatZZpFFPACK& nullspace)
{
  bool right_side = true;  // This function is written so that one could set
                           // right_side to false.
  // (It used to be a parameter).

  DMatZZpFFPACK N(mat);  // copy of mat
  size_t nr = mat.numRows();
  size_t nc = mat.numColumns();

  DMatZZpFFPACK::ElementType* nullspaceFFPACK = 0;

  size_t nullspace_dim;
  size_t nullspace_leading_dim;

  FFPACK::NullSpaceBasis(mat.ring().field(),
                         (right_side ? FFLAS::FflasRight : FFLAS::FflasLeft),
                         nr,
                         nc,
                         N.array(),
                         nc,
                         nullspaceFFPACK,
                         nullspace_leading_dim,
                         nullspace_dim);

  //    std::cerr << "leading dim = " << nullspace_leading_dim << " and dim = "
  //    << nullspace_dim << std::endl;
  if (right_side && nullspace_dim != nullspace_leading_dim)
    {
      std::cerr << "error: this should not happen!" << std::endl;
    }
  else if (!right_side && nullspace_leading_dim != nc)
    {
      std::cerr << "error: this should not happen either!" << std::endl;
    }

  if (right_side)
    nullspace.resize(nc, nullspace_dim);
  else
    nullspace.resize(nullspace_dim, nr);

  std::swap(nullspace.array(), nullspaceFFPACK);

  delete[] nullspaceFFPACK;
  return nullspace_dim;
}

bool solveLinear(const DMatZZpFFPACK& A,
                 const DMatZZpFFPACK& B,
                 bool right_side,
                 DMatZZpFFPACK& X,
                 bool declare_A_is_invertible)  // this parameter is unused
{
  //    std::cerr << "inside FFpackSolveLinear" << std::endl;

  size_t a_rows = A.numRows();
  size_t a_cols = A.numColumns();

  size_t b_rows = B.numRows();
  size_t b_cols = B.numColumns();

  DMatZZpFFPACK copyA(A);
  DMatZZpFFPACK copyB(B);

  // preallocate the space for the solutions:
  size_t x_rows = (right_side ? a_cols : b_rows);
  size_t x_cols = (right_side ? b_cols : a_rows);

  X.resize(x_rows, x_cols);  // sets it to 0 too.

  int info = 0;  // >0 if the system is inconsistent, ==0 means success

  FFPACK::fgesv(A.ring().field(),
                (right_side ? FFLAS::FflasLeft : FFLAS::FflasRight),
                a_rows,
                a_cols,
                (right_side ? b_cols : b_rows),
                copyA.array(),
                a_cols,  // leading dim of A
                X.array(),
                x_cols,
                copyB.array(),
                b_cols,
                &info);

  if (info > 0)
    {
      // the system is inconsistent
      return false;
    }

  return true;
}

bool solveLinear(const DMatZZpFFPACK& A,
                 const DMatZZpFFPACK& B,
                 DMatZZpFFPACK& X)
{
  return solveLinear(A, B, true, X, false);
}

M2_arrayintOrNull rankProfile(const DMatZZpFFPACK& mat, bool row_profile)

{
  DMatZZpFFPACK N(mat);

  size_t* prof;  // this is where the result will be placed
  size_t rk;
  if (row_profile)
    rk = FFPACK::RowRankProfile(mat.ring().field(),
                                mat.numRows(),
                                mat.numColumns(),
                                N.array(),
                                mat.numColumns(),
                                prof);
  else
    rk = FFPACK::ColumnRankProfile(mat.ring().field(),
                                   mat.numRows(),
                                   mat.numColumns(),
                                   N.array(),
                                   mat.numColumns(),
                                   prof);

  M2_arrayint profile = M2_makearrayint(static_cast<int>(rk));
  for (size_t i = 0; i < rk; i++) profile->array[i] = static_cast<int>(prof[i]);

  delete[] prof;
  return profile;
}

void rankProfile(const DMatZZpFFPACK& mat,
                 bool row_profile,
                 std::vector<size_t>& result_profile)

{
  DMatZZpFFPACK N(mat);

  size_t* prof;  // this is where the result will be placed
  size_t rk;
  if (row_profile)
    rk = FFPACK::RowRankProfile(mat.ring().field(),
                                mat.numRows(),
                                mat.numColumns(),
                                N.array(),
                                mat.numColumns(),
                                prof);
  else
    rk = FFPACK::ColumnRankProfile(mat.ring().field(),
                                   mat.numRows(),
                                   mat.numColumns(),
                                   N.array(),
                                   mat.numColumns(),
                                   prof);

  result_profile.resize(0);
  for (size_t i = 0; i < rk; i++) result_profile.push_back(prof[i]);

  delete[] prof;
}

};  // namespace ffpackInterface

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
