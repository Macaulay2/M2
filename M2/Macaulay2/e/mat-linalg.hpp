// Copyright 2013  Michael E. Stillman

#ifndef _mat_linalg_hpp_
#define _mat_linalg_hpp_


/**
 * \ingroup matrices
 */

#include "util.hpp"
#include "exceptions.hpp"
#include "dmat.hpp"

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "aring-zzp.hpp"
#include "aring-m2-gf.hpp"
typedef DMat<M2::ARingZZp> DMatZZp;
typedef DMat<M2::ARingGFM2> DMatGFM2;

#include "aring-zzp-ffpack.hpp"
typedef M2::ARingZZpFFPACK ZZpFFPACK;
#define DMatZZpFFPACK DMat<ZZpFFPACK>

#include "aring-zz-gmp.hpp"
#include "aring-qq.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-gf-flint-big.hpp"
#include "aring-gf-flint.hpp"

typedef DMat<M2::ARingZZGMP> DMatZZGMP;
typedef DMat<M2::ARingZZ> DMatZZ;  // flint
typedef DMat<M2::ARingQQ> DMatQQ;
typedef DMat<M2::ARingQQFlint> DMatQQFlint;
typedef DMat<M2::ARingZZpFlint> DMatZZpFlint;
typedef DMat<M2::ARingGFFlintBig> DMatGFFlintBig;
typedef DMat<M2::ARingGFFlint> DMatGFFlint;

typedef DMat<M2::ARingRRR> DMatRRR;
typedef DMat<M2::ARingCCC> DMatCCC;
typedef DMat<M2::ARingRR> DMatRR;
typedef DMat<M2::ARingCC> DMatCC;

#include "lapack.hpp"
#include "mat-arith.hpp"
#include "dmat-lu.hpp"
#include "dmat-qq-interface-flint.hpp"

#include "eigen.hpp"

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fmpz_mat.h>
#pragma GCC diagnostic pop

#include <iostream>
#include <algorithm>

namespace MatrixOps {
/// @brief the rank of a matrix
///
/// throws an engine_error for ring/matrix types where the function is not
/// implemented.
/// This version is deterministic.
template <typename Mat>
size_t rank(const Mat& A)
{
  throw exc::engine_error(
      "'rank' not implemented for this kind of matrix over this ring");
  return 0;
}

/// @brief the determinant of a square matrix
///
/// result_det should be a previously initialized ElementType.
/// throws an engine_error for ring/matrix types where the function is not
/// implemented.
template <typename Mat>
void determinant(const Mat& A, typename Mat::ElementType& result_det)
{
  throw exc::engine_error(
      "'determinant' not implemented for this kind of matrix over this ring");
}

/// @brief the inverse of a square matrix
///
/// result_inv is set to the inverse of the square matrix A, if A is invertible.
/// result_inv should be a Mat, with the same ring/type as the input matrix A.
///   result_inv does not need to be the same size as A, it will be resized if
///   needed.
/// returns true exactly when the matrix is invertible, and result_inv has been
/// set.
///
/// throws an engine_error for ring/matrix types where the function is not
/// implemented.
/// throws an error if the matrix is not square.
///
/// Note: the inverse of a 0 x 0 matrix is another 0 x 0 matrix.
template <typename Mat>
bool inverse(const Mat& A, Mat& result_inv)
{
  throw exc::engine_error(
      "'invert' not implemented for this kind of matrix over this ring");
}

/// @brief the row reduced echelon form of a matrix over a field, or ZZ.
///
/// result_rref should be a Mat, with the same ring/type as the input matrix A.
///   result_rref does not need to be the same size as A, it will be resized if
///   needed.
/// returns the rank of A.
///
/// throws an engine_error for ring/matrix types where the function is not
/// implemented.
template <typename Mat>
size_t rowReducedEchelonForm(const Mat& A, Mat& result_rref)
{
  throw exc::engine_error(
      "'rowReducedEchelonForm' not implemented for this kind of matrix over "
      "this ring");
}

/// @brief the product of two matrices
///
/// result_product is set to the product A*B
/// result_product should be a Mat, with the same ring/type as the input
/// matrices A,B.
///   result_product does not need to be the same size as A*B, it will be
///   resized if needed.
///
/// throws an engine_error for ring/matrix types where the function is not
/// implemented.
/// throws an error if the number of columns of A is not the number of rows of
/// B.
/// result_prod should not be the same as A or B (assertion error).
template <typename Mat>
void mult(const Mat& A, const Mat& B, Mat& result_product)
{
  throw exc::engine_error(
      "'mult matrices' not implemented for this kind of matrix over this ring");
}

/// @brief the null space of a matrix
///
///   result_nullspace is set to the matrix whose columns form a basis for {x |
///   Ax = 0}.
/// Returns the dimension of the nullspace.
///
/// result_nullspace should be a Mat, with the same ring/type as the input
/// matrix A.
///   result_nullspace does not need to be the correct size, it will be resized
///   if needed.
///
/// throws an engine_error for ring/matrix types where the function is not
/// implemented.
template <typename Mat>
size_t nullSpace(const Mat& A, Mat& result_nullspace)
{
  throw exc::engine_error(
      "'nullSpace' not implemented for this kind of matrix over this ring");
}

/// @brief solve AX=B, return true if the system has a solution.
template <typename Mat>
bool solveLinear(const Mat& A, const Mat& B, Mat& X)
{
  throw exc::engine_error(
      "'solveLinear' not implemented for this kind of matrix over this ring");
}

/// @brief solve AX=B, where A is a square (invertible) matrix.
///
/// return true if A is invertible, and in this case, sets X.  If false is
/// returned, X's value is
/// undefined.
template <typename Mat>
bool solveInvertible(const Mat& A, const Mat& B, Mat& X)
{
  throw exc::engine_error(
      "'solveInvertible' not implemented for this kind of matrix over this "
      "ring");
}

/// @brief Returns either the row or column rank profile of A
///
/// if row_profile is true, then row profile is computed, otherwise
/// the column profile is computed.
///
/// The return value is an ascending sequence of non-negative integers
/// with an entry a occurring iff the submatrix of A of the first
/// (a-1) rows (resp columns) has lower rank than the submatrix of the
/// first a rows (resp columns).
///
/// Notice that if the matrix is non-zero and the first row is
/// non-zero, then the first entry will be 0.
template <typename Mat>
M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
{
  throw exc::engine_error(
      "'rankProfile' not implemented for this kind of matrix over this ring");
}

/// @brief Set C += A*B
///
/// Throws an exception if not yet implemented for this ring/matrix type.
/// The sizes of C,A,B must be compatible.  These are checked only via
/// assertions.
template <typename Mat>
void addMultipleTo(Mat& C, const Mat& A, const Mat& B)
// C = C + A*B
{
  throw exc::engine_error(
      "'addMultipleTo' not implemented for this kind of matrix over this ring");
}

/// @brief Set C -= A*B
///
/// Throws an exception if not yet implemented for this ring/matrix type.
/// The sizes of C,A,B must be compatible.  These are checked only via
/// assertions.
template <typename Mat>
void subtractMultipleTo(Mat& C, const Mat& A, const Mat& B)
// C = C - A*B
{
  throw exc::engine_error(
      "'subtractMultipleTo' not implemented for this kind of matrix over this "
      "ring");
}

template <typename Mat>
M2_arrayintOrNull LU(const Mat& A, Mat& L, Mat& U)
{
  throw exc::engine_error(
      "'LU' not implemented for this kind of matrix over this ring");
}

template <typename Mat>
M2_arrayintOrNull LUincremental(std::vector<size_t>& P, Mat& LU, const Mat& v, int i)
{
  throw exc::engine_error(
      "'LUincremental' not implemented for this kind of matrix over this ring");
}

template <typename Mat>
void triangularSolve(Mat& Lv, Mat& x, int m, int strategy)
{
  throw exc::engine_error(
      "'triangularSolve' not implemented for this kind of matrix over this "
      "ring");
}

template <typename Mat, typename Mat2>
bool eigenvalues(const Mat& A, Mat2& eigenvals)
{
  throw exc::engine_error(
      "'eigenvalues' not implemented for this kind of matrix over this ring");
}

template <typename Mat, typename Mat2>
bool eigenvaluesHermitian(const Mat& A, Mat2& eigenvals)
{
  throw exc::engine_error(
      "'eigenvalues' not implemented for this kind of matrix over this ring");
}

template <typename Mat, typename Mat2, typename Mat3>
bool eigenvectors(const Mat& A, Mat2& eigenvals, Mat3& eigenvecs)
{
  throw exc::engine_error(
      "'eigenvectors' not implemented for this kind of matrix over this ring");
}

template <typename Mat, typename Mat2, typename Mat3>
bool eigenvectorsHermitian(const Mat& A, Mat2& eigenvals, Mat3& eigenvecs)
{
  throw exc::engine_error(
      "'eigenvectors' not implemented for this kind of matrix over this ring");
}

template <typename Mat>
bool leastSquares(const Mat& A, const Mat& B, Mat& X, bool assume_full_rank)
{
  throw exc::engine_error(
      "'leastSquares' not implemented for this kind of matrix over this ring");
}

template <typename Mat, typename Mat2>
bool SVD(const Mat& A, Mat2& Sigma, Mat& U, Mat& Vt, int strategy)
{
  throw exc::engine_error(
      "'SVD' not implemented for this kind of matrix over this ring");
}

template <typename Mat, typename Mat2, typename Mat3>
bool QR(const Mat& A, Mat2& Q, Mat3& R, bool return_QR)
{
  throw exc::engine_error(
      "'QR' not implemented for this kind of matrix over this ring");
}

template <typename T>
void clean(gmp_RR epsilon, T& mat)
{
  throw exc::engine_error(
      "'clean' not implemented for this kind of matrix over this ring");
}

template <typename T>
void increase_norm(gmp_RRmutable nm, const T& mat)
{
  throw exc::engine_error(
      "'norm' not implemented for this kind of matrix over this ring");
}

/////////////////////////////////
// Generic functions for DMat ///
/////////////////////////////////

template <typename RT>
void mult(const DMat<RT>& A, const DMat<RT>& B, DMat<RT>& result_product)
{
  // printf("entering dmat mult\n");
  typedef typename RT::ElementType ElementType;
  typedef typename DMat<RT>::ConstIterator ConstIterator;

  assert(A.numColumns() == B.numRows());
  assert(A.numRows() == result_product.numRows());
  assert(B.numColumns() == result_product.numColumns());

  ElementType* result = result_product.array();

  ElementType tmp;
  A.ring().init(tmp);
  // WARNING: this routine expects the result matrix to be in ROW MAJOR ORDER
  for (size_t i = 0; i < A.numRows(); i++)
    for (size_t j = 0; j < B.numColumns(); j++)
      {
        ConstIterator i1 = A.rowBegin(i);
        ConstIterator iend = A.rowEnd(i);
        ConstIterator j1 = B.columnBegin(j);

        while (i1 != iend)
          {
            A.ring().mult(tmp, *i1, *j1);
            A.ring().add(*result, *result, tmp);
            ++i1;
            ++j1;
          }
        result++;
      }
  A.ring().clear(tmp);
}

template <typename RT>
void addMultipleTo(DMat<RT>& C, const DMat<RT>& A, const DMat<RT>& B)
// C = C + A*B
{
  mult(A, B, C);
}

template <typename RT>
void subtractMultipleTo(DMat<RT>& C, const DMat<RT>& A, const DMat<RT>& B)
// C = C - A*B
{
  typedef typename RT::ElementType ElementType;
  typedef typename DMat<RT>::ConstIterator ConstIterator;

  assert(A.numColumns() == B.numRows());
  assert(A.numRows() == C.numRows());
  assert(B.numColumns() == C.numColumns());

  ElementType* result = C.array();

  ElementType tmp;
  A.ring().init(tmp);
  // WARNING: this routine expects the result matrix to be in ROW MAJOR ORDER
  for (size_t i = 0; i < A.numRows(); i++)
    for (size_t j = 0; j < B.numColumns(); j++)
      {
        ConstIterator i1 = A.rowBegin(i);
        ConstIterator iend = A.rowEnd(i);
        ConstIterator j1 = B.columnBegin(j);

        while (i1 != iend)
          {
            A.ring().mult(tmp, *i1, *j1);
            A.ring().subtract(*result, *result, tmp);
            ++i1;
            ++j1;
          }
        result++;
      }
  A.ring().clear(tmp);
}

// Note: this default version only works for fields.  Any other rings
// MUST redefine these functions
template <typename RT>
inline void determinant(const DMat<RT>& A, typename RT::ElementType& result)
{
  DMatLinAlg<RT> LUdecomp(A);
  LUdecomp.determinant(result);
}

template <typename RT>
inline M2_arrayintOrNull LU(const DMat<RT>& A, DMat<RT>& L, DMat<RT>& U)
{
  std::vector<size_t> perm;
  DMatLinAlg<RT> LUdecomp(A);
  LUdecomp.matrixPLU(perm, L, U);
  return stdvector_to_M2_arrayint(perm);
}

/*
  Cases for strategy:
  00 lower triangular (forward substitution)
  01 lower triangular, assume 1 on diagonal
  10 upper triangular (backward substitution)
  11 upper triangular, assume 1 on diagonal
  Note: the rest of the matrix need not be 0 filled.
*/
template <typename RT>
void triangularSolve(DMat<RT>& Lv, DMat<RT>& x, int m, int strategy)
{
  // TODO: check rings match
  // TODO: no divide by zero
  // TODO: size of matrices
  // TODO: add tests
  // TODO: for size 0 also
  switch (strategy)
    {
      case 0:
        // TODO: change to iter
        for (size_t i = 0; i < m; i++)
          {
            auto& a = x.entry(i, 0);
            x.ring().divide(a, Lv.entry(i, m), Lv.entry(i, i));
            x.ring().negate(a, a);
            MatElementaryOps<DMat<RT>>::column_op(Lv, m, a, i);
            x.ring().negate(a, a);
          }
        break;
      case 1:
        // TODO: change to iter
        for (size_t i = 0; i < m; i++)
          {
            auto& a = x.entry(i, 0);
            x.ring().negate(a, Lv.entry(i, m));
            MatElementaryOps<DMat<RT>>::column_op(Lv, m, a, i);
            x.ring().negate(a, a);
          }
        break;
      case 2:
        // TODO: change to iter
        for (size_t i = 1; i < m + 1; i++)
          {
            auto& a = x.entry(m - i, 0);
            x.ring().divide(a, Lv.entry(m - i, m), Lv.entry(m - i, m - i));
            x.ring().negate(a, a);
            MatElementaryOps<DMat<RT>>::column_op(Lv, m, a, m - i);
            x.ring().negate(a, a);
          }
        break;
      case 3:
        // TODO: change to iter
        for (size_t i = 1; i < m + 1; i++)
          {
            auto& a = x.entry(m - i, 0);
            x.ring().negate(a, Lv.entry(m - i, m));
            MatElementaryOps<DMat<RT>>::column_op(Lv, m, a, m - i);
            x.ring().negate(a, a);
          }
        break;
    }
}

template <typename RT>
M2_arrayintOrNull LUincremental(std::vector<size_t>& P, DMat<RT>& LU, const DMat<RT>& v, int m)
{
  size_t n = LU.numRows();

  // copy permuted v to m-th column of LU
  // TODO: change to iter
  for (size_t j = 0; j < n; j++)
    LU.ring().set(LU.entry(j, m), v.entry(P[j], 0));

  // reduce the m-th column of LU and forward solve
  DMat<RT> x{LU.ring(), n, 1};
  triangularSolve(LU, x, m, 1);
  // place solution of forward solve in U
  // TODO: change to iter
  for (size_t i = 0; i < m; i++)
    LU.ring().set(LU.entry(i, m), x.entry(i, 0));

  // look for a pivot in L
  int pivotPosition = -1;
  // TODO: change to iter
  for (size_t j = m; j < n; j++)
    if (!LU.ring().is_zero(LU.entry(j, m)))
      {
        pivotPosition = j;
        break;
      }
  // if no pivot found, return
  if (pivotPosition == -1) return stdvector_to_M2_arrayint(P);
  // otherwise swap rows and update P
  MatElementaryOps<DMat<RT>>::interchange_rows(LU, pivotPosition, m);
  std::swap(P[pivotPosition], P[m]);

  // scale column of L
  // TODO: change to iter
  for (int j = m + 1; j < n; j++)
    LU.ring().divide(LU.entry(j, m), LU.entry(j, m), LU.entry(m, m));

  return stdvector_to_M2_arrayint(P);
}

template <typename RT>
inline size_t rank(const DMat<RT>& A)
{
  DMatLinAlg<RT> LUdecomp(A);
  return LUdecomp.rank();
}

template <typename RT>
inline M2_arrayintOrNull rankProfile(const DMat<RT>& A, bool row_profile)
{
  std::vector<size_t> profile;
  if (row_profile)
    {
      // First transpose A
      DMat<RT> B(A.ring(), A.numColumns(), A.numRows());
      MatrixOps::transpose(A, B);
      DMatLinAlg<RT> LUdecomp(B);
      LUdecomp.columnRankProfile(profile);
      return stdvector_to_M2_arrayint(profile);
    }
  else
    {
      DMatLinAlg<RT> LUdecomp(A);
      LUdecomp.columnRankProfile(profile);
      return stdvector_to_M2_arrayint(profile);
    }
}

template <typename RT>
inline bool inverse(const DMat<RT>& A, DMat<RT>& result_inv)
{
  DMatLinAlg<RT> LUdecomp(A);
  return LUdecomp.inverse(result_inv);
}

template <typename RT>
inline size_t nullSpace(const DMat<RT>& A, DMat<RT>& result_nullspace)
{
  DMatLinAlg<RT> LUdecomp(A);
  return LUdecomp.kernel(result_nullspace);
}

template <typename RT>
inline bool solveLinear(const DMat<RT>& A, const DMat<RT>& B, DMat<RT>& X)
{
  DMatLinAlg<RT> LUdecomp(A);
  return LUdecomp.solve(B, X);
}

template <typename RT>
inline bool solveInvertible(const DMat<RT>& A, const DMat<RT>& B, DMat<RT>& X)
{
  DMatLinAlg<RT> LUdecomp(A);
  return LUdecomp.solveInvertible(B, X);
}

////////////////////////
// ZZpFFPACK ///////////
////////////////////////
// namespace MatrixOps
void mult(const DMatZZpFFPACK& A,
          const DMatZZpFFPACK& B,
          DMatZZpFFPACK& result_product);

void addMultipleTo(DMatZZpFFPACK& C,
                   const DMatZZpFFPACK& A,
                   const DMatZZpFFPACK& B);

void subtractMultipleTo(DMatZZpFFPACK& C,
                        const DMatZZpFFPACK& A,
                        const DMatZZpFFPACK& B);

//////////////////////
// ZZ (ARingZZGMP) ///
//////////////////////

inline M2_arrayintOrNull LU(const DMatZZGMP& A, DMatZZGMP& L, DMatZZGMP& U)
{
  throw exc::engine_error(
      "'LU' not implemented for this kind of matrix over this ring");
}

inline M2_arrayintOrNull rankProfile(const DMatZZGMP& A, bool row_profile)
{
  throw exc::engine_error(
      "'rankProfile' not implemented for this kind of matrix over this ring");
}

inline bool inverse(const DMatZZGMP& A, DMatZZGMP& result_inv)
{
  throw exc::engine_error(
      "'invert' not implemented for this kind of matrix over this ring");
}

inline size_t nullSpace(const DMatZZGMP& A, DMatZZGMP& result_nullspace)
{
  throw exc::engine_error(
      "'nullSpace' not implemented for this kind of matrix over this ring");
}

inline bool solveLinear(const DMatZZGMP& A, const DMatZZGMP& B, DMatZZGMP& X)
{
  throw exc::engine_error(
      "'solveLinear' not implemented for this kind of matrix over this ring");
}

inline bool solveInvertible(const DMatZZGMP& A,
                            const DMatZZGMP& B,
                            DMatZZGMP& X)
{
  throw exc::engine_error(
      "'solveInvertible' not implemented for this kind of matrix over this "
      "ring");
}

inline void mult(const DMatZZGMP& A,
                 const DMatZZGMP& B,
                 DMatZZGMP& result_product)
{
  FlintZZMat A1(A);
  FlintZZMat B1(B);
  FlintZZMat result1(A.numRows(), B.numColumns());

  fmpz_mat_mul(result1.value(), A1.value(), B1.value());

  result1.toDMat(result_product);
}

inline void addMultipleTo(DMatZZGMP& C, const DMatZZGMP& A, const DMatZZGMP& B)
{
  FlintZZMat A1(A);
  FlintZZMat B1(B);
  FlintZZMat C1(C);
  FlintZZMat result1(A.numRows(), B.numColumns());

  FlintZZMat D1(A.numRows(), B.numColumns());
  fmpz_mat_mul(D1.value(), A1.value(), B1.value());
  fmpz_mat_add(C1.value(), C1.value(), D1.value());

  C1.toDMat(C);
}

inline void subtractMultipleTo(DMatZZGMP& C,
                               const DMatZZGMP& A,
                               const DMatZZGMP& B)
{
  FlintZZMat A1(A);
  FlintZZMat B1(B);
  FlintZZMat C1(C);
  FlintZZMat result1(A.numRows(), B.numColumns());

  FlintZZMat D1(A.numRows(), B.numColumns());
  fmpz_mat_mul(D1.value(), A1.value(), B1.value());
  fmpz_mat_sub(C1.value(), C1.value(), D1.value());

  C1.toDMat(C);
}

inline size_t rank(const DMatZZGMP& A)
{
  FlintZZMat A1(A);
  return fmpz_mat_rank(A1.value());
}

inline void determinant(const DMatZZGMP& A,
                        M2::ARingZZGMP::ElementType& result_det)
{
  FlintZZMat A1(A);
  fmpz_t det;
  fmpz_init(det);
  fmpz_mat_det(det, A1.value());
  fmpz_get_mpz(&result_det, det);
  fmpz_clear(det);
}

//////////////////////
// ZZFlint ///////////
//////////////////////
// Warning: nullSpace is WRONG, and needs to be rewritten,
// using an algorithm that will compute kernel over ZZ.

// Functions for DMatZZ

inline size_t rank(const DMatZZ& A) { return fmpz_mat_rank(A.fmpz_mat()); }
inline void determinant(const DMatZZ& A, M2::ARingZZ::ElementType& result_det)
{
  fmpz_mat_det(&result_det, A.fmpz_mat());
}

inline bool inverse(const DMatZZ& A, DMatZZ& result_inv)
{
  DMatZZ::ElementType den;
  A.ring().init(den);
  bool result = fmpz_mat_inv(result_inv.fmpz_mat(), &den, A.fmpz_mat());
  if (!fmpz_is_pm1(&den)) result = false;
  A.ring().clear(den);
  return result;
}

inline void mult(const DMatZZ& A, const DMatZZ& B, DMatZZ& result_product)
{
  // The A1 and B1 on the next line are switched because the memory layout
  // expected
  // is the transpose of what we have for DMat.
  fmpz_mat_mul(result_product.fmpz_mat(), A.fmpz_mat(), B.fmpz_mat());
}

inline size_t nullSpace(const DMatZZ& A, DMatZZ& result_nullspace)
{
  long nullity = fmpz_mat_nullspace(result_nullspace.fmpz_mat(), A.fmpz_mat());
  return nullity;
}

inline bool solveLinear(const DMatZZ& A, const DMatZZ& B, DMatZZ& X)
{
  DMatZZ::ElementType den;
  A.ring().init(den);
  bool result = fmpz_mat_solve(X.fmpz_mat(), &den, B.fmpz_mat(), A.fmpz_mat());
  if (!fmpz_is_pm1(&den)) result = false;
  A.ring().clear(den);
  return result;
}

inline M2_arrayintOrNull rankProfile(const DMatZZ& A, bool row_profile)
{
  throw exc::engine_error(
      "'rankProfile' not implemented for this kind of matrix over this ring");
}

inline void addMultipleTo(DMatZZ& C, const DMatZZ& A, const DMatZZ& B)
{
  DMatZZ D(C.ring(), A.numRows(), B.numColumns());
  fmpz_mat_mul(D.fmpz_mat(), A.fmpz_mat(), B.fmpz_mat());
  fmpz_mat_add(C.fmpz_mat(), C.fmpz_mat(), D.fmpz_mat());
}

inline void subtractMultipleTo(DMatZZ& C, const DMatZZ& A, const DMatZZ& B)
{
  DMatZZ D(C.ring(), A.numRows(), B.numColumns());
  fmpz_mat_mul(D.fmpz_mat(), A.fmpz_mat(), B.fmpz_mat());
  fmpz_mat_sub(C.fmpz_mat(), C.fmpz_mat(), D.fmpz_mat());
}

//////////////////////
// ZZpFlint //////////
//////////////////////

// Functions for DMatZZpFlint

inline void addMultipleTo(DMatZZpFlint& C,
                          const DMatZZpFlint& A,
                          const DMatZZpFlint& B)
{
  DMatZZpFlint D(C.ring(), A.numRows(), B.numColumns());
  nmod_mat_mul(D.nmod_mat(), A.nmod_mat(), B.nmod_mat());
  nmod_mat_add(C.nmod_mat(), C.nmod_mat(), D.nmod_mat());
}

inline void subtractMultipleTo(DMatZZpFlint& C,
                               const DMatZZpFlint& A,
                               const DMatZZpFlint& B)
{
  DMatZZpFlint D(C.ring(), A.numRows(), B.numColumns());
  nmod_mat_mul(D.nmod_mat(), A.nmod_mat(), B.nmod_mat());
  nmod_mat_sub(C.nmod_mat(), C.nmod_mat(), D.nmod_mat());
}

inline void mult(const DMatZZpFlint& A,
                 const DMatZZpFlint& B,
                 DMatZZpFlint& result_product)
{
  //    DMatZZpFlint& A1 = const_cast<DMatZZpFlint&>(A); // needed because
  //    nmod_mat_mul doesn't declare params const
  //    DMatZZpFlint& B1 = const_cast<DMatZZpFlint&>(B);
  // The A1 and B1 on the next line are switched because the memory layout
  // expected
  // is the transpose of what we have for DMat.
  nmod_mat_mul(result_product.nmod_mat(), A.nmod_mat(), B.nmod_mat());
}

inline size_t rowReducedEchelonForm(const DMatZZpFlint& A,
                                    DMatZZpFlint& result_rref)
{
  DMatZZpFlint A1(A);
  long rank = nmod_mat_rref(A1.nmod_mat());
  result_rref.swap(A1);
  return rank;
}

////////////////////////
// GFFlintBig //////////
////////////////////////

// Functions for DMatGFFlintBig, linear algebra is sent out to LU

inline void addMultipleTo(DMatGFFlintBig& C,
                          const DMatGFFlintBig& A,
                          const DMatGFFlintBig& B)
{
  DMatGFFlintBig D(C.ring(), A.numRows(), B.numColumns());
  fq_nmod_mat_mul(D.fq_nmod_mat(),
                  A.fq_nmod_mat(),
                  B.fq_nmod_mat(),
                  A.ring().flintContext());
  fq_nmod_mat_add(C.fq_nmod_mat(),
                  C.fq_nmod_mat(),
                  D.fq_nmod_mat(),
                  A.ring().flintContext());
}

inline void subtractMultipleTo(DMatGFFlintBig& C,
                               const DMatGFFlintBig& A,
                               const DMatGFFlintBig& B)
{
  DMatGFFlintBig D(C.ring(), A.numRows(), B.numColumns());
  fq_nmod_mat_mul(D.fq_nmod_mat(),
                  A.fq_nmod_mat(),
                  B.fq_nmod_mat(),
                  A.ring().flintContext());
  fq_nmod_mat_sub(C.fq_nmod_mat(),
                  C.fq_nmod_mat(),
                  D.fq_nmod_mat(),
                  A.ring().flintContext());
}

inline void mult(const DMatGFFlintBig& A,
                 const DMatGFFlintBig& B,
                 DMatGFFlintBig& result_product)
{
  //    DMatGFFlintBig& A1 = const_cast<DMatGFFlintBig&>(A); // needed because
  //    nmod_mat_mul doesn't declare params const
  //    DMatGFFlintBig& B1 = const_cast<DMatGFFlintBig&>(B);
  // The A1 and B1 on the next line are switched because the memory layout
  // expected
  // is the transpose of what we have for DMat.
  fq_nmod_mat_mul(result_product.fq_nmod_mat(),
                  A.fq_nmod_mat(),
                  B.fq_nmod_mat(),
                  A.ring().flintContext());
}

inline size_t rowReducedEchelonForm(const DMatGFFlintBig& A,
                                    DMatGFFlintBig& result_rref)
{
  DMatGFFlintBig A1(A);
  long rank = fq_nmod_mat_rref(A1.fq_nmod_mat(), A.ring().flintContext());
  result_rref.swap(A1);
  return rank;
}

////////////////////////
// GFFlint /////////////
////////////////////////

// Functions for DMatGFFlint, linear algebra is sent out to LU

inline void addMultipleTo(DMatGFFlint& C,
                          const DMatGFFlint& A,
                          const DMatGFFlint& B)
{
  DMatGFFlint D(C.ring(), A.numRows(), B.numColumns());
  fq_zech_mat_mul(D.fq_zech_mat(),
                  A.fq_zech_mat(),
                  B.fq_zech_mat(),
                  A.ring().flintContext());
  fq_zech_mat_add(C.fq_zech_mat(),
                  C.fq_zech_mat(),
                  D.fq_zech_mat(),
                  A.ring().flintContext());
}

inline void subtractMultipleTo(DMatGFFlint& C,
                               const DMatGFFlint& A,
                               const DMatGFFlint& B)
{
  DMatGFFlint D(C.ring(), A.numRows(), B.numColumns());
  fq_zech_mat_mul(D.fq_zech_mat(),
                  A.fq_zech_mat(),
                  B.fq_zech_mat(),
                  A.ring().flintContext());
  fq_zech_mat_sub(C.fq_zech_mat(),
                  C.fq_zech_mat(),
                  D.fq_zech_mat(),
                  A.ring().flintContext());
}

inline void mult(const DMatGFFlint& A,
                 const DMatGFFlint& B,
                 DMatGFFlint& result_product)
{
  //    DMatGFFlint& A1 = const_cast<DMatGFFlint&>(A); // needed because
  //    nmod_mat_mul doesn't declare params const
  //    DMatGFFlint& B1 = const_cast<DMatGFFlint&>(B);
  // The A1 and B1 on the next line are switched because the memory layout
  // expected
  // is the transpose of what we have for DMat.
  fq_zech_mat_mul(result_product.fq_zech_mat(),
                  A.fq_zech_mat(),
                  B.fq_zech_mat(),
                  A.ring().flintContext());
}

inline size_t rowReducedEchelonForm(const DMatGFFlint& A,
                                    DMatGFFlint& result_rref)
{
  DMatGFFlint A1(A);
  long rank = fq_zech_mat_rref(A1.fq_zech_mat(), A.ring().flintContext());
  result_rref.swap(A1);
  return rank;
}

//////////////////////
// QQ ////////////////
//////////////////////

inline void mult(const DMatQQ& A, const DMatQQ& B, DMatQQ& result_product)
{
  FlintQQMat A1(A);
  FlintQQMat B1(B);
  FlintQQMat result1(A.numRows(), B.numColumns());

  fmpq_mat_mul(result1.value(), A1.value(), B1.value());

  result1.toDMat(result_product);
}

inline void addMultipleTo(DMatQQ& C, const DMatQQ& A, const DMatQQ& B)
{
  FlintQQMat A1(A);
  FlintQQMat B1(B);
  FlintQQMat C1(C);
  FlintQQMat result1(A.numRows(), B.numColumns());

  FlintQQMat D1(A.numRows(), B.numColumns());
  fmpq_mat_mul(D1.value(), A1.value(), B1.value());
  fmpq_mat_add(C1.value(), C1.value(), D1.value());

  C1.toDMat(C);
}

inline void subtractMultipleTo(DMatQQ& C, const DMatQQ& A, const DMatQQ& B)
{
  FlintQQMat A1(A);
  FlintQQMat B1(B);
  FlintQQMat C1(C);
  FlintQQMat result1(A.numRows(), B.numColumns());

  FlintQQMat D1(A.numRows(), B.numColumns());
  fmpq_mat_mul(D1.value(), A1.value(), B1.value());
  fmpq_mat_sub(C1.value(), C1.value(), D1.value());

  C1.toDMat(C);
}

inline size_t rowReducedEchelonForm(const DMatQQ& A, DMatQQ& result_rref)
{
  FlintQQMat A1(A);
  long rank = fmpq_mat_rref(A1.value(), A1.value());
  A1.toDMat(result_rref);
  return rank;
}

//////////////////////
// QQFlint ///////////
//////////////////////

// Functions for DMatQQFlint

inline size_t rank(const DMatQQFlint& A)
{
  // fmpq_mat has no rank function.
  // So we clear denominators row-wise (or column-wise), and compute the rank of
  // that matrix.
  fmpz_mat_t m1;
  fmpz_mat_init(m1, A.numRows(), A.numColumns());
  fmpq_mat_get_fmpz_mat_rowwise(m1, NULL, A.fmpq_mat());
  // fmpz_mat_print_pretty(m1);
  size_t rk = fmpz_mat_rank(m1);
  fmpz_mat_clear(m1);
  return rk;
}

inline void determinant(const DMatQQFlint& A,
                        M2::ARingQQFlint::ElementType& result_det)
{
  fmpq_mat_det(&result_det, A.fmpq_mat());
}

inline bool inverse(const DMatQQFlint& A, DMatQQFlint& result_inv)
{
  return fmpq_mat_inv(result_inv.fmpq_mat(), A.fmpq_mat());
}

inline size_t rowReducedEchelonForm(const DMatQQFlint& A,
                                    DMatQQFlint& result_rref)
{
  return fmpq_mat_rref(result_rref.fmpq_mat(), A.fmpq_mat());
}

inline size_t nullSpace(const DMatQQFlint& A, DMatQQFlint& result_nullspace)
{
  fmpz_mat_t m1;
  fmpz_mat_t m2;
  fmpz_mat_init(m1, A.numRows(), A.numColumns());
  fmpz_mat_init(m2, A.numColumns(), A.numColumns());
  fmpq_mat_get_fmpz_mat_rowwise(m1, NULL, A.fmpq_mat());
  // fmpz_mat_print_pretty(m1);
  size_t nullity = fmpz_mat_nullspace(m2, m1);
  // now copy the first 'nullity' columns into result_nullspace
  result_nullspace.resize(A.numColumns(), nullity);
  for (size_t c = 0; c < nullity; c++)
    for (size_t r = 0; r < A.numColumns(); r++)
      fmpz_set(fmpq_numref(&result_nullspace.entry(r, c)),
               fmpz_mat_entry(m2, r, c));
  fmpz_mat_clear(m1);
  fmpz_mat_clear(m2);
  return nullity;
}

inline bool solveLinear(const DMatQQFlint& A,
                        const DMatQQFlint& B,
                        DMatQQFlint& X)
{
  // TODO: WRITE ME
  // DMatQQFlint& A1 = const_cast<DMatQQFlint&>(A); // needed because
  // fmpq_mat_solve doesn't declare params const
  // DMatQQFlint& B1 = const_cast<DMatQQFlint&>(B);
  //    return fmpq_mat_solve(X.fmpq_mat(), B1.fmpq_mat(), A1.fmpq_mat());
  return false;
}

inline M2_arrayintOrNull rankProfile(const DMatQQFlint& A, bool row_profile)
{
  // TODO: WRITE ME
  throw exc::engine_error(
      "'rankProfile' not implemented for this kind of matrix over this ring");
}

inline void addMultipleTo(DMatQQFlint& C,
                          const DMatQQFlint& A,
                          const DMatQQFlint& B)
{
  DMatQQFlint D(C.ring(), A.numRows(), B.numColumns());
  fmpq_mat_mul(D.fmpq_mat(), A.fmpq_mat(), B.fmpq_mat());
  fmpq_mat_add(C.fmpq_mat(), C.fmpq_mat(), D.fmpq_mat());
}

inline void subtractMultipleTo(DMatQQFlint& C,
                               const DMatQQFlint& A,
                               const DMatQQFlint& B)
{
  DMatQQFlint D(C.ring(), A.numRows(), B.numColumns());
  fmpq_mat_mul(D.fmpq_mat(), A.fmpq_mat(), B.fmpq_mat());
  fmpq_mat_sub(C.fmpq_mat(), C.fmpq_mat(), D.fmpq_mat());
}

inline void mult(const DMatQQFlint& A,
                 const DMatQQFlint& B,
                 DMatQQFlint& result_product)
{
  // The A and B on the next line are switched because the memory layout
  // expected
  // is the transpose of what we have for DMat.
  fmpq_mat_mul(result_product.fmpq_mat(), A.fmpq_mat(), B.fmpq_mat());
}

////////
// RR //
////////
inline bool eigenvaluesHermitian(const DMatRR& A, DMatRR& eigenvals)
{
#ifndef NO_LAPACK
  return Lapack::eigenvalues_symmetric(&A, &eigenvals);
#else
  return EigenM2::eigenvalues_hermitian(&A, &eigenvals);
#endif
}

inline bool eigenvalues(const DMatRR& A, DMatCC& eigenvals)
{
#ifndef NO_LAPACK
  return Lapack::eigenvalues(&A, &eigenvals);
#else
  return EigenM2::eigenvalues(&A, &eigenvals);
#endif
}

inline bool eigenvectorsHermitian(const DMatRR& A,
  DMatRR& eigenvals,
  DMatRR& eigenvecs)
{
#ifndef NO_LAPACK
  return Lapack::eigenvectors_symmetric(&A, &eigenvals, &eigenvecs);
#else
  return EigenM2::eigenvectors_hermitian(&A, &eigenvals, &eigenvecs);
#endif
}

inline bool eigenvectors(const DMatRR& A, DMatCC& eigenvals, DMatCC& eigenvecs)
{
#ifndef NO_LAPACK
  return Lapack::eigenvectors(&A, &eigenvals, &eigenvecs);
#else
  return EigenM2::eigenvectors(&A, &eigenvals, &eigenvecs);
#endif
}

inline bool leastSquares(const DMatRR& A,
  const DMatRR& B,
  DMatRR& X,
  bool assume_full_rank)
{
#ifndef NO_LAPACK
  if (assume_full_rank)
    return Lapack::least_squares(&A, &B, &X);
  else
    return Lapack::least_squares_deficient(&A, &B, &X);
#else
  // place eigen code here
  return EigenM2::least_squares(&A, &B, &X);
  // throw exc::engine_error( "not implemented!!!");
  // return false; // indicates error
#endif
}

inline bool SVD(const DMatRR& A,
  DMatRR& Sigma,
  DMatRR& U,
  DMatRR& Vt,
  int strategy)
{
#ifndef NO_LAPACK
  if (strategy == 1) return Lapack::SVD_divide_conquer(&A, &Sigma, &U, &Vt);
  return Lapack::SVD(&A, &Sigma, &U, &Vt);
#else
  if (strategy == 1) return EigenM2::SVD_divide_conquer(&A, &Sigma, &U, &Vt);
  return EigenM2::SVD(&A, &Sigma, &U, &Vt);
#endif
}

inline bool QR(const DMatRR& A, DMatRR& Q, DMatRR& R, bool return_QR)
{
  return Lapack::QR(&A, &Q, &R, return_QR);
}

inline bool QR(const DMatCC& A, DMatCC& Q, DMatCC& R, bool return_QR)
{
  return Lapack::QR(&A, &Q, &R, return_QR);
}

inline void clean(gmp_RR epsilon, DMatRR& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().zeroize_tiny(epsilon, *p);
}

inline void increase_norm(gmp_RRmutable norm, const DMatRR& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().increase_norm(norm, *p);
}

////////
// CC //
////////
inline bool eigenvaluesHermitian(const DMatCC& A, DMatRR& eigenvals)
{
#ifndef NO_LAPACK
  return Lapack::eigenvalues_hermitian(&A, &eigenvals);
#else
  return EigenM2::eigenvalues_hermitian(&A, &eigenvals);
#endif
}

inline bool eigenvalues(const DMatCC& A, DMatCC& eigenvals)
{
#ifndef NO_LAPACK
  return Lapack::eigenvalues(&A, &eigenvals);
#else
  return EigenM2::eigenvalues(&A, &eigenvals);
#endif
}

inline bool eigenvectorsHermitian(const DMatCC& A,
  DMatRR& eigenvals,
  DMatCC& eigenvecs)
{
#ifndef NO_LAPACK
  return Lapack::eigenvectors_hermitian(&A, &eigenvals, &eigenvecs);
#else
  return EigenM2::eigenvectors_hermitian(&A, &eigenvals, &eigenvecs);
#endif
}

inline bool eigenvectors(const DMatCC& A, DMatCC& eigenvals, DMatCC& eigenvecs)
{
#ifndef NO_LAPACK
  return Lapack::eigenvectors(&A, &eigenvals, &eigenvecs);
#else
  return EigenM2::eigenvectors(&A, &eigenvals, &eigenvecs);
#endif
}

inline bool leastSquares(const DMatCC& A,
  const DMatCC& B,
  DMatCC& X,
  bool assume_full_rank)
{
#ifndef NO_LAPACK
  if (assume_full_rank)
    return Lapack::least_squares(&A, &B, &X);
  else
    return Lapack::least_squares_deficient(&A, &B, &X);
#else
  return EigenM2::least_squares(&A, &B, &X);
#endif
}

inline bool SVD(const DMatCC& A,
  DMatRR& Sigma,
  DMatCC& U,
  DMatCC& Vt,
  int strategy)
{
#ifndef NO_LAPACK
  if (strategy == 1) return Lapack::SVD_divide_conquer(&A, &Sigma, &U, &Vt);
  return Lapack::SVD(&A, &Sigma, &U, &Vt);
#else
  if (strategy == 1) return EigenM2::SVD_divide_conquer(&A, &Sigma, &U, &Vt);
  return EigenM2::SVD(&A, &Sigma, &U, &Vt);
#endif
}

inline void clean(gmp_RR epsilon, DMatCC& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().zeroize_tiny(epsilon, *p);
}

inline void increase_norm(gmp_RRmutable norm, const DMatCC& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().increase_norm(norm, *p);
}

/////////
// RRR //
/////////

inline bool eigenvaluesHermitian(const DMatRRR& A, DMatRRR& eigenvals)
{
  return EigenM2::eigenvalues_hermitian(&A, &eigenvals);
}

inline bool eigenvalues(const DMatRRR& A, DMatCCC& eigenvals)
{
  return EigenM2::eigenvalues(&A, &eigenvals);
}

inline bool eigenvectorsHermitian(const DMatRRR& A,
  DMatRRR& eigenvals,
  DMatRRR& eigenvecs)
{
  return EigenM2::eigenvectors_hermitian(&A, &eigenvals, &eigenvecs);
}

inline bool eigenvectors(const DMatRRR& A,
  DMatCCC& eigenvals,
  DMatCCC& eigenvecs)
{
  return EigenM2::eigenvectors(&A, &eigenvals, &eigenvecs);
}

inline bool leastSquares(const DMatRRR& A,
  const DMatRRR& B,
  DMatRRR& X,
  bool assume_full_rank)
{
  return EigenM2::least_squares(&A, &B, &X);
}

inline bool SVD(const DMatRRR& A,
  DMatRRR& Sigma,
  DMatRRR& U,
  DMatRRR& Vt,
  int strategy)
{
  if (strategy == 1) return EigenM2::SVD_divide_conquer(&A, &Sigma, &U, &Vt);
  return EigenM2::SVD(&A, &Sigma, &U, &Vt);
}

inline void clean(gmp_RR epsilon, DMatRRR& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().zeroize_tiny(epsilon, *p);
}

inline void increase_norm(gmp_RRmutable norm, const DMatRRR& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().increase_norm(norm, *p);
}

/////////
// CCC //  TODO: rewrite not using lapack
/////////

inline bool eigenvaluesHermitian(const DMatCCC& A, DMatRRR& eigenvals)
{
  return EigenM2::eigenvalues_hermitian(&A, &eigenvals);
}

inline bool eigenvalues(const DMatCCC& A, DMatCCC& eigenvals)
{
  return EigenM2::eigenvalues(&A, &eigenvals);
}

inline bool eigenvectorsHermitian(const DMatCCC& A,
  DMatRRR& eigenvals,
  DMatCCC& eigenvecs)
{
  return EigenM2::eigenvectors_hermitian(&A, &eigenvals, &eigenvecs);
}

inline bool eigenvectors(const DMatCCC& A,
  DMatCCC& eigenvals,
  DMatCCC& eigenvecs)
{
  return EigenM2::eigenvectors(&A, &eigenvals, &eigenvecs);
}

inline bool leastSquares(const DMatCCC& A,
  const DMatCCC& B,
  DMatCCC& X,
  bool assume_full_rank)
{
  return EigenM2::least_squares(&A, &B, &X);
}

inline bool SVD(const DMatCCC& A,
  DMatRRR& Sigma,
  DMatCCC& U,
  DMatCCC& Vt,
  int strategy)
{
  if (strategy == 1) return EigenM2::SVD_divide_conquer(&A, &Sigma, &U, &Vt);
  return EigenM2::SVD(&A, &Sigma, &U, &Vt);
}

inline void clean(gmp_RR epsilon, DMatCCC& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().zeroize_tiny(epsilon, *p);
}

inline void increase_norm(gmp_RRmutable norm, const DMatCCC& mat)
{
  auto p = mat.array();
  size_t len = mat.numRows() * mat.numColumns();
  for (size_t i = 0; i < len; i++, ++p) mat.ring().increase_norm(norm, *p);
}
};  // namespace MatrixOps

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
