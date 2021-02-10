// Copyright 2013  Michael E. Stillman

#ifndef _dmat_lu_hpp_
#define _dmat_lu_hpp_

#include "dmat.hpp"
#include "mat-elem-ops.hpp"
#include "mat-util.hpp"

#include "dmat-lu-inplace.hpp"

template <class RingType>
class DMatLinAlg;

#include "dmat-lu-zzp-ffpack.hpp"
#include "dmat-lu-zzp-flint.hpp"
#include "dmat-lu-qq.hpp"

typedef DMat<M2::ARingGFFlintBig> DMatGFFlintBig;

template <class RingType>
class DMatLinAlg
{
 public:
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;

 private:
  DMatLUinPlace<RingType> mLUObject;

 public:
  /// Copies A into mLU and initializes all fields
  DMatLinAlg(const Mat& A);

  /// Input: B, a matrix, the right hand side of AX=B
  /// Output: X, a matrix, solution to the above
  ///         returns false iff inconsistent
  bool solve(const Mat& B, Mat& X);

  /// Input: B, a matrix, the right hand side of AX=B
  ///        A, a square matrix (presumed to be invertible)
  /// Output: X, a matrix, solution to the above, if A is invertible
  ///         returns false iff A is not invertible
  bool solveInvertible(const Mat& B, Mat& X);

  /// Output: X, a matrix, the inverse of A
  ///         returns false iff A is (near)singular
  bool inverse(Mat& X);

  /// Output: result, the determinant of A
  void determinant(ElementType& result);

  /// Output: P = mPerm,
  ///         L,U; the corresponding parts mLU
  ///         returns false iff there is an error
  bool matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U);

  /// Output: X, a matrix, columns form a basis of Ax=0
  ///         returns dim of nullspace
  size_t kernel(Mat& X);

  /// Output: returns the approximate rank of A (-1 if fails)
  size_t rank();

  /// Output: profile, a vector of size equal to rank(A)
  ///                  containing the numbers of the columns
  ///                  where the rank increases
  void columnRankProfile(std::vector<size_t>& profile);

 private:
  // Private functions below this line
  const RingType& ring() const { return mLUObject.ring(); }
  void setUpperLower(const Mat& LU, Mat& lower, Mat& upper);

  void debug_out()
  {
    buffer o;
    displayMat(o, mLUObject.LU());
    std::cout << o.str() << std::endl;
  }

  void debug_out_list(ElementType* x, size_t len)
  {
    buffer o;
    o << "[ ";
    for (size_t i = 0; i < len; i++)
      {
        ring().elem_text_out(o, x[i], true, false);
        o << " ";
      }
    o << "]" << newline;
    std::cout << o.str();
  }
};

template <class RingType>
DMatLinAlg<RingType>::DMatLinAlg(const Mat& A) : mLUObject(A)
{
}

#if 0
template <class RingType>
size_t DMatLinAlg<RingType>::findPivot(size_t row, size_t col)
{
  // Look at elements A[row,col], A[row+1,col], ..., A[nrows-1, col]
  // Return the index r s.y. abs(A[r,col]) is maximum over all of these

  auto A = mLU.rowMajorArray();
  A += row * mLU.numColumns() + col;
  for (size_t i=row; i<mLU.numRows(); i++)
    {
      if (mLU.ring().is_zero(*A))
        A += mLU.numColumns();
      else
        return i;
    }
  return static_cast<size_t>(-1);
}
#endif
#if 0
template <class RingType>
void DMatLinAlg<RingType>::computeLU()
{
  //TODO: this is likely all wrong, and needs testing

  if (mIsDone) return;

  ElementType tmp;
  mLU.ring().init(tmp);
  size_t col = 0;
  size_t nrows = mLU.numRows();
  size_t ncols = mLU.numColumns();

  auto array = mLU.rowMajorArray();

  // The following 3 arrays are used to point to the places we need: A is the location we are working on
  // B is the part from 'L', C is the part from 'U' (and the original matrix)
  auto A = array; // mLU[row, col+1]
  auto B = array; // mLU[row,0]
  auto C = array; // mLU[0,col]

  for (size_t row = 0; row < nrows; row++)
    {
      // First, find a pivot, if any, in this column (from row row..nrows-1)
      // Find the element with largest absolute value:

      size_t k = findPivot(row,col);
      if (k != row) MatElementaryOps<Mat>::interchange_rows(mLU, k, row);

      const ElementType& pivot = *A;
      // Now set the elements in the pivot row at columns col+1..ncols-1

      auto B1 = B;
      auto C1 = C;
      for (size_t c = col+1; c < ncols; c++)
        {
          for (size_t i = 0; i<row; i++)
            {
              mLU.ring().mult(tmp, *B1++, *C1);
              mLU.ring().subtract(*A, *A, tmp);
              C1 += ncols;
            }
        }

      B1 = B;
      C1 = C;
      // Now set the elements in L in column 'row'
      for (size_t r = row+1; r < nrows; r++)
        {
          for (size_t i = 0; i<row; i++)
            {
              mLU.ring().mult(tmp, *B1++, *C1);
              mLU.ring().subtract(*A, *A, tmp);
              C += ncols;
            }
          mLU.ring().divide(*A, *A, pivot);
          *A /= pivot;
          A += ncols;
        }

      // Now place A,B,C into the right location
      B += ncols;
      C++;
      A += ncols;
    }

  mLU.ring().clear(tmp);
}
#endif
#if 0
template <class RingType>
void DMatLinAlg<RingType>::setUpperLower(const Mat& LU, Mat& lower, Mat& upper)
{
  // NOT TESTED YET!!
  // MAJOR ASSUMPTION: the matrices lower, upper, and mLU are stored in row major order!

  size_t min = std::min(LU.numRows(), LU.numColumns());
  lower.resize(LU.numRows(), min);
  upper.resize(min, LU.numColumns());

  // At this point, lower and upper should be zero matrices.
  assert(MatrixOps::isZero(lower));
  assert(MatrixOps::isZero(upper));

  auto LUraw = LU.rowMajorArray();
  auto L = lower.rowMajorArray();
  auto U = upper.rowMajorArray();

  for (size_t c=0; c<upper.numColumns(); c++)
    {
      auto U1 = U;
      for (size_t r=0; r<=c; r++)
        {
          if (r >= upper.numRows()) break;
          upper.ring().set(*U1, *LUraw++);
          U1 += upper.numColumns();
        }
      U++; // change to next column

      if (c < lower.numColumns())
        {
          lower.ring().set_from_long(*L, 1); // diagonal entry of L should be 1
          L += lower.numColumns(); // pointing to entry right below diagonal
          auto L1 = L; // will increment by lower.numRows() each loop here
          for (size_t r=c+1; r<lower.numRows(); r++)
            {
              lower.ring().set(*L1, *LUraw++);
              L1 += lower.numColumns(); // to place next entry.
            }
          L++; // change to next column
        }
    }
}
#endif
template <class RingType>
void DMatLinAlg<RingType>::setUpperLower(const Mat& LU, Mat& lower, Mat& upper)
{
  size_t min = std::min(LU.numRows(), LU.numColumns());
  lower.resize(LU.numRows(), min);
  upper.resize(min, LU.numColumns());

  // At this point, lower and upper should be zero matrices.
  assert(MatrixOps::isZero(lower));
  assert(MatrixOps::isZero(upper));

  for (size_t c = 0; c < LU.numColumns(); c++)
    {
      if (c < min) ring().set_from_long(lower.entry(c, c), 1);
      for (size_t r = 0; r < LU.numRows(); r++)
        {
          if (r <= c)
            ring().set(upper.entry(r, c), LU.entry(r, c));
          else if (c < lower.numRows())
            {
              ring().set(lower.entry(r, c), LU.entry(r, c));
            }
        }
    }
}

template <class RingType>
bool DMatLinAlg<RingType>::matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
{
  const Mat& LU = mLUObject.LUinPlace();

  setUpperLower(LU, L, U);
  P = mLUObject.permutation();
  return mLUObject.signOfPermutation();
}

template <class RingType>
void DMatLinAlg<RingType>::determinant(ElementType& result)
{
  const Mat& LU = mLUObject.LUinPlace();

  // This is just the product of the diagonal entries of mLU.
  assert(LU.numRows() == LU.numColumns());

  if (mLUObject.signOfPermutation())
    ring().set_from_long(result, 1);
  else
    ring().set_from_long(result, -1);
  for (size_t i = 0; i < LU.numRows(); i++)
    ring().mult(result, result, LU.entry(i, i));
}

template <class RingType>
void DMatLinAlg<RingType>::columnRankProfile(std::vector<size_t>& profile)
{
  mLUObject.LUinPlace();

  profile = mLUObject.pivotColumns();
}

template <class RingType>
size_t DMatLinAlg<RingType>::rank()
{
  mLUObject.LUinPlace();

  return mLUObject.pivotColumns().size();
}

template <class RingType>
bool DMatLinAlg<RingType>::solve(const Mat& B, Mat& X)
{
  // printf("in dmat-lu: solve\n");
  const Mat& LU = mLUObject.LUinPlace();
  // printf("in dmat-lu: after LU solve\n");

  // For each column of B, we solve it separately.

  // Step 1: Take a column, and permute it via mPerm, into b.
  // Step 2: Solve the lower triangular system Ly=b
  //  If there is no solution, then return false.
  // Step 3: Solve the upper triangular system Ux = y.

  // Sizes of objects here:
  //  A is m x n
  //  L is m x r
  //  U is r x n.  Here, r <= m, and A has rank r.
  //  b is a vector 0..m-1
  //  y is a vector 0..r-1
  //  x is a vector 0..n-1

  // printf("entering DMatLinAlg::solve\n");
  const std::vector<size_t>& pivotColumns = mLUObject.pivotColumns();
  const std::vector<size_t>& perm = mLUObject.permutation();
  size_t rk = pivotColumns.size();

  ElementType tmp, tmp2;
  ring().init(tmp);
  ring().init(tmp2);

  ElementType* b = newarray(ElementType, LU.numRows());
  ElementType* y = newarray(ElementType, rk);
  ElementType* x = newarray(ElementType, LU.numColumns());

  for (size_t i = 0; i < LU.numRows(); i++) ring().init(b[i]);
  for (size_t i = 0; i < rk; i++) ring().init(y[i]);
  for (size_t i = 0; i < LU.numColumns(); i++) ring().init(x[i]);

  X.resize(LU.numColumns(), B.numColumns());

  for (size_t col = 0; col < B.numColumns(); col++)
    {
      // Step 0: erase x (b will be set below, y is also set when needed).

      for (size_t i = 0; i < LU.numColumns(); i++) ring().set_zero(x[i]);

      // Step 1: set b to be the permuted i-th column of B.
      for (size_t r = 0; r < B.numRows(); r++)
        ring().set(b[r], B.entry(perm[r], col));

      /// printf("b:\n");
      /// debug_out_list(b, LU.numRows());

      // Step 2: Solve Ly=b
      for (size_t i = 0; i < rk; i++)
        {
          ring().set(y[i], b[i]);
          for (size_t j = 0; j < i; j++)
            {
              ring().mult(tmp, LU.entry(i, j), y[j]);
              ring().subtract(y[i], y[i], tmp);
            }
        }

      /// printf("y:\n");
      /// debug_out_list(y, rk);

      // Step 2B: see if the solution is consistent
      for (size_t i = rk; i < LU.numRows(); i++)
        {
          ring().set(tmp, b[i]);
          for (size_t j = 0; j < rk; j++)
            {
              ring().mult(tmp2, LU.entry(i, j), y[j]);
              ring().subtract(tmp, tmp, tmp2);
            }
          if (!ring().is_zero(tmp))
            {
              // Cleanup, and return false
              deletearray(b);
              deletearray(y);
              deletearray(x);
              ring().clear(tmp);
              ring().clear(tmp2);
              // printf("returning false\n");
              return false;
            }
        }

      ///      printf("past test for consistency\n");

      // Step 3: Solve Ux=y
      // and place x back into X as col-th column
      for (long i = rk - 1; i >= 0; --i)
        {
          ring().set(x[i], y[i]);
          for (size_t j = i + 1; j <= rk - 1; j++)
            {
              ring().mult(tmp, LU.entry(i, pivotColumns[j]), x[j]);
              ring().subtract(x[i], x[i], tmp);
            }
          ring().divide(x[i], x[i], LU.entry(i, pivotColumns[i]));
          ring().set(X.entry(pivotColumns[i], col), x[i]);

          /// buffer o;
          /// printf("after i=%ld\n", i);
          /// displayMat(o, X);
          /// printf("%s\n", o.str());
        }
      /// printf("x:\n");
      /// debug_out_list(x, LU.numColumns());

      /// buffer o;
      /// printf("after col=%ld\n", col);
      /// displayMat(o, X);
      /// printf("%s\n", o.str());
    }
  ring().clear(tmp);
  ring().clear(tmp2);

  for (size_t i = 0; i < LU.numRows(); i++) ring().clear(b[i]);
  for (size_t i = 0; i < rk; i++) ring().clear(y[i]);
  for (size_t i = 0; i < LU.numColumns(); i++) ring().clear(x[i]);
  deletearray(b);
  deletearray(y);
  deletearray(x);
  return true;  // The system seems to have been consistent
}

#if 0
template <class RingType>
bool DMatLinAlg<RingType>::solveInvertible(const Mat& B, Mat& X)
{
  // possible TODO: incorporate a faster method if we know matrix is invertible...
  assert(mLUObject.numRows() == mLUObject.numColumns());
  assert(mLUObject.numRows() == B.numRows());

  if (rank() < mLUObject.numRows()) return false;
  solve(B,X);
  return true;
}
#endif

template <class Mat>
void permuteRows(const Mat& B,
                 const std::vector<size_t> permutation,
                 Mat& result)
{
  // Better would be if the Mat type allows easy swapping of rows...
  result.resize(B.numRows(),
                B.numColumns());  // leaves B alone if correct size already...
  for (long r = 0; r < B.numRows(); r++)
    for (long c = 0; c < B.numColumns(); c++)
      B.ring().set(result.entry(r, c), B.entry(permutation[r], c));
}

template <class Mat>
void solveLowerTriangular(const Mat& LU, const Mat& B, Mat& X)
{
}

template <>
inline void solveLowerTriangular<DMatGFFlintBig>(const DMatGFFlintBig& LU,
                                                 const DMatGFFlintBig& B,
                                                 DMatGFFlintBig& X)
{
  fq_nmod_mat_solve_tril(X.fq_nmod_mat(),
                         LU.fq_nmod_mat(),
                         B.fq_nmod_mat(),
                         1,
                         LU.ring().flintContext());
}

template <>
inline void solveLowerTriangular<DMatGFFlint>(const DMatGFFlint& LU,
                                              const DMatGFFlint& B,
                                              DMatGFFlint& X)
{
  fq_zech_mat_solve_tril(X.fq_zech_mat(),
                         LU.fq_zech_mat(),
                         B.fq_zech_mat(),
                         1,
                         LU.ring().flintContext());
}

template <class Mat>
void solveUpperTriangular(const Mat& LU, const Mat& B, Mat& X)
{
}

template <>
inline void solveUpperTriangular<DMatGFFlint>(const DMatGFFlint& LU,
                                              const DMatGFFlint& B,
                                              DMatGFFlint& X)
{
  fq_zech_mat_solve_triu(X.fq_zech_mat(),
                         LU.fq_zech_mat(),
                         B.fq_zech_mat(),
                         0,
                         LU.ring().flintContext());
}
template <>
inline void solveUpperTriangular<DMatGFFlintBig>(const DMatGFFlintBig& LU,
                                                 const DMatGFFlintBig& B,
                                                 DMatGFFlintBig& X)
{
  fq_nmod_mat_solve_triu(X.fq_nmod_mat(),
                         LU.fq_nmod_mat(),
                         B.fq_nmod_mat(),
                         0,
                         LU.ring().flintContext());
}

template <class RingType>
bool DMatLinAlg<RingType>::solveInvertible(const Mat& B, Mat& X)
{
  // printf("in dmat-lu solveInvertible\n");
  // possible TODO: incorporate a faster method if we know matrix is
  // invertible...
  assert(mLUObject.numRows() == mLUObject.numColumns());
  assert(mLUObject.numRows() == B.numRows());

  X.resize(mLUObject.numColumns(), B.numColumns());
  mLUObject.LUinPlace();
  if (rank() < mLUObject.numRows()) return false;
  solve(B, X);
  return true;
#if 0
  // The following code doesn't really seem to be any faster than the naive code we have
  // at least for sizes less than a few thousand rows/columns.
  Mat B1(ring(), LU.numRows(), LU.numRows()); // square matrix
  printf("in dmat-lu solveInvertible step 3\n");
  permuteRows<Mat>(B, mLUObject.permutation(), B1);
  printf("in dmat-lu solveInvertible step 4\n");
  solveLowerTriangular(LU, B, X);
  printf("in dmat-lu solveInvertible step 5\n");
  solveUpperTriangular(LU, X, X);
  printf("in dmat-lu solveInvertible step 6\n");
  return true;
#endif
}

template <class RingType>
bool DMatLinAlg<RingType>::inverse(Mat& X)
{
  const Mat& LU = mLUObject.LUinPlace();

  // Make the identity matrix
  // printf("in DMatLinAlg<RingType>::inverse\n");

  Mat id(ring(), LU.numRows(), LU.numRows());
  for (size_t i = 0; i < LU.numRows(); i++)
    ring().set_from_long(id.entry(i, i), 1);

  solve(id, X);
  return true;
}

template <class RingType>
size_t DMatLinAlg<RingType>::kernel(Mat& X)
{
  const Mat& LU = mLUObject.LUinPlace();
  const std::vector<size_t>& pivotColumns = mLUObject.pivotColumns();

  // THIS SHOULD NOT BE SO PAINFUL!!
  ElementType tmp, tmp2;
  ring().init(tmp);
  ring().init(tmp2);

  // First, let's set X to be the correct size:
  X.resize(LU.numColumns(), LU.numColumns() - pivotColumns.size());

  size_t col = 0;
  size_t nextpivotidx = 0;
  size_t nextpivotcol =
      (pivotColumns.size() > 0 ? pivotColumns[0] : LU.numColumns());
  size_t colX = 0;
  while (colX < X.numColumns())
    {
      if (col == nextpivotcol)
        {
          col++;
          nextpivotidx++;
          nextpivotcol =
              (nextpivotidx < pivotColumns.size() ? pivotColumns[nextpivotidx]
                                                  : LU.numColumns());
          continue;
        }
      // At this point, we are ready to create a column of X.
      ring().set_from_long(X.entry(col, colX), -1);
      // Now we loop through and set the elements in the rows of X = pivot
      // columns.
      for (long p = nextpivotidx - 1; p >= 0; p--)
        {
          // set X.entry(pivotColumns[p], colX)
          ring().set(tmp, LU.entry(p, col));
          for (size_t i = nextpivotidx - 1; i >= p + 1; i--)
            {
              ring().mult(tmp2,
                          LU.entry(p, pivotColumns[i]),
                          X.entry(pivotColumns[i], colX));
              ring().subtract(tmp, tmp, tmp2);
            }
          ring().divide(tmp, tmp, LU.entry(p, pivotColumns[p]));
          ring().set(X.entry(pivotColumns[p], colX), tmp);
        }
      colX++;
      col++;
    }

  ring().clear(tmp);
  ring().clear(tmp2);
  return X.numColumns();
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
