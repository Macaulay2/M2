// Copyright 2014  Michael E. Stillman

#ifndef _dmat_lu_inplace_hpp_
#define _dmat_lu_inplace_hpp_

#include "dmat.hpp"
#include "mat-elem-ops.hpp"
#include "mat-util.hpp"

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>
#include <flint/perm.h>

template <typename RT>
class LUUtil
{
 public:
  typedef RT RingType;
  typedef DMat<RingType> Mat;

  static void setUpperLower(const Mat& LU, Mat& lower, Mat& upper);
  static void computePivotColumns(const Mat& LU,
                                  std::vector<size_t>& result_columns);
  // not written yet:
  // static bool computeSign(const std::vector<size_t>& perm);  // true: 1,
  // false: -1

  void debug_out(const Mat& M)
  {
    buffer o;
    displayMat(o, M);
    std::cout << o.str() << std::endl;
  }
};

template <typename RT>
class DMatLUinPlace;

template <typename RT>
class DMatLUinPlace
{
 public:
  typedef RT RingType;
  typedef DMat<RingType> Mat;

 public:
  DMatLUinPlace(const Mat& A);

  const RingType& ring() const { return mLU.ring(); }
  long numRows() const { return mLU.numRows(); }
  long numColumns() const { return mLU.numColumns(); }
  const Mat& LUinPlace()
  {
    computeLU();
    return mLU;
  }  // raises an exception if there is an error
  // Can be called repeatedly: the result is remembered once done.
  // Returns a constant ref to the internal "in place" LU.

  bool signOfPermutation() { return mSign; }
  const std::vector<size_t>& permutation() { return mPerm; }
  const std::vector<size_t>& pivotColumns() { return mPivotColumns; }
 private:
  typedef typename RingType::ElementType ElementType;

  void computeLU();
  size_t findPivot(size_t row, size_t col);

 private:
  Mat mLU;
  std::vector<size_t> mPerm;
  bool mSign;
  bool mIsDone;
  std::vector<size_t> mPivotColumns;
};

template <>
inline void DMatLUinPlace<M2::ARingGFFlintBig>::computeLU()
{
  if (mIsDone) return;

  mp_limb_signed_t* perm = newarray_atomic(mp_limb_signed_t, mLU.numRows());
  fq_nmod_mat_lu(perm, mLU.fq_nmod_mat(), false, ring().flintContext());
  // Now we set mPerm:
  mPerm.clear();
  for (long i = 0; i < mLU.numRows(); i++) mPerm.push_back(perm[i]);
  mSign = (_perm_parity(perm, mLU.numRows()) == 0);
  freemem(perm);

  // Now we set mPivotColumns
  LUUtil<RingType>::computePivotColumns(mLU, mPivotColumns);

  mIsDone = true;
}

template <>
inline void DMatLUinPlace<M2::ARingGFFlint>::computeLU()
{
  if (mIsDone) return;

  mp_limb_signed_t* perm = newarray_atomic(mp_limb_signed_t, mLU.numRows());
  fq_zech_mat_lu(perm, mLU.fq_zech_mat(), false, ring().flintContext());
  // Now we set mPerm:
  mPerm.clear();
  for (long i = 0; i < mLU.numRows(); i++) mPerm.push_back(perm[i]);
  mSign = (_perm_parity(perm, mLU.numRows()) == 0);
  freemem(perm);

  // Now we set mPivotColumns
  LUUtil<RingType>::computePivotColumns(mLU, mPivotColumns);

  mIsDone = true;
}

template <class RingType>
DMatLUinPlace<RingType>::DMatLUinPlace(const Mat& A)
    : mLU(A),       // copies A
      mSign(true),  // sign = 1
      mIsDone(false)
{
  for (size_t i = 0; i < A.numRows(); i++) mPerm.push_back(i);
}

template <class RingType>
size_t DMatLUinPlace<RingType>::findPivot(size_t row, size_t col)
{
  // Look at elements A[row,col], A[row+1,col], ..., A[nrows-1, col]
  // Return the index r s.y. abs(A[r,col]) is maximum over all of these

  for (size_t i = row; i < mLU.numRows(); i++)
    {
      if (!ring().is_zero(mLU.entry(i, col))) return i;
    }
  return static_cast<size_t>(-1);
}

template <>
inline size_t DMatLUinPlace<M2::ARingRRR>::findPivot(size_t row, size_t col)
{
  // Look at elements A[row,col], A[row+1,col], ..., A[nrows-1, col]
  // Return the index r s.y. abs(A[r,col]) is maximum over all of these

  ElementType largest;
  ElementType abs;
  size_t best_row_so_far = static_cast<size_t>(-1);

  ring().init(largest);
  ring().init(abs);
  ring().set_zero(largest);

  for (size_t i = row; i < mLU.numRows(); i++)
    {
      ring().abs(abs, mLU.entry(i, col));
      if (ring().compare_elems(abs, largest) > 0)
        {
          best_row_so_far = i;
          ring().set(largest, abs);
        }
    }
  ring().clear(abs);
  ring().clear(largest);
  return best_row_so_far;
}

template <>
inline size_t DMatLUinPlace<M2::ARingCCC>::findPivot(size_t row, size_t col)
{
  // Look at elements A[row,col], A[row+1,col], ..., A[nrows-1, col]
  // Return the index r s.y. abs(A[r,col]) is maximum over all of these

  const M2::ARingRRR& RR = ring().real_ring();
  M2::ARingRRR::ElementType largest;
  M2::ARingRRR::ElementType abs;
  size_t best_row_so_far = static_cast<size_t>(-1);

  RR.init(largest);
  RR.init(abs);
  RR.set_zero(largest);

  for (size_t i = row; i < mLU.numRows(); i++)
    {
      ring().abs(abs, mLU.entry(i, col));
      if (RR.compare_elems(abs, largest) > 0)
        {
          best_row_so_far = i;
          RR.set(largest, abs);
        }
    }
  RR.clear(abs);
  RR.clear(largest);
  return best_row_so_far;
}

template <class RingType>
void DMatLUinPlace<RingType>::computeLU()
{
  if (mIsDone) return;

  //  std::cout << "computing LU decomposition NAIVE version" << std::endl;
  ElementType tmp;
  mLU.ring().init(tmp);

  size_t col = 0;  // current column we are working on
  size_t row = 0;  // current row we are working on
  size_t nrows = mLU.numRows();
  size_t ncols = mLU.numColumns();

  while (col < ncols && row < nrows)
    {
      // printf("*** in naive row,col = (%ld, %ld) ***\n", row, col);
      // debug_out();

      // Step 1: Set the 'upper' values: (row,col)..(nrows-1,col)
      for (size_t r = row; r < nrows; r++)
        {
          for (size_t i = 0; i < row; i++)
            {
              mLU.ring().mult(tmp, mLU.entry(r, i), mLU.entry(i, col));
              mLU.ring().subtract(mLU.entry(r, col), mLU.entry(r, col), tmp);
            }
        }

      // printf("after step 1\n");
      // debug_out();

      // Step 2: Find a pivot among the elements in step 1.
      //  If one: swap rows if needed
      //  If none, increment 'col', and continue at start of loop
      size_t k = findPivot(row, col);
      if (k == static_cast<size_t>(-1))
        {
          col = col + 1;
          continue;
        }
      // printf("pivot is in row %ld\n", k);
      std::swap(mPerm[row], mPerm[k]);
      if (k != row)
        {
          MatElementaryOps<Mat>::interchange_rows(mLU, k, row);
          mSign = !mSign;
        }
      mPivotColumns.push_back(col);
      const ElementType& pivot = mLU.entry(row, col);

      // printf("after step 2:\n");
      // debug_out();

      // Step 3A: Set the 'upper' elements in (row,col+1), ..., (row,ncols-1).
      for (size_t c = col + 1; c < ncols; c++)
        {
          for (size_t i = 0; i < row; i++)
            {
              mLU.ring().mult(tmp, mLU.entry(row, i), mLU.entry(i, c));
              mLU.ring().subtract(mLU.entry(row, c), mLU.entry(row, c), tmp);
            }
        }
      // printf("after step 3A:\n");
      // debug_out();

      // Step 3B: Set the 'lower' elements in (row+1,row), ..., (nrows-1,row)
      //  from (row+1,col), ..., (nrows-1,col)
      // This just means dividing then by the pivot
      // except, if we have skipped columns for pivots, we must set these
      // elements
      // in column 'row', not 'col'...
      // Step 3C: if row != col, then set these elements to 0:
      //  (row+1,col), ..., (nrows-1,col)
      for (size_t r = row + 1; r < nrows; r++)
        {
          mLU.ring().divide(mLU.entry(r, row), mLU.entry(r, col), pivot);
          if (row < col) ring().set_zero(mLU.entry(r, col));
        }

      // printf("after step 3B:\n");
      // debug_out();

      row++;
      col++;
    }

  mIsDone = true;
  mLU.ring().clear(tmp);
}

template <>
inline void DMatLUinPlace<M2::ARingRR>::computeLU()
{
  if (mIsDone) return;

  int rows = static_cast<int>(mLU.numRows());
  int cols = static_cast<int>(mLU.numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  // printf("entering DMatLUinPlace::computeLUNaive for RR\n");

  int* perm = newarray_atomic(int, min);

  double* copyA = newarray_atomic(double, mLU.numRows() * mLU.numColumns());

  // place all elements of mLU, but in column major order.
  double* p = copyA;
  for (size_t c = 0; c < mLU.numColumns(); c++)
    {
      auto end = mLU.columnEnd(c);
      for (auto a = mLU.columnBegin(c); a != end; ++a) *p++ = *a;
    }

  dgetrf_(&rows, &cols, copyA, &rows, perm, &info);

  if (info < 0)
    {
      // First, clean up, then throw an exception
      throw exc::engine_error("argument passed to dgetrf had an illegal value");
      // return;
    }

  // Now copy back to row major order
  p = copyA;
  for (size_t c = 0; c < mLU.numColumns(); c++)
    {
      auto end = mLU.columnEnd(c);
      for (auto a = mLU.columnBegin(c); a != end; ++a) *a = *p++;
    }

  // Now place the correct permutation into mPerm
  for (int i = 0; i < min; i++)
    {
      int thisloc = perm[i] - 1;
      if (i != thisloc)
        {
          mSign = not mSign;
          size_t tmp = mPerm[thisloc];
          mPerm[thisloc] = mPerm[i];
          mPerm[i] = tmp;
        }
    }

  LUUtil<RingType>::computePivotColumns(mLU, mPivotColumns);
  mIsDone = true;

  freemem(perm);
  freemem(copyA);
}

template <>
inline void DMatLUinPlace<M2::ARingCC>::computeLU()
{
  if (mIsDone) return;

  int rows = static_cast<int>(mLU.numRows());
  int cols = static_cast<int>(mLU.numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  // printf("entering DMatLUtemplate::computeLUNaive for RR\n");

  int* perm = newarray_atomic(int, min);

  double* copyA = newarray_atomic(double, 2 * mLU.numRows() * mLU.numColumns());

  // place all elements of mLU, but in column major order.
  double* p = copyA;
  for (size_t c = 0; c < mLU.numColumns(); c++)
    {
      auto end = mLU.columnEnd(c);
      for (auto a = mLU.columnBegin(c); a != end; ++a)
        {
          *p++ = (*a).re;
          *p++ = (*a).im;
        }
    }

  zgetrf_(&rows, &cols, copyA, &rows, perm, &info);

  if (info < 0)
    {
      // First, clean up, then throw an exception
      throw exc::engine_error("argument passed to zgetrf had an illegal value");
      // return;
    }

  // Now copy back to row major order
  p = copyA;
  for (size_t c = 0; c < mLU.numColumns(); c++)
    {
      auto end = mLU.columnEnd(c);
      for (auto a = mLU.columnBegin(c); a != end; ++a)
        {
          (*a).re = *p++;
          (*a).im = *p++;
        }
    }

  // Now place the correct permutation into mPerm
  for (int i = 0; i < min; i++)
    {
      int thisloc = perm[i] - 1;
      if (i != thisloc)
        {
          mSign = not mSign;
          size_t tmp = mPerm[thisloc];
          mPerm[thisloc] = mPerm[i];
          mPerm[i] = tmp;
        }
    }

  LUUtil<RingType>::computePivotColumns(mLU, mPivotColumns);
  mIsDone = true;

  freemem(perm);
  freemem(copyA);
}

template <class RingType>
void LUUtil<RingType>::setUpperLower(const Mat& LU, Mat& lower, Mat& upper)
{
  size_t min = std::min(LU.numRows(), LU.numColumns());
  lower.resize(LU.numRows(), min);
  upper.resize(min, LU.numColumns());

  // At this point, lower and upper should be zero matrices.
  assert(MatrixOps::isZero(lower));
  assert(MatrixOps::isZero(upper));

  for (size_t c = 0; c < LU.numColumns(); c++)
    {
      if (c < min) LU.ring().set_from_long(lower.entry(c, c), 1);
      for (size_t r = 0; r < LU.numRows(); r++)
        {
          if (r <= c)
            LU.ring().set(upper.entry(r, c), LU.entry(r, c));
          else if (c < lower.numRows())
            {
              LU.ring().set(lower.entry(r, c), LU.entry(r, c));
            }
        }
    }
}

template <class RingType>
void LUUtil<RingType>::computePivotColumns(const Mat& LU,
                                           std::vector<size_t>& result_columns)
{
  result_columns.clear();
  size_t thiscol = 0;
  size_t thisrow = 0;
  while (thisrow < LU.numRows() and thiscol < LU.numColumns())
    {
      if (not LU.ring().is_zero(LU.entry(thisrow, thiscol)))
        {
          result_columns.push_back(thiscol);
          thisrow++;
        }
      thiscol++;
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
