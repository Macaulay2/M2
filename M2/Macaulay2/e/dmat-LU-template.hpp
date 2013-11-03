// Copyright 2013  Michael E. Stillman

#ifndef _dmat_LU_template_hpp_
#define _dmat_LU_template_hpp_

#include "engine.h"
#include "dmat.hpp"
#include "mat-elem-ops.hpp"
#include "mat-util.hpp"

template <class RingType>
class DMatLUtemplate
{
public:
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;

private:
  Mat mLU;
  std::vector<size_t> mPerm;
  bool mSign;
  bool mIsDone;
  bool mError;

public:
  /// Copies A into mLU and initializes all fields
  DMatLUtemplate(const Mat& A);

  /// Input: B, a matrix, the right hand side of AX=B
  /// Output: X, a matrix, solution to the above
  ///         returns false iff inconsistent
  bool solve(const Mat& B, Mat& X); 

  /// Output: X, a matrix, the inverse of A
  ///         returns false iff A is (near)singular
  bool inverse(Mat& X); 

  /// Output: result, the determinant of A
  ///         returns false iff fails
  bool determinant(ElementType& result); 

  /// Output: P = mPerm,
  ///         L,U; the corresponding parts mLU
  ///         returns false iff there is an error 
  bool MatrixPLU(std::vector<size_t>& P, Mat& L, Mat& U); 

  /// Output: X, a matrix, columns form a basis of Ax=0
  ///         returns false iff A is (near)singular
  bool kernel(Mat& X); 

  /// Output: returns the approximate rank of A (-1 if fails)
  size_t rank(); 

  /// Output: profile, a vector of size equal to rank(A) 
  ///                  containing the numbers of the columns 
  ///                  where the rank increases
  ///         returns false iff there is an error 
  bool columnRankProfile(std::vector<size_t>& profile); 

private:
  // Private functions below this line
  const RingType& ring() const { return mLU.ring(); }

  // compute the LU decomposition, if mIsDone is not set, and then set it.
  // This sets mError if there is a problem.
  void computeLU();
  void computeLUNAIVE();

  void setUpperLower(Mat& lower, Mat& upper);
  void setUpperLowerNAIVE(Mat& lower, Mat& upper);

  size_t findPivot(size_t row, size_t col);
  size_t findPivotNAIVE(size_t row, size_t col);

  void debug_out()
  {
    buffer o;
    displayMat(o,mLU);
    std::cout << o.str() << std::endl;
#if 0
    for (size_t r1 = 0; r1 < mLU.numRows(); r1++)
      {
        for (size_t c1 = 0; c1 < mLU.numColumns(); c1++)
          {
            buffer o;
            ring().elem_text_out(o, mLU.entry(r1,c1), true, false);
            printf("%s\t", o.str());
          }
        printf("\n");
      }
#endif
  }

};

template <class RingType>
DMatLUtemplate<RingType>::DMatLUtemplate(const Mat& A)
  : mLU(A),  // copies A
    mSign(true), // sign = 1
    mIsDone(false),
    mError(false)
{
  printf("entering DMatLUtemplate\n");
  for (size_t i=0; i<A.numRows(); i++)
    mPerm.push_back(i);
}

template <class RingType>
size_t DMatLUtemplate<RingType>::findPivotNAIVE(size_t row, size_t col)
{
  // Look at elements A[row,col], A[row+1,col], ..., A[nrows-1, col]
  // Return the index r s.y. abs(A[r,col]) is maximum over all of these

  for (size_t i=row; i<mLU.numRows(); i++)
    {
      if (!ring().is_zero(mLU.entry(i,col)))
          return i;
    }
  return static_cast<size_t>(-1);
}

template <class RingType>
size_t DMatLUtemplate<RingType>::findPivot(size_t row, size_t col)
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

template <class RingType>
void DMatLUtemplate<RingType>::computeLU()
{
  if (mIsDone) return;

  ElementType tmp;
  mLU.ring().init(tmp);
  size_t col = 0;
  size_t nrows = mLU.numRows();
  size_t ncols = mLU.numColumns();

  auto array = mLU.rowMajorArray();

  // The following 3 arrays are used to point to the places we need: A is the location we are working on
  // B is the part from 'L', C is the part from 'U' (and the orginal matrix)
  auto A = array; // mLU[row, col+1]
  auto B = array; // mLU[row,0]
  auto C = array; // mLU[0,col]

  for (size_t row = 0; row < nrows; row++)
    {
      printf("in row %ld\n", row);
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

template <class RingType>
void DMatLUtemplate<RingType>::computeLUNAIVE()
{
  if (mIsDone) return;

  ElementType tmp;
  mLU.ring().init(tmp);

  size_t col = 0; // current column we are working on
  size_t row = 0; // current row we are working on
  size_t nrows = mLU.numRows();
  size_t ncols = mLU.numColumns();

  while (col < ncols && row < nrows)
    {
      // Step 1: Set the 'upper' values: (row,col)..(nrows-1,col)

      // Step 2: Find a pivot among the elements in step 1.
      //  If one: swap rows if needed
      //  If none, increment 'col', and continue at start of loop
      //
      // Step 3A: Set the 'upper' elements in (row,col+1), ..., (row,ncols-1).
      // Step 3B: Set the 'lower' elements in (row+1,row), ..., (nrows-1,row)
      //  from (row+1,col), ..., (nrows-1,col)
      // Step 3C: if row != col, then set these elements to 0:
      //  (row+1,col), ..., (nrows-1,col)

      printf("*** in naive row,col = (%ld, %ld) ***\n", row, col);
      debug_out();

      // Step 1: Set the 'upper' values: (row,col)..(nrows-1,col)
      for (size_t r = row; r < nrows; r++)
        {
          for (size_t i = 0; i<row; i++)
            {
              mLU.ring().mult(tmp, mLU.entry(r,i), mLU.entry(i,col));
              mLU.ring().subtract(mLU.entry(r,col), mLU.entry(r,col), tmp);
            }
          // This is done at a different time          mLU.ring().divide(mLU.entry(r,col), mLU.entry(r,col), pivot);
        }

      printf("after step 1\n");
      debug_out();

      // Step 2: Find a pivot among the elements in step 1.
      //  If one: swap rows if needed
      //  If none, increment 'col', and continue at start of loop
      size_t k = findPivotNAIVE(row,col);
      if (k == static_cast<size_t>(-1))
        {
          col = col+1;
          continue;
        }
      printf("pivot is in row %ld\n", k);
      std::swap(mPerm[row], mPerm[k]);
      if (k != row) 
        {
          MatElementaryOps<Mat>::interchange_rows(mLU, k, row);
          mSign = !mSign;
        }
      const ElementType& pivot = mLU.entry(row,col);

      printf("after step 2:\n");
      debug_out();

      // Step 3A: Set the 'upper' elements in (row,col+1), ..., (row,ncols-1).
      for (size_t c = col+1; c < ncols; c++)
        {
          for (size_t i = 0; i<row; i++)
            {
              mLU.ring().mult(tmp, mLU.entry(row,i), mLU.entry(i,c));
              mLU.ring().subtract(mLU.entry(row,c), mLU.entry(row,c), tmp);
            }
        }
      printf("after step 3A:\n");
      debug_out();

      // Step 3B: Set the 'lower' elements in (row+1,col), ..., (nrows-1,col)
      // This just means dividing then by the pivot
      // except, if we have skipped columns for pivots, we must set these elements
      // in column 'row', not 'col'...
      for (size_t r = row+1; r < nrows; r++)
        {
          mLU.ring().divide(mLU.entry(r,row), mLU.entry(r,col), pivot);
          if (row < col) ring().set_zero(mLU.entry(r,col)); 
        }

      printf("after step 3B:\n");
      debug_out();

      row++;
      col++;
    }

  mLU.ring().clear(tmp);
}

template <class RingType>
void DMatLUtemplate<RingType>::setUpperLower(Mat& lower, Mat& upper)
{
  // MAJOR ASSUMPTION: the matrices lower, upper, and mLU are stored in row major order!

  M2_ASSERT(!mError);
  M2_ASSERT(mIsDone);

  lower.resize(mLU.numRows(), mLU.numRows());
  upper.resize(mLU.numRows(), mLU.numColumns());

  // At this point, lower and upper should be zero matrices.
  M2_ASSERT(MatrixOppies::isZero(lower));
  M2_ASSERT(MatrixOppies::isZero(upper));

  auto LU = mLU.rowMajorArray();
  auto L = lower.rowMajorArray();
  auto U = upper.rowMajorArray();

  for (size_t c=0; c<upper.numColumns(); c++)
    {
      auto U1 = U;
      for (size_t r=0; r<=c; r++)
        {
          if (r >= upper.numRows()) break;
          upper.ring().set(*U1, *LU++);
          U1 += upper.numColumns();
        }
      U++; // change to next column

      if (c < lower.numColumns())
        {
          lower.ring().set_from_int(*L, 1); // diagonal entry of L should be 1
          L += lower.numColumns(); // pointing to entry right below diagonal
          auto L1 = L; // will increment by lower.numRows() each loop here
          for (size_t r=c+1; r<lower.numRows(); r++)
            {
              lower.ring().set(*L1, *LU++);
              L1 += lower.numColumns(); // to place next entry.
            }
          L++; // change to next column
        }
    }
}

template <class RingType>
void DMatLUtemplate<RingType>::setUpperLowerNAIVE(Mat& lower, Mat& upper)
{

  // MAJOR ASSUMPTION: the matrices lower, upper, and mLU are stored in row major order!
  M2_ASSERT(!mError);
  M2_ASSERT(mIsDone);

  lower.resize(mLU.numRows(), mLU.numRows());
  upper.resize(mLU.numRows(), mLU.numColumns());

  // At this point, lower and upper should be zero matrices.
  M2_ASSERT(MatrixOppies::isZero(lower));
  M2_ASSERT(MatrixOppies::isZero(upper));

  for (size_t c=0; c<mLU.numColumns(); c++)
    {
      if (c < lower.numRows())
        ring().set_from_int(lower.entry(c,c), 1);
      for (size_t r=0; r<mLU.numRows(); r++)
        {
          if (r <= c)
            ring().set(upper.entry(r,c), mLU.entry(r,c));
          else if (c < lower.numRows())
            {
              ring().set(lower.entry(r,c), mLU.entry(r,c));
            }
        }
    }
}

template <class RingType>
bool DMatLUtemplate<RingType>::MatrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
{
  computeLUNAIVE();
  if (mError) return false;

  setUpperLowerNAIVE(L,U);
  for (size_t i=0; i<mPerm.size(); i++)
    P.push_back(mPerm[i]);

  return true;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
