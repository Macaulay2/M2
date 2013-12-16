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

  // The following information is stored, but could be regenerated when needed
  std::vector<size_t> mPivotColumns;

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

  void computePivotColumns(); // Sets mPivotColumns, assumes LU has been computed

  void debug_out()
  {
    buffer o;
    displayMat(o,mLU);
    std::cout << o.str() << std::endl;
  }

  void debug_out_list(ElementType* x, size_t len)
  {
    buffer o;
    o << "[ ";
    for (size_t i=0; i<len; i++)
      {
        ring().elem_text_out(o, x[i], true, false);
        o << " ";
      }
    o << "]" << newline;
    std::cout << o.str();
  }
};

template <class RingType>
DMatLUtemplate<RingType>::DMatLUtemplate(const Mat& A)
  : mLU(A),  // copies A
    mSign(true), // sign = 1
    mIsDone(false),
    mError(false)
{
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

template <>
inline size_t DMatLUtemplate<M2::ARingRRR>::findPivotNAIVE(size_t row, size_t col)
{
  // Look at elements A[row,col], A[row+1,col], ..., A[nrows-1, col]
  // Return the index r s.y. abs(A[r,col]) is maximum over all of these

  ElementType largest;
  ElementType abs;
  size_t best_row_so_far = static_cast<size_t>(-1);

  ring().init(largest);
  ring().init(abs);
  ring().set_zero(largest);
  
  for (size_t i=row; i<mLU.numRows(); i++)
    {
      ring().abs(abs, mLU.entry(i,col));
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
inline size_t DMatLUtemplate<M2::ARingCCC>::findPivotNAIVE(size_t row, size_t col)
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
  
  for (size_t i=row; i<mLU.numRows(); i++)
    {
      ring().abs(abs, mLU.entry(i,col));
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
  //TODO: this is likely all wrong, and needs testing

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

  //  std::cout << "computing LU decomposition NAIVE version" << std::endl;
  ElementType tmp;
  mLU.ring().init(tmp);

  size_t col = 0; // current column we are working on
  size_t row = 0; // current row we are working on
  size_t nrows = mLU.numRows();
  size_t ncols = mLU.numColumns();

  while (col < ncols && row < nrows)
    {
      // printf("*** in naive row,col = (%ld, %ld) ***\n", row, col);
      // debug_out();

      // Step 1: Set the 'upper' values: (row,col)..(nrows-1,col)
      for (size_t r = row; r < nrows; r++)
        {
          for (size_t i = 0; i<row; i++)
            {
              mLU.ring().mult(tmp, mLU.entry(r,i), mLU.entry(i,col));
              mLU.ring().subtract(mLU.entry(r,col), mLU.entry(r,col), tmp);
            }
        }

      // printf("after step 1\n");
      // debug_out();

      // Step 2: Find a pivot among the elements in step 1.
      //  If one: swap rows if needed
      //  If none, increment 'col', and continue at start of loop
      size_t k = findPivotNAIVE(row,col);
      if (k == static_cast<size_t>(-1))
        {
          col = col+1;
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
      const ElementType& pivot = mLU.entry(row,col);

      // printf("after step 2:\n");
      // debug_out();

      // Step 3A: Set the 'upper' elements in (row,col+1), ..., (row,ncols-1).
      for (size_t c = col+1; c < ncols; c++)
        {
          for (size_t i = 0; i<row; i++)
            {
              mLU.ring().mult(tmp, mLU.entry(row,i), mLU.entry(i,c));
              mLU.ring().subtract(mLU.entry(row,c), mLU.entry(row,c), tmp);
            }
        }
      // printf("after step 3A:\n");
      // debug_out();

      // Step 3B: Set the 'lower' elements in (row+1,row), ..., (nrows-1,row)
      //  from (row+1,col), ..., (nrows-1,col)
      // This just means dividing then by the pivot
      // except, if we have skipped columns for pivots, we must set these elements
      // in column 'row', not 'col'...
      // Step 3C: if row != col, then set these elements to 0:
      //  (row+1,col), ..., (nrows-1,col)
      for (size_t r = row+1; r < nrows; r++)
        {
          mLU.ring().divide(mLU.entry(r,row), mLU.entry(r,col), pivot);
          if (row < col) ring().set_zero(mLU.entry(r,col)); 
        }

      // printf("after step 3B:\n");
      // debug_out();

      row++;
      col++;
    }

  mIsDone = true;
  mLU.ring().clear(tmp);
}

template <class RingType>
void DMatLUtemplate<RingType>::setUpperLower(Mat& lower, Mat& upper)
{
  // NOT TESTED YET!!
  // MAJOR ASSUMPTION: the matrices lower, upper, and mLU are stored in row major order!

  M2_ASSERT(!mError);
  M2_ASSERT(mIsDone);

  size_t min = std::min(mLU.numRows(), mLU.numColumns());
  lower.resize(mLU.numRows(), min);
  upper.resize(min, mLU.numColumns());

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

  size_t min = std::min(mLU.numRows(), mLU.numColumns());
  lower.resize(mLU.numRows(), min);
  upper.resize(min, mLU.numColumns());

  // At this point, lower and upper should be zero matrices.
  M2_ASSERT(MatrixOppies::isZero(lower));
  M2_ASSERT(MatrixOppies::isZero(upper));

  for (size_t c=0; c<mLU.numColumns(); c++)
    {
      if (c < min)
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

template <class RingType>
bool DMatLUtemplate<RingType>::determinant(ElementType& result)
{
  computeLUNAIVE();
  if (mError) return false;
  // This is just the product of the diagonal entries of mLU.
  M2_ASSERT(mLU.numRows() == mLU.numColumns());

  if (mSign)
    ring().set_from_int(result, 1);
  else
    ring().set_from_int(result, -1);
  for (size_t i=0; i<mLU.numRows(); i++)
    ring().mult(result, result, mLU.entry(i,i));

  return true;
}

template <class RingType>
void DMatLUtemplate<RingType>::computePivotColumns()
{
  M2_ASSERT(mIsDone); // This should only be called once an LU decomposition is complete

  mPivotColumns.clear();
  size_t thiscol = 0;
  size_t thisrow = 0;
  while (thisrow < mLU.numRows() 
         and thiscol < mLU.numColumns())
    {
      if (not ring().is_zero(mLU.entry(thisrow, thiscol)))
        {
          mPivotColumns.push_back(thiscol);
          thisrow++;
        }
      thiscol++;
    }
}

template <class RingType>
bool DMatLUtemplate<RingType>::columnRankProfile(std::vector<size_t>& profile)
{
  computeLUNAIVE();
  if (mError) return false;

  for (size_t i=0; i<mPivotColumns.size(); i++)
    profile.push_back(mPivotColumns[i]);
  return true;

  size_t row = 0;
  size_t col = 0;
  while (row < mLU.numRows() && col < mLU.numColumns())
    {
      if (!ring().is_zero(mLU.entry(row,col)))
        {
          profile.push_back(col);
          row++;
        }
      col++;
    }
  return true;
}

template <class RingType>
size_t DMatLUtemplate<RingType>::rank()
{
  computeLUNAIVE();
  if (mError) return static_cast<size_t>(-1);

  return mPivotColumns.size();
}

template <class RingType>
bool DMatLUtemplate<RingType>::solve(const Mat& B, Mat& X)
{
  computeLUNAIVE();
  if (mError) return false;

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

  // printf("entering DMatLUtemplate::solve\n");
  size_t rk = mPivotColumns.size();

  ElementType tmp, tmp2;
  ring().init(tmp);
  ring().init(tmp2);

  ElementType* b = newarray(ElementType, mLU.numRows());
  ElementType* y = newarray(ElementType, rk);
  ElementType* x = newarray(ElementType, mLU.numColumns());

  for (size_t i=0; i<mLU.numRows(); i++)
    ring().init(b[i]);
  for (size_t i=0; i<rk; i++)
    ring().init(y[i]);
  for (size_t i=0; i<mLU.numColumns(); i++)
    ring().init(x[i]);

  X.resize(mLU.numColumns(), B.numColumns());

  for (size_t col = 0; col < B.numColumns(); col++)
    {
      // Step 0: erase x (b will be set below, y is also set when needed).

      for (size_t i=0; i<mLU.numColumns(); i++)
        ring().set_zero(x[i]);

      // Step 1: set b to be the permuted i-th column of B.
      for (size_t r = 0; r<B.numRows(); r++)
        ring().set(b[r], B.entry(mPerm[r],col));

      /// printf("b:\n");
      /// debug_out_list(b, mLU.numRows());

      // Step 2: Solve Ly=b
      for (size_t i=0; i<rk; i++)
        {
          ring().set(y[i], b[i]);
          for (size_t j=0; j<i; j++)
            {
              ring().mult(tmp, mLU.entry(i,j), y[j]);
              ring().subtract(y[i], y[i], tmp);
            }
        }

      /// printf("y:\n");
      /// debug_out_list(y, rk);

      // Step 2B: see if the solution is consistent
      for (size_t i=rk; i<mLU.numRows(); i++)
        {
          ring().set(tmp, b[i]);
          for (size_t j=0; j<rk; j++)
            {
              ring().mult(tmp2, mLU.entry(i,j), y[j]);
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
              printf("returning false\n");
              return false;
            }
        }

      ///      printf("past test for consistency\n");

      // Step 3: Solve Ux=y
      // and place x back into X as col-th column
      for (long i=rk-1; i>=0; --i)
        {
          ring().set(x[i], y[i]);
          for (size_t j=i+1; j<=rk-1; j++)
            {
              ring().mult(tmp, mLU.entry(i,mPivotColumns[j]), x[j]);
              ring().subtract(x[i], x[i], tmp);
            }
          ring().divide(x[i], x[i], mLU.entry(i,mPivotColumns[i]));
          ring().set(X.entry(mPivotColumns[i], col), x[i]);

          /// buffer o;
          /// printf("after i=%ld\n", i);
          /// displayMat(o, X);
          /// printf("%s\n", o.str());
        }
      /// printf("x:\n");
      /// debug_out_list(x, mLU.numColumns());


      /// buffer o;
      /// printf("after col=%ld\n", col);
      /// displayMat(o, X);
      /// printf("%s\n", o.str());
    }
  ring().clear(tmp);
  ring().clear(tmp2);

  for (size_t i=0; i<mLU.numRows(); i++)
    ring().clear(b[i]);
  for (size_t i=0; i<rk; i++)
    ring().clear(y[i]);
  for (size_t i=0; i<mLU.numColumns(); i++)
    ring().clear(x[i]);
  deletearray(b);
  deletearray(y);
  deletearray(x);
  return true; // The system seems to have been consistent
}

template <class RingType>
bool DMatLUtemplate<RingType>::inverse(Mat& X)
{
  computeLUNAIVE();
  if (mError) return false;

  // Make the identity matrix
  Mat id(ring(), mLU.numRows(), mLU.numRows());
  for (size_t i=0; i<mLU.numRows(); i++)
    ring().set_from_int(id.entry(i,i), 1);

  solve(id, X);
  return true;
}

template <class RingType>
bool DMatLUtemplate<RingType>::kernel(Mat& X)
{
  // THIS SHOULD NOT BE SO PAINFUL!!

  computeLUNAIVE();
  if (mError) return false;

  ElementType tmp, tmp2;
  ring().init(tmp);
  ring().init(tmp2);

  // First, let's set X to be the correct size:
  X.resize(mLU.numColumns(), mLU.numColumns() - mPivotColumns.size());

  size_t col = 0;
  size_t nextpivotidx = 0;
  size_t nextpivotcol = (mPivotColumns.size()>0 ? mPivotColumns[0] : mLU.numColumns());
  size_t colX = 0;
  while (colX < X.numColumns())
    {
      if (col == nextpivotcol)
        {
          col++;
          nextpivotidx++;
          nextpivotcol = (nextpivotidx < mPivotColumns.size() ? mPivotColumns[nextpivotidx] : mLU.numColumns());
          continue;
        }
      // At this point, we are ready to create a column of X.
      ring().set_from_int(X.entry(col,colX), -1);
      // Now we loop through and set the elements in the rows of X = pivot columns.
      for (long p = nextpivotidx-1; p >= 0; p--)
        {
          // set X.entry(mPivotColumns[p], colX)
          ring().set(tmp, mLU.entry(p, col));
          for (size_t i=nextpivotidx-1; i>=p+1; i--)
            {
              ring().mult(tmp2, mLU.entry(p,mPivotColumns[i]), X.entry(mPivotColumns[i],colX));
              ring().subtract(tmp, tmp, tmp2);
            }
          ring().divide(tmp, tmp, mLU.entry(p, mPivotColumns[p]));
          ring().set(X.entry(mPivotColumns[p],colX), tmp);
        }
      colX++;
      col++;
    }

  ring().clear(tmp);
  ring().clear(tmp2);
  return true;
}

template <>
inline void DMatLUtemplate<M2::ARingRR>::computeLUNAIVE()
{
  if (mIsDone) return;

  int rows = static_cast<int>(mLU.numRows());
  int cols = static_cast<int>(mLU.numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  //printf("entering DMatLUtemplate::computeLUNaive for RR\n");

  int *perm = newarray_atomic(int, min);

  double* copyA = newarray_atomic(double, mLU.numRows() * mLU.numColumns());

  // place all elements of mLU, but in column major order.
  double* p = copyA;
  for (size_t c=0; c<mLU.numColumns(); c++) 
    {
      auto end = mLU.columnEnd(c);
      for (auto a=mLU.columnBegin(c); a!=end; ++a)
        *p++ = *a;
    }

  dgetrf_(&rows, &cols, copyA,
          &rows, perm, &info);

  if (info < 0)
    {
      // First, clean up, then throw an exception
      ERROR("argument passed to dgetrf had an illegal value");
      mError = true;
      return;
    }

  // Now copy back to row major order
  p = copyA;
  for (size_t c=0; c<mLU.numColumns(); c++) 
    {
      auto end = mLU.columnEnd(c);
      for (auto a=mLU.columnBegin(c); a!=end; ++a)
        *a = *p++;
    }

  // Now place the correct permutation into mPerm
  for (int i=0; i<min; i++)
    {
      int thisloc = perm[i]-1;
      size_t tmp = mPerm[thisloc];
      mPerm[thisloc] = mPerm[i];
      mPerm[i] = tmp;
      mSign = not mSign;
    }

  mIsDone = true;
  computePivotColumns();

  deletearray(perm);
  deletearray(copyA);
}

template <>
inline void DMatLUtemplate<M2::ARingCC>::computeLUNAIVE()
{
  if (mIsDone) return;

  int rows = static_cast<int>(mLU.numRows());
  int cols = static_cast<int>(mLU.numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  //printf("entering DMatLUtemplate::computeLUNaive for RR\n");

  int *perm = newarray_atomic(int, min);

  double* copyA = newarray_atomic(double, 2 * mLU.numRows() * mLU.numColumns());

  // place all elements of mLU, but in column major order.
  double* p = copyA;
  for (size_t c=0; c<mLU.numColumns(); c++) 
    {
      auto end = mLU.columnEnd(c);
      for (auto a=mLU.columnBegin(c); a!=end; ++a)
        {
          *p++ = (*a).re;
          *p++ = (*a).im;
        }
    }

  zgetrf_(&rows, &cols, copyA,
          &rows, perm, &info);

  if (info < 0)
    {
      // First, clean up, then throw an exception
      ERROR("argument passed to zgetrf had an illegal value");
      mError = true;
      return;
    }

  // Now copy back to row major order
  p = copyA;
  for (size_t c=0; c<mLU.numColumns(); c++) 
    {
      auto end = mLU.columnEnd(c);
      for (auto a=mLU.columnBegin(c); a!=end; ++a)
        {
          (*a).re = *p++;
          (*a).im = *p++;
        }
    }

  // Now place the correct permutation into mPerm
  for (int i=0; i<min; i++)
    {
      int thisloc = perm[i]-1;
      size_t tmp = mPerm[thisloc];
      mPerm[thisloc] = mPerm[i];
      mPerm[i] = tmp;
      mSign = not mSign;
    }

  mIsDone = true;
  computePivotColumns();

  deletearray(perm);
  deletearray(copyA);
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
