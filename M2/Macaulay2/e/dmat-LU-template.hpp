// Copyright 2013  Michael E. Stillman

#ifndef _dmat_LU_template_hpp_
#define _dmat_LU_template_hpp_

#include "engine.h"
#include "dmat.hpp"

template <class RingType>
class DMatLUtemplate
{
public:
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;

private:
  Mat mLU;
  std::vector<size_t>& mPerm;
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

  // compute the LU decomposition, if mIsDone is not set, and then set it.
  // This sets mError if there is a problem.
  void computeLU();

  bool setUpperLower(Mat& lower, Mat& upper);
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
void DMatLUtemplate<RingType>::computeLU()
{
  if (mIsDone) return;

  //TODO: now do the rest of the decomposition
}

template <class RingType>
bool DMatLUtemplate<RingType>::setUpperLower(Mat& lower, Mat& upper)
{
  // MAJOR ASSUMPTION: the matrices lower, upper, and mLU are stored in row major order!

  M2_ASSERT(!mError);
  M2_ASSERT(mIsDone);

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
bool DMatLUtemplate<RingType>::MatrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
{
  computeLU();
  if (mError) return false;

  //TODO: make L and U into the right shapes, make sure they are zero?
  setUpperLower(L,U);
  //TODO: copy mPerm into P.

  return true;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
