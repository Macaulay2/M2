// Copyright 2013  Michael E. Stillman

#ifndef _dmat_LU_template_hpp_
#define _dmat_LU_template_hpp_

#include "engine.h"
#include "dmat.hpp"

template <class RingType>
class DMatLUtemplate
{
public:
  typedef RingType::ElementType ElementType;
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
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
