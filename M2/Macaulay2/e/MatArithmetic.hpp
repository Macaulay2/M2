// Copyright 2013  Michael E. Stillman

#ifndef _mat_arithmetic_hpp_
#define _mat_arithmetic_hpp_

template <typename MT> class MatElementaryOps;
#include "dmat.hpp"
#include "smat.hpp"

template <typename RT>
class MatArithmetic< DMat<RT> >
{
public:
  typedef DMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;

  static void addInPlace(Mat&A, const Mat& B)
  // A += B.
  {
    M2_ASSERT(&B.ring() == &A.ring());
    M2_ASSERT(B.numRows() == A.numRows());
    M2_ASSERT(B.numColumns() == A.numColumns());
    
    for (size_t i=0; i<A.numRows()*A.numColumns(); i++)
      {
        A.ring().add(A.array()[i], A.array()[i], B.array()[i]);
      }
  }

};

template <typename RT>
class MatArithmetic< SMat<RT> >
{
public:
  typedef SMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;

  static void addInPlace(Mat&A, const Mat& B) 
  {
    A.addInPlace(B);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
