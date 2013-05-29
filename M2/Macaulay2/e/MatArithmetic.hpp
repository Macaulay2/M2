// Copyright 2013  Michael E. Stillman

#ifndef _mat_arithmetic_hpp_
#define _mat_arithmetic_hpp_

template <typename MT> class MatElementaryOps;
template <typename MT> class MatArithmetic;
#include "dmat.hpp"
#include "smat.hpp"

template <typename RT>
class MatArithmetic< DMat<RT> >
{
public:
  typedef DMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;

  static bool isZero(const Mat& A)
  {
    size_t len = A.numRows() * A.numColumns();
    if (len == 0) return true;
    for (const ElementType *t = A.array() + len - 1; t >= A.array(); t--)
      if (!A.ring().is_zero(*t))
        return false;
    return true;
  }

  static bool isEqual(const Mat& A, const Mat& B)
  {
    M2_ASSERT(&A.ring() == &B.ring());
    if (B.numRows() != A.numRows()) return false;
    if (B.numColumns() != A.numColumns()) return false;
    size_t top = A.numRows() * A.numColumns();
    const ElementType * elemsA = A.array();
    const ElementType * elemsB = B.array();
    for (size_t i = 0; i < top; i++)
      if (!A.ring().is_equal(*elemsA++, *elemsB++))
        return false;
    return true;
  }

  static void scalarMultInPlace(Mat& A, const ElementType &f)
  {
    for (size_t i=0; i<A.numRows()*A.numColumns(); i++)
      {
        A.ring().mult(A.array()[i], f, A.array()[i]);
      }
  }

  static void negateInPlace(Mat& A)
  // A = -A
  {
    size_t len = A.numRows() * A.numColumns();
    for (size_t i=0; i<len; i++)
      {
        A.ring().negate(A.array()[i], A.array()[i]);
      }
  }

  static void addInPlace(Mat&A, const Mat& B)
  // A += B.
  {
    M2_ASSERT(&B.ring() == &A.ring());
    M2_ASSERT(B.numRows() == A.numRows());
    M2_ASSERT(B.numColumns() == A.numColumns());
    
    size_t len = A.numRows() * A.numColumns();
    for (size_t i=0; i<len; i++)
      {
        A.ring().add(A.array()[i], A.array()[i], B.array()[i]);
      }
  }

  static void subtractInPlace(Mat& A, const Mat& B)
  // A -= B
  {
    M2_ASSERT(&B.ring() == &A.ring());
    M2_ASSERT(B.numRows() == A.numRows());
    M2_ASSERT(B.numColumns() == A.numColumns());
    
    size_t len = A.numRows() * A.numColumns();
    for (size_t i=0; i<len; i++)
      {
        A.ring().subtract(A.array()[i], A.array()[i], B.array()[i]);
      }
  }
  
};

template <typename RT>
class MatArithmetic< SMat<RT> >
{
public:
  typedef SMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;

  static bool isZero(const Mat& A)
  {
    return A.is_zero();
  }

  static bool isEqual(const Mat& A, const Mat& B)
  {
    return A.is_equal(B);
  }

  static void scalarMultInPlace(Mat& A, const ElementType &f)
  // A = f*A
  {
    A.scalarMultInPlace(f);
  }

  static void negateInPlace(Mat& A)
  // A = -A
  {
    A.negateInPlace();
  }

  static void addInPlace(Mat&A, const Mat& B) 
  {
    M2_ASSERT(&B.ring() == &A.ring());
    M2_ASSERT(B.numRows() == A.numRows());
    M2_ASSERT(B.numColumns() == A.numColumns());

    A.addInPlace(B);
  }

  static void subtractInPlace(Mat& A, const Mat& B)
  // A -= B
  {
    M2_ASSERT(&B.ring() == &A.ring());
    M2_ASSERT(B.numRows() == A.numRows());
    M2_ASSERT(B.numColumns() == A.numColumns());

    A.subtractInPlace(B);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
