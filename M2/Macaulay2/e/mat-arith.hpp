// Copyright 2013  Michael E. Stillman

#ifndef _mat_arith_hpp_
#define _mat_arith_hpp_

template <typename MT> class MatElementaryOps;
//template <typename MT> class MatArithmetic;
#include "dmat.hpp"
#include "smat.hpp"

namespace MatrixOppies
{
  template <typename RT>
  bool isZero(const DMat<RT>& A)
  {
    size_t len = A.numRows() * A.numColumns();
    if (len == 0) return true;
    for (auto t = A.array() + len - 1; t >= A.array(); t--)
      if (!A.ring().is_zero(*t))
        return false;
    return true;
  }

  template <typename RT>
  bool isEqual(const DMat<RT>& A, const DMat<RT>& B)
  {
    M2_ASSERT(&A.ring() == &B.ring());
    if (B.numRows() != A.numRows()) return false;
    if (B.numColumns() != A.numColumns()) return false;
    size_t top = A.numRows() * A.numColumns();
    auto elemsA = A.array();
    auto elemsB = B.array();
    for (size_t i = 0; i < top; i++)
      if (!A.ring().is_equal(*elemsA++, *elemsB++))
        return false;
    return true;
  }

  template <typename RT>
  void scalarMultInPlace(DMat<RT>& A, const typename RT::ElementType &f)
  {
    for (size_t i=0; i<A.numRows()*A.numColumns(); i++)
      {
        A.ring().mult(A.array()[i], f, A.array()[i]);
      }
  }

  template <typename RT>
  void negateInPlace(DMat<RT>& A)
  // A = -A
  {
    size_t len = A.numRows() * A.numColumns();
    for (size_t i=0; i<len; i++)
      {
        A.ring().negate(A.array()[i], A.array()[i]);
      }
  }

  template <typename RT>
  void addInPlace(DMat<RT>&A, const DMat<RT>& B)
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

  template <typename RT>
  void subtractInPlace(DMat<RT>& A, const DMat<RT>& B)
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

  template <typename RT>
  void transpose(const DMat<RT>& A, DMat<RT>& result)
  {
    M2_ASSERT(&A != &result);  // these cannot be aliased!
    M2_ASSERT(result.numRows() == A.numColumns());
    M2_ASSERT(result.numColumns() == A.numRows());
    for (size_t c = 0; c < A.numColumns(); ++c)
      {
        auto i = A.columnBegin(c);
        auto j = result.rowBegin(c);
        auto end = A.columnEnd(c);
        for ( ; i != end; ++i, ++j)
          A.ring().set(*j, *i);
      }
  }

///////////////////////////////////////////////////////////
// Sparse matrix template routines for matrix operations //
///////////////////////////////////////////////////////////

  template <typename RT>
  bool isZero(const SMat<RT>& A)
  {
    return A.is_zero();
  }

  template <typename RT>
  bool isEqual(const SMat<RT>& A, const SMat<RT>& B)
  {
    return A.is_equal(B);
  }

  template <typename RT>
  void scalarMultInPlace(SMat<RT>& A, const typename RT::ElementType &f)
  // A = f*A
  {
    A.scalarMultInPlace(f);
  }

  template <typename RT>
  void negateInPlace(SMat<RT>& A)
  // A = -A
  {
    A.negateInPlace();
  }

  template <typename RT>
  void addInPlace(SMat<RT>&A, const SMat<RT>& B) 
  {
    M2_ASSERT(&B.ring() == &A.ring());
    M2_ASSERT(B.numRows() == A.numRows());
    M2_ASSERT(B.numColumns() == A.numColumns());

    A.addInPlace(B);
  }

  template <typename RT>
  void subtractInPlace(SMat<RT>& A, const SMat<RT>& B)
  // A -= B
  {
    M2_ASSERT(&B.ring() == &A.ring());
    M2_ASSERT(B.numRows() == A.numRows());
    M2_ASSERT(B.numColumns() == A.numColumns());

    A.subtractInPlace(B);
  }

  template <typename RT>
  void transpose(const SMat<RT>& A, SMat<RT>& result)
  {
    // result should be the 0 matrix of the correct size.
    M2_ASSERT(&A != &result);  // these cannot be aliased!
    M2_ASSERT(result.numRows() == A.numColumns());
    M2_ASSERT(result.numColumns() == A.numRows());
    throw exc::engine_error("'transpose' not writtten for sparse mutable matrices");
    //TODO: MES: write this!!
  }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
