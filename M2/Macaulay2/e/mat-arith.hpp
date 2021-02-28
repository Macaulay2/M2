// Copyright 2013  Michael E. Stillman

#ifndef _mat_arith_hpp_
#define _mat_arith_hpp_

template <typename MT>
class MatElementaryOps;
// template <typename MT> class MatArithmetic;
#include "dmat.hpp"
#include "smat.hpp"

// Use below via
//  MatrixWindow(first_row, first_col, #rows, #columns)
struct MatrixWindow
{
  long begin_row;
  long begin_column;
  long end_row;
  long end_column;
  MatrixWindow(long x, long y, long nrows, long ncols)
      : begin_row(x), begin_column(y), end_row(x + nrows), end_column(y + ncols)
  {
    // empty body OK
  }

  bool sameSize(const MatrixWindow& b) const
  {
    return (end_row - begin_row == b.end_row - b.begin_row) and
           (end_column - begin_column == b.end_column - b.begin_column);
  }
};

template <typename MatType>
struct SubMatrix
{
  MatType& matrix;
  const long begin_row;
  const long end_row;
  const long begin_column;
  const long end_column;

  SubMatrix(MatType& m)
      : matrix(m),
        begin_row(0),
        end_row(m.numRows()),
        begin_column(0),
        end_column(m.numColumns())
  {
  }

  SubMatrix(MatType& m, long x, long y, long nrows, long ncols)
      : matrix(m),
        begin_row(x),
        end_row(x + nrows),
        begin_column(y),
        end_column(y + ncols)
  {
  }

  bool sameSize(const SubMatrix& b) const
  {
    return (end_row - begin_row == b.end_row - b.begin_row) and
           (end_column - begin_column == b.end_column - b.begin_column);
  }

  void operator=(int x)
  {
    if (x == 0)
      {
        for (long rA = begin_row; rA < end_row; ++rA)
          for (size_t cA = begin_column; cA < end_column; ++cA)
            matrix.ring().set_zero(matrix.entry(rA, cA));
      }
  }

  void operator=(SubMatrix<MatType> src)
  {
    assert(sameSize(src));
    long rA = begin_row;
    long rB = src.begin_row;
    for (; rA < end_row; ++rA, ++rB)
      {
        long cA = begin_column;
        long cB = src.begin_column;
        for (; cA < end_column; ++cA, ++cB)
          matrix.ring().set(matrix.entry(rA, cA), src.matrix.entry(rB, cB));
      }
  }

  void operator+=(SubMatrix<MatType> src)
  {
    assert(sameSize(src));
    long rA = begin_row;
    long rB = src.begin_row;
    for (; rA < end_row; ++rA, ++rB)
      {
        long cA = begin_column;
        long cB = src.begin_column;
        for (; cA < end_column; ++cA, ++cB)
          {
            auto& a = matrix.entry(rA, cA);
            matrix.ring().add(a, a, src.matrix.entry(rB, cB));
          }
      }
  }

  template <typename ElementType>
  void addMultipleTo(ElementType& c, SubMatrix<MatType> src)
  {
    assert(sameSize(src));
    long rA = begin_row;
    long rB = src.begin_row;
    for (; rA < end_row; ++rA, ++rB)
      {
        long cA = begin_column;
        long cB = src.begin_column;
        for (; cA < end_column; ++cA, ++cB)
          matrix.ring().addMultipleTo(
              matrix.entry(rA, cA), c, src.matrix.entry(rB, cB));
      }
  }

  template <typename ElementType>
  void operator*=(ElementType& c)
  {
    for (long rA = begin_row; rA < end_row; ++rA)
      {
        for (long cA = begin_column; cA < end_column; ++cA)
          {
            auto& a = matrix.entry(rA, cA);
            matrix.ring().mult(a, a, c);
          }
      }
  }
};

template <typename MatType>
void normSquared(SubMatrix<MatType> M,
                 typename MatType::CoeffRing::RealElementType& result)
{
  const auto& C = M.matrix.ring();
  const auto& R = C.real_ring();
  typename MatType::CoeffRing::RealElementType c;
  R.init(c);
  R.set_zero(result);
  for (long rA = M.begin_row; rA < M.end_row; ++rA)
    for (long cA = M.begin_column; cA < M.end_column; ++cA)
      {
        C.abs_squared(c, M.matrix.entry(rA, cA));
        R.add(result, result, c);
      }
  R.clear(c);
}

template <typename MatType>
SubMatrix<MatType> submatrix(MatType& m)
{
  return SubMatrix<MatType>(m);
}
template <typename MatType>
SubMatrix<MatType> submatrix(MatType& m, long x, long y, long nrows, long ncols)
{
  return SubMatrix<MatType>(m, x, y, nrows, ncols);
}

namespace MatrixOps {
template <typename RT>
bool isZero(const DMat<RT>& A)
{
  size_t len = A.numRows() * A.numColumns();
  if (len == 0) return true;
  for (auto t = A.array() + len - 1; t >= A.array(); t--)
    if (!A.ring().is_zero(*t)) return false;
  return true;
}

template <typename RT>
bool isEqual(const DMat<RT>& A, const DMat<RT>& B)
{
  assert(&A.ring() == &B.ring());
  if (B.numRows() != A.numRows()) return false;
  if (B.numColumns() != A.numColumns()) return false;
  size_t top = A.numRows() * A.numColumns();
  auto elemsA = A.array();
  auto elemsB = B.array();
  for (size_t i = 0; i < top; i++)
    if (!A.ring().is_equal(*elemsA++, *elemsB++)) return false;
  return true;
}

template <typename RT>
void scalarMultInPlace(DMat<RT>& A, const typename RT::ElementType& f)
{
  for (size_t i = 0; i < A.numRows() * A.numColumns(); i++)
    {
      A.ring().mult(A.array()[i], f, A.array()[i]);
    }
}

template <typename RT>
void negateInPlace(DMat<RT>& A)
// A = -A
{
  size_t len = A.numRows() * A.numColumns();
  for (size_t i = 0; i < len; i++)
    {
      A.ring().negate(A.array()[i], A.array()[i]);
    }
}

template <typename RT>
void addInPlace(DMat<RT>& A, const DMat<RT>& B)
// A += B.
{
  assert(&B.ring() == &A.ring());
  assert(B.numRows() == A.numRows());
  assert(B.numColumns() == A.numColumns());

  size_t len = A.numRows() * A.numColumns();
  for (size_t i = 0; i < len; i++)
    {
      A.ring().add(A.array()[i], A.array()[i], B.array()[i]);
    }
}

template <typename RT>
void subtractInPlace(DMat<RT>& A, const DMat<RT>& B)
// A -= B
{
  assert(&B.ring() == &A.ring());
  assert(B.numRows() == A.numRows());
  assert(B.numColumns() == A.numColumns());

  size_t len = A.numRows() * A.numColumns();
  for (size_t i = 0; i < len; i++)
    {
      A.ring().subtract(A.array()[i], A.array()[i], B.array()[i]);
    }
}

template <typename RT>
void transpose(const DMat<RT>& A, DMat<RT>& result)
{
  assert(&A != &result);  // these cannot be aliased!
  assert(result.numRows() == A.numColumns());
  assert(result.numColumns() == A.numRows());
  for (size_t c = 0; c < A.numColumns(); ++c)
    {
      auto i = A.columnBegin(c);
      auto j = result.rowBegin(c);
      auto end = A.columnEnd(c);
      for (; i != end; ++i, ++j) A.ring().set(*j, *i);
    }
}

//  wA = 0
template <typename RT>
void setZero(DMat<RT>& A, MatrixWindow wA)
{
  for (long rA = wA.begin_row; rA < wA.end_row; ++rA)
    for (size_t cA = wA.begin_column; cA < wA.end_column; ++cA)
      A.ring().set_zero(A.entry(rA, cA));
}

template <typename MatType>
void setZero(SubMatrix<MatType> A)
{
  auto& M = A.matrix;
  for (long rA = A.begin_row; rA < A.end_row; ++rA)
    for (size_t cA = A.begin_column; cA < A.end_column; ++cA)
      M.ring().set_zero(M.entry(rA, cA));
}

// wA = wB
template <typename RT>
void set(DMat<RT>& A, MatrixWindow wA, const DMat<RT>& B, MatrixWindow wB)
{
  assert(wA.sameSize(wB));
  long rA = wA.begin_row;
  long rB = wB.begin_row;
  for (; rA < wA.end_row; ++rA, ++rB)
    {
      long cA = wA.begin_column;
      long cB = wB.begin_column;
      for (; cA < wA.end_column; ++cA, ++cB)
        A.ring().set(A.entry(rA, cA), B.entry(rB, cB));
    }
}

// wA += wB
template <typename RT>
void addTo(DMat<RT>& A, MatrixWindow wA, const DMat<RT>& B, MatrixWindow wB)
{
  assert(wA.sameSize(wB));
  long rA = wA.begin_row;
  long rB = wB.begin_row;
  for (; rA < wA.end_row; ++rA, ++rB)
    {
      long cA = wA.begin_column;
      long cB = wB.begin_column;
      for (; cA < wA.end_column; ++cA, ++cB)
        {
          auto& a = A.entry(rA, cA);
          A.ring().add(a, a, B.entry(rB, cB));
        }
    }
}

// wA += c * wB
template <typename RT>
void addMultipleTo(DMat<RT>& A,
                   MatrixWindow wA,
                   const typename RT::ElementType& c,
                   const DMat<RT>& B,
                   MatrixWindow wB)
{
  assert(wA.sameSize(wB));
  typename RT::ElementType tmp;
  A.ring().init(tmp);
  long rA = wA.begin_row;
  long rB = wB.begin_row;
  for (; rA < wA.end_row; ++rA, ++rB)
    {
      long cA = wA.begin_column;
      long cB = wB.begin_column;
      for (; cA < wA.end_column; ++cA, ++cB)
        {
          A.ring().mult(tmp, c, B.entry(rB, cB));
          auto& a = A.entry(rA, cA);
          A.ring().add(a, a, tmp);
        }
    }
  A.ring().clear(tmp);
}

// wA = c * wB
template <typename RT>
void scalarMult(DMat<RT>& A,
                MatrixWindow wA,
                const typename RT::ElementType& c,
                const DMat<RT>& B,
                MatrixWindow wB)
{
  assert(wA.sameSize(wB));
  long rA = wA.begin_row;
  long rB = wB.begin_row;
  for (; rA < wA.end_row; ++rA, ++rB)
    {
      long cA = wA.begin_column;
      long cB = wB.begin_column;
      for (; cA < wA.end_column; ++cA, ++cB)
        {
          A.ring().mult(A.entry(rA, cA), c, B.entry(rB, cB));
        }
    }
}

// wA *= c
template <typename RT>
void scalarMultInPlace(DMat<RT>& A,
                       MatrixWindow wA,
                       const typename RT::ElementType& c)
{
  long rA = wA.begin_row;
  for (; rA < wA.end_row; ++rA)
    {
      long cA = wA.begin_column;
      for (; cA < wA.end_column; ++cA)
        {
          auto& a = A.entry(rA, cA);
          A.ring().mult(a, a, c);
        }
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
void scalarMultInPlace(SMat<RT>& A, const typename RT::ElementType& f)
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
void addInPlace(SMat<RT>& A, const SMat<RT>& B)
{
  assert(&B.ring() == &A.ring());
  assert(B.numRows() == A.numRows());
  assert(B.numColumns() == A.numColumns());

  A.addInPlace(B);
}

template <typename RT>
void subtractInPlace(SMat<RT>& A, const SMat<RT>& B)
// A -= B
{
  assert(&B.ring() == &A.ring());
  assert(B.numRows() == A.numRows());
  assert(B.numColumns() == A.numColumns());

  A.subtractInPlace(B);
}

template <typename RT>
void transpose(const SMat<RT>& A, SMat<RT>& result)
{
  // result should be the 0 matrix of the correct size.
  assert(&A != &result);  // these cannot be aliased!
  assert(result.numRows() == A.numColumns());
  assert(result.numColumns() == A.numRows());
  throw exc::engine_error(
      "'transpose' not written for sparse mutable matrices");
  // TODO: MES: write this!!
}
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
