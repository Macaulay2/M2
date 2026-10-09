// Copyright 2026, The Macaulay2 Authors.
//
// ARingMatrixGenerator: the matrix analogue of ARingElementGenerator, for
// DMat<R> and SMat<R> over an ARing.  Like the element generator, it fills an
// already-constructed object in place and inherits the deterministic-prefix
// -then-random contract from getElement<>: the first 49 element draws are the
// small integers -24..24, later draws are random.
//
//   DMat<M2::ARingZZp> M(R, 5, 5);
//   ARingMatrixGenerator<DMat<M2::ARingZZp>> gen(R);
//   gen.nextMatrix(M);                                // dense
//   gen.nextMatrix(M, MatrixShape::SkewSymmetric);
//
// NOTE: this header includes ARingTest.hpp, which shares an include guard with
// RingTest.hpp.  It therefore cannot be combined with MatrixTest.hpp in one
// translation unit.

#ifndef M2_UNITTESTS__ARING_MATRIX_TEST_HPP__
#define M2_UNITTESTS__ARING_MATRIX_TEST_HPP__

#include <gtest/gtest.h>

#include <cassert>
#include <vector>

#include "unit-tests/ARingTest.hpp"
#include "unit-tests/MatrixShape.hpp"
#include "basic-mutable-matrices/dmat.hpp"
#include "basic-mutable-matrices/smat.hpp"
#include "basic-mutable-matrices/mat-arith.hpp"

// DMat hands out a mutable entry reference; SMat only has set_entry.  In an
// ARing, CoeffRing::elem and CoeffRing::ElementType are the same typedef, so
// one trait covers both.  The primary template is left undefined so that an
// unsupported matrix type gives a readable compile error.
template <typename MatType>
struct MatrixEntrySetter;

template <typename ACoeffRing>
struct MatrixEntrySetter<DMat<ACoeffRing>>
{
  typedef typename ACoeffRing::ElementType ElementType;
  static void set(DMat<ACoeffRing>& M,
                  size_t r,
                  size_t c,
                  const ElementType& a)
  {
    M.ring().copy(M.entry(r, c), a);
  }

  // 'result' must already be init'ed by the caller.
  static void get(const DMat<ACoeffRing>& M,
                  size_t r,
                  size_t c,
                  ElementType& result)
  {
    M.ring().copy(result, M.entry(r, c));
  }

  static bool isZeroEntry(const DMat<ACoeffRing>& M, size_t r, size_t c)
  {
    return M.ring().is_zero(M.entry(r, c));
  }
};

template <typename ACoeffRing>
struct MatrixEntrySetter<SMat<ACoeffRing>>
{
  typedef typename ACoeffRing::ElementType ElementType;
  static void set(SMat<ACoeffRing>& M,
                  size_t r,
                  size_t c,
                  const ElementType& a)
  {
    M.set_entry(r, c, a);
  }

  // SMat has no entry(), so this is the only way to inspect it -- and
  // displayMat()/submatrix() from mat-util.hpp and mat-arith.hpp do not work
  // on SMat for the same reason, despite what mat-util.hpp's comment says.
  // SMat::vec_get_entry leaves 'result' UNTOUCHED when it returns false
  // (smat.hpp:304-317), unlike MutableMatrix::get_entry, so zero it first.
  static void get(const SMat<ACoeffRing>& M,
                  size_t r,
                  size_t c,
                  ElementType& result)
  {
    M.ring().set_zero(result);
    M.get_entry(r, c, result);
  }

  static bool isZeroEntry(const SMat<ACoeffRing>& M, size_t r, size_t c)
  {
    typename ACoeffRing::Element e(M.ring());
    get(M, r, c, e);
    return M.ring().is_zero(e);
  }
};

template <typename MatType>
class ARingMatrixGenerator
{
 public:
  typedef typename MatType::CoeffRing CoeffRing;
  typedef typename MatType::ElementType ElementType;
  typedef typename CoeffRing::Element Element;
  typedef MatrixEntrySetter<MatType> Setter;

  explicit ARingMatrixGenerator(const CoeffRing& R) : mRing(R), mElements(R) {}

  // Fill 'result' in place.  'result' must already have the desired size; the
  // DMat/SMat constructors zero-fill, and every shape below overwrites only
  // the positions it owns, so a reused matrix should be zeroed by the caller.
  void nextMatrix(MatType& result) { nextMatrix(result, mShape); }

  void nextMatrix(MatType& result, MatrixShape shape)
  {
    size_t nrows = result.numRows();
    size_t ncols = result.numColumns();

    switch (shape)
      {
        case MatrixShape::Zero:
          return;
        case MatrixShape::Identity:
          assert(nrows == ncols && "Identity requires a square matrix");
          fillIdentity(result);
          return;
        case MatrixShape::Symmetric:
          assert(nrows == ncols && "Symmetric requires a square matrix");
          fillSymmetric(result, false);
          return;
        case MatrixShape::SkewSymmetric:
          assert(nrows == ncols && "SkewSymmetric requires a square matrix");
          fillSymmetric(result, true);
          return;
        case MatrixShape::PrescribedRank:
          fillPrescribedRank(result);
          return;
        case MatrixShape::Sparse:
          for (size_t r = 0; r < nrows; r++)
            for (size_t c = 0; c < ncols; c++)
              if (selectPosition(mDensity)) fillNonzeroEntry(result, r, c);
          return;
        default:
          for (size_t r = 0; r < nrows; r++)
            for (size_t c = 0; c < ncols; c++)
              if (shapeCoversPosition(shape, r, c)) fillEntry(result, r, c);
          return;
      }
  }

  // Explicitly specified entries, for tests that need a known matrix rather
  // than a generated one.  All four forms only write the positions they are
  // given, so they compose with nextMatrix(): shape first, then override.
  // Combine with MatrixShape::Zero for a blank canvas.

  // Triples, integer coefficients: {{0,1,1}, {0,4,2}, {2,0,5}}
  void setEntries(MatType& M, std::initializer_list<MatrixEntry> entries)
  {
    Element a(mRing);
    for (const auto& e : entries)
      {
        assert(e.row < M.numRows() && e.col < M.numColumns());
        mRing.set(a, static_cast<int>(e.coeff));
        Setter::set(M, e.row, e.col, a);
      }
  }

  // Row-major, integer coefficients: {1,2,3, 4,5,6} for a 2x3 matrix.
  void setEntries(MatType& M, std::initializer_list<long> values)
  {
    assert(values.size() == M.numRows() * M.numColumns() &&
           "row-major entry list must have exactly numRows*numColumns values");
    Element a(mRing);
    size_t i = 0;
    for (long v : values)
      {
        mRing.set(a, static_cast<int>(v));
        Setter::set(M, i / M.numColumns(), i % M.numColumns(), a);
        i++;
      }
  }

  // Triples and row-major with ring-element coefficients.  These take vectors
  // rather than initializer lists: the elements have to be constructed against
  // a ring first anyway, and for a ring whose ElementType is int (ARingZZp)
  // an initializer_list overload would be ambiguous with the integer forms.
  void setEntries(MatType& M,
                  const std::vector<MatrixElementEntry<ElementType>>& entries)
  {
    for (const auto& e : entries)
      {
        assert(e.row < M.numRows() && e.col < M.numColumns());
        Setter::set(M, e.row, e.col, e.coeff);
      }
  }

  void setEntries(MatType& M, const std::vector<ElementType>& values)
  {
    assert(values.size() == M.numRows() * M.numColumns() &&
           "row-major entry list must have exactly numRows*numColumns values");
    for (size_t i = 0; i < values.size(); i++)
      Setter::set(M, i / M.numColumns(), i % M.numColumns(), values[i]);
  }

  void setShape(MatrixShape shape) { mShape = shape; }
  void setDensity(double density) { mDensity = density; }
  void setRank(size_t rank) { mRank = rank; }

  // Restarts the element generator's deterministic prefix.  Does NOT restore
  // the sparsity pattern of MatrixShape::Sparse or the vectors chosen by
  // PrescribedRank -- those use the global PRNG.
  void reset() { mElements.reset(); }

 private:
  void fillEntry(MatType& M, size_t r, size_t c)
  {
    Element a(mRing);
    mElements.nextElement(a);
    Setter::set(M, r, c, a);
  }

  // Sparse entries must actually be nonzero, but the deterministic prefix of
  // getElement<> includes 0 (at index 25).  Draw again, but bounded: fall back
  // to 1 rather than spin if a ring's getElement only ever yields zero.
  void fillNonzeroEntry(MatType& M, size_t r, size_t c)
  {
    Element a(mRing);
    for (int i = 0; i < 100; i++)
      {
        mElements.nextElement(a);
        if (!mRing.is_zero(a)) break;
      }
    if (mRing.is_zero(a)) mRing.set(a, 1);
    Setter::set(M, r, c, a);
  }

  void fillIdentity(MatType& M)
  {
    Element one(mRing);
    mRing.set(one, 1);
    for (size_t i = 0; i < M.numRows(); i++) Setter::set(M, i, i, one);
  }

  // skew: M(c,r) = -M(r,c) with a zero diagonal.  The diagonal is left alone
  // rather than derived, because in characteristic 2 negation is the identity
  // and a = -a would not force a == 0; pfaffians need the alternating form.
  void fillSymmetric(MatType& M, bool skew)
  {
    size_t n = M.numRows();
    Element a(mRing);
    Element b(mRing);
    for (size_t r = 0; r < n; r++)
      for (size_t c = (skew ? r + 1 : r); c < n; c++)
        {
          mElements.nextElement(a);
          Setter::set(M, r, c, a);
          if (c == r) continue;
          if (skew)
            {
              mRing.negate(b, a);
              Setter::set(M, c, r, b);
            }
          else
            Setter::set(M, c, r, a);
        }
  }

  // M = sum_{k < mRank} u_k v_k^T, with u_k and v_k unit-trapezoidal:
  // u_k(i) = 0 for i < k, 1 for i == k, generated for i > k.  The leading
  // rank x rank blocks of U and V are then unit triangular, so that minor of
  // M has determinant 1 -- giving rank exactly mRank over any ring with
  // 1 != 0, rather than only generically.  A sum of fully random outer
  // products would drop rank often over a small field such as ZZ/2.
  //
  // Uses ring arithmetic only, so it works for every ARing; MatrixOps::mult
  // is not available for all of them.  Note that "rank" is only a meaningful
  // invariant over a field or domain.
  void fillPrescribedRank(MatType& M)
  {
    size_t nrows = M.numRows();
    size_t ncols = M.numColumns();
    size_t rank = mRank;
    if (rank > nrows) rank = nrows;
    if (rank > ncols) rank = ncols;
    if (rank == 0) return;

    typename CoeffRing::ElementArray u(mRing, rank * nrows);
    typename CoeffRing::ElementArray v(mRing, rank * ncols);
    for (size_t k = 0; k < rank; k++)
      {
        for (size_t i = 0; i < nrows; i++)
          {
            if (i < k) mRing.set_zero(u[k * nrows + i]);
            else if (i == k) mRing.set(u[k * nrows + i], 1);
            else mElements.nextElement(u[k * nrows + i]);
          }
        for (size_t j = 0; j < ncols; j++)
          {
            if (j < k) mRing.set_zero(v[k * ncols + j]);
            else if (j == k) mRing.set(v[k * ncols + j], 1);
            else mElements.nextElement(v[k * ncols + j]);
          }
      }

    Element sum(mRing);
    Element term(mRing);
    for (size_t i = 0; i < nrows; i++)
      for (size_t j = 0; j < ncols; j++)
        {
          mRing.set_zero(sum);
          for (size_t k = 0; k < rank; k++)
            {
              mRing.mult(term, u[k * nrows + i], v[k * ncols + j]);
              mRing.add(sum, sum, term);
            }
          Setter::set(M, i, j, sum);
        }
  }

  const CoeffRing& mRing;
  ARingElementGenerator<CoeffRing> mElements;
  MatrixShape mShape = MatrixShape::Dense;
  double mDensity = 1.0;
  size_t mRank = 0;
};

// Check matrix addition against entrywise ring arithmetic. Keep the generator
// across trials so that its deterministic prefix is followed by random draws.
template <typename MatType>
void testMatrixAdd(const typename MatType::CoeffRing& R,
                   int ntrials,
                   size_t nrows,
                   size_t ncols)
{
  typedef typename MatType::CoeffRing CoeffRing;
  typedef typename CoeffRing::Element Element;
  typedef MatrixEntrySetter<MatType> Entries;

  ARingMatrixGenerator<MatType> matgen(R);
  Element a(R), b(R), actual(R), expected(R);

  for (int trial = 0; trial < ntrials; ++trial)
    {
      // Fresh matrices each trial; B starts at zero.
      MatType M(R, nrows, ncols);
      MatType N(R, nrows, ncols);
      MatType B(R, nrows, ncols);

      matgen.nextMatrix(M);
      matgen.nextMatrix(N);
      MatrixOps::addInPlace(B, M);
      MatrixOps::addInPlace(B, N);

      for (size_t i = 0; i < nrows; ++i)
        for (size_t j = 0; j < ncols; ++j)
          {
            Entries::get(M, i, j, a);
            Entries::get(N, i, j, b);
            Entries::get(B, i, j, actual);
            R.add(expected, a, b);
            EXPECT_TRUE(R.is_equal(actual, expected))
                << "trial " << trial << ", entry (" << i << ", " << j << ")";
          }
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
