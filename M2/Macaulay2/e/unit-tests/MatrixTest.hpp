// Copyright 2026, The Macaulay2 Authors.
//
// MutableMatrixGenerator and MatrixGenerator: the matrix analogues of
// RingElementGenerator, working through the ring_elem interface.  Both inherit
// the deterministic-prefix-then-random contract from getElement<>: the first
// 49 element draws are the small integers -24..24, later draws are random.
//
//   MutableMatrixGenerator<Z_mod> gen(*R);
//   MutableMatrix* M = gen.nextMatrix(5, 5);
//   MutableMatrix* S = gen.nextMatrix(5, 5, /* dense */ false,
//                                     MatrixShape::SkewSymmetric);
//
// Matrix::random (matrices/matrix.hpp:230) covers general/upper-triangular
// with a density, but draws straight from Ring::random and so has no
// deterministic prefix and no reset(); use it directly if that is what you
// want.
//
// NOTE: this header includes RingTest.hpp, which shares an include guard with
// ARingTest.hpp.  It therefore cannot be combined with ARingMatrixTest.hpp in
// one translation unit.

#ifndef M2_UNITTESTS__MATRIX_TEST_HPP__
#define M2_UNITTESTS__MATRIX_TEST_HPP__

#include <cassert>

#include "unit-tests/RingTest.hpp"
#include "unit-tests/MatrixShape.hpp"
#include "basic-mutable-matrices/mat.hpp"
#include "matrices/matrix.hpp"
#include "matrices/matrix-con.hpp"
#include "free-modules/freemod.hpp"
#include "newdelete.hpp"

// Shape logic shared by both generators below.  'setEntry' is any callable
// with signature void(size_t r, size_t c, ring_elem a).  Everything here uses
// only Ring operations, so it works over every ring that has a getElement<>
// specialization.
template <typename RingType, typename SetEntry>
void fillRingElemShape(const RingType& R,
                       RingElementGenerator<RingType>& gen,
                       size_t nrows,
                       size_t ncols,
                       MatrixShape shape,
                       double density,
                       size_t rank,
                       SetEntry setEntry)
{
  switch (shape)
    {
      case MatrixShape::Zero:
        return;
      case MatrixShape::Identity:
        assert(nrows == ncols && "Identity requires a square matrix");
        for (size_t i = 0; i < nrows; i++) setEntry(i, i, R.from_long(1));
        return;
      case MatrixShape::Symmetric:
      case MatrixShape::SkewSymmetric:
        {
          assert(nrows == ncols && "Symmetric requires a square matrix");
          bool skew = (shape == MatrixShape::SkewSymmetric);
          // The diagonal of a skew matrix is set to zero explicitly rather
          // than derived: in characteristic 2 negation is the identity, so
          // a == -a would not force a == 0, and pfaffians need the
          // alternating form.
          for (size_t r = 0; r < nrows; r++)
            for (size_t c = (skew ? r + 1 : r); c < ncols; c++)
              {
                ring_elem a = gen.nextElement();
                setEntry(r, c, a);
                if (c == r) continue;
                setEntry(c, r, skew ? R.negate(a) : a);
              }
          return;
        }
      case MatrixShape::PrescribedRank:
        {
          // M = sum_{k < rank} u_k v_k^T, with u_k and v_k unit-trapezoidal:
          // u_k(i) = 0 for i < k, 1 for i == k, generated for i > k.  The
          // leading rank x rank blocks of U and V are then unit triangular,
          // so that minor of M has determinant 1 -- giving rank exactly
          // 'rank' over any ring with 1 != 0, rather than only generically.
          // A sum of fully random outer products would drop rank often over
          // a small field such as ZZ/2.  Note "rank" is only a meaningful
          // invariant over a field or domain.
          if (rank > nrows) rank = nrows;
          if (rank > ncols) rank = ncols;
          if (rank == 0) return;
          // gc_vector, not std::vector: these hold GC pointers that must stay
          // traceable while only this buffer references them.
          VECTOR(ring_elem) u(rank * nrows);
          VECTOR(ring_elem) v(rank * ncols);
          for (size_t k = 0; k < rank; k++)
            {
              for (size_t i = 0; i < nrows; i++)
                u[k * nrows + i] = (i < k    ? R.zero()
                                    : i == k ? R.from_long(1)
                                             : gen.nextElement());
              for (size_t j = 0; j < ncols; j++)
                v[k * ncols + j] = (j < k    ? R.zero()
                                    : j == k ? R.from_long(1)
                                             : gen.nextElement());
            }
          for (size_t i = 0; i < nrows; i++)
            for (size_t j = 0; j < ncols; j++)
              {
                ring_elem sum = R.zero();
                for (size_t k = 0; k < rank; k++)
                  sum = R.add(sum,
                              R.mult(u[k * nrows + i], v[k * ncols + j]));
                setEntry(i, j, sum);
              }
          return;
        }
      case MatrixShape::Sparse:
        // Sparse entries must actually be nonzero, but the deterministic
        // prefix of getElement<> includes 0 (at index 25).  Draw again, but
        // bounded: fall back to 1 rather than spin.
        for (size_t r = 0; r < nrows; r++)
          for (size_t c = 0; c < ncols; c++)
            {
              if (!selectPosition(density)) continue;
              ring_elem a = R.zero();
              for (int i = 0; i < 100; i++)
                {
                  a = gen.nextElement();
                  if (!R.is_zero(a)) break;
                }
              if (R.is_zero(a)) a = R.from_long(1);
              setEntry(r, c, a);
            }
        return;
      default:
        for (size_t r = 0; r < nrows; r++)
          for (size_t c = 0; c < ncols; c++)
            if (shapeCoversPosition(shape, r, c))
              setEntry(r, c, gen.nextElement());
        return;
    }
}

template <typename RingType>
class MutableMatrixGenerator
{
 public:
  explicit MutableMatrixGenerator(const RingType& R) : mRing(R), mElements(R) {}

  // 'dense' picks the backing store; MatrixShape::Sparse plus setDensity()
  // controls how many entries are nonzero.  They are independent knobs.
  MutableMatrix* nextMatrix(size_t nrows, size_t ncols, bool dense = true)
  {
    return nextMatrix(nrows, ncols, dense, mShape);
  }

  MutableMatrix* nextMatrix(size_t nrows,
                            size_t ncols,
                            bool dense,
                            MatrixShape shape)
  {
    if (shape == MatrixShape::Identity)
      {
        assert(nrows == ncols && "Identity requires a square matrix");
        return MutableMatrix::identity(&mRing, nrows, dense);
      }
    MutableMatrix* result =
        MutableMatrix::zero_matrix(&mRing, nrows, ncols, dense);
    fillMatrix(*result, shape);
    return result;
  }

  void fillMatrix(MutableMatrix& result) { fillMatrix(result, mShape); }

  void fillMatrix(MutableMatrix& result, MatrixShape shape)
  {
    fillRingElemShape(
        mRing,
        mElements,
        result.n_rows(),
        result.n_cols(),
        shape,
        mDensity,
        mRank,
        [&result](size_t r, size_t c, ring_elem a) {
          result.set_entry(r, c, a);
        });
  }

  void setShape(MatrixShape shape) { mShape = shape; }
  void setDensity(double density) { mDensity = density; }
  void setRank(size_t rank) { mRank = rank; }

  // Restarts the element generator's deterministic prefix.  Does NOT restore
  // the sparsity pattern of MatrixShape::Sparse or the vectors chosen by
  // PrescribedRank -- those use the global PRNG.
  void reset() { mElements.reset(); }

 private:
  const RingType& mRing;
  RingElementGenerator<RingType> mElements;
  MatrixShape mShape = MatrixShape::Dense;
  double mDensity = 1.0;
  size_t mRank = 0;
};

template <typename RingType>
class MatrixGenerator
{
 public:
  explicit MatrixGenerator(const RingType& R) : mRing(R), mElements(R) {}

  // Free modules of rank nrows/ncols, all generators in degree 0.
  Matrix* nextMatrix(size_t nrows, size_t ncols)
  {
    return nextMatrix(nrows, ncols, mShape);
  }

  Matrix* nextMatrix(size_t nrows, size_t ncols, MatrixShape shape)
  {
    const FreeModule* target = mRing.make_FreeModule(static_cast<int>(nrows));
    if (shape == MatrixShape::Identity)
      {
        assert(nrows == ncols && "Identity requires a square matrix");
        return Matrix::identity(target);
      }
    // The two-argument constructor builds the source free module itself and
    // leaves the columns unfrozen, so the degrees can be computed.
    MatrixConstructor mat(target, static_cast<int>(ncols));
    fill(mat, nrows, ncols, shape);
    mat.compute_column_degrees();
    return mat.to_matrix();
  }

  // Caller-supplied free modules, for degree-aware tests.
  Matrix* nextMatrix(const FreeModule* target, const FreeModule* source)
  {
    return nextMatrix(target, source, mShape);
  }

  Matrix* nextMatrix(const FreeModule* target,
                     const FreeModule* source,
                     MatrixShape shape)
  {
    // The three-argument constructor freezes the source, so
    // compute_column_degrees() must not be called on this path.
    MatrixConstructor mat(target, source);
    fill(mat, target->rank(), source->rank(), shape);
    return mat.to_matrix();
  }

  void setShape(MatrixShape shape) { mShape = shape; }
  void setDensity(double density) { mDensity = density; }
  void setRank(size_t rank) { mRank = rank; }

  // Restarts the element generator's deterministic prefix.  Does NOT restore
  // the sparsity pattern of MatrixShape::Sparse or the vectors chosen by
  // PrescribedRank -- those use the global PRNG.
  void reset() { mElements.reset(); }

 private:
  void fill(MatrixConstructor& mat,
            size_t nrows,
            size_t ncols,
            MatrixShape shape)
  {
    fillRingElemShape(mRing,
                      mElements,
                      nrows,
                      ncols,
                      shape,
                      mDensity,
                      mRank,
                      [&mat](size_t r, size_t c, ring_elem a) {
                        mat.set_entry(static_cast<int>(r),
                                      static_cast<int>(c),
                                      a);
                      });
  }

  const RingType& mRing;
  RingElementGenerator<RingType> mElements;
  MatrixShape mShape = MatrixShape::Dense;
  double mDensity = 1.0;
  size_t mRank = 0;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
