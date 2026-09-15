// Copyright 2026, The Macaulay2 Authors.
//
// Shapes shared by the matrix generators in ARingMatrixTest.hpp (DMat/SMat)
// and MatrixTest.hpp (MutableMatrix, Matrix).  Kept in its own header so that
// neither of those has to include the other: ARingTest.hpp and RingTest.hpp
// share an include guard, so they cannot appear in the same translation unit.

#ifndef M2_UNITTESTS__MATRIX_SHAPE_HPP__
#define M2_UNITTESTS__MATRIX_SHAPE_HPP__

#include <cstddef>

#include "interface/random.h"

enum class MatrixShape {
  Zero,             // leave the zero-initialized matrix alone
  Dense,            // every entry drawn from the element generator
  Sparse,           // each entry nonzero with probability mDensity
  Identity,         // 1 on the diagonal (square only)
  UpperTriangular,  // fill r <= c
  LowerTriangular,  // fill r >= c
  Symmetric,        // fill r <= c, mirror to (c,r) (square only)
  SkewSymmetric,    // fill r < c, mirror negated, zero diagonal (square only)
  PrescribedRank    // sum of mRank outer products; rank <= mRank
};

// Does position (r,c) get a nonzero entry, at the given density?
// Uses the global PRNG, so a generator's reset() cannot reproduce the pattern.
inline bool selectPosition(double density)
{
  if (density >= 1.0) return true;
  if (density <= 0.0) return false;
  return rawRandomInt(1000000) < static_cast<int32_t>(density * 1000000);
}

// Which (r,c) does a shape write to?  Shapes needing more than a per-position
// decision (Identity, Symmetric, SkewSymmetric, PrescribedRank) are handled by
// the generators themselves.
inline bool shapeCoversPosition(MatrixShape shape, size_t r, size_t c)
{
  switch (shape)
    {
      case MatrixShape::UpperTriangular: return r <= c;
      case MatrixShape::LowerTriangular: return r >= c;
      default: return true;
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
