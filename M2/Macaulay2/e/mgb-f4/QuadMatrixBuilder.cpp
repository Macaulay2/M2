// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "QuadMatrixBuilder.hpp"

#include "QuadMatrix.hpp"
#include "ScopeExit.hpp"
#include "mathic/mathic.h"
#include <sstream>

MATHICGB_NAMESPACE_BEGIN

QuadMatrixBuilder::QuadMatrixBuilder(
  const PolyRing& ring,
  Map& map,
  Monomials& monomialsLeft,
  Monomials& monomialsRight,
  const size_t memoryQuantum
):
  mMonomialsLeft(monomialsLeft),
  mMonomialsRight(monomialsRight),
  mMonomialToCol(map),
  mTopLeft(memoryQuantum),
  mTopRight(memoryQuantum),
  mBottomLeft(memoryQuantum),
  mBottomRight(memoryQuantum)
{}

void QuadMatrixBuilder::takeRowsFrom(QuadMatrix&& matrix) {
  MATHICGB_ASSERT(&ring() == &matrix.ring());
  MATHICGB_ASSERT(matrix.debugAssertValid());

  mTopLeft.takeRowsFrom(std::move(matrix.topLeft));
  mTopRight.takeRowsFrom(std::move(matrix.topRight));
  mBottomLeft.takeRowsFrom(std::move(matrix.bottomLeft));
  mBottomRight.takeRowsFrom(std::move(matrix.bottomRight));
}

namespace {
  /// Creates a column and updates the associated data structures that
  /// are passed in. Copies mono - ownership is not taken over. The
  /// purpose of this function is to avoid code duplication. It is a            
  /// template in order to avoid referring to private types of
  /// QuadMatrixBuilder.
  template<class ToMono, class ToCol, class Monoid>
  std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonomial>
  createCol(
    typename Monoid::ConstMonoRef mono,
    SparseMatrix& top,
    SparseMatrix& bottom,
    ToMono& toMonomial,
    ToCol& toCol,
    const Monoid& monoid,
    const bool left
  ) {
    MATHICGB_ASSERT(typename ToCol::Reader(toCol).find(mono).first == 0);

    const auto colCount =
      static_cast<QuadMatrixBuilder::ColIndex>(toMonomial.size());
    if (colCount == std::numeric_limits<QuadMatrixBuilder::ColIndex>::max())
      throw std::overflow_error("Too many columns in QuadMatrixBuilder");

    // allocate memory in toMonomial now to ensure no bad_alloc later
    toMonomial.emplace_back(nullptr);
    MATHICGB_SCOPE_EXIT(toMonomialGuard) {toMonomial.pop_back();};
    auto copied = monoid.alloc();
    monoid.copy(mono, copied);
    auto inserted = toCol.insert(
      std::make_pair(
        copied.ptr(),
        QuadMatrixBuilder::LeftRightColIndex(colCount, left)
      )
    );
    MATHICGB_ASSERT(inserted.second);
    MATHICGB_ASSERT(inserted.first.first != 0);
    auto p(inserted.first);
    toMonomial.back() = copied;

    MATHICGB_ASSERT(monoid.equalHintTrue(*copied, *p.second));
    MATHICGB_ASSERT(*p.first == QuadMatrixBuilder::LeftRightColIndex(colCount, left));

    auto ptr = const_cast<exponent*>(Monoid::toOld(*p.second));
    toMonomialGuard.dismiss();
    copied.release();
    return std::make_pair(*p.first, ptr);
  }
}

std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonomial>
QuadMatrixBuilder::createColumnLeft(
  ConstMonoRef monomialToBeCopied
) {
  return createCol(
    monomialToBeCopied,
    mTopLeft,
    mBottomLeft,
    mMonomialsLeft,
    mMonomialToCol,
    monoid(),
    true
  );
}

std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonomial>
QuadMatrixBuilder::createColumnRight(
  ConstMonoRef monomialToBeCopied
) {
  return createCol(
    monomialToBeCopied,
    mTopRight,
    mBottomRight,
    mMonomialsRight,
    mMonomialToCol,
    monoid(),
    false
  );
}

QuadMatrix QuadMatrixBuilder::buildMatrixAndClear() {
  QuadMatrix out(ring());

  mTopLeft.swap(out.topLeft);
  mTopRight.swap(out.topRight);
  mBottomLeft.swap(out.bottomLeft);
  mBottomRight.swap(out.bottomRight);

  mTopLeft.clear();
  mTopRight.clear();
  mBottomLeft.clear();
  mBottomRight.clear();

  MATHICGB_ASSERT(out.debugAssertValid());
  return out;
}

MATHICGB_NAMESPACE_END
