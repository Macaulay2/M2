// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "F4MatrixProjection.hpp"

#include "ScopeExit.hpp"

MATHICGB_NAMESPACE_BEGIN

F4MatrixProjection::F4MatrixProjection(
  const PolyRing& ring,
  ColIndex colCount
):
  mColProjectTo(colCount),
  mRing(ring)
{}

void F4MatrixProjection::addColumn(
  const ColIndex projectFrom,
  ConstMonoRef mono,
  const bool isLeft
) {
  MATHICGB_ASSERT(projectFrom < mColProjectTo.size());
  MATHICGB_ASSERT
    (mLeftMonomials.size() + mRightMonomials.size() < mColProjectTo.size());

  auto monoCopy = mRing.monoid().alloc();
  mRing.monoid().copy(mono, *monoCopy);

  auto& projected = mColProjectTo[projectFrom];
  if (isLeft) {
    projected.isLeft = true;
    projected.index = static_cast<ColIndex>(mLeftMonomials.size());
    mLeftMonomials.push_back(monoCopy.ptr());
  } else {
    projected.isLeft = false;
    projected.index = static_cast<ColIndex>(mRightMonomials.size());
    mRightMonomials.push_back(monoCopy.ptr());
  }
  monoCopy.release();
}

struct RowData : F4ProtoMatrix::Row {
  RowData(): F4ProtoMatrix::Row() {}
  RowData(const F4ProtoMatrix::Row& row): F4ProtoMatrix::Row(row) {}
};

typedef std::pair<RowData, SparseMatrix::Scalar> RowProjectFrom;


template<class Row>
class F4MatrixProjection::TopBottom {
public:
  typedef std::pair<Row, Scalar> RowMultiple;
  typedef std::vector<RowMultiple> RowVector;

  TopBottom(const size_t leftColCount, const PolyRing& ring):
    mModulus(static_cast<Scalar>(ring.charac())),
    mTopRows(leftColCount)
  {
    MATHICGB_ASSERT(ring.charac() <= std::numeric_limits<Scalar>::max());
    MATHICGB_ASSERT(leftColCount <= std::numeric_limits<ColIndex>::max());
  }

  void addRow(const Row& row, ColIndex leadIndex, Scalar leadScalar) {
    if (row.entryCount == 0)
      return; // Skip zero rows.
    if (leadIndex == std::numeric_limits<ColIndex>::max()) {
      // this row has no left entries, so it cannot be a top row.
      mBottomRows.push_back(RowMultiple(row, 1));
      return;
    }

    const auto currentTop = mTopRows[leadIndex].first;
    if (
      currentTop.entryCount != 0 && // already have a reducer and...
      currentTop.entryCount < row.entryCount // ...it is sparser/better
    ) {
      mBottomRows.push_back(RowMultiple(row, 1));
    } else {
      if (currentTop.entryCount != 0)
        mBottomRows.push_back(std::make_pair(currentTop, 1));
      MATHICGB_ASSERT(leadScalar != 0);
      const auto inverse = leadScalar == 1 ? // 1 is a common case
        1 : modularInverse(leadScalar, mModulus);
      mTopRows[leadIndex] = RowMultiple(row, inverse);
    }
  }

  bool debugAssertValid() {
#ifdef MATHICGB_DEBUG
    auto check = [](RowMultiple r) {
      MATHICGB_ASSERT(r.first.entryCount > 0);
      MATHICGB_ASSERT(r.second != 0);
    };
    std::for_each(mTopRows.begin(), mTopRows.end(), check);
    std::for_each(mBottomRows.begin(), mBottomRows.end(), check);
#endif
    return true;
  }

  Scalar modulus() const {return mModulus;}
  const RowVector& top() const {return mTopRows;}
  const RowVector& bottom() const {return mBottomRows;}

  RowVector moveTop() {return mTopRows;}
  RowVector moveBottom() {return mBottomRows;}

private:
  const Scalar mModulus;
  RowVector mTopRows;
  RowVector mBottomRows;
};

class F4MatrixProjection::LeftRight {
public:
  typedef F4ProtoMatrix::ExternalScalar ExternalScalar;
  typedef F4ProtoMatrix::Row Row;

  LeftRight(
    const std::vector<ColProjectTo>& colProjectTo,
    const PolyRing& ring,
    const size_t quantum
  ):
    mColProjectTo(colProjectTo),
    mModulus(static_cast<Scalar>(ring.charac())),
    mLeft(quantum),
    mRight(quantum)
  {
    MATHICGB_ASSERT(ring.charac() < std::numeric_limits<Scalar>::max());
    mLeft.clear();
    mRight.clear();
  }

  template<class Pair>
  void appendRowsPermuted(const std::vector<Pair>& rows) {
    const auto end = rows.end();
    for (auto it = rows.begin(); it != end; ++it)
      appendRow(it->first, it->second);
  }

  void appendRows(const std::vector<F4ProtoMatrix*>& preBlocks) {
    const auto end = preBlocks.end();
    for (auto it = preBlocks.begin(); it != end; ++it) {
      auto& block = **it;
      const auto rowCount = block.rowCount();
      for (SparseMatrix::RowIndex r = 0; r < rowCount; ++r) {
        const auto row = block.row(r);
        if (row.entryCount > 0)
          appendRow(row);
      }
    }
  }

  void appendRow(const Row& row) {
    MATHICGB_ASSERT(row.entryCount > 0); // could be OK, but unexpected

    const auto indicesEnd = row.indices + row.entryCount;
    if (row.scalars == nullptr)
      appendRow(row.indices, indicesEnd, row.externalScalars);
    else
      appendRow(row.indices, indicesEnd, row.scalars);
  }

  void appendRow(const Row& row, Scalar multiplyBy) {
    MATHICGB_ASSERT(multiplyBy != 0);
    appendRow(row);
    if (multiplyBy != 1) {
      const auto rowIndex = mLeft.rowCount() - 1;
      mLeft.multiplyRow(rowIndex, multiplyBy, mModulus);
      mRight.multiplyRow(rowIndex, multiplyBy, mModulus);
    }
  }

  template<class IndexIter, class ScalarIter>
  void appendRow(
    IndexIter indices,
    const IndexIter indicesEnd,
    ScalarIter scalars
  ) {
    for (; indices != indicesEnd; ++indices, ++scalars)
      appendEntry(*indices, *scalars);
    rowDone();
  }

  void appendEntry(const ColIndex projectMe, const Scalar scalar) {
    MATHICGB_ASSERT(scalar < mModulus);
    MATHICGB_ASSERT(mLeft.rowCount() == mRight.rowCount());
    MATHICGB_ASSERT(projectMe < mColProjectTo.size());
    const auto projected = mColProjectTo[projectMe];
    if (projected.isLeft)
      mLeft.appendEntry(projected.index, scalar);
    else
      mRight.appendEntry(projected.index, scalar);
  }

  void appendEntry(const ColIndex projectMe, const ExternalScalar scalar) {
    MATHICGB_ASSERT(scalar <= std::numeric_limits<Scalar>::max());
    appendEntry(projectMe, static_cast<Scalar>(scalar));
  }

  void rowDone() {
    MATHICGB_ASSERT(mLeft.rowCount() == mRight.rowCount());
    mLeft.rowDone();
    mRight.rowDone();
  };

  const SparseMatrix& left() const {return mLeft;}
  const SparseMatrix& right() const {return mRight;}

  SparseMatrix moveLeft() {return std::move(mLeft);}
  SparseMatrix moveRight() {return std::move(mRight);}

private:
  const std::vector<ColProjectTo>& mColProjectTo;
  const Scalar mModulus;

  SparseMatrix mLeft;
  SparseMatrix mRight;
};

QuadMatrix F4MatrixProjection::makeAndClear(const size_t quantum) {
  if (true)
    return makeAndClearOneStep(quantum);
  else
    return makeAndClearTwoStep(quantum);
}

QuadMatrix F4MatrixProjection::makeAndClearOneStep(const size_t quantum) {
  // Construct top/bottom row permutation
   TopBottom<F4ProtoMatrix::Row> tb(mLeftMonomials.size(), ring());
  const auto end = mMatrices.end();
  for (auto it = mMatrices.begin(); it != end; ++it) {
    const auto& matrix = **it;
    const auto rowCount = matrix.rowCount();
    for (RowIndex r = 0; r < rowCount; ++r) {
      const auto& row = matrix.row(r); // const ref keeps temporary alive
      if (row.entryCount == 0)
        continue; // ignore zero rows

      // *** Look for leading left entry
      ColIndex lead = 0;
      for (; lead < row.entryCount; ++lead) {
        MATHICGB_ASSERT(row.indices[lead] < mColProjectTo.size());
        auto const projected = mColProjectTo[row.indices[lead]];
        if (projected.isLeft) {
          const auto leadScalar = row.scalars != nullptr ?
            row.scalars[lead] :
            static_cast<Scalar>(row.externalScalars[lead]);
          tb.addRow(row, projected.index, leadScalar);
          goto done;
        }
      }
      // Did not find any left entry.
      tb.addRow(row, std::numeric_limits<ColIndex>::max(), 0);
done:;
    }
  }
  MATHICGB_ASSERT(tb.debugAssertValid());

  // Split left/right and top/bottom simultaneously
  LeftRight top(mColProjectTo, ring(), quantum);
  top.appendRowsPermuted(tb.moveTop());

  LeftRight bottom(mColProjectTo, ring(), 0);
  bottom.appendRowsPermuted(tb.moveBottom());

  // Move the data into place
  QuadMatrix qm(ring());
  qm.leftColumnMonomials = std::move(mLeftMonomials);
  qm.rightColumnMonomials = std::move(mRightMonomials);

  qm.topLeft = top.moveLeft();
  qm.topRight = top.moveRight();
  qm.bottomLeft = bottom.moveLeft();
  qm.bottomRight = bottom.moveRight();

  return qm;
}

namespace {
  // Helper function for F4MatrixProjection::makeAndClearTwoStep
  template<class TopBottom>
  std::pair<SparseMatrix, SparseMatrix> projectRows(
    const TopBottom& tb,
    size_t quantum,
    SparseMatrix&& in
  ) {
    const auto modulus = tb.modulus();

    SparseMatrix top(quantum);
    const auto topRows = tb.top();
    const auto rowCountTop =
      static_cast<SparseMatrix::RowIndex>(topRows.size());
    for (SparseMatrix::RowIndex toRow = 0; toRow < rowCountTop; ++toRow) {
      top.appendRow(in, topRows[toRow].first.index);
      if (topRows[toRow].second != 1)
        top.multiplyRow(toRow, topRows[toRow].second, modulus);
    }

    SparseMatrix bottom(quantum);
    const auto bottomRows = tb.bottom();
    const auto rowCountBottom =
      static_cast<SparseMatrix::RowIndex>(bottomRows.size());
    for (SparseMatrix::RowIndex toRow = 0; toRow < rowCountBottom; ++toRow) {
      bottom.appendRow(in, bottomRows[toRow].first.index);
      if (bottomRows[toRow].second != 1)
          bottom.multiplyRow(toRow, bottomRows[toRow].second, modulus);
    }

    in.clear();
    return std::make_pair(std::move(top), std::move(bottom));
  }
}

QuadMatrix F4MatrixProjection::makeAndClearTwoStep(const size_t quantum) {
  // Split whole matrix into left/right
  LeftRight lr(mColProjectTo, ring(), quantum);
  lr.appendRows(mMatrices);

  // Construct top/bottom matrix permutation
  struct Row {
    RowIndex index;
    ColIndex entryCount;
  };
  TopBottom<Row> tb(mLeftMonomials.size(), ring());
  const auto rowCount = lr.left().rowCount();
  for (SparseMatrix::RowIndex row = 0; row < rowCount; ++row) {
    const auto leftEntryCount = lr.left().entryCountInRow(row);
    const auto entryCount = leftEntryCount + lr.right().entryCountInRow(row);
    MATHICGB_ASSERT(entryCount >= leftEntryCount); // no overflow
    if (entryCount == 0)
      continue; // ignore zero rows

    const Row r = {row, entryCount};
    if (leftEntryCount == 0)
      tb.addRow(r, std::numeric_limits<ColIndex>::max(), 0);
    else {
      const auto entry = lr.left().rowBegin(row);
      tb.addRow(r, entry.index(), entry.scalar());
    }
  }
  MATHICGB_ASSERT(tb.debugAssertValid());

  QuadMatrix qm(ring());
  auto left = projectRows(tb, quantum, lr.moveLeft());
  auto right = projectRows(tb, quantum, lr.moveRight());

  qm.topLeft = std::move(left.first);
  qm.bottomLeft = std::move(left.second);
  qm.topRight = std::move(right.first);
  qm.bottomRight = std::move(right.second);
  qm.leftColumnMonomials = std::move(mLeftMonomials);
  qm.rightColumnMonomials = std::move(mRightMonomials);
  return qm;
}

MATHICGB_NAMESPACE_END
