// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "F4ProtoMatrix.hpp"

MATHICGB_NAMESPACE_BEGIN

auto F4ProtoMatrix::row(const RowIndex row) const -> Row {
  MATHICGB_ASSERT(row < mRows.size());
  const auto& r = mRows[row];
  Row rr;
  rr.indices = mIndices.data() + r.indicesBegin;
  rr.entryCount = r.entryCount;
  if (!r.scalarsStoredExternally) {
    rr.scalars = mScalars.data() + r.scalarsBegin;
  } else {
    rr.scalars = nullptr;
    rr.externalScalars = r.externalScalars;
  }
  return rr;
}

auto F4ProtoMatrix::makeRowWithTheseScalars(const Poly& scalars) -> ColIndex*
{
  MATHICGB_ASSERT(rowCount() < std::numeric_limits<RowIndex>::max());
  MATHICGB_ASSERT(scalars.termCount() < std::numeric_limits<ColIndex>::max());

  InternalRow row;
  row.indicesBegin = mIndices.size();
  row.scalarsBegin = std::numeric_limits<decltype(row.scalarsBegin)>::max();
  row.entryCount = static_cast<ColIndex>(scalars.termCount());
  row.scalarsStoredExternally = true;
  row.externalScalars = scalars.coefBegin();
  mRows.push_back(row);

  mIndices.resize(mIndices.size() + row.entryCount);
  return mIndices.data() + row.indicesBegin;
}

auto F4ProtoMatrix::makeRow(ColIndex entryCount) -> std::pair<ColIndex*, Scalar*> {
  MATHICGB_ASSERT(rowCount() < std::numeric_limits<RowIndex>::max());

  InternalRow row;
  row.indicesBegin = mIndices.size();
  row.scalarsBegin = mScalars.size();
  row.entryCount = entryCount;
  row.scalarsStoredExternally = false;
  mRows.push_back(row);

  mIndices.resize(mIndices.size() + entryCount);
  mScalars.resize(mScalars.size() + entryCount);
  return std::make_pair(
    mIndices.data() + row.indicesBegin,
    mScalars.data() + row.scalarsBegin
  );
}

void F4ProtoMatrix::removeLastEntries(const RowIndex row, const ColIndex count) {
  MATHICGB_ASSERT(row < rowCount());
  MATHICGB_ASSERT(mRows[row].entryCount >= count);
  mRows[row].entryCount -= count;
  if (row != rowCount() - 1)
    return;
  mIndices.resize(mIndices.size() - count);
  if (!mRows[row].scalarsStoredExternally)
    mScalars.resize(mScalars.size() - count);
}

MATHICGB_NAMESPACE_END
