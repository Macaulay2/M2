// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "SparseMatrix.hpp"

#include "Poly.hpp"
#include <algorithm>

MATHICGB_NAMESPACE_BEGIN

void SparseMatrix::takeRowsFrom(SparseMatrix&& matrix) {
  if (matrix.mRows.empty())
    return;

  if (mRows.empty()) {
    *this = std::move(matrix);
    return;
  }

  Block* oldestBlock = &matrix.mBlock;
  while (oldestBlock->mPreviousBlock != 0)
    oldestBlock = oldestBlock->mPreviousBlock;

  if (mBlock.mHasNoRows) // only put mBlock in chain of blocks if non-empty
    oldestBlock->mPreviousBlock = mBlock.mPreviousBlock;
  else
    oldestBlock->mPreviousBlock = new Block(std::move(mBlock));
  mBlock = std::move(matrix.mBlock);

  mRows.insert(mRows.end(), matrix.mRows.begin(), matrix.mRows.end());
  matrix.clear();
}

void SparseMatrix::rowToPolynomial(
  const RowIndex row,
  const std::vector<PolyRing::Monoid::ConstMonoPtr>& colMonomials,
  Poly& poly
) {
  poly.setToZero();
  poly.reserve(entryCountInRow(row));
  const auto end = rowEnd(row);
  for (auto it = rowBegin(row); it != end; ++it) {
    MATHICGB_ASSERT(it.index() < colMonomials.size());
    if (it.scalar() != 0)
      poly.append(it.scalar(), *colMonomials[it.index()]);
  }
  MATHICGB_ASSERT(poly.termsAreInDescendingOrder());
}

void SparseMatrix::sortRowsByIncreasingPivots() {
  SparseMatrix ordered;
  const auto rowCount = this->rowCount();

  std::vector<RowIndex> rows(rowCount);
  for (RowIndex row = 0; row < rowCount; ++row)
    rows[row] = row;

  const auto lexLess = [&](const RowIndex a, const RowIndex b) -> bool {
    auto aIt = rowBegin(a);
    auto bIt = rowBegin(b);
    const auto aEnd = rowEnd(a);
    const auto bEnd = rowEnd(b);
    for (; aIt != aEnd && bIt != bEnd; ++aIt, ++bIt) {
      if (*aIt < *bIt)
        return true;
      if (*aIt > *bIt)
        return false;
      ++aIt;
      ++bIt;
    }
    return aIt == aEnd && bIt != bEnd;
  };
  std::sort(rows.begin(), rows.end(), lexLess);

  // construct ordered with pivot columns in increasing order
  ordered.clear();
  for (size_t i = 0; i < rowCount; ++i)
    ordered.appendRow(*this, rows[i]);
  *this = std::move(ordered);
}

void SparseMatrix::applyColumnMap(const std::vector<ColIndex>& colMap) {
  MATHICGB_ASSERT(colMap.size() >= computeColCount());
  Block* block = &mBlock;
  for (; block != 0; block = block->mPreviousBlock) {
    const auto end = block->mColIndices.end();
    for (auto it = block->mColIndices.begin(); it != end; ++it)
      *it = colMap[*it];
  }
}

void SparseMatrix::multiplyRow(
  const RowIndex row,
  const Scalar multiplier,
  const Scalar modulus
) {
  MATHICGB_ASSERT(row < rowCount());
  const auto end = rowEnd(row);
  for (auto it = rowBegin(row); it != end; ++it)
    it.setScalar(modularProduct(it.scalar(), multiplier, modulus));
}

void SparseMatrix::print(std::ostream& out) const {
  if (rowCount() == 0)
    out << "matrix with no rows\n";
  for (RowIndex row = 0; row < rowCount(); ++row) {
    out << row << ':';
    const auto end = rowEnd(row);
    for (auto it = rowBegin(row); it != end; ++it)
      out << ' ' << it.index() << '#' << it.scalar();
    out << '\n';
  }
}

void SparseMatrix::printStatistics(std::ostream& out) const {
  typedef mathic::ColumnPrinter ColPr;

  ColPr pr;
  pr.addColumn(false, " ", "");
  pr.addColumn(false, "", "");
  pr.addColumn(true, "", "");

  const auto memory = memoryUse();
  const auto colCount = computeColCount();
  const auto entryCount = this->entryCount();
  const uint64 area =
    static_cast<uint64>(rowCount()) * static_cast<uint64>(colCount);

  pr[0] << "\n/\n" << ColPr::commafy(rowCount())
    << " |\nrows |\n\\\n";

  const char* const line = "------------------\n";
  pr[1] << ColPr::commafy(colCount) << " \n"
    << line
    << ColPr::withSIPrefix(entryCount) << " -"
    << ColPr::percentIntegerFixed(entryCount, area) << " \n"
    << ColPr::bytesInUnit(memory) << " -"
    << ColPr::percentIntegerFixed(memoryUseTrimmed(), memory) << " \n"
    << line;

  pr[2] << "  columns\n\\\n| non-zero (density)\n| memory (used)\n/\n";

  out << '\n' << pr << "\n";
}

std::string SparseMatrix::toString() const {
  std::ostringstream out;
  print(out);
  return out.str();
}

void SparseMatrix::appendRowAndNormalize(
  const SparseMatrix& matrix,
  const RowIndex row,
  const Scalar modulus
) {
  MATHICGB_ASSERT(row < matrix.rowCount());
  auto it = matrix.rowBegin(row);
  const auto end = matrix.rowEnd(row);
  if (it != end) {
    appendEntry(it.index(), 1);
    const Scalar lead = it.scalar();
    ++it;
    if (it != end) {
      const Scalar inverse = modularInverse(lead, modulus);
      do {
        const uint32 prod = static_cast<uint32>(inverse) * it.scalar();
        const uint16 prodMod = static_cast<uint16>(prod % modulus);
        appendEntry(it.index(), prodMod);
        ++it;
      } while (it != end);
    }
  }
  rowDone();
}

void SparseMatrix::appendRow(const SparseMatrix& matrix, const RowIndex row) {
  MATHICGB_ASSERT(row < matrix.rowCount()); 

  const auto size = matrix.entryCountInRow(row);
  while (mBlock.mScalars.capacityToGo() < size)
    growEntryCapacity();
  MATHICGB_ASSERT(mBlock.mScalars.capacityToGo() ==
    mBlock.mColIndices.capacityToGo());

  auto const data = matrix.mRows[row];
  mBlock.mScalars.memcpy(data.mScalarsBegin, size);
  mBlock.mColIndices.memcpy(data.mIndicesBegin, size);
  rowDone();
}
  
SparseMatrix& SparseMatrix::operator=(const SparseMatrix& matrix) {
  // todo: use copy-swap or copy-move.
  clear();
  mMemoryQuantum = matrix.mMemoryQuantum;
  // A version that works on each block would be faster, but this is not
  // used anywhere time-critical right now. Improve this if it turns
  // up in profiling at some point.
  for (RowIndex row = 0; row < matrix.rowCount(); ++row)
    appendRow(matrix, row);
  return *this;
}

void SparseMatrix::swap(SparseMatrix& matrix) {
  mBlock.swap(matrix.mBlock);
  using std::swap;
  swap(mRows, matrix.mRows);
  swap(mMemoryQuantum, matrix.mMemoryQuantum);
}

bool SparseMatrix::operator==(const SparseMatrix& matrix) const {
  const auto count = rowCount();
  if (count != matrix.rowCount())
    return false;
  for (RowIndex row = 0; row < count; ++row) {
    if (entryCountInRow(row) != matrix.entryCountInRow(row))
      return false;
    const auto end = rowEnd(row);
    auto it = rowBegin(row);
    auto matrixIt = matrix.rowBegin(row);
    for (auto it = rowBegin(row); it != end; ++it, ++matrixIt)
      if (*it != *matrixIt)
        return false;
  }
  return true;
}

SparseMatrix::ColIndex SparseMatrix::computeColCount() const {
  // Obviously this can be done faster, but there has not been a need for that
  // so far.
  ColIndex colCount = 0;
  for (RowIndex row = 0; row < rowCount(); ++row) {
    const auto end = rowEnd(row);
    for (auto it = rowBegin(row); it != end; ++it)
      colCount = std::max(colCount, it.index() + 1);
  }
  return colCount;
}

void SparseMatrix::clear() {
  Block* block = &mBlock;
  while (block != 0) {
    delete[] block->mColIndices.releaseMemory();
    delete[] block->mScalars.releaseMemory();
    Block* const tmp = block->mPreviousBlock;
    if (block != &mBlock)
      delete block;
    block = tmp;
  }
  mBlock.mPreviousBlock = 0;
  mBlock.mHasNoRows = true;
  mRows.clear();
}

void SparseMatrix::appendRowWithModulus(
  std::vector<uint64> const& v,
  const Scalar modulus
) {
  const auto count = static_cast<ColIndex>(v.size());
  for (ColIndex col = 0; col < count; ++col) {
    const Scalar scalar = static_cast<Scalar>(v[col] % modulus);
    if (scalar != 0)
      appendEntry(col, scalar);
  }
  rowDone();
}

void SparseMatrix::appendRowWithModulusNormalized(
  std::vector<uint64> const& v,
  const Scalar modulus
) {
  uint16 multiply = 1; 
  bool first = true;
  const auto count = static_cast<ColIndex>(v.size());
  for (ColIndex col = 0; col < count; ++col) {
    Scalar scalar = static_cast<Scalar>(v[col] % modulus);
    if (scalar == 0)
      continue;
    if (first) {
      multiply = modularInverse(scalar, modulus);
      scalar = 1;
      first = false;
    } else {
      uint32 prod = static_cast<uint32>(multiply) * scalar;
      scalar = prod % modulus;
    }
    appendEntry(col, scalar);
  }
  rowDone();
}

bool SparseMatrix::appendRowWithModulusIfNonZero(
  std::vector<uint64> const& v,
  const Scalar modulus
) {
  appendRowWithModulus(v, modulus);
  MATHICGB_ASSERT(rowCount() > 0);
  if (mRows.back().empty()) {
    mRows.pop_back();
    return false;
  } else
    return true;
}

void SparseMatrix::trimLeadingZeroColumns(const ColIndex trimThisMany) {
  Block* block = &mBlock;
  for (; block != 0; block = block->mPreviousBlock) {
    const auto end = block->mColIndices.end();
    for (auto it = block->mColIndices.begin(); it != end; ++it) {
      MATHICGB_ASSERT(*it >= trimThisMany);
      *it -= trimThisMany;
    }
  }
}

void SparseMatrix::reserveFreeEntries(const size_t freeCount) {
  if (freeCount <= mBlock.mColIndices.capacity() - mBlock.mColIndices.size())
    return;
  // We need to copy over the pending entries, so we need space for those
  // entries on top of freeCount.
  const size_t count = freeCount + ( // todo: detect overflow for this addition
    mBlock.mHasNoRows ?
    mBlock.mColIndices.size() :
    std::distance(mRows.back().mIndicesEnd, mBlock.mColIndices.end())
  );

  // @todo: fix memory leaks on exception

  auto oldBlock = new Block(std::move(mBlock));
  MATHICGB_ASSERT(mBlock.mColIndices.begin() == 0);
  MATHICGB_ASSERT(mBlock.mScalars.begin() == 0);
  MATHICGB_ASSERT(mBlock.mHasNoRows);
  MATHICGB_ASSERT(mBlock.mPreviousBlock == 0);

  {
    const auto begin = new ColIndex[count];
    const auto capacityEnd = begin + count;
    mBlock.mColIndices.releaseAndSetMemory(begin, begin, capacityEnd);
  }

  {
    const auto begin = new Scalar[count];
    const auto capacityEnd = begin + count;
    mBlock.mScalars.releaseAndSetMemory(begin, begin, capacityEnd);
  }

  // copy pending entries over
  if (oldBlock->mHasNoRows) {
    mBlock.mColIndices.rawAssign
      (oldBlock->mColIndices.begin(), oldBlock->mColIndices.end());
    mBlock.mScalars.rawAssign
      (oldBlock->mScalars.begin(), oldBlock->mScalars.end());
    delete oldBlock; // no reason to keep it around
  } else {
    mBlock.mColIndices.rawAssign
      (mRows.back().mIndicesEnd, oldBlock->mColIndices.end());
    mBlock.mScalars.rawAssign
      (mRows.back().mScalarsEnd, oldBlock->mScalars.end());

    // remove the pending entries from old block so that counting the number
    // of entries will give the correct result in future.
    oldBlock->mColIndices.resize
      (std::distance(oldBlock->mColIndices.begin(), mRows.back().mIndicesEnd));
    oldBlock->mScalars.resize
      (std::distance(oldBlock->mScalars.begin(), mRows.back().mScalarsEnd));      
    mBlock.mPreviousBlock = oldBlock;
  }
}

void SparseMatrix::growEntryCapacity() {
  MATHICGB_ASSERT(mBlock.mColIndices.size() == mBlock.mScalars.size());
  MATHICGB_ASSERT(mBlock.mColIndices.capacity() == mBlock.mScalars.capacity());
  MATHICGB_ASSERT(mBlock.mColIndices.size() <= mBlock.mColIndices.capacity());

  // TODO: handle overflow of arithmetic here
  if (mMemoryQuantum != 0 &&
    (!mBlock.mHasNoRows || mBlock.mPreviousBlock == 0)
  )
    reserveFreeEntries(mMemoryQuantum);
  else if (mBlock.mColIndices.capacity() == 0)
    reserveFreeEntries(1 << 14); // minimum block size
  else {
    // do this if the quantum is not set or if the quantum is too small
    // to store a single row being built.
    reserveFreeEntries(mBlock.mColIndices.capacity() * 2);
  }

  MATHICGB_ASSERT(mBlock.mColIndices.size() <= mBlock.mColIndices.capacity());

  MATHICGB_ASSERT(mBlock.mColIndices.size() == mBlock.mScalars.size());
}

float SparseMatrix::computeDensity() const {
  const auto rowCount = static_cast<float>(this->rowCount());
  const auto colCount = static_cast<float>(computeColCount());
  const auto entryCount = static_cast<float>(this->entryCount());
  return entryCount / (rowCount * colCount);
}

size_t SparseMatrix::entryCount() const {
  size_t count = 0;
  const Block* block = &mBlock;
  for (; block != 0; block = block->mPreviousBlock)
    count += block->mColIndices.size();
  return count;
}

size_t SparseMatrix::memoryUse() const {
  size_t count = 0;
  for (auto block = &mBlock; block != 0; block = block->mPreviousBlock)
    count += block->memoryUse() + sizeof(Block);
  return count;
}

size_t SparseMatrix::memoryUseTrimmed() const {
  size_t count = 0;
  for (auto block = &mBlock; block != 0; block = block->mPreviousBlock)
    count += block->memoryUseTrimmed() + sizeof(Block);
  return count;
}

size_t SparseMatrix::Block::memoryUse() const {
  return mColIndices.memoryUse() + mScalars.memoryUse();
}

size_t SparseMatrix::Block::memoryUseTrimmed() const {
  return mColIndices.memoryUseTrimmed() + mScalars.memoryUseTrimmed();
}

std::ostream& operator<<(std::ostream& out, const SparseMatrix& matrix) {
  matrix.print(out);
  return out;
}

namespace {
  template<class T>
  T readOne(FILE* file) {
    T t;
    if (fread(&t, sizeof(T), 1, file) != 1)
      mathic::reportError("error while reading file.");
    return t;
  }

  template<class T>
  void writeOne(const T& t, FILE* file) {
    if (fwrite(&t, sizeof(T), 1, file) != 1)
      mathic::reportError("error while writing to file.");
  }

  template<class T>
  void writeMany(const std::vector<T>& v, FILE* file) {
    if (v.empty())
      return;
    if (fwrite(v.data(), sizeof(T), v.size(), file) != v.size())
      mathic::reportError("error while writing to file.");
  }

  template<class T>
  void readMany(FILE* file, size_t count, std::vector<T>& v) {
    size_t const originalSize = v.size();
    v.resize(originalSize + count);
    if (fread(v.data() + originalSize, sizeof(T), count, file) != count)
      mathic::reportError("error while reading file.");
  }

}

void SparseMatrix::write(const Scalar modulus, FILE* file) const {
  const auto storedRowCount = rowCount();

  writeOne(static_cast<uint32>(storedRowCount), file);
  writeOne(static_cast<uint32>(computeColCount()), file);
  writeOne(static_cast<uint32>(modulus), file);
  writeOne(static_cast<uint64>(entryCount()), file);

  // write scalars
  for (SparseMatrix::RowIndex row = 0; row < storedRowCount; ++row) {
    const auto count = entryCountInRow(row);
    if (fwrite(&rowBegin(row).scalar(), sizeof(uint16), count, file) != count)
      mathic::reportError("error while writing to file.");
  }

  // write indices
  for (SparseMatrix::RowIndex row = 0; row < storedRowCount; ++row) {
    const auto count = entryCountInRow(row);
    if (fwrite(&rowBegin(row).index(), sizeof(uint32), count, file) != count)
      mathic::reportError("error while writing to file.");
  }

  std::vector<uint32> entryCounts;
  for (SparseMatrix::RowIndex row = 0; row < storedRowCount; ++row)
    entryCounts.push_back(entryCountInRow(row));
  writeMany<uint32>(entryCounts, file);
}

SparseMatrix::Scalar SparseMatrix::read(FILE* file) {
  MATHICGB_ASSERT(file != 0);

  const auto rowCount = readOne<uint32>(file);
  const auto colCount = readOne<uint32>(file);
  const auto modulus = readOne<uint32>(file);
  const auto entryCount64 = readOne<uint64>(file);
  if (entryCount64 > std::numeric_limits<size_t>::max())
    throw std::bad_alloc();
  const auto entryCount = static_cast<size_t>(entryCount64);

  // Allocate memory to hold the matrix in one block.
  clear();
  reserveFreeEntries(entryCount);
  mRows.reserve(rowCount);
  MATHICGB_ASSERT(mBlock.mPreviousBlock == 0); // only one block

  // @todo: we can read directly into the block. Do that.

  // Read scalars.
  {
    mBlock.mScalars.resize(entryCount);
    std::vector<uint16> scalars;
    readMany(file, entryCount, scalars);
    std::copy(scalars.begin(), scalars.end(), mBlock.mScalars.begin());
  }

  // Read column indices.
  {
    mBlock.mColIndices.resize(entryCount);
    std::vector<uint32> indices;
    readMany(file, entryCount, indices);
    std::copy(indices.begin(), indices.end(), mBlock.mColIndices.begin());
  }

  // Read where rows begin and end.
  {
    std::vector<uint32> sizes;
    readMany(file, rowCount, sizes);
    uint32 runningOffset = 0;
    for (auto it = sizes.begin(); it != sizes.end(); ++it) {
      Row row;
      row.mIndicesBegin = mBlock.mColIndices.begin() + runningOffset;
      row.mScalarsBegin = mBlock.mScalars.begin() + runningOffset;
      runningOffset += *it;
      row.mIndicesEnd = mBlock.mColIndices.begin() + runningOffset;
      row.mScalarsEnd = mBlock.mScalars.begin() + runningOffset;
      mRows.push_back(row);
    }
    MATHICGB_ASSERT(runningOffset == entryCount);
  }

  MATHICGB_ASSERT(mBlock.mPreviousBlock == 0); // still only one block
  return modulus;
}

void SparseMatrix::writePBM(FILE* file) {
  // See http://netpbm.sourceforge.net/doc/pbm.html

  const auto rowCount = this->rowCount();
  auto colCount = this->computeColCount();
  colCount += 8-(colCount % 8);

  // Write PBM header
  {
    std::stringstream out;
    out << "P1 " << colCount << ' ' << rowCount << '\n';
    fputs(out.str().c_str(), file);
  }

  for (RowIndex row = 0; row < rowCount; ++row) {
    const auto end = rowEnd(row);
    auto it = rowBegin(row);

    unsigned char byte = 0;
    unsigned int bit = (1 << 8);
    for (ColIndex col = 0; col < colCount; ++col) {
      if (it != end && col == it.index()) {
        fputc('1', file);
        byte |= bit;
        ++it;
      } else
        fputc('0', file);

      bit >>= 1;
      if (bit == 0) {
//        fputc(byte, file);
        byte = 0;
        bit = (1 << 8);
      }
    }
    fputc('\n', file);
   // if (bit != (1 << 8))
     // fputc(byte, file);
  }
}

bool SparseMatrix::debugAssertValid() const {
  for (RowIndex row = 0; row < rowCount(); ++row) {
    for (auto it = rowBegin(row); it != rowEnd(row); ++it) {
      // A scalar of 0 is not necessarily bad, it is just not expected
      // at the time of writing this assert. Feel free to remove this assert
      // if you are creating scalars that are zero on purpose.
      MATHICGB_ASSERT(it.scalar() != 0);
    }
  }
  return true;
}

MATHICGB_NAMESPACE_END
