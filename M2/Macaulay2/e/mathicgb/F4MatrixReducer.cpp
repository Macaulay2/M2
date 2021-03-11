// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "F4MatrixReducer.hpp"

#include "QuadMatrix.hpp"
#include "SparseMatrix.hpp"
#include "PolyRing.hpp"
#include "LogDomain.hpp"
#include "mtbb.hpp"
#include <algorithm>
#include <vector>
#include <stdexcept>
#include <map>  
#include <string>
#include <cstdio>
#include <iostream>

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixReduce,
  "Displays statistics about matrices that are row reduced."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatReduceTop,
  "Displays time to reduce each F4 matrix to the bottom right submatrix."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  F4RedBottomRight,
  "Displays time to reduce the bottom right submatrix of each F4 matrix."
);

MATHICGB_NAMESPACE_BEGIN

namespace {
  class DenseRow {
  public:
    typedef uint16 Scalar;
    typedef uint32 ScalarProduct;
    typedef uint64 ScalarProductSum;

    static ScalarProduct product(const Scalar a, const Scalar b) {
      return static_cast<ScalarProduct>(a) * b;
    }

    static void multiplyAdd(
      const Scalar a,
      const Scalar b,
      ScalarProductSum& x
    ) {
      x += product(a, b);
    }

    static void add(const Scalar a, ScalarProductSum& sum) {
      sum += a;
    }

    Scalar modulusOf(ScalarProductSum x, Scalar modulus) {
      return static_cast<Scalar>(x % modulus);
    }

    DenseRow() {}
    DenseRow(size_t colCount): mEntries(colCount) {}

    /// returns false if all entries are zero
    bool takeModulus(const SparseMatrix::Scalar modulus) {
      ScalarProductSum bitwiseOr = 0; // bitwise or of all entries after modulus
      const auto end = mEntries.end();
      for (auto it = mEntries.begin(); it != end; ++it) {
        if (*it >= modulus)
          *it = modulusOf(*it, modulus);
        bitwiseOr |= *it;
      }
      return bitwiseOr != 0;
    }

    size_t colCount() const {return mEntries.size();}
    bool empty() const {return mEntries.empty();}

    void clear(size_t colCount = 0) {
      mEntries.clear();
      mEntries.resize(colCount);
    }

    ScalarProductSum& operator[](size_t col) {
      MATHICGB_ASSERT(col < colCount());
      return mEntries[col];
    }

    ScalarProductSum const& operator[](size_t col) const {
      MATHICGB_ASSERT(col < colCount());
      return mEntries[col];
    }

    void appendTo(SparseMatrix& matrix) {matrix.appendRow(mEntries);}

    void makeUnitary(const SparseMatrix::Scalar modulus, const size_t lead) {
      MATHICGB_ASSERT(lead < colCount());
      MATHICGB_ASSERT(mEntries[lead] != 0);

      const auto end = mEntries.end();
      auto it = mEntries.begin() + lead;
      const auto toInvert = static_cast<SparseMatrix::Scalar>(*it % modulus);
      const auto multiply = modularInverse(toInvert, modulus);
      *it = 1;
      for (++it; it != end; ++it) {
        const auto entry = modulusOf(*it, modulus);
        if (entry != 0)
          *it = modulusOf(product(entry, multiply), modulus);
        else
          *it = 0;
      }
    }

    void addRow(const SparseMatrix& matrix, SparseMatrix::RowIndex row) {
      MATHICGB_ASSERT(row < matrix.rowCount());
      const auto end = matrix.rowEnd(row);
      for (auto it = matrix.rowBegin(row); it != end; ++it) {
        MATHICGB_ASSERT(it.index() < colCount());
        add(it.scalar(), mEntries[it.index()]);
      }
    }

    template<class Iter>
    void addRowMultiple(
      const SparseMatrix::Scalar multiple,
      const Iter begin,
      const Iter end
    ) {
      // I have a matrix reduction that goes from 2.8s to 2.4s on MSVC 2012 by
      // using entries instead of mEntries, even after removing restrict and
      // const from entries. That does not make sense to me, but it is a fact
      // none-the-less, so don't replace entries by mEntries unless you think
      // it's worth a 14% slowdown of matrix reduction (the whole computation,
      // not just this method).
      ScalarProductSum* const MATHICGB_RESTRICT entries = mEntries.data();

#ifdef MATHICGB_DEBUG
      // These asserts are separated out since otherwise they would also need
      // to be duplicated due to the manual unrolling.
      for (auto it = begin; it != end; ++it) {
        MATHICGB_ASSERT(it.index() < colCount());
        MATHICGB_ASSERT(entries + it.index() == &mEntries[it.index()]);
      }
#endif
      // I have a matrix reduction that goes from 2.601s to 2.480s on MSVC 2012
      // by unrolling this loop manually. Unrolling more than once was not a
      // benefit. So don't undo the unrolling unless you think it's worth a 5%
      // slowdown of matrix reduction (the whole computation, not just this
      // method).

      auto it = begin;
      if (std::distance(begin, end) % 2 == 1) {
        // Replacing this by a goto into the middle of the following loop
        // (similar to Duff's device) made the code slower on MSVC 2012.
        multiplyAdd(it.scalar(), multiple, entries[it.index()]);
        ++it;
      }
      while (it != end) {
        multiplyAdd(it.scalar(), multiple, entries[it.index()]);
        ++it;
        multiplyAdd(it.scalar(), multiple, entries[it.index()]);
        ++it;
      }
    }

    void rowReduceByUnitary(
      const SparseMatrix::RowIndex pivotRow,
      const SparseMatrix& matrix,
      const SparseMatrix::Scalar modulus
    ) {
      MATHICGB_ASSERT(matrix.rowBegin(pivotRow).scalar() == 1); // unitary
      MATHICGB_ASSERT(modulus > 1);

      auto begin = matrix.rowBegin(pivotRow);
      const auto col = begin.index();
      const auto entry = modulusOf(mEntries[col], modulus);
      mEntries[col] = 0;
      if (entry == 0)
        return;
      ++begin; // can skip first entry as we just set it to zero.
      addRowMultiple(
        modularNegativeNonZero(entry, modulus),
        begin,
        matrix.rowEnd(pivotRow)
      );
    }

  private:
    std::vector<ScalarProductSum> mEntries;
  };

  SparseMatrix reduce(
    const QuadMatrix& qm,
    SparseMatrix::Scalar modulus
  ) {
    const SparseMatrix& toReduceLeft = qm.bottomLeft;
    const SparseMatrix& toReduceRight = qm.bottomRight;
    const SparseMatrix& reduceByLeft = qm.topLeft;
    const SparseMatrix& reduceByRight = qm.topRight;

    const auto leftColCount = qm.computeLeftColCount();
    const auto rightColCount =
      static_cast<SparseMatrix::ColIndex>(qm.computeRightColCount());
    MATHICGB_ASSERT(leftColCount == reduceByLeft.rowCount());
    const auto pivotCount = leftColCount;
    const auto rowCount = toReduceLeft.rowCount();

    // ** pre-calculate what rows are pivots for what columns.

    // Store column indexes instead of row indices as the matrix is square
    // anyway (so all indices fit) and we are going to store this as a column
    // index later on.
    std::vector<SparseMatrix::ColIndex> rowThatReducesCol(pivotCount);
#ifdef MATHICGB_DEBUG
    // fill in an invalid value that can be recognized by asserts to be invalid.
    std::fill(rowThatReducesCol.begin(), rowThatReducesCol.end(), pivotCount);
#endif
    for (SparseMatrix::ColIndex pivot = 0; pivot < pivotCount; ++pivot) {
      MATHICGB_ASSERT(!reduceByLeft.emptyRow(pivot));
      SparseMatrix::ColIndex col = reduceByLeft.leadCol(pivot);
      MATHICGB_ASSERT(rowThatReducesCol[col] == pivotCount);
      rowThatReducesCol[col] = pivot;
    }
#ifdef MATHICGB_DEBUG
    for (SparseMatrix::ColIndex col = 0; col < pivotCount; ++col) {
      MATHICGB_ASSERT(rowThatReducesCol[col] < pivotCount);
    }
#endif

    SparseMatrix reduced(qm.topRight.memoryQuantum());

    mgb::mtbb::enumerable_thread_specific<DenseRow> denseRowPerThread([&](){
      return DenseRow();
    }); 

    SparseMatrix tmp(qm.topRight.memoryQuantum());

    std::vector<SparseMatrix::RowIndex> rowOrder(rowCount);

    mgb::mtbb::mutex lock;
    mgb::mtbb::parallel_for(mgb::mtbb::blocked_range<SparseMatrix::RowIndex>(0, rowCount, 2),
      [&](const mgb::mtbb::blocked_range<SparseMatrix::RowIndex>& range)
    {
      auto& denseRow = denseRowPerThread.local();
      for (auto it = range.begin(); it != range.end(); ++it) {
        const auto row = it;
        denseRow.clear(leftColCount);
        denseRow.addRow(toReduceLeft, row);

        MATHICGB_ASSERT(leftColCount == pivotCount);
        for  (size_t pivot = 0; pivot < pivotCount; ++pivot) {
          if (denseRow[pivot] != 0) {
            auto entry = denseRow[pivot];
            entry %= modulus;
            if (entry == 0) {
              denseRow[pivot] = 0;
            } else {
              entry = modulus - entry;
              const auto row = rowThatReducesCol[pivot];
              MATHICGB_ASSERT(row < pivotCount);
              MATHICGB_ASSERT(!reduceByLeft.emptyRow(row));
              MATHICGB_ASSERT(reduceByLeft.leadCol(row) == pivot);
              MATHICGB_ASSERT(entry < std::numeric_limits<SparseMatrix::Scalar>::max());
              denseRow.addRowMultiple(
                static_cast<SparseMatrix::Scalar>(entry),
                ++reduceByLeft.rowBegin(row),
                reduceByLeft.rowEnd(row)
              );
              denseRow[pivot] = entry;
            }
          }
        }
        mgb::mtbb::mutex::scoped_lock lockGuard(lock);
        for (size_t pivot = 0; pivot < pivotCount; ++pivot) {
		  MATHICGB_ASSERT(denseRow[pivot] < std::numeric_limits<SparseMatrix::Scalar>::max());
          if (denseRow[pivot] != 0)
            tmp.appendEntry(rowThatReducesCol[pivot], static_cast<SparseMatrix::Scalar>(denseRow[pivot]));
	    }
        tmp.rowDone();
        rowOrder[tmp.rowCount() - 1] = row;
      }
    });

    mgb::mtbb::parallel_for(mgb::mtbb::blocked_range<SparseMatrix::RowIndex>(0, rowCount),
      [&](const mgb::mtbb::blocked_range<SparseMatrix::RowIndex>& range)
      {for (auto iter = range.begin(); iter != range.end(); ++iter)
    {
      const auto i = iter;
      const auto row = rowOrder[i];
      auto& denseRow = denseRowPerThread.local();

      denseRow.clear(rightColCount);
      denseRow.addRow(toReduceRight, row);
      auto it = tmp.rowBegin(i);
      const auto end = tmp.rowEnd(i);
      for (; it != end; ++it) {
        const auto begin = reduceByRight.rowBegin(it.index());
        const auto end = reduceByRight.rowEnd(it.index());
        denseRow.addRowMultiple(it.scalar(), begin, end);
      }

      mgb::mtbb::mutex::scoped_lock lockGuard(lock);
      bool zero = true;
	  for (SparseMatrix::ColIndex col = 0; col < rightColCount; ++col) {
        const auto entry =
          static_cast<SparseMatrix::Scalar>(denseRow[col] % modulus);
        if (entry != 0) {
          reduced.appendEntry(col, entry);
          zero = false;
        }
      }
      if (!zero)
        reduced.rowDone();
    }});
    return reduced;
  }

  SparseMatrix reduceToEchelonFormSparse(
    const SparseMatrix& toReduce,
    const SparseMatrix::Scalar modulus
  ) {
    const auto colCount = toReduce.computeColCount();

    const auto noRow = static_cast<SparseMatrix::RowIndex>(-1);

    // pivotRowOfCol[i] is the pivot in column i or noRow
    // if we have not identified such a pivot so far.
    std::vector<SparseMatrix::RowIndex> pivotRowOfCol(colCount, noRow);

    DenseRow rowToReduce(colCount);

    // ** Reduce to row echelon form -- every row is a pivot row.
    SparseMatrix pivots(colCount);
    for (SparseMatrix::RowIndex row = 0; row < toReduce.rowCount(); ++row) {
      if (toReduce.emptyRow(row))
        continue;
      rowToReduce.clear(colCount);
      rowToReduce.addRow(toReduce, row);

      SparseMatrix::ColIndex leadingCol = 0;
      while (true) { // reduce row by previous pivots
        for (; leadingCol < colCount; ++leadingCol) {
          auto& entry = rowToReduce[leadingCol];
          if (entry != 0) {
            entry %= modulus;
            if (entry != 0)
              break;
          }
        }
        if (leadingCol == colCount)
          break; // The row has been reduced to zero.
        const auto pivotRow = pivotRowOfCol[leadingCol];
        if (pivotRow == noRow) { // If the row is a new pivot.
          rowToReduce.makeUnitary(modulus, leadingCol);
          pivotRowOfCol[leadingCol] = pivots.rowCount();
          rowToReduce.appendTo(pivots);
          break;
        }
        rowToReduce.rowReduceByUnitary(pivotRow, pivots, modulus);
      }
    }

    // ** Reduce from row echelon form to reduced row echelon form

    SparseMatrix reduced(colCount);
    auto pivotCol = colCount;
    // Reduce pivot rows in descending order of leading column. The reduced
    // pivots go into reduced and we update pivotRowOfCol to refer to the
    // row indices in reduced as we go along.
    while (pivotCol != 0) {
      --pivotCol;
      const auto row = pivotRowOfCol[pivotCol];
      if (row == noRow)
        continue;
      rowToReduce.clear(colCount);
      rowToReduce.addRow(pivots, row);
      MATHICGB_ASSERT(rowToReduce[pivotCol] == 1); // unitary
      for (auto col = pivotCol + 1; col != colCount; ++col) {
        auto& entry = rowToReduce[col];
        if (entry == 0)
          continue;
        entry %= modulus;
        if (entry == 0)
          continue;
        const auto pivotRow = pivotRowOfCol[col];
        if (pivotRow != noRow)
          rowToReduce.rowReduceByUnitary(pivotRow, reduced, modulus);
        MATHICGB_ASSERT(entry < modulus);
      }
      pivotRowOfCol[pivotCol] = reduced.rowCount();
      rowToReduce.appendTo(reduced);
    }
    return reduced;
  }

  SparseMatrix reduceToEchelonForm(
    const SparseMatrix& toReduce,
    const SparseMatrix::Scalar modulus
  ) {
    const auto colCount = toReduce.computeColCount();
    const auto rowCount = toReduce.rowCount();

    // convert to dense representation 
    std::vector<DenseRow> dense(rowCount);
    mgb::mtbb::parallel_for(mgb::mtbb::blocked_range<SparseMatrix::RowIndex>(0, rowCount),
      [&](const mgb::mtbb::blocked_range<SparseMatrix::RowIndex>& range)
      {for (auto it = range.begin(); it != range.end(); ++it)
    {
      const auto row = it;
      if (toReduce.emptyRow(row))
        continue;
      dense[row].clear(colCount);
      dense[row].addRow(toReduce, row);
    }});

    // invariant: all columns in row to the left of leadCols[row] are zero.
    std::vector<SparseMatrix::ColIndex> leadCols(rowCount);

    // pivot rows get copied here before being used to reduce the matrix.
    SparseMatrix reduced(toReduce.memoryQuantum());

    // (col,row) in nextReducers, then use row as a pivot in column col
    // for the next iteration.
    std::vector< std::pair<SparseMatrix::ColIndex, SparseMatrix::RowIndex> > nextReducers;

    // isPivotRow[row] is true if row is or has been used as a pivot.
    std::vector<bool> isPivotRow(rowCount);

    // columnHasPivot[col] is true if a pivot row for column col has
    // been chosen.
    std::vector<bool> columnHasPivot(colCount);

    bool firstIteration = true;
    while (firstIteration || reduced.rowCount() > 0) {
      firstIteration = false;
      size_t const reducerCount = reduced.rowCount();

      //std::cout << "reducing " << reduced.rowCount() << " out of " << toReduce.rowCount() << std::endl;
      mgb::mtbb::mutex lock;
      mgb::mtbb::parallel_for(mgb::mtbb::blocked_range<SparseMatrix::RowIndex>(0, rowCount),
        [&](const mgb::mtbb::blocked_range<SparseMatrix::RowIndex>& range)
        {for (auto it = range.begin(); it != range.end(); ++it)
      {
        const auto row = it;
        MATHICGB_ASSERT(leadCols[row] <= colCount);
        DenseRow& denseRow = dense[row];
        if (denseRow.empty())
          continue;

        // reduce by each row of reduced.
        for (SparseMatrix::RowIndex reducerRow = 0; reducerRow < reducerCount; ++reducerRow) {
          const auto col = reduced.rowBegin(reducerRow).index();
          if (denseRow[col] == 0 || (isPivotRow[row] && col == leadCols[row]))
            continue;
          denseRow.rowReduceByUnitary(reducerRow, reduced, modulus);
        }

        // update leadCols[row]
        SparseMatrix::ColIndex col;
        MATHICGB_ASSERT(leadCols[row] <= colCount);
        for (col = leadCols[row]; col < colCount; ++col) {
          denseRow[col] %= modulus;
          if (denseRow[col] != 0)
            break;
        }
        leadCols[row] = col;
        MATHICGB_ASSERT(leadCols[row] <= colCount);

        // note if we have found a new pivot row
        if (col == colCount)
          denseRow.clear();
        else {
          MATHICGB_ASSERT(col < colCount);
          bool isNewReducer = false;
          {
            mgb::mtbb::mutex::scoped_lock lockGuard(lock);
            if (!columnHasPivot[col]) {
              columnHasPivot[col] = true;
              isNewReducer = true;
              nextReducers.push_back(std::make_pair(col, row));
            }
          }
          if (isNewReducer)
            denseRow.makeUnitary(modulus, col);
        }
      }});

      reduced.clear();
      std::sort(nextReducers.begin(), nextReducers.end());
      for (size_t i = 0; i < nextReducers.size(); ++i) {
        size_t const row = nextReducers[i].second;

        MATHICGB_ASSERT(static_cast<bool>
          (columnHasPivot[nextReducers[i].first]));
        MATHICGB_ASSERT(dense[row].colCount() == colCount);
        MATHICGB_ASSERT(dense[row][nextReducers[i].first] == 1);
        MATHICGB_ASSERT(reduced.rowCount() == i);
        MATHICGB_ASSERT(!isPivotRow[row]);

        dense[row].appendTo(reduced); // already unitary
        isPivotRow[row] = true;
      }
      nextReducers.clear();
    }

    mgb::mtbb::parallel_for(mgb::mtbb::blocked_range<size_t>(0, rowCount),
      [&](const mgb::mtbb::blocked_range<size_t>& range)
      {for (auto it = range.begin(); it != range.end(); ++it)
    {
      const size_t row = it;
      dense[row].takeModulus(modulus);
    }});

#ifdef MATHICGB_DEBUG
    std::vector<char> sawPivot(colCount);
    for (SparseMatrix::RowIndex row = 0; row < rowCount; ++row) {
      if (dense[row].empty()) {
        MATHICGB_ASSERT(!isPivotRow[row]);
        MATHICGB_ASSERT(leadCols[row] == colCount);
      } else {
        MATHICGB_ASSERT(isPivotRow[row]);

        const auto leadCol = leadCols[row];
        MATHICGB_ASSERT(leadCol < colCount);
        MATHICGB_ASSERT(columnHasPivot[leadCol]);
        MATHICGB_ASSERT(dense[row][leadCol] == 1);
        MATHICGB_ASSERT(!sawPivot[leadCol]);
        sawPivot[leadCol] = true;
        for (size_t col = 0; col < colCount; ++col) {
          const auto scalar = dense[row][col];
          if (col < leadCol) {
            MATHICGB_ASSERT(scalar == 0);
          } else if (col == leadCol) {
            MATHICGB_ASSERT(scalar == 1);
          } else {
            MATHICGB_ASSERT(scalar == 0 || !columnHasPivot[col]);
          }
        }
      }
    }
#endif

    reduced.clear();
    for (size_t row = 0; row < rowCount; ++row)
      if (!dense[row].empty())
        dense[row].appendTo(reduced);
    return reduced;
  }
}

void addRowMultipleInplace(
  std::vector< std::vector<SparseMatrix::Scalar> >& matrix,
  const SparseMatrix::RowIndex addRow,
  const SparseMatrix::Scalar multiple,
  const SparseMatrix::RowIndex row,
  const SparseMatrix::ColIndex leadingCol,
  const SparseMatrix::ColIndex colCount,
  const SparseMatrix::Scalar modulus
) {
  assert(addRow < matrix.size());
  assert(row < matrix.size());
  assert(row != addRow);
  assert(leadingCol < colCount);
  assert(matrix[row].size() == colCount);
  assert(matrix[addRow].size() == colCount);
  for(auto col = leadingCol; col < colCount; ++col){
    const auto product = modularProduct
      (multiple, matrix[addRow][col], modulus);
    matrix[row][col] = modularSum(matrix[row][col], product, modulus);
  }
}

void makeRowUnitary(
  std::vector< std::vector<SparseMatrix::Scalar>>& matrix,
  const SparseMatrix::RowIndex row,
  const SparseMatrix::ColIndex colCount,
  const SparseMatrix::ColIndex leadingCol,
  const SparseMatrix::Scalar modulus
) {
  assert(row<matrix.size());
  assert(matrix[row].size() == colCount);
  assert(leadingCol < colCount);
  assert(modulus> 1);
  const auto leadingScalar = matrix[row][leadingCol];
  assert(leadingScalar != 0);
  auto multiply = modularInverse(leadingScalar, modulus);
  for(SparseMatrix::ColIndex col = leadingCol; col < colCount; ++col)
    matrix[row][col] = modularProduct(matrix[row][col], multiply, modulus);
}

SparseMatrix::ColIndex leadingColumn(
  const std::vector< std::vector<SparseMatrix::Scalar>>& matrix,
  const SparseMatrix::RowIndex row,
  const SparseMatrix::ColIndex colCount,
  SparseMatrix::ColIndex startAtCol 
) {
  assert(row < matrix.size());
  assert(matrix[row].size() == colCount);
  for(auto col = startAtCol; col < colCount; ++col){
    if(matrix[row][col] != 0)
      return col;
  }
  return colCount;
}

void rowReducedEchelonMatrix(
  std::vector< std::vector<SparseMatrix::Scalar> >& matrix,
  const SparseMatrix::ColIndex colCount,
  const SparseMatrix::Scalar modulus
) {
  assert(matrix.empty() || matrix[0].size() == colCount);
  const	SparseMatrix::RowIndex rowCount =
    static_cast<SparseMatrix::RowIndex>(matrix.size());
  // pivotRowOfCol[i] is the pivot in column i or rowCount
  // if we have not identified such a pivot so far.
  std::vector<SparseMatrix::RowIndex> pivotRowOfCol(colCount, rowCount);
  
  // row reduce to row echelon form
  for(SparseMatrix::RowIndex row=0; row<rowCount;++row) { 
    SparseMatrix::ColIndex leadingCol = 0;
    while (true) { // reduce row by previous pivots
      leadingCol = leadingColumn(matrix, row, colCount, leadingCol);
      if(leadingCol==colCount)
        break; // row was zero
      const auto pivotRow = pivotRowOfCol[leadingCol];
      if(pivotRow == rowCount) {
        makeRowUnitary(matrix, row, colCount, leadingCol, modulus);
        pivotRowOfCol[leadingCol] = row;
        break; // row is now a pivot
      }
      const auto multiple = modularNegative(matrix[row][leadingCol], modulus);
	  addRowMultipleInplace
	    (matrix, pivotRow, multiple, row, leadingCol, colCount, modulus);
    }
  }

  // row reduce to reduced row echelon form
  for (SparseMatrix::RowIndex row = 0; row < rowCount;++row) { 
    const auto lead = leadingColumn(matrix, row, colCount, 0);
    if (lead == colCount)
      continue; // row is zero
    for (auto col = lead + 1; col < colCount; ++col) {
      const auto pivotRow = pivotRowOfCol[col];
      if(pivotRow == rowCount)
        continue; // no pivot for this column
      const auto multiple = modularNegative(matrix[row][col], modulus);
	  addRowMultipleInplace
        (matrix, pivotRow, multiple, row, col, colCount, modulus);
    }
  }
}   

SparseMatrix reduceToEchelonFormShrawan(
  const SparseMatrix& toReduce,
  SparseMatrix::Scalar modulus
) {
  const SparseMatrix::RowIndex rowCount = toReduce.rowCount();
  const auto colCount = toReduce.computeColCount();

  // Convert input matrix to dense format
  std::vector< std::vector<SparseMatrix::Scalar>> matrix(rowCount);
  for (SparseMatrix::RowIndex row = 0; row < rowCount; ++row) {
    MATHICGB_ASSERT(!toReduce.emptyRow(row));
    matrix[row].resize(colCount);
    const auto end = toReduce.rowEnd(row);
    for (auto it = toReduce.rowBegin(row); it != end; ++it) {
      MATHICGB_ASSERT(it.index() < colCount);
      matrix[row][it.index()] = it.scalar();
    }
  }

  rowReducedEchelonMatrix(matrix, colCount, modulus);

  // convert reduced matrix to SparseMatrix.
  SparseMatrix reduced;
  for (size_t row = 0; row < rowCount; ++row) {
    bool rowIsZero = true;
    for (SparseMatrix::ColIndex col = 0; col < colCount; ++col) {
      if (matrix[row][col] != 0) {
        rowIsZero = false;
        reduced.appendEntry(col, matrix[row][col]);
      }
    }
    if (!rowIsZero)
      reduced.rowDone();
  }
  return reduced;
}

SparseMatrix reduceToEchelonFormShrawanDelayedModulus(
  const SparseMatrix& toReduce,
  SparseMatrix::Scalar modulus
) {
  const SparseMatrix::RowIndex rowCount = toReduce.rowCount();
  const auto colCount = toReduce.computeColCount();

  // Convert input matrix to dense format
  std::vector< std::vector<SparseMatrix::Scalar>> matrix(rowCount);
  for (SparseMatrix::RowIndex row = 0; row < rowCount; ++row) {
    MATHICGB_ASSERT(!toReduce.emptyRow(row));
    matrix[row].resize(colCount);
    const auto end = toReduce.rowEnd(row);
    for (auto it = toReduce.rowBegin(row); it != end; ++it) {
      MATHICGB_ASSERT(it.index() < colCount);
      matrix[row][it.index()] = it.scalar();
    }
  }

  rowReducedEchelonMatrix(matrix, colCount, modulus);

  // convert reduced matrix to SparseMatrix.
  SparseMatrix reduced;
  for (size_t row = 0; row < rowCount; ++row) {
    bool rowIsZero = true;
    for (SparseMatrix::ColIndex col = 0; col < colCount; ++col) {
      if (matrix[row][col] != 0) {
        rowIsZero = false;
        reduced.appendEntry(col, matrix[row][col]);
      }
    }
    if (!rowIsZero)
      reduced.rowDone();
  }
  return reduced;
}

SparseMatrix F4MatrixReducer::reduceToBottomRight(const QuadMatrix& matrix) {
  MATHICGB_ASSERT(matrix.debugAssertValid());
  MATHICGB_LOG_TIME(F4MatReduceTop);
  MATHICGB_LOG_TIME(F4MatrixReduce) <<
    "\n***** Reducing QuadMatrix to bottom right matrix *****\n";
  MATHICGB_IF_STREAM_LOG(F4MatrixReduce)
    {matrix.printStatistics(log.stream());};

  return reduce(matrix, mModulus);
}

SparseMatrix F4MatrixReducer::reducedRowEchelonForm(
  const SparseMatrix& matrix
) {
  MATHICGB_LOG_TIME(F4RedBottomRight);
  MATHICGB_LOG_TIME(F4MatrixReduce) <<
    "\n***** Reducing SparseMatrix to reduced row echelon form *****\n";
  MATHICGB_IF_STREAM_LOG(F4MatrixReduce)
    {matrix.printStatistics(log.stream());};

  const bool useShrawan = false;
  const bool useDelayedModulus = false;
  if (useShrawan) {
    if (useDelayedModulus)
      return reduceToEchelonFormShrawanDelayedModulus(matrix, mModulus);
    else    
      return reduceToEchelonFormShrawan(matrix, mModulus);
  } else {
    // todo: actually do some work to find a good way to determine
    // when to use the sparse method, or alternatively make some
    // sort of hybrid.
    if (matrix.computeDensity() < 0.02)
      return reduceToEchelonFormSparse(matrix, mModulus);
    else
      return reduceToEchelonForm(matrix, mModulus);
  }
}

SparseMatrix F4MatrixReducer::reducedRowEchelonFormBottomRight(
  const QuadMatrix& matrix
) {
  return reducedRowEchelonForm(reduceToBottomRight(matrix));
}

namespace {
  /// this has to be a separate function that returns the scalar since signed
  /// overflow is undefine behavior so we cannot check after the cast and
  /// we also cannot set the modulus field inside the constructor since it is
  /// const.
  SparseMatrix::Scalar checkModulus(const coefficient modulus) {
    // this assert has to be NO_ASSUME as otherwise the branch below will get
    // optimized out.
    MATHICGB_ASSERT_NO_ASSUME(modulus <=
      std::numeric_limits<SparseMatrix::Scalar>::max());
    if (modulus > std::numeric_limits<SparseMatrix::Scalar>::max())
      throw std::overflow_error("Too large modulus in F4 matrix reduction.");
    return static_cast<SparseMatrix::Scalar>(modulus);
  }
}

F4MatrixReducer::F4MatrixReducer(const coefficient modulus):
  mModulus(checkModulus(modulus)) {}

MATHICGB_NAMESPACE_END
