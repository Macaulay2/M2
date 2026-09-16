#include "basic-mutable-matrices/smat.hpp"

#include <gtest/gtest.h>

#include <cstddef>
#include <cstdlib>
#include <initializer_list>
#include <limits>
#include <memory>
#include <vector>

#include "basic-rings/aring-ZZp.hpp"
#include "util.hpp"

namespace {

M2_arrayint indices(std::initializer_list<int> values)
{ return stdvector_to_M2_arrayint(std::vector<int>(values)); }

class SMatTest : public ::testing::Test
{
 protected:
  using Ring = M2::ARingZZp;
  using Mat = SMat<Ring>;
  Ring ring {101};

  Ring::ElementType scalar(int value) const
  {
    Ring::Element result(ring);
    ring.set(result, value);
    return result;
  }

  void fill(Mat& matrix, std::initializer_list<int> values)
  {
    ASSERT_EQ(values.size(), matrix.numRows() * matrix.numColumns());
    auto value = values.begin();
    for (size_t r = 0; r < matrix.numRows(); ++r)
      for (size_t c = 0; c < matrix.numColumns(); ++c)
        matrix.set_entry(r, c, scalar(*value++));
  }

  void expectMatrix(const Mat& matrix,
                    size_t rows,
                    size_t cols,
                    const std::vector<int>& values)
  {
    ASSERT_EQ(matrix.numRows(), rows);
    ASSERT_EQ(matrix.numColumns(), cols);
    ASSERT_EQ(values.size(), rows * cols);
    const Ring& R = matrix.ring();
    Ring::Element actual(R), expected(R);
    bool zero = true;
    for (size_t c = 0; c < cols; ++c)
      {
        SCOPED_TRACE(::testing::Message() << "column " << c);
        size_t count = 0;
        size_t lead = std::numeric_limits<size_t>::max();
        for (size_t r = 0; r < rows; ++r)
          {
            SCOPED_TRACE(::testing::Message() << "row " << r);
            R.set(expected, values[r * cols + c]);
            bool nonzero = !R.is_zero(expected);
            EXPECT_EQ(matrix.get_entry(r, c, actual), nonzero);
            if (nonzero)
              {
                EXPECT_EQ(R.coerceToLongInteger(actual),
                          R.coerceToLongInteger(expected));
                lead = r;
                ++count;
                zero = false;
              }
          }
        EXPECT_EQ(matrix.lead_row(c), lead);
        EXPECT_EQ(matrix.lead_row(c, actual), lead);
        if (count != 0)
          {
            R.set(expected, values[lead * cols + c]);
            EXPECT_EQ(R.coerceToLongInteger(actual),
                      R.coerceToLongInteger(expected));
          }

        // Check the stored nodes as well as lookup: zero nodes and broken row
        // ordering can be invisible when only the matrix entries are checked.
        auto it = matrix.begin();
        it.set(c);
        size_t previous = rows;
        size_t visited = 0;
        while (it.valid())
          {
            ASSERT_LT(visited, count);
            ASSERT_LT(it.row(), previous);
            R.set(expected, values[it.row() * cols + c]);
            EXPECT_FALSE(R.is_zero(it.value()));
            EXPECT_EQ(R.coerceToLongInteger(it.value()),
                      R.coerceToLongInteger(expected));
            previous = it.row();
            ++visited;
            it.next();
          }
        EXPECT_EQ(visited, count);
      }
    EXPECT_EQ(matrix.is_zero(), zero);
  }
};

TEST_F(SMatTest, construction)
{
  // Empty dimensions and rectangular zero matrices must preserve their shape.
  Mat empty;
  EXPECT_EQ(empty.numRows(), 0);
  EXPECT_EQ(empty.numColumns(), 0);
  EXPECT_TRUE(empty.is_zero());
  EXPECT_FALSE(empty.is_dense());

  for (const auto& shape : {std::vector<size_t> {0, 0}, {0, 3}, {3, 0}, {2, 3}})
    {
      SCOPED_TRACE(::testing::Message() << shape[0] << " x " << shape[1]);
      Mat matrix(ring, shape[0], shape[1]);
      EXPECT_EQ(&matrix.ring(), &ring);
      EXPECT_FALSE(matrix.is_dense());
      expectMatrix(
          matrix, shape[0], shape[1], std::vector<int>(shape[0] * shape[1], 0));
    }
}

TEST_F(SMatTest, entryInsertionReplacementAndRemoval)
{
  // Insert out of order, replace, then remove head, interior, and tail nodes.
  Mat matrix(ring, 5, 2);
  matrix.set_entry(2, 0, scalar(3));
  matrix.set_entry(0, 0, scalar(-2));
  matrix.set_entry(4, 0, scalar(7));
  matrix.set_entry(3, 0, scalar(5));
  matrix.set_entry(1, 0, scalar(0));
  matrix.set_entry(2, 0, scalar(-4));
  expectMatrix(matrix, 5, 2, {-2, 0, 0, 0, -4, 0, 5, 0, 7, 0});

  for (size_t row : {3, 4, 0, 2}) matrix.set_entry(row, 0, scalar(0));
  matrix.set_entry(2, 1, scalar(0));
  expectMatrix(matrix, 5, 2, std::vector<int>(10, 0));
}

TEST_F(SMatTest, iteratorConversionAndReset)
{
  // Iterators start unset and can be reused across populated and empty columns.
  Mat matrix(ring, 3, 3);
  fill(matrix, {2, 0, 0, 0, 0, 5, -3, 0, 0});
  auto it = matrix.begin();
  EXPECT_FALSE(it.valid());
  it.set(0);
  ASSERT_TRUE(it.valid());
  EXPECT_EQ(it.row(), 2);
  ring_elem converted;
  it.copy_elem(converted);
  Ring::Element actual(ring);
  ring.from_ring_elem(actual, converted);
  EXPECT_TRUE(ring.is_equal(actual, scalar(-3)));
  it.next();
  ASSERT_TRUE(it.valid());
  EXPECT_EQ(it.row(), 0);
  EXPECT_TRUE(ring.is_equal(it.value(), scalar(2)));
  it.next();
  EXPECT_FALSE(it.valid());
  it.set(1);
  EXPECT_FALSE(it.valid());
  it.set(2);
  ASSERT_TRUE(it.valid());
  EXPECT_EQ(it.row(), 1);
  EXPECT_TRUE(ring.is_equal(it.value(), scalar(5)));
}

TEST_F(SMatTest, copiesOwnTheirEntries)
{
  // Both copy interfaces must preserve zeros and own independently mutable
  // nodes.
  Mat source(ring, 3, 2);
  fill(source, {2, 0, 0, 0, -3, 5});
  Mat copied(source);
  std::unique_ptr<Mat> cloned(source.copy());
  source.set_entry(0, 0, scalar(7));
  copied.set_entry(2, 1, scalar(0));
  cloned->set_entry(1, 0, scalar(11));

  expectMatrix(source, 3, 2, {7, 0, 0, 0, -3, 5});
  expectMatrix(copied, 3, 2, {2, 0, 0, 0, -3, 0});
  expectMatrix(*cloned, 3, 2, {2, 0, 11, 0, -3, 5});
  EXPECT_EQ(&copied.ring(), &ring);
  EXPECT_EQ(&cloned->ring(), &ring);
}

TEST_F(SMatTest, grabSwapsRingShapeAndEntries)
{
  // Swapping differently shaped matrices must also transfer the coefficient
  // ring.
  Ring otherRing(7);
  Mat first(ring, 2, 1), second(otherRing, 1, 2);
  fill(first, {2, -3});
  Ring::Element value(otherRing);
  otherRing.set(value, 4);
  second.set_entry(0, 1, value);

  first.grab(&second);

  EXPECT_EQ(&first.ring(), &otherRing);
  EXPECT_EQ(&second.ring(), &ring);
  expectMatrix(first, 1, 2, {0, 4});
  expectMatrix(second, 2, 1, {2, -3});
  second.grab(&second);
  expectMatrix(second, 2, 1, {2, -3});
}

TEST_F(SMatTest, interchangeRows)
{
  // Exercise moving nodes in both directions, including adjacent and absent
  // rows.
  struct Case
  {
    const char* name;
    size_t first, second;
    std::vector<int> expected;
  };
  const Case cases[] = {{"both present", 0, 4, {7, 0, 3, 0, 2}},
                        {"upper row absent", 0, 3, {0, 0, 3, 2, 7}},
                        {"lower row absent", 4, 1, {2, 7, 3, 0, 0}},
                        {"adjacent node moves up", 2, 3, {2, 0, 0, 3, 7}},
                        {"adjacent node moves down", 2, 1, {2, 3, 0, 0, 7}},
                        {"both absent", 1, 3, {2, 0, 3, 0, 7}},
                        {"same row", 2, 2, {2, 0, 3, 0, 7}}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Mat matrix(ring, 5, 1);
      fill(matrix, {2, 0, 3, 0, 7});
      matrix.interchange_rows(sample.first, sample.second);
      expectMatrix(matrix, 5, 1, sample.expected);
    }
  Mat empty(ring, 5, 1);
  empty.interchange_rows(0, 4);
  expectMatrix(empty, 5, 1, {0, 0, 0, 0, 0});
}

TEST_F(SMatTest, interchangeColumns)
{
  // Populated and empty columns must move together with their sparse nodes.
  Mat matrix(ring, 2, 3);
  fill(matrix, {2, 0, 5, 0, 0, -3});
  matrix.interchange_columns(0, 2);
  expectMatrix(matrix, 2, 3, {5, 0, 2, -3, 0, 0});
  matrix.interchange_columns(0, 1);
  matrix.interchange_columns(2, 2);
  expectMatrix(matrix, 2, 3, {0, 5, 2, 0, -3, 0});
}

TEST_F(SMatTest, scaleAndDivideRows)
{
  // Nonzero division reverses scaling; multiplying by zero removes stored
  // nodes.
  Mat matrix(ring, 3, 3);
  fill(matrix, {2, 0, 0, 3, -4, 0, 0, 5, 0});
  matrix.scale_row(1, scalar(-2));
  expectMatrix(matrix, 3, 3, {2, 0, 0, -6, 8, 0, 0, 5, 0});
  matrix.divide_row(1, scalar(-2));
  expectMatrix(matrix, 3, 3, {2, 0, 0, 3, -4, 0, 0, 5, 0});
  matrix.scale_row(1, scalar(0));
  // Row 2 lies beyond the last stored entry in column 0.
  matrix.scale_row(2, scalar(1));
  matrix.divide_row(2, scalar(1));
  expectMatrix(matrix, 3, 3, {2, 0, 0, 0, 0, 0, 0, 5, 0});
}

TEST_F(SMatTest, scaleAndDivideColumns)
{
  // Scale a column with gaps and verify nonzero division and zero removal.
  Mat matrix(ring, 3, 3);
  fill(matrix, {2, 0, 3, 0, 0, -4, 5, 0, 0});
  matrix.scale_column(0, scalar(-2));
  matrix.divide_column(1, scalar(3));
  expectMatrix(matrix, 3, 3, {-4, 0, 3, 0, 0, -4, -10, 0, 0});
  matrix.divide_column(0, scalar(-2));
  expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 0, -4, 5, 0, 0});
  matrix.scale_column(0, scalar(0));
  expectMatrix(matrix, 3, 3, {0, 0, 3, 0, 0, -4, 0, 0, 0});
}

TEST_F(SMatTest, rowAddition)
{
  // Row addition must insert missing entries and remove cancellations, even in
  // place.
  Mat matrix(ring, 3, 4);
  fill(matrix, {2, 0, 3, 0, -2, 5, 0, 0, 0, 0, 7, 0});
  matrix.row_op(0, scalar(1), 1);
  expectMatrix(matrix, 3, 4, {0, 5, 3, 0, -2, 5, 0, 0, 0, 0, 7, 0});
  matrix.row_op(2, scalar(0), 1);
  matrix.row_op(1, scalar(-1), 1);
  expectMatrix(matrix, 3, 4, {0, 5, 3, 0, 0, 0, 0, 0, 0, 0, 7, 0});
}

TEST_F(SMatTest, columnAddition)
{
  // Interleaved supports test sparse merging; aliasing must use the original
  // column.
  Mat matrix(ring, 5, 3);
  fill(matrix, {2, 0, 0, 0, 5, 0, 3, -3, 0, 0, 7, 0, 11, 0, 0});
  matrix.column_op(0, scalar(1), 1);
  expectMatrix(matrix, 5, 3, {2, 0, 0, 5, 5, 0, 0, -3, 0, 7, 7, 0, 11, 0, 0});
  matrix.column_op(2, scalar(0), 0);
  matrix.column_op(1, scalar(-1), 1);
  expectMatrix(matrix, 5, 3, {2, 0, 0, 5, 0, 0, 0, 0, 0, 7, 0, 0, 11, 0, 0});
}

TEST_F(SMatTest, simultaneousRowTransformation)
{
  // Both new rows must use the old values, including disjoint support and
  // cancellation.
  Mat matrix(ring, 3, 5);
  fill(matrix, {2, 0, 3, 0, 0, 2, 5, 0, 0, 0, 0, 0, 0, 7, 0});
  matrix.row2by2(0, 1, scalar(1), scalar(-1), scalar(2), scalar(3));
  expectMatrix(matrix, 3, 5, {0, -5, 3, 0, 0, 10, 15, 6, 0, 0, 0, 0, 0, 7, 0});
}

TEST_F(SMatTest, simultaneousColumnTransformation)
{
  // Unequal coefficients expose accidentally reusing the updated first column.
  Mat matrix(ring, 5, 3);
  fill(matrix, {2, 2, 0, 0, 5, 0, 3, 0, 0, 0, 0, 7, 0, 0, 0});
  matrix.column2by2(0, 1, scalar(1), scalar(-1), scalar(2), scalar(3));
  expectMatrix(matrix, 5, 3, {0, 10, 0, -5, 15, 0, 3, 6, 0, 0, 0, 7, 0, 0, 0});
}

TEST_F(SMatTest, dotProducts)
{
  // Overlap, disjoint support, and empty columns must all overwrite the output.
  Mat matrix(ring, 5, 4);
  fill(matrix, {2, 0, 0, 0, 0, 5, 0, 0, 3, -4, 0, 0, 0, 0, 7, 0, -2, 6, 0, 0});
  struct Case
  {
    const char* name;
    size_t first, second;
    int expected;
  };
  const Case cases[] = {{"overlap", 0, 1, -24},
                        {"reverse overlap", 1, 0, -24},
                        {"self", 0, 0, 17},
                        {"disjoint", 0, 2, 0},
                        {"empty right", 0, 3, 0},
                        {"empty left", 3, 0, 0}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Ring::Element result(ring);
      ring.set(result, 19);
      matrix.dot_product(sample.first, sample.second, result);
      EXPECT_TRUE(ring.is_equal(result, scalar(sample.expected)))
          << "expected " << sample.expected;
    }
}

TEST_F(SMatTest, rowPermutations)
{
  // A nonzero offset and a three-cycle expose reversed permutation conventions.
  Mat matrix(ring, 5, 3);
  fill(matrix, {2, 0, 0, 3, 0, 0, 5, 11, 0, 7, 0, 0, -2, 0, 0});
  EXPECT_TRUE(matrix.row_permute(1, indices({2, 0, 1})));
  expectMatrix(matrix, 5, 3, {2, 0, 0, 7, 0, 0, 3, 0, 0, 5, 11, 0, -2, 0, 0});
  EXPECT_TRUE(matrix.row_permute(0, indices({0, 1, 2, 3, 4})));
  EXPECT_TRUE(matrix.row_permute(5, indices({})));
  expectMatrix(matrix, 5, 3, {2, 0, 0, 7, 0, 0, 3, 0, 0, 5, 11, 0, -2, 0, 0});
}

TEST_F(SMatTest, columnPermutations)
{
  // A partial cycle moves an empty column and leaves the outer columns
  // untouched.
  Mat matrix(ring, 2, 5);
  fill(matrix, {2, 3, 0, 5, 7, -2, 0, 0, 11, 0});
  EXPECT_TRUE(matrix.column_permute(1, indices({2, 0, 1})));
  expectMatrix(matrix, 2, 5, {2, 5, 3, 0, 7, -2, 11, 0, 0, 0});
  EXPECT_TRUE(matrix.column_permute(0, indices({0, 1, 2, 3, 4})));
  EXPECT_TRUE(matrix.column_permute(5, indices({})));
  expectMatrix(matrix, 2, 5, {2, 5, 3, 0, 7, -2, 11, 0, 0, 0});
}

TEST_F(SMatTest, duplicatePermutationsAreRejected)
{
  // Duplicate indices are rejected before any entries move; indices stay in
  // range.
  Mat matrix(ring, 3, 3);
  fill(matrix, {2, 0, 3, 0, 5, 0, 7, 0, 11});
  EXPECT_FALSE(matrix.row_permute(0, indices({1, 1, 0})));
  expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 5, 0, 7, 0, 11});
  EXPECT_FALSE(matrix.column_permute(0, indices({1, 1, 0})));
  expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 5, 0, 7, 0, 11});
}

TEST_F(SMatTest, insertAndDeleteRows)
{
  // Insert at every boundary, then delete populated rows and the entire matrix
  // height.
  for (size_t position : {0, 1, 3})
    {
      SCOPED_TRACE(::testing::Message() << "insert rows at " << position);
      Mat matrix(ring, 3, 2);
      fill(matrix, {2, 0, 0, 0, -3, 5});
      std::vector<int> expected {2, 0, 0, 0, -3, 5};
      expected.insert(expected.begin() + 2 * position, 4, 0);
      matrix.insert_rows(position, 2);
      expectMatrix(matrix, 5, 2, expected);
      matrix.delete_rows(position, position + 1);
      expectMatrix(matrix, 3, 2, {2, 0, 0, 0, -3, 5});
    }
  Mat matrix(ring, 4, 2);
  fill(matrix, {2, 0, 3, 5, 7, 0, 11, 13});
  matrix.insert_rows(2, 0);
  matrix.delete_rows(1, 2);
  expectMatrix(matrix, 2, 2, {2, 0, 11, 13});
  matrix.delete_rows(0, 1);
  expectMatrix(matrix, 0, 2, {});
  matrix.insert_rows(0, 1);
  expectMatrix(matrix, 1, 2, {0, 0});
}

TEST_F(SMatTest, insertAndDeleteColumns)
{
  // New columns must be empty and deletion must retain both surviving sides.
  for (size_t position : {0, 1, 3})
    {
      SCOPED_TRACE(::testing::Message() << "insert columns at " << position);
      Mat matrix(ring, 1, 3);
      fill(matrix, {2, 0, -3});
      std::vector<int> expected {2, 0, -3};
      expected.insert(expected.begin() + position, 2, 0);
      matrix.insert_columns(position, 2);
      expectMatrix(matrix, 1, 5, expected);
      matrix.delete_columns(position, position + 1);
      expectMatrix(matrix, 1, 3, {2, 0, -3});
    }
  Mat matrix(ring, 2, 4);
  fill(matrix, {2, 3, 0, 5, 0, 7, 11, 13});
  matrix.insert_columns(2, 0);
  matrix.delete_columns(1, 2);
  expectMatrix(matrix, 2, 2, {2, 5, 0, 13});
  matrix.delete_columns(0, 1);
  expectMatrix(matrix, 2, 0, {});
  matrix.insert_columns(0, 1);
  expectMatrix(matrix, 2, 1, {0, 0});
}

TEST_F(SMatTest, matrixAddition)
{
  // Merge overlapping and disjoint supports, preserving the source and
  // cancelling zeros.
  Mat left(ring, 4, 4), right(ring, 4, 4);
  fill(left, {2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 7, 0, 0, 0, 0, 0});
  fill(right, {0, 0, 17, 0, 5, 11, 0, 0, -3, 0, -7, 0, 13, 0, 0, 0});
  left.addInPlace(right);
  expectMatrix(left, 4, 4, {2, 0, 17, 0, 5, 11, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0});
  expectMatrix(
      right, 4, 4, {0, 0, 17, 0, 5, 11, 0, 0, -3, 0, -7, 0, 13, 0, 0, 0});
  left.addInPlace(left);
  expectMatrix(
      left, 4, 4, {4, 0, 34, 0, 10, 22, 0, 0, 0, 0, 0, 0, 26, 0, 0, 0});
}

TEST_F(SMatTest, negationAndScalarMultiplication)
{
  // Mixed signs, singleton columns, and empty columns must survive whole-matrix
  // arithmetic.
  Mat matrix(ring, 3, 3);
  fill(matrix, {2, 0, 0, 0, -3, 0, 5, 0, 0});
  matrix.negateInPlace();
  expectMatrix(matrix, 3, 3, {-2, 0, 0, 0, 3, 0, -5, 0, 0});
  matrix.scalarMultInPlace(scalar(-2));
  expectMatrix(matrix, 3, 3, {4, 0, 0, 0, -6, 0, 10, 0, 0});
  matrix.scalarMultInPlace(scalar(0));
  matrix.negateInPlace();
  expectMatrix(matrix, 3, 3, std::vector<int>(9, 0));
}

TEST_F(SMatTest, equalityWithMatchingSupportAndDifferentShapes)
{
  // Compare coefficients on matching supports and reject incompatible
  // dimensions.
  Mat matrix(ring, 2, 3);
  fill(matrix, {2, 0, 0, -3, 0, 5});
  Mat copy(matrix), otherRows(ring, 3, 3), otherCols(ring, 2, 2);
  EXPECT_TRUE(matrix.is_equal(matrix));
  EXPECT_TRUE(matrix.is_equal(copy));
  EXPECT_FALSE(matrix.is_equal(otherRows));
  EXPECT_FALSE(matrix.is_equal(otherCols));
  copy.set_entry(1, 0, scalar(7));
  EXPECT_FALSE(matrix.is_equal(copy));
  EXPECT_FALSE(copy.is_equal(matrix));
}

TEST_F(SMatTest, submatrixSelection)
{
  // Reordered and repeated indices must produce independent entries, including
  // zeros.
  Mat source(ring, 3, 4), selected, columns;
  fill(source, {2, 0, 3, 0, 0, 5, 0, 0, 7, 0, 11, 0});
  selected.setFromSubmatrix(source, indices({2, 0, 2}), indices({2, 1, 0}));
  columns.setFromSubmatrix(source, indices({3, 2, 2}));
  EXPECT_EQ(&selected.ring(), &ring);
  EXPECT_EQ(&columns.ring(), &ring);
  expectMatrix(selected, 3, 3, {11, 0, 7, 3, 0, 2, 11, 0, 7});
  expectMatrix(columns, 3, 3, {0, 3, 3, 0, 0, 0, 0, 11, 11});
  selected.set_entry(0, 0, scalar(19));
  columns.set_entry(0, 1, scalar(23));
  expectMatrix(selected, 3, 3, {19, 0, 7, 3, 0, 2, 11, 0, 7});
  expectMatrix(columns, 3, 3, {0, 23, 3, 0, 0, 0, 0, 11, 11});
  expectMatrix(source, 3, 4, {2, 0, 3, 0, 0, 5, 0, 0, 7, 0, 11, 0});
}

TEST_F(SMatTest, emptySubmatrices)
{
  // Empty selections retain the unselected dimension and acquire the source
  // ring.
  Mat source(ring, 2, 3), noRows, noColumns;
  fill(source, {2, 0, 3, 0, 5, 0});
  noRows.setFromSubmatrix(source, indices({}), indices({2, 0}));
  noColumns.setFromSubmatrix(source, indices({}));
  EXPECT_EQ(&noRows.ring(), &ring);
  EXPECT_EQ(&noColumns.ring(), &ring);
  expectMatrix(noRows, 0, 2, {});
  expectMatrix(noColumns, 2, 0, {});
}

// vec_equals ignores row indices and accepts a common prefix of coefficients.
// Disabled until it checks both sparse supports and simultaneous list
// exhaustion. https://github.com/Macaulay2/M2/issues/4706
TEST_F(SMatTest, DISABLED_equalityDistinguishesSparseSupports)
{
  // Equal coefficient sequences must not conceal different row positions or
  // lengths.
  struct Case
  {
    const char* name;
    std::initializer_list<int> left, right;
  };
  const Case cases[] = {
      {"empty versus nonempty", {0, 0, 0}, {2, 0, 0}},
      {"different row positions", {2, 0, 0}, {0, 2, 0}},
      {"common prefix with extra node", {0, 2, 0}, {3, 2, 0}}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Mat left(ring, 3, 1), right(ring, 3, 1);
      fill(left, sample.left);
      fill(right, sample.right);

      EXPECT_FALSE(left.is_equal(right));
      EXPECT_FALSE(right.is_equal(left));
    }
}

// vec_negate skips the last node, so subtraction adds that coefficient instead.
// Disabled until subtraction negates every stored entry.
// https://github.com/Macaulay2/M2/issues/4707
TEST_F(SMatTest, DISABLED_subtractionIncludesLastStoredEntry)
{
  // Distinct inputs and self-subtraction must subtract the entire sparse
  // column.
  {
    SCOPED_TRACE("subtract: distinct nonempty columns");
    Mat left(ring, 2, 1), right(ring, 2, 1);
    fill(left, {7, 11});
    fill(right, {2, 3});

    left.subtractInPlace(right);

    expectMatrix(left, 2, 1, {5, 8});
    expectMatrix(right, 2, 1, {2, 3});
  }
  {
    SCOPED_TRACE("subtract: singleton aliases itself");
    Mat matrix(ring, 2, 1);
    fill(matrix, {0, 5});

    matrix.subtractInPlace(matrix);

    expectMatrix(matrix, 2, 1, {0, 0});
  }
}

// vec_negate dereferences null when the subtracted column is empty.
// Disabled until subtractInPlace accepts zero columns; the child process keeps
// an explicit run of this regression from terminating the rest of the suite.
// https://github.com/Macaulay2/M2/issues/4707
TEST_F(SMatTest, DISABLED_subtractingZeroLeavesMatrixUnchanged)
{
  // Subtracting zero must exit normally and preserve both entries.
  EXPECT_EXIT(
      {
        Mat left(ring, 2, 1);
        Mat right(ring, 2, 1);
        fill(left, {7, 11});
        left.subtractInPlace(right);
        Ring::Element value(ring);
        bool unchanged =
            left.get_entry(0, 0, value) && ring.is_equal(value, scalar(7)) &&
            left.get_entry(1, 0, value) && ring.is_equal(value, scalar(11));
        std::_Exit(unchanged ? EXIT_SUCCESS : EXIT_FAILURE);
      },
      ::testing::ExitedWithCode(EXIT_SUCCESS),
      "");
}

}  // namespace
