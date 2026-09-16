#include "basic-mutable-matrices/smat.hpp"

#include <gtest/gtest.h>

#include <cstddef>
#include <cstdlib>
#include <initializer_list>
#include <limits>
#include <memory>
#include <type_traits>
#include <vector>

#include "basic-rings/aring-ZZp.hpp"
#include "basic-rings/aring-ZZp-ffpack.hpp"
#include "basic-rings/aring-ZZp-flint.hpp"
#include "basic-rings/aring-ZZ-gmp.hpp"
#include "basic-rings/aring-ZZ-flint.hpp"
#include "basic-rings/aring-QQ-gmp.hpp"
#include "basic-rings/aring-QQ-flint.hpp"
#include "basic-rings/aring-RR.hpp"
#include "basic-rings/aring-RRR.hpp"
#include "basic-rings/aring-CC.hpp"
#include "basic-rings/aring-CCC.hpp"
#include "basic-rings/aring-RRi.hpp"
#include "basic-rings/aring-CCi.hpp"
#include "basic-rings/aring-GF-flint.hpp"
#include "basic-rings/aring-GF-flint-big.hpp"
#include "basic-rings/aring-m2-GF.hpp"
#include "coeffrings.hpp"
#include "basic-rings/aring-glue.hpp"
#include "unit-tests/SMatTest.hpp"
#include "unit-tests/util-polyring-creation.hpp"
#include "util.hpp"

namespace {

M2_arrayint indices(std::initializer_list<int> values)
{ return stdvector_to_M2_arrayint(std::vector<int>(values)); }

// These are the coefficient types for which the engine instantiates SMat.
// Use one common contract so changes exercise every backend consistently.
template <typename RT>
struct SMatRingFactory
{
  static std::unique_ptr<RT> make()
  {
    if constexpr (std::is_same_v<RT, M2::ARingZZp> ||
                  std::is_same_v<RT, M2::ARingZZpFFPACK> ||
                  std::is_same_v<RT, M2::ARingZZpFlint>)
      return std::make_unique<RT>(101);
    else if constexpr (std::is_same_v<RT, M2::ARingRRR> ||
                       std::is_same_v<RT, M2::ARingCCC> ||
                       std::is_same_v<RT, M2::ARingRRi> ||
                       std::is_same_v<RT, M2::ARingCCi>)
      return std::make_unique<RT>(100);
    else if constexpr (std::is_same_v<RT, CoefficientRingR>)
      return std::make_unique<RT>(globalQQ);
    else if constexpr (std::is_same_v<RT, M2::ARingGFFlint> ||
                       std::is_same_v<RT, M2::ARingGFFlintBig> ||
                       std::is_same_v<RT, M2::ARingGFM2>)
      {
        // This primitive quadratic keeps the legacy GF table small; its
        // generator has order 1368. Each fixture owns its coefficient ring.
        static const auto* quotient =
            dynamic_cast<const PolynomialRing*>(simpleQuotientRing(
                simplePolynomialRing(37, {"x"}), {"x^2-8*x+18"}));
        return std::make_unique<RT>(*quotient, quotient->var(0));
      }
    else
      return std::make_unique<RT>();
  }
};

using SMatRings = ::testing::Types<M2::ARingZZp,
                                   M2::ARingZZpFFPACK,
                                   M2::ARingZZpFlint,
                                   M2::ARingZZGMP,
                                   M2::ARingZZ,
                                   M2::ARingQQGMP,
                                   M2::ARingQQFlint,
                                   M2::ARingRR,
                                   M2::ARingRRR,
                                   M2::ARingCC,
                                   M2::ARingCCC,
                                   M2::ARingRRi,
                                   M2::ARingCCi,
                                   M2::ARingGFFlint,
                                   M2::ARingGFFlintBig,
                                   M2::ARingGFM2,
                                   CoefficientRingR>;
// ARingTower is still under development and is not ready for SMat tests.
TYPED_TEST_SUITE(SMatTest, SMatRings);

TYPED_TEST(SMatTest, construction)
{
  // Empty dimensions and rectangular zero matrices must preserve their shape.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
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
      this->expectMatrix(
          matrix, shape[0], shape[1], std::vector<int>(shape[0] * shape[1], 0));
    }
}

TYPED_TEST(SMatTest, entryInsertionReplacementAndRemoval)
{
  // Insert out of order, replace, then remove head, interior, and tail nodes.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 2);
  matrix.set_entry(2, 0, this->scalar(3));
  matrix.set_entry(0, 0, this->scalar(-2));
  matrix.set_entry(4, 0, this->scalar(7));
  matrix.set_entry(3, 0, this->scalar(5));
  matrix.set_entry(1, 0, this->scalar(0));
  matrix.set_entry(2, 0, this->scalar(-4));
  this->expectMatrix(matrix, 5, 2, {-2, 0, 0, 0, -4, 0, 5, 0, 7, 0});

  for (size_t row : {3, 4, 0, 2}) matrix.set_entry(row, 0, this->scalar(0));
  matrix.set_entry(2, 1, this->scalar(0));
  this->expectMatrix(matrix, 5, 2, std::vector<int>(10, 0));
}

TYPED_TEST(SMatTest, iteratorConversionAndReset)
{
  // Iterators start unset and can be reused across populated and empty columns.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 0, 0, 0, 5, -3, 0, 0});
  auto it = matrix.begin();
  EXPECT_FALSE(it.valid());
  it.set(0);
  ASSERT_TRUE(it.valid());
  EXPECT_EQ(it.row(), 2);
  ring_elem converted;
  it.copy_elem(converted);
  typename Ring::Element actual(ring);
  ring.from_ring_elem(actual, converted);
  EXPECT_TRUE(ring.is_equal(actual, this->scalar(-3)));
  it.next();
  ASSERT_TRUE(it.valid());
  EXPECT_EQ(it.row(), 0);
  EXPECT_TRUE(ring.is_equal(it.value(), this->scalar(2)));
  it.next();
  EXPECT_FALSE(it.valid());
  it.set(1);
  EXPECT_FALSE(it.valid());
  it.set(2);
  ASSERT_TRUE(it.valid());
  EXPECT_EQ(it.row(), 1);
  EXPECT_TRUE(ring.is_equal(it.value(), this->scalar(5)));
}

TYPED_TEST(SMatTest, copiesOwnTheirEntries)
{
  // Both copy interfaces must preserve zeros and own independently mutable
  // nodes.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat source(ring, 3, 2);
  this->fill(source, {2, 0, 0, 0, -3, 5});
  Mat copied(source);
  std::unique_ptr<Mat> cloned(source.copy());
  source.set_entry(0, 0, this->scalar(7));
  copied.set_entry(2, 1, this->scalar(0));
  cloned->set_entry(1, 0, this->scalar(11));

  this->expectMatrix(source, 3, 2, {7, 0, 0, 0, -3, 5});
  this->expectMatrix(copied, 3, 2, {2, 0, 0, 0, -3, 0});
  this->expectMatrix(*cloned, 3, 2, {2, 0, 11, 0, -3, 5});
  EXPECT_EQ(&copied.ring(), &ring);
  EXPECT_EQ(&cloned->ring(), &ring);
}

TYPED_TEST(SMatTest, grabSwapsRingShapeAndEntries)
{
  // Swapping differently shaped matrices must also transfer the coefficient
  // ring.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  auto otherRingOwner = SMatRingFactory<Ring>::make();
  Ring& otherRing = *otherRingOwner;
  Mat first(ring, 2, 1), second(otherRing, 1, 2);
  this->fill(first, {2, -3});
  typename Ring::Element value(otherRing);
  otherRing.set(value, 4);
  second.set_entry(0, 1, value);

  first.grab(&second);

  EXPECT_EQ(&first.ring(), &otherRing);
  EXPECT_EQ(&second.ring(), &ring);
  this->expectMatrix(first, 1, 2, {0, 4});
  this->expectMatrix(second, 2, 1, {2, -3});
  second.grab(&second);
  this->expectMatrix(second, 2, 1, {2, -3});
}

TYPED_TEST(SMatTest, interchangeRows)
{
  // Exercise moving nodes in both directions, including adjacent and absent
  // rows.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
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
      this->fill(matrix, {2, 0, 3, 0, 7});
      matrix.interchange_rows(sample.first, sample.second);
      this->expectMatrix(matrix, 5, 1, sample.expected);
    }
  Mat empty(ring, 5, 1);
  empty.interchange_rows(0, 4);
  this->expectMatrix(empty, 5, 1, {0, 0, 0, 0, 0});
}

TYPED_TEST(SMatTest, interchangeColumns)
{
  // Populated and empty columns must move together with their sparse nodes.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 2, 3);
  this->fill(matrix, {2, 0, 5, 0, 0, -3});
  matrix.interchange_columns(0, 2);
  this->expectMatrix(matrix, 2, 3, {5, 0, 2, -3, 0, 0});
  matrix.interchange_columns(0, 1);
  matrix.interchange_columns(2, 2);
  this->expectMatrix(matrix, 2, 3, {0, 5, 2, 0, -3, 0});
}

TYPED_TEST(SMatTest, scaleAndDivideRows)
{
  // Nonzero division reverses scaling; multiplying by zero removes stored
  // nodes.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 0, 3, -4, 0, 0, 5, 0});
  matrix.scale_row(1, this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {2, 0, 0, -6, 8, 0, 0, 5, 0});
  matrix.divide_row(1, this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {2, 0, 0, 3, -4, 0, 0, 5, 0});
  matrix.scale_row(1, this->scalar(0));
  // Row 2 lies beyond the last stored entry in column 0.
  matrix.scale_row(2, this->scalar(1));
  matrix.divide_row(2, this->scalar(1));
  this->expectMatrix(matrix, 3, 3, {2, 0, 0, 0, 0, 0, 0, 5, 0});
}

TYPED_TEST(SMatTest, scaleAndDivideColumns)
{
  // Scale a column with gaps and verify nonzero division and zero removal.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 3, 0, 0, -4, 5, 0, 0});
  matrix.scale_column(0, this->scalar(-2));
  matrix.divide_column(1, this->scalar(3));
  this->expectMatrix(matrix, 3, 3, {-4, 0, 3, 0, 0, -4, -10, 0, 0});
  matrix.divide_column(0, this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 0, -4, 5, 0, 0});
  matrix.scale_column(0, this->scalar(0));
  this->expectMatrix(matrix, 3, 3, {0, 0, 3, 0, 0, -4, 0, 0, 0});
}

TYPED_TEST(SMatTest, rowAddition)
{
  // Row addition must insert missing entries and remove cancellations, even in
  // place.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 4);
  this->fill(matrix, {2, 0, 3, 0, -2, 5, 0, 0, 0, 0, 7, 0});
  matrix.row_op(0, this->scalar(1), 1);
  this->expectMatrix(matrix, 3, 4, {0, 5, 3, 0, -2, 5, 0, 0, 0, 0, 7, 0});
  matrix.row_op(2, this->scalar(0), 1);
  matrix.row_op(1, this->scalar(-1), 1);
  this->expectMatrix(matrix, 3, 4, {0, 5, 3, 0, 0, 0, 0, 0, 0, 0, 7, 0});
}

TYPED_TEST(SMatTest, columnAddition)
{
  // Interleaved supports test sparse merging; aliasing must use the original
  // column.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 3);
  this->fill(matrix, {2, 0, 0, 0, 5, 0, 3, -3, 0, 0, 7, 0, 11, 0, 0});
  matrix.column_op(0, this->scalar(1), 1);
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 5, 5, 0, 0, -3, 0, 7, 7, 0, 11, 0, 0});
  matrix.column_op(2, this->scalar(0), 0);
  matrix.column_op(1, this->scalar(-1), 1);
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 5, 0, 0, 0, 0, 0, 7, 0, 0, 11, 0, 0});
}

TYPED_TEST(SMatTest, simultaneousRowTransformation)
{
  // Both new rows must use the old values, including disjoint support and
  // cancellation.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 5);
  this->fill(matrix, {2, 0, 3, 0, 0, 2, 5, 0, 0, 0, 0, 0, 0, 7, 0});
  matrix.row2by2(0,
                 1,
                 this->scalar(1),
                 this->scalar(-1),
                 this->scalar(2),
                 this->scalar(3));
  this->expectMatrix(
      matrix, 3, 5, {0, -5, 3, 0, 0, 10, 15, 6, 0, 0, 0, 0, 0, 7, 0});
}

TYPED_TEST(SMatTest, simultaneousColumnTransformation)
{
  // Unequal coefficients expose accidentally reusing the updated first column.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 3);
  this->fill(matrix, {2, 2, 0, 0, 5, 0, 3, 0, 0, 0, 0, 7, 0, 0, 0});
  matrix.column2by2(0,
                    1,
                    this->scalar(1),
                    this->scalar(-1),
                    this->scalar(2),
                    this->scalar(3));
  this->expectMatrix(
      matrix, 5, 3, {0, 10, 0, -5, 15, 0, 3, 6, 0, 0, 0, 7, 0, 0, 0});
}

TYPED_TEST(SMatTest, dotProducts)
{
  // Overlap, disjoint support, and empty columns must all overwrite the output.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 4);
  this->fill(matrix,
             {2, 0, 0, 0, 0, 5, 0, 0, 3, -4, 0, 0, 0, 0, 7, 0, -2, 6, 0, 0});
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
      typename Ring::Element result(ring);
      ring.set(result, 19);
      matrix.dot_product(sample.first, sample.second, result);
      EXPECT_TRUE(ring.is_equal(result, this->scalar(sample.expected)))
          << "expected " << sample.expected;
    }
}

TYPED_TEST(SMatTest, rowPermutations)
{
  // A nonzero offset and a three-cycle expose reversed permutation conventions.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 3);
  this->fill(matrix, {2, 0, 0, 3, 0, 0, 5, 11, 0, 7, 0, 0, -2, 0, 0});
  EXPECT_TRUE(matrix.row_permute(1, indices({2, 0, 1})));
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 7, 0, 0, 3, 0, 0, 5, 11, 0, -2, 0, 0});
  EXPECT_TRUE(matrix.row_permute(0, indices({0, 1, 2, 3, 4})));
  EXPECT_TRUE(matrix.row_permute(5, indices({})));
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 7, 0, 0, 3, 0, 0, 5, 11, 0, -2, 0, 0});
}

TYPED_TEST(SMatTest, columnPermutations)
{
  // A partial cycle moves an empty column and leaves the outer columns
  // untouched.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 2, 5);
  this->fill(matrix, {2, 3, 0, 5, 7, -2, 0, 0, 11, 0});
  EXPECT_TRUE(matrix.column_permute(1, indices({2, 0, 1})));
  this->expectMatrix(matrix, 2, 5, {2, 5, 3, 0, 7, -2, 11, 0, 0, 0});
  EXPECT_TRUE(matrix.column_permute(0, indices({0, 1, 2, 3, 4})));
  EXPECT_TRUE(matrix.column_permute(5, indices({})));
  this->expectMatrix(matrix, 2, 5, {2, 5, 3, 0, 7, -2, 11, 0, 0, 0});
}

TYPED_TEST(SMatTest, duplicatePermutationsAreRejected)
{
  // Duplicate indices are rejected before any entries move; indices stay in
  // range.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 3, 0, 5, 0, 7, 0, 11});
  EXPECT_FALSE(matrix.row_permute(0, indices({1, 1, 0})));
  this->expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 5, 0, 7, 0, 11});
  EXPECT_FALSE(matrix.column_permute(0, indices({1, 1, 0})));
  this->expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 5, 0, 7, 0, 11});
}

TYPED_TEST(SMatTest, insertAndDeleteRows)
{
  // Insert at every boundary, then delete populated rows and the entire matrix
  // height.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  for (size_t position : {0, 1, 3})
    {
      SCOPED_TRACE(::testing::Message() << "insert rows at " << position);
      Mat matrix(ring, 3, 2);
      this->fill(matrix, {2, 0, 0, 0, -3, 5});
      std::vector<int> expected {2, 0, 0, 0, -3, 5};
      expected.insert(expected.begin() + 2 * position, 4, 0);
      matrix.insert_rows(position, 2);
      this->expectMatrix(matrix, 5, 2, expected);
      matrix.delete_rows(position, position + 1);
      this->expectMatrix(matrix, 3, 2, {2, 0, 0, 0, -3, 5});
    }
  Mat matrix(ring, 4, 2);
  this->fill(matrix, {2, 0, 3, 5, 7, 0, 11, 13});
  matrix.insert_rows(2, 0);
  matrix.delete_rows(1, 2);
  this->expectMatrix(matrix, 2, 2, {2, 0, 11, 13});
  matrix.delete_rows(0, 1);
  this->expectMatrix(matrix, 0, 2, {});
  matrix.insert_rows(0, 1);
  this->expectMatrix(matrix, 1, 2, {0, 0});
}

TYPED_TEST(SMatTest, insertAndDeleteColumns)
{
  // New columns must be empty and deletion must retain both surviving sides.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  for (size_t position : {0, 1, 3})
    {
      SCOPED_TRACE(::testing::Message() << "insert columns at " << position);
      Mat matrix(ring, 1, 3);
      this->fill(matrix, {2, 0, -3});
      std::vector<int> expected {2, 0, -3};
      expected.insert(expected.begin() + position, 2, 0);
      matrix.insert_columns(position, 2);
      this->expectMatrix(matrix, 1, 5, expected);
      matrix.delete_columns(position, position + 1);
      this->expectMatrix(matrix, 1, 3, {2, 0, -3});
    }
  Mat matrix(ring, 2, 4);
  this->fill(matrix, {2, 3, 0, 5, 0, 7, 11, 13});
  matrix.insert_columns(2, 0);
  matrix.delete_columns(1, 2);
  this->expectMatrix(matrix, 2, 2, {2, 5, 0, 13});
  matrix.delete_columns(0, 1);
  this->expectMatrix(matrix, 2, 0, {});
  matrix.insert_columns(0, 1);
  this->expectMatrix(matrix, 2, 1, {0, 0});
}

TYPED_TEST(SMatTest, matrixAddition)
{
  // Merge overlapping and disjoint supports, preserving the source and
  // cancelling zeros.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat left(ring, 4, 4), right(ring, 4, 4);
  this->fill(left, {2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 7, 0, 0, 0, 0, 0});
  this->fill(right, {0, 0, 17, 0, 5, 11, 0, 0, -3, 0, -7, 0, 13, 0, 0, 0});
  left.addInPlace(right);
  this->expectMatrix(
      left, 4, 4, {2, 0, 17, 0, 5, 11, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0});
  this->expectMatrix(
      right, 4, 4, {0, 0, 17, 0, 5, 11, 0, 0, -3, 0, -7, 0, 13, 0, 0, 0});
  left.addInPlace(left);
  this->expectMatrix(
      left, 4, 4, {4, 0, 34, 0, 10, 22, 0, 0, 0, 0, 0, 0, 26, 0, 0, 0});
}

TYPED_TEST(SMatTest, negationAndScalarMultiplication)
{
  // Mixed signs, singleton columns, and empty columns must survive whole-matrix
  // arithmetic.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 0, 0, -3, 0, 5, 0, 0});
  matrix.negateInPlace();
  this->expectMatrix(matrix, 3, 3, {-2, 0, 0, 0, 3, 0, -5, 0, 0});
  matrix.scalarMultInPlace(this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {4, 0, 0, 0, -6, 0, 10, 0, 0});
  matrix.scalarMultInPlace(this->scalar(0));
  matrix.negateInPlace();
  this->expectMatrix(matrix, 3, 3, std::vector<int>(9, 0));
}

TYPED_TEST(SMatTest, equalityWithMatchingSupportAndDifferentShapes)
{
  // Compare coefficients on matching supports and reject incompatible
  // dimensions.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 2, 3);
  this->fill(matrix, {2, 0, 0, -3, 0, 5});
  Mat copy(matrix), otherRows(ring, 3, 3), otherCols(ring, 2, 2);
  EXPECT_TRUE(matrix.is_equal(matrix));
  EXPECT_TRUE(matrix.is_equal(copy));
  EXPECT_FALSE(matrix.is_equal(otherRows));
  EXPECT_FALSE(matrix.is_equal(otherCols));
  copy.set_entry(1, 0, this->scalar(7));
  EXPECT_FALSE(matrix.is_equal(copy));
  EXPECT_FALSE(copy.is_equal(matrix));
}

TYPED_TEST(SMatTest, submatrixSelection)
{
  // Reordered and repeated indices must produce independent entries, including
  // zeros.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat source(ring, 3, 4), selected, columns;
  this->fill(source, {2, 0, 3, 0, 0, 5, 0, 0, 7, 0, 11, 0});
  selected.setFromSubmatrix(source, indices({2, 0, 2}), indices({2, 1, 0}));
  columns.setFromSubmatrix(source, indices({3, 2, 2}));
  EXPECT_EQ(&selected.ring(), &ring);
  EXPECT_EQ(&columns.ring(), &ring);
  this->expectMatrix(selected, 3, 3, {11, 0, 7, 3, 0, 2, 11, 0, 7});
  this->expectMatrix(columns, 3, 3, {0, 3, 3, 0, 0, 0, 0, 11, 11});
  selected.set_entry(0, 0, this->scalar(19));
  columns.set_entry(0, 1, this->scalar(23));
  this->expectMatrix(selected, 3, 3, {19, 0, 7, 3, 0, 2, 11, 0, 7});
  this->expectMatrix(columns, 3, 3, {0, 23, 3, 0, 0, 0, 0, 11, 11});
  this->expectMatrix(source, 3, 4, {2, 0, 3, 0, 0, 5, 0, 0, 7, 0, 11, 0});
}

TYPED_TEST(SMatTest, emptySubmatrices)
{
  // Empty selections retain the unselected dimension and acquire the source
  // ring.
  using Ring = TypeParam;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  Mat source(ring, 2, 3), noRows, noColumns;
  this->fill(source, {2, 0, 3, 0, 5, 0});
  noRows.setFromSubmatrix(source, indices({}), indices({2, 0}));
  noColumns.setFromSubmatrix(source, indices({}));
  EXPECT_EQ(&noRows.ring(), &ring);
  EXPECT_EQ(&noColumns.ring(), &ring);
  this->expectMatrix(noRows, 0, 2, {});
  this->expectMatrix(noColumns, 2, 0, {});
}

using SMatZZpTest = SMatTest<M2::ARingZZp>;

// vec_equals ignores row indices and accepts a common prefix of coefficients.
// Disabled until it checks both sparse supports and simultaneous list
// exhaustion. https://github.com/Macaulay2/M2/issues/4706
TEST_F(SMatZZpTest, DISABLED_equalityDistinguishesSparseSupports)
{
  // Equal coefficient sequences must not conceal different row positions or
  // lengths.
  using Ring = M2::ARingZZp;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
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
      this->fill(left, sample.left);
      this->fill(right, sample.right);

      EXPECT_FALSE(left.is_equal(right));
      EXPECT_FALSE(right.is_equal(left));
    }
}

// vec_negate skips the last node, so subtraction adds that coefficient instead.
// Disabled until subtraction negates every stored entry.
// https://github.com/Macaulay2/M2/issues/4707
TEST_F(SMatZZpTest, DISABLED_subtractionIncludesLastStoredEntry)
{
  // Distinct inputs and self-subtraction must subtract the entire sparse
  // column.
  using Ring = M2::ARingZZp;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  {
    SCOPED_TRACE("subtract: distinct nonempty columns");
    Mat left(ring, 2, 1), right(ring, 2, 1);
    this->fill(left, {7, 11});
    this->fill(right, {2, 3});

    left.subtractInPlace(right);

    this->expectMatrix(left, 2, 1, {5, 8});
    this->expectMatrix(right, 2, 1, {2, 3});
  }
  {
    SCOPED_TRACE("subtract: singleton aliases itself");
    Mat matrix(ring, 2, 1);
    this->fill(matrix, {0, 5});

    matrix.subtractInPlace(matrix);

    this->expectMatrix(matrix, 2, 1, {0, 0});
  }
}

// vec_negate dereferences null when the subtracted column is empty.
// Disabled until subtractInPlace accepts zero columns; the child process keeps
// an explicit run of this regression from terminating the rest of the suite.
// https://github.com/Macaulay2/M2/issues/4707
TEST_F(SMatZZpTest, DISABLED_subtractingZeroLeavesMatrixUnchanged)
{
  // Subtracting zero must exit normally and preserve both entries.
  using Ring = M2::ARingZZp;
  using Mat = SMat<Ring>;
  auto& ring = this->ring;
  EXPECT_EXIT(
      {
        Mat left(ring, 2, 1);
        Mat right(ring, 2, 1);
        this->fill(left, {7, 11});
        left.subtractInPlace(right);
        typename Ring::Element value(ring);
        bool unchanged = left.get_entry(0, 0, value) &&
                         ring.is_equal(value, this->scalar(7)) &&
                         left.get_entry(1, 0, value) &&
                         ring.is_equal(value, this->scalar(11));
        std::_Exit(unchanged ? EXIT_SUCCESS : EXIT_FAILURE);
      },
      ::testing::ExitedWithCode(EXIT_SUCCESS),
      "");
}

TEST(SMatZeroDivisorTest, scalingRemovesAnnihilatedEntries)
{
  // Over ZZ/101[x]/(x^2), multiplying the nonzero entry x by x removes it.
  // A field-only suite cannot exercise this sparse-node removal path.
  const Ring* quotient =
      simpleQuotientRing(simplePolynomialRing(101, {"x"}), {"x^2"});
  CoefficientRingR ring(quotient);
  const ring_elem x = quotient->var(0);
  SMat<CoefficientRingR> row(ring, 2, 2), column(ring, 2, 2);
  row.set_entry(1, 0, x);
  row.set_entry(1, 1, x);
  column.set_entry(1, 0, x);

  row.scale_row(1, x);
  column.scale_column(0, x);

  EXPECT_TRUE(row.is_zero());
  EXPECT_TRUE(column.is_zero());
  auto it = column.begin();
  it.set(0);
  EXPECT_FALSE(it.valid());
  EXPECT_EQ(row.lead_row(0), std::numeric_limits<size_t>::max());
  EXPECT_EQ(row.lead_row(1), std::numeric_limits<size_t>::max());
}

TEST(SMatUnderflowTest, divisionRemovesRoundedZero)
{
  // The smallest positive double divided by two rounds to zero. Both row
  // and column division must remove the stored node, not just its value.
  M2::ARingRR ring;
  SMat<M2::ARingRR> row(ring, 2, 1), column(ring, 2, 1);
  row.set_entry(1, 0, std::numeric_limits<double>::denorm_min());
  column.set_entry(1, 0, std::numeric_limits<double>::denorm_min());

  row.divide_row(1, 2.0);
  column.divide_column(0, 2.0);

  EXPECT_TRUE(row.is_zero());
  EXPECT_TRUE(column.is_zero());
  EXPECT_EQ(row.lead_row(0), std::numeric_limits<size_t>::max());
  auto it = column.begin();
  it.set(0);
  EXPECT_FALSE(it.valid());
}

// vec_scale advances past the replacement next node after deleting a zero.
// Disabled until scaling processes that node as well.
// https://github.com/Macaulay2/M2/issues/4708
TEST(SMatZeroDivisorTest, DISABLED_columnScalingVisitsNodeAfterRemoval)
{
  // Removing the leading x*x entry must not skip multiplication of the next
  // node.
  const Ring* quotient =
      simpleQuotientRing(simplePolynomialRing(101, {"x"}), {"x^2"});
  CoefficientRingR ring(quotient);
  const ring_elem x = quotient->var(0);
  SMat<CoefficientRingR> matrix(ring, 2, 1);
  matrix.set_entry(1, 0, x);
  matrix.set_entry(0, 0, quotient->from_long(1));

  matrix.scale_column(0, x);

  CoefficientRingR::Element result(ring);
  ASSERT_TRUE(matrix.get_entry(0, 0, result));
  EXPECT_TRUE(ring.is_equal(result, x));
  EXPECT_FALSE(matrix.get_entry(1, 0, result));
  EXPECT_EQ(matrix.lead_row(0), 0);
}

// vec_divide skips the next node after removing an entry rounded to zero.
// Disabled until division processes every surviving node.
// https://github.com/Macaulay2/M2/issues/4708
TEST(SMatUnderflowTest, DISABLED_columnDivisionVisitsNodeAfterRemoval)
{
  // An underflowing leading entry must not prevent division of the next node.
  M2::ARingRR ring;
  SMat<M2::ARingRR> matrix(ring, 2, 1);
  matrix.set_entry(1, 0, std::numeric_limits<double>::denorm_min());
  matrix.set_entry(0, 0, 4.0);

  matrix.divide_column(0, 2.0);

  double result = 0.0;
  ASSERT_TRUE(matrix.get_entry(0, 0, result));
  EXPECT_EQ(result, 2.0);
  EXPECT_FALSE(matrix.get_entry(1, 0, result));
  EXPECT_EQ(matrix.lead_row(0), 0);
}

}  // namespace
