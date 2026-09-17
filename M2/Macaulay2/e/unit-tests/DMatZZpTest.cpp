#include "basic-mutable-matrices/mat-arith.hpp"

#include <gtest/gtest.h>

#include "basic-rings/aring-ZZp.hpp"
#include "basic-mutable-matrices/mat-linalg.hpp"
#include "unit-tests/ARingMatrixTest.hpp"
#include "unit-tests/MatrixShape.hpp"

// The matrix generator uses the specialization defined in ARingZZpTest.cpp.
template <>
void getElement<M2::ARingZZp>(const M2::ARingZZp& R,
                              int index,
                              M2::ARingZZp::ElementType& result);

namespace {

using Ring = M2::ARingZZp;
using Matrix = DMat<Ring>;

::testing::AssertionResult entryEquals(const Matrix& matrix,
                                       size_t row,
                                       size_t column,
                                       long expected)
{
  Ring::Element value {matrix.ring()};
  matrix.ring().set(value, expected);
  if (matrix.ring().is_equal(matrix.entry(row, column), value))
    return ::testing::AssertionSuccess();
  return ::testing::AssertionFailure()
         << "entry (" << row << ", " << column << ") differs from " << expected;
}

TEST(DMatZZp, create)
{
  // The dense matrix retains its coefficient ring and stores a known nonzero
  // entry.
  Ring ring(101);
  Matrix matrix(ring, 5, 5);
  EXPECT_EQ(&matrix.ring(), &ring);
  EXPECT_EQ(matrix.numRows(), 5);
  EXPECT_EQ(matrix.numColumns(), 5);
  ring.set(matrix.entry(0, 2), 7);
  EXPECT_TRUE(entryEquals(matrix, 0, 2, 7));
  EXPECT_TRUE(entryEquals(matrix, 0, 0, 0));
}

TEST(DMatZZp, symmetricIsSymmetric)
{
  // A generated symmetric matrix is unchanged by transposition.
  Ring R(101);
  ARingMatrixGenerator<Matrix> matgen(R);
  Matrix N(R, 5, 5);
  Matrix M(R, 5, 5);
  matgen.nextMatrix(N, MatrixShape::Symmetric);
  MatrixOps::transpose(N, M);
  EXPECT_TRUE(MatrixOps::isEqual(N, M));
}

TEST(DMatZZp, identityIsNeutral)
{
  // Multiplication by the identity preserves every entry.
  Ring R(101);
  ARingMatrixGenerator<Matrix> matgen(R);
  Matrix M(R, 5, 5);
  Matrix N(R, 5, 5);
  Matrix I(R, 5, 5);
  matgen.nextMatrix(M, MatrixShape::Dense);
  matgen.nextMatrix(I, MatrixShape::Identity);
  MatrixOps::mult(M, I, N);
  EXPECT_TRUE(MatrixOps::isEqual(N, M));
}

TEST(DMatZZp, scaleByCharacteristic)
{
  // The characteristic maps to zero and annihilates a nonzero matrix.
  int characteristic = 101;
  Ring R(characteristic);
  ARingMatrixGenerator<Matrix> matgen(R);
  Ring::Element a(R);
  R.set(a, characteristic);
  Matrix M(R, 5, 5);
  matgen.nextMatrix(M, MatrixShape::Dense);
  EXPECT_FALSE(MatrixOps::isZero(M));
  MatrixOps::scalarMultInPlace(M, a);
  EXPECT_TRUE(MatrixOps::isZero(M));
}

TEST(DMatZZp, addition)
{
  // Entrywise ring sums check addition for square and rectangular matrices.
  seedRandom(0x444d4154);
  SCOPED_TRACE("seed 0x444d4154");
  Ring R(101);
  testMatrixAdd<Matrix>(R, ntrials, 2, 2);
  testMatrixAdd<Matrix>(R, ntrials, 2, 3);
}

TEST(DMatZZp, negateInPlace)
{
  // In-place negation agrees with multiplication by minus one.
  Ring R(101);
  ARingMatrixGenerator<Matrix> matgen(R);
  Matrix M(R, 5, 5);
  Matrix N(R, 5, 5);
  matgen.nextMatrix(M, MatrixShape::Dense);
  submatrix(N) = submatrix(M);
  EXPECT_TRUE(MatrixOps::isEqual(N, M));
  MatrixOps::negateInPlace(N);
  Ring::Element minusOne(R);
  R.set(minusOne, -1);
  MatrixOps::scalarMultInPlace(M, minusOne);
  EXPECT_TRUE(MatrixOps::isEqual(M, N));
}

TEST(DMatZZp, submatrix)
{
  // Each submatrix operation starts with its own entries and checks exact
  // positions.
  Ring ring(101);
  {
    // Copying a one-entry view preserves the source while filling the
    // destination.
    SCOPED_TRACE("assign: separate one-entry views");
    Matrix matrix(ring, 5, 5);
    ring.set(matrix.entry(0, 2), 7);
    submatrix(matrix, 0, 0, 1, 1) = submatrix(matrix, 0, 2, 1, 1);
    EXPECT_TRUE(entryEquals(matrix, 0, 0, 7));
    EXPECT_TRUE(entryEquals(matrix, 0, 2, 7));
  }
  {
    // Zeroing one view leaves entries outside it intact.
    SCOPED_TRACE("zero: partial view");
    Matrix matrix(ring, 5, 5);
    ring.set(matrix.entry(0, 0), 3);
    ring.set(matrix.entry(0, 2), 7);
    submatrix(matrix, 0, 0, 2, 2) = 0;
    EXPECT_TRUE(entryEquals(matrix, 0, 0, 0));
    EXPECT_TRUE(entryEquals(matrix, 0, 2, 7));
    EXPECT_FALSE(MatrixOps::isZero(matrix));
  }
  {
    // A view covering the only nonzero entry clears the whole matrix.
    SCOPED_TRACE("zero: covering view");
    Matrix matrix(ring, 5, 5);
    ring.set(matrix.entry(0, 2), 7);
    submatrix(matrix, 0, 2, 2, 2) = 0;
    EXPECT_TRUE(MatrixOps::isZero(matrix));
  }
  {
    // The full view includes the final row and column.
    SCOPED_TRACE("zero: full view");
    Matrix matrix(ring, 5, 5);
    ring.set(matrix.entry(4, 4), 7);
    submatrix(matrix) = 0;
    EXPECT_TRUE(MatrixOps::isZero(matrix));
  }
  {
    // Shifted identity blocks expose destination offsets and scalar
    // multiplication.
    SCOPED_TRACE("assign, add, and scale: shifted identity");
    Matrix matrix(ring, 5, 5), identity(ring, 2, 2);
    ring.set(identity.entry(0, 0), 1);
    ring.set(identity.entry(1, 1), 1);
    Ring::Element scalar {ring};
    ring.set(scalar, 7);
    submatrix(matrix, 0, 1, 2, 2) = submatrix(identity);
    submatrix(matrix, 0, 0, 2, 2) += submatrix(identity);
    submatrix(matrix, 0, 0, 2, 2) *= scalar;
    for (size_t row = 0; row < 5; ++row)
      for (size_t column = 0; column < 5; ++column)
        {
          const long expected = row == 0 && column <= 1   ? 7
                                : row == 1 && column == 1 ? 7
                                : row == 1 && column == 2 ? 1
                                                          : 0;
          EXPECT_TRUE(entryEquals(matrix, row, column, expected));
        }
  }
}

}  // namespace
