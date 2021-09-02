// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_QUAD_MATRIX_GUARD
#define MATHICGB_QUAD_MATRIX_GUARD

#include "PolyRing.hpp"
#include "SparseMatrix.hpp"
#include <vector>
#include <string>
#include <ostream>

MATHICGB_NAMESPACE_BEGIN

/// Represents a matrix composed of 4 sub-matrices that fit together
/// into one matrix divided into top left, top right, bottom left and
/// bottom right. This is a convenient representation of the matrices
/// encountered in the F4 polynomial reduction algorithm.
class QuadMatrix {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  QuadMatrix(): mRing(nullptr) {}
  QuadMatrix(const PolyRing& ring): mRing(&ring) {}

  QuadMatrix(QuadMatrix&& matrix):
    topLeft(std::move(matrix.topLeft)),
    topRight(std::move(matrix.topRight)),
    bottomLeft(std::move(matrix.bottomLeft)),
    bottomRight(std::move(matrix.bottomRight)),
    leftColumnMonomials(std::move(matrix.leftColumnMonomials)),
    rightColumnMonomials(std::move(matrix.rightColumnMonomials)),
    mRing(&matrix.ring())
  {}

  QuadMatrix& operator=(QuadMatrix&& matrix) {
    MATHICGB_ASSERT(mRing == matrix.mRing);
    this->~QuadMatrix();
    new (this) QuadMatrix(std::move(matrix));
    return *this;
  }

  void clear() {
    *this = QuadMatrix(ring());
  }

  typedef std::vector<ConstMonoPtr> Monomials;

  SparseMatrix topLeft; 
  SparseMatrix topRight;
  SparseMatrix bottomLeft;
  SparseMatrix bottomRight;
  Monomials leftColumnMonomials;
  Monomials rightColumnMonomials;

  /// Prints whole matrix to out in human-readable format. Useful for
  /// debugging.
  void print(std::ostream& out) const;

  void printStatistics(std::ostream& out) const;

  size_t memoryUse() const;
  size_t memoryUseTrimmed() const;

  /// Shows whole matrix in a string. Useful for debugging.
  std::string toString() const;

  /// Return the combined number of non-zero entries.
  size_t entryCount() const;

  /// Return the combined number of left and right columns.
  size_t rowCount() const;

  /// Return the number of left columns.
  SparseMatrix::ColIndex computeLeftColCount() const;

  /// Return the number of right columns.
  SparseMatrix::ColIndex computeRightColCount() const;

  void write(SparseMatrix::Scalar modulus, FILE* file) const;

  /// Read a matrix from file into *this. Return the modulus from file.
  /// This method clears the column monomials and the ring pointer.
  SparseMatrix::Scalar read(FILE* file);

  /// Sort the left columns to be in decreasing order according to the monomial
  /// order from the ring. The operation is done in parallel.
  void sortColumnsLeftRightParallel();

  /// Makes a copy of this matrix whose rows are sorted in some canonical way.
  /// TODO: Actually only coarsely sorts the top rows right now.
  QuadMatrix toCanonical() const;

  /// Asserts internal invariants if asserts are turned on.
  bool debugAssertValid() const;

  const PolyRing& ring() const {return *mRing;}
  const Monoid& monoid() const {return ring().monoid();}

private:
  QuadMatrix(const QuadMatrix&); // not available
  void operator=(const QuadMatrix&); // not available

  const PolyRing* const mRing;
};

std::ostream& operator<<(std::ostream& out, const QuadMatrix& qm);

MATHICGB_NAMESPACE_END

#endif
