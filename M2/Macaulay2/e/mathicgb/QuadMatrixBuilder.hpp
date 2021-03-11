// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_QUAD_MATRIX_BUILDER_GUARD
#define MATHICGB_QUAD_MATRIX_BUILDER_GUARD

#include "MonomialMap.hpp"
#include "SparseMatrix.hpp"
#include "PolyRing.hpp"
#include <vector>
#include <map>
#include <limits>
#include <string>
#include <ostream>
#include "memtailor/memtailor.h"

MATHICGB_NAMESPACE_BEGIN

class QuadMatrix;

/// Builder for QuadMatrix. This is not quite the builder pattern in
/// that the interface is not virtual and the implementation cannot be
/// swapped out - it only follows the builder pattern in that it is a
/// class that allows step-wise construction of a final product.
class QuadMatrixBuilder {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  typedef SparseMatrix::RowIndex RowIndex;
  typedef SparseMatrix::ColIndex ColIndex;
  typedef SparseMatrix::Scalar Scalar;

  /// The index of a column that can be either on the left or the
  /// right side. The largest representable ColIndex is an invalid
  /// index. This is the default value. The only allowed method to
  /// call for an invalid index is valid().
  class LeftRightColIndex {
  public:
    LeftRightColIndex():
      mRawIndex(std::numeric_limits<ColIndex>::max()), mLeft(false) {}

    LeftRightColIndex(ColIndex index, bool left):
      mRawIndex(index), mLeft(left)
    {}

    ColIndex leftIndex() const {
      MATHICGB_ASSERT(left());
      return index();
    }

    ColIndex rightIndex() const {
      MATHICGB_ASSERT(right());
      return index();
    }

    /// Use leftIndex() or rightIndex() instead if you know what side
    /// you are expecting, as those do an assert on your expectation.
    ColIndex index() const {
      MATHICGB_ASSERT(valid());
      return mRawIndex;
    }

    bool left() const {
      MATHICGB_ASSERT(valid());
      return mLeft;
    }

    bool right() const {
      MATHICGB_ASSERT(valid());
      return !left();
    }

    bool valid() const {
      return mRawIndex != std::numeric_limits<ColIndex>::max();
    }

    bool operator==(const LeftRightColIndex& index) const {
      return mRawIndex == index.mRawIndex && mLeft == index.mLeft;
    }

    bool operator!=(const LeftRightColIndex& index) const {
      return !(*this == index);
    }

  private:
    ColIndex mRawIndex;
    bool mLeft;
  };

  typedef MonomialMap<LeftRightColIndex> Map;
  typedef std::vector<ConstMonoPtr> Monomials;

  QuadMatrixBuilder(
    const PolyRing& ring,
    Map& map,
    Monomials& monomialsLeft,
    Monomials& monomialsRight,
    size_t memoryQuantum = 0
  );

  /// Inserts the rows from builder. To avoid an assert either the matrix must
  /// have no column monomials specified or the monomials that are specified
  /// must match exactly to the column monomials for this object --- including
  /// the ordering of the monomials.
  void takeRowsFrom(QuadMatrix&& matrix);

  size_t memoryQuantum() const {
    return mTopLeft.memoryQuantum();
  }

  // **** Appending entries to top matrices.
  // Same interface as SparseMatrix except with two matrices and here
  // you have to create columns before you can use them.

  void appendEntryTopLeft(ColIndex col, Scalar scalar) {
    mTopLeft.appendEntry(col, scalar);
  }

  void appendEntryTopRight(ColIndex col, Scalar scalar) {
    mTopRight.appendEntry(col, scalar);
  }

  void appendEntryTop(LeftRightColIndex col, Scalar scalar) {
    MATHICGB_ASSERT(col.valid());
    if (col.left())
      appendEntryTopLeft(col.leftIndex(), scalar);
    else
      appendEntryTopRight(col.rightIndex(), scalar);
  }

  void rowDoneTopLeftAndRight() {
    mTopLeft.rowDone();
    mTopRight.rowDone();
  }

  // **** Appending entries to bottom matrices
  // Same interface as SparseMatrix except with two matrices and here
  // you have to create columns before you can use them.

  void appendEntryBottomLeft(ColIndex col, Scalar scalar) {
    mBottomLeft.appendEntry(col, scalar);
  }

  void appendEntryBottomRight(ColIndex col, Scalar scalar) {
    mBottomRight.appendEntry(col, scalar);
  }

  void appendEntryBottom(LeftRightColIndex col, Scalar scalar) {
    MATHICGB_ASSERT(col.valid());
    if (col.left())
      appendEntryBottomLeft(col.leftIndex(), scalar);
    else
      appendEntryBottomRight(col.rightIndex(), scalar);
  }

  void rowDoneBottomLeftAndRight() {
    mBottomLeft.rowDone();
    mBottomRight.rowDone();
  }

  // *** Creating and reordering columns
  // You have to create columns before you can append entries in those columns.
  // All passed in monomials are copied so that ownership of the memory is
  // not taken over. The creation methods return a LeftRightColIndex instead
  // of just a ColIndex to allow more of a chance for asserts to catch errors
  // and to avoid the need for the client to immediately construct a
  // LeftRightColIndex based on the return value.

  /// Creates a new column associated to the monomial
  /// monomialToBeCopied to the left matrices. There must not already
  /// exist a column for this monomial on the left or on the right.
  std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonomial>
  createColumnLeft(ConstMonoRef monomialToBeCopied);

  /// Creates a new column associated to the monomial monomialToBeCopied
  /// to the right matrices. There must not already exist a column for
  /// this monomial on the left or on the right.
  std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonomial>
  createColumnRight(ConstMonoRef monomialToBeCopied);

  // *** Querying columns

  const SparseMatrix& topLeft() const {return mTopLeft;}
  const SparseMatrix& topRight() const {return mTopRight;}
  const SparseMatrix& bottomLeft() const {return mBottomLeft;}
  const SparseMatrix& bottomRight() const {return mBottomRight;}

  const PolyRing& ring() const {return mMonomialToCol.ring();}
  const Monoid& monoid() const {return ring().monoid();}

  /// Returns the built matrix and sets the builder to a state
  /// with no columns and no rows.
  QuadMatrix buildMatrixAndClear();

private:
  Monomials& mMonomialsLeft; /// stores one monomial per left column
  Monomials& mMonomialsRight; /// stores one monomial per right column

  /// Used for fast determination of which column has a given monomial.
  Map& mMonomialToCol;

  SparseMatrix mTopLeft;
  SparseMatrix mTopRight;
  SparseMatrix mBottomLeft;
  SparseMatrix mBottomRight;
};

MATHICGB_NAMESPACE_END
#endif
