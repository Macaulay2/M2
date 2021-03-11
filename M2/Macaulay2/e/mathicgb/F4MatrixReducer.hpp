// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_F4_MATRIX_REDUCER_GUARD
#define MATHICGB_F4_MATRIX_REDUCER_GUARD

#include "SparseMatrix.hpp"

MATHICGB_NAMESPACE_BEGIN

class QuadMatrix;
class PolyRing;

/// Class that reduces an F4 matrix represented as a QuadMatrix. The
/// answer that you get is the submatrix that contains new pivots.
///
/// All QuadMatrix parameters passed into methods on this class are
/// assumed to have a permutation of the top rows and left columns so
/// that the top left matrix is upper unitriangular. In this way the
/// lower left part of the matrix becomes all-zero after row reduction.
class F4MatrixReducer {
public:
  /// The ring used is Z/pZ where modulus is the prime p.
  F4MatrixReducer(coefficient modulus);

  /// Reduces the bottom rows by the top rows and returns the bottom right
  /// submatrix of the resulting quad matrix. The lower left submatrix
  /// is not returned because it is always zero after row reduction.
  SparseMatrix reduceToBottomRight(const QuadMatrix& matrix);

  /// Returns the reduced row echelon form of matrix.
  SparseMatrix reducedRowEchelonForm(const SparseMatrix& matrix);

  /// Returns the lower right submatrix of the reduced row echelon
  /// form of matrix. The lower left part is not returned because it is
  /// always zero after row reduction.
  SparseMatrix reducedRowEchelonFormBottomRight(const QuadMatrix& matrix);

private:
  const SparseMatrix::Scalar mModulus;
};

MATHICGB_NAMESPACE_END
#endif
