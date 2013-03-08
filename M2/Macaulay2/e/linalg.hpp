// Copyright 2013  Michael E. Stillman
#pragma once

/// LinearAlgebra
/// template class, meant to be specialized for specific ring types

#include "exceptions.hpp"

/** LinearAlgebra<MatrixType>
 ** 
 ** note: MT should be a "matrix type", e.g. DMat<RT>, or SMat<RT>
 ** where RT is a "ring type" such as CoefficientRingRRR
 **/

template <typename MT>
class LinearAlgebra
{
public:
  typedef typename MT::ElementType ElementType;

  static size_t rank(const MT& m) 
  {
    throw exc::engine_error("'rank' not implemented for this kind of matrix over this ring");
    return 0;
  }

  // Compute the determinant of the square matrix 'm', placing the result
  // into (the already initialized) 'result'.
  // Throws an error if we cannot compute determinants yet over this type of ring/matrix
  // or the matrix is not square.
  static void determinant(const MT& m, ElementType &result)
  {
    throw exc::engine_error("'determinant' not implemented for this kind of matrix over this ring");
  }

  // Set 'inverse' with the inverse of 'm'.  If the matrix is not square, or 
  // the matrix is not invertible, or
  // the ring is one in which the matrix cannot be inverted,
  // then an exception is thown.
  static void invert(const MT& m, MT& result_inverse) 
  {
    throw exc::engine_error("'invert' not implemented for this kind of matrix over this ring");
  }

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  static M2_arrayintOrNull rankProfile(const MT& m, bool row_profile) 
  {
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
    return 0;
  }

  // Find a spanning set for the null space.
  // If right_side is true, return a matrix whose rows span {x |  xM = 0},
  // otherwise return a matrix whose columns span {x | Mx = 0}
  static void nullSpace(const MT& m, MT& nullspace, bool right_side) 
  {
    throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
  }

  // X is set to  a matrix whose rows or columns solve either AX = B (right_side=true)
  // or XA = B (right_side=false). If no solutions are found, false is returned,
  // and X is left in a valid state, whose value should be ignored.
  // If A and B have incompatible sizes, then an exception is returned.
  static bool solveLinear(const MT& A, const MT& B, const MT& X, bool right_side) 
  {
    throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return false;
  }

  /** C,A,B should be mutable matrices over the same ring, 
        AND of the same density type.
      a,b are elements of this ring. 
      C := b*C + a * op(A)*op(B),
      where op(A) = A or transpose(A), depending on transposeA
      where op(B) = B or transpose(B), depending on transposeB
      If the sizes do not match up, or this function is not implemented,
      then an exception is thrown.
  */  
  static void addMultipleTo(MT& C,
                            const MT& A,
                            const MT& B,
                            bool transposeA,
                            bool transposeB,
                            const ElementType& a,
                            const ElementType& b) 
  {
    throw exc::engine_error("'addMultipleTo' not implemented for this kind of matrix over this ring");
  }

  /** if 'this' is a square n x n matrix, return
      an array, {a0,a1,a2,...,an} such that
      the characteristic polynomial is
      det(t*I - this) = a0 + a1 * t + ... + an * t^n .
  */
  static void characteristicPolynomial(MT& m, MT& result)
  {
    throw exc::engine_error("'characteristicPolynomial' not implemented for this kind of matrix over this ring");
  }

  /** if 'this' is a square n x n matrix, return
      an array, {a0,a1,a2,...,am} such that
      the minimal monic polynomial of 'this' is
      a0 + a1 * t + ... + am * t^m .
  */
  static void minimalPolynomial(MT& m, MT& result)
  {
    throw exc::engine_error("'minimalPolynomial' not implemented for this kind of matrix over this ring");
  }
  
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

