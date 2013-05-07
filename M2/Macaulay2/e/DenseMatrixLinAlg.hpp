// Copyright 2013  Michael E. Stillman

#ifndef _dense_matrix_lin_alg_hpp_
#define _dense_matrix_lin_alg_hpp_

/**
 * \ingroup matrices
 */

#include "exceptions.hpp"
#include "DenseMatrixDef.hpp"
#include "aring-ffpack.hpp"
#include "aring-zzp-flint.hpp"

template<typename RT> // ring type
class DenseMatrixLinAlg
{
public:
  typedef DenseMatrixDef<RT> MatType;
  typedef typename MatType::ElementType ElementType;
  static size_t rank(const MatType& A)
  {
    throw exc::engine_error("'rank' not implemented for this kind of matrix over this ring");
    return 0;
  }

  static void determinant(const MatType& A, ElementType& result_det)
  {
    throw exc::engine_error("'determinant' not implemented for this kind of matrix over this ring");
  }

  // Set 'result_inv' with the inverse of 'A'.  If the matrix is not square, or 
  // the matrix is not invertible, or
  // the ring is one in which the matrix cannot be inverted,
  // then an exception is thown.
  static bool inverse(const MatType& A, MatType& result_inv)
  {
    throw exc::engine_error("'invert' not implemented for this kind of matrix over this ring");
  }

  static void mult(const MatType& A, const MatType& B, MatType& result_product)
  {
    throw exc::engine_error("'mult matrices' not implemented for this kind of matrix over this ring");
  }

  // If A is non-singular, then place into X the unique solution to AX=B.
  // otherwise return false
  static bool solveLinear(const MatType& A, const MatType& B, MatType& X)
  {
    throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return false;
  }

  // Find a spanning set for the null space.
  // Set 'result_nullspace' with a matrix whose columns span {x | Ax = 0}
  // Return the dimension of the nullspace
  static size_t nullSpace(const MatType& A, MatType& result_nullspace) 
  {
    throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
  }

  // To add?
  // transpose
  // multiply by a scalar
  // -A
  // A = A + B
  // A = A-B
  // A = A + B*C
  // A = A - B*C
  // trace
  // is_equal
};

template<>
class DenseMatrixLinAlg<M2::ARingZZpFFPACK>
{
public:
  typedef M2::ARingZZpFFPACK RT;
  typedef DenseMatrixDef<RT> MatType;
  typedef MatType::ElementType ElementType;

  static size_t rank(const MatType& A);

  static void determinant(const MatType& A, ElementType& result_det);

  static bool inverse(const MatType& A, MatType& result_inv);

  static void mult(const MatType& A, const MatType& B, MatType& result_product);

  static bool solveLinear(const MatType& A, const MatType& B, MatType& X);

  static size_t nullSpace(const MatType& A, MatType& result_nullspace);
};

template<>
class DenseMatrixLinAlg<M2::ARingZZpFlint>
{
public:
  typedef M2::ARingZZpFlint RT;
  typedef DenseMatrixDef<RT> MatType;
  typedef MatType::ElementType ElementType;

  static size_t rank(const MatType& A) { 
    std::cout << "calling flint rank code" << std::endl;
    return nmod_mat_rank(A.nmod_mat()); 
  }

  static void determinant(const MatType& A, ElementType& result_det) {
    result_det = nmod_mat_det(A.nmod_mat());
  }

  static bool inverse(const MatType& A, MatType& result_inv) {
    MatType& B = const_cast<MatType&>(A);
    return nmod_mat_inv(result_inv.nmod_mat(), B.nmod_mat());
  }

  static void mult(const MatType& A, const MatType& B, MatType& result_product) {
    MatType& A1 = const_cast<MatType&>(A); // needed because nmod_mat_mul doesn't declare params const
    MatType& B1 = const_cast<MatType&>(B);
    // The A1 and B1 on the next line are switched because the memory layout expected
    // is the transpose of what we have for DMat.
    nmod_mat_mul(result_product.nmod_mat(), B1.nmod_mat(), A1.nmod_mat());
  }

  static bool solveLinear(const MatType& A, const MatType& B, MatType& X) {
    MatType& A1 = const_cast<MatType&>(A); // needed because nmod_mat_solve doesn't declare params const
    MatType& B1 = const_cast<MatType&>(B);
    return nmod_mat_solve(X.nmod_mat(), B1.nmod_mat(), A1.nmod_mat());
  }

  static size_t nullSpace(const MatType& A, MatType& result_nullspace) {
    MatType& A1 = const_cast<MatType&>(A); // needed because nmod_mat_solve doesn't declare params const
    long rank = nmod_mat_nullspace(result_nullspace.nmod_mat(), A1.nmod_mat());
    return (A.numColumns() - rank);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
