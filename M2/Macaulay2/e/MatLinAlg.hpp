// Copyright 2013  Michael E. Stillman

#ifndef _mat_lin_alg_hpp_
#define _mat_lin_alg_hpp_

/**
 * \ingroup matrices
 */

#include "exceptions.hpp"
#include "dmat.hpp"

#ifdef HAVE_FFLAS_FFPACK
#include "aring-ffpack.hpp"
#endif

#ifdef HAVE_FLINT
#include "aring-zz-flint.hpp"
#include "aring-zzp-flint.hpp"
#endif

template<typename MT> // matrix type
class MatLinAlg
{
public:
  typedef MT Mat;
  typedef typename Mat::ElementType ElementType;

  static size_t rank(const Mat& A)
  {
    throw exc::engine_error("'rank' not implemented for this kind of matrix over this ring");
    return 0;
  }

  static void determinant(const Mat& A, ElementType& result_det)
  {
    throw exc::engine_error("'determinant' not implemented for this kind of matrix over this ring");
  }

  // Set 'result_inv' with the inverse of 'A'.  If the matrix is not square, or 
  // the matrix is not invertible, or
  // the ring is one in which the matrix cannot be inverted,
  // then an exception is thown.
  static bool inverse(const Mat& A, Mat& result_inv)
  {
    throw exc::engine_error("'invert' not implemented for this kind of matrix over this ring");
  }

  static void mult(const Mat& A, const Mat& B, Mat& result_product)
  {
    throw exc::engine_error("'mult matrices' not implemented for this kind of matrix over this ring");
  }

  static void addMultipleTo(Mat& C, const Mat& A, const Mat& B)
  // C = C + A*B
  {
    throw exc::engine_error("'addMultipleTo' not implemented for this kind of matrix over this ring");
  }

  // If A is non-singular, then place into X the unique solution to AX=B.
  // otherwise return false
  static bool solveLinear(const Mat& A, const Mat& B, Mat& X)
  {
    throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return false;
  }

  // If A is non-singular, then place into X the unique solution to AX=B.
  // otherwise return false
  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X)
  {
    throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return false;
  }

  // Find a spanning set for the null space.
  // Set 'result_nullspace' with a matrix whose columns span {x | Ax = 0}
  // Return the dimension of the nullspace
  static size_t nullSpace(const Mat& A, Mat& result_nullspace) 
  {
    throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
  }

  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace) 
  {
    throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
  }

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
  {
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
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

#ifdef HAVE_FFLAS_FFPACK
template<>
class MatLinAlg< DMat<M2::ARingZZpFFPACK> >
{
public:
  typedef M2::ARingZZpFFPACK RT;
  typedef DMat<RT> Mat;
  typedef Mat::ElementType ElementType;

  static size_t rank(const Mat& A);

  static void determinant(const Mat& A, ElementType& result_det);

  static bool inverse(const Mat& A, Mat& result_inv);

  static void mult(const Mat& A, const Mat& B, Mat& result_product);

  static bool solveLinear(const Mat& A, const Mat& B, Mat& X);

  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X);

  static size_t nullSpace(const Mat& A, Mat& result_nullspace);

  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace);

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile);
};
#endif

#ifdef HAVE_FLINT
template<>
class MatLinAlg< DMat<M2::ARingZZ> >
{
public:
  typedef M2::ARingZZ RT;
  typedef DMat<RT> Mat;
  typedef Mat::ElementType ElementType;

  static size_t rank(const Mat& A) { 
    std::cout << "calling flint rank code" << std::endl;
    return fmpz_mat_rank(A.fmpz_mat()); 
  }

  static void determinant(const Mat& A, ElementType& result_det) {
    fmpz_mat_det(& result_det, A.fmpz_mat());
  }

  static bool inverse(const Mat& A, Mat& result_inv) {
    ElementType den;
    A.ring().init(den);
    bool result = fmpz_mat_inv(result_inv.fmpz_mat(), &den, A.fmpz_mat());
    if (!fmpz_is_pm1(&den)) 
      result = false;
    A.ring().clear(den);
    return result;
  }

  static void mult(const Mat& A, const Mat& B, Mat& result_product) {
    // The A1 and B1 on the next line are switched because the memory layout expected
    // is the transpose of what we have for DMat.
    fmpz_mat_mul(result_product.fmpz_mat(), B.fmpz_mat(), A.fmpz_mat());
  }

  static bool solveLinear(const Mat& A, const Mat& B, Mat& X) {
    ElementType den;
    A.ring().init(den);
    bool result = fmpz_mat_solve(X.fmpz_mat(), &den, B.fmpz_mat(), A.fmpz_mat());
    if (!fmpz_is_pm1(&den)) 
      result = false;
    A.ring().clear(den);
    return result;
  }

  static size_t nullSpace(const Mat& A, Mat& result_nullspace) {
    long rank = fmpz_mat_nullspace(result_nullspace.fmpz_mat(), A.fmpz_mat());
    return (A.numColumns() - rank);
  }

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
  {
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
  }
  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace) 
  {
    throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
  }
  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X)
  {
    throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return false;
  }
};
#endif

#ifdef HAVE_FLINT
template<>
class MatLinAlg< DMat<M2::ARingZZpFlint> >
{
public:
  typedef M2::ARingZZpFlint RT;
  typedef DMat<RT> Mat;
  typedef Mat::ElementType ElementType;

  static size_t rank(const Mat& A) { 
    std::cout << "calling flint rank code" << std::endl;
    return nmod_mat_rank(A.nmod_mat()); 
  }

  static void determinant(const Mat& A, ElementType& result_det) {
    result_det = nmod_mat_det(A.nmod_mat());
  }

  static bool inverse(const Mat& A, Mat& result_inv) {
    Mat& B = const_cast<Mat&>(A);
    return nmod_mat_inv(result_inv.nmod_mat(), B.nmod_mat());
  }

  static void mult(const Mat& A, const Mat& B, Mat& result_product) {
    Mat& A1 = const_cast<Mat&>(A); // needed because nmod_mat_mul doesn't declare params const
    Mat& B1 = const_cast<Mat&>(B);
    // The A1 and B1 on the next line are switched because the memory layout expected
    // is the transpose of what we have for DMat.
    nmod_mat_mul(result_product.nmod_mat(), B1.nmod_mat(), A1.nmod_mat());
  }

  static bool solveLinear(const Mat& A, const Mat& B, Mat& X) {
    Mat& A1 = const_cast<Mat&>(A); // needed because nmod_mat_solve doesn't declare params const
    Mat& B1 = const_cast<Mat&>(B);
    return nmod_mat_solve(X.nmod_mat(), B1.nmod_mat(), A1.nmod_mat());
  }

  static size_t nullSpace(const Mat& A, Mat& result_nullspace) {
    Mat& A1 = const_cast<Mat&>(A); // needed because nmod_mat_solve doesn't declare params const
    long rank = nmod_mat_nullspace(result_nullspace.nmod_mat(), A1.nmod_mat());
    return (A.numColumns() - rank);
  }

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
  {
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
  }
  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace) 
  {
    throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
  }
  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X)
  {
    throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return false;
  }
};
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
