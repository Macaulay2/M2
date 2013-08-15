// Copyright 2013  Michael E. Stillman

#ifndef _mat_lin_alg_hpp_
#define _mat_lin_alg_hpp_

/**
 * \ingroup matrices
 */

#include "exceptions.hpp"
#include "dmat.hpp"

#ifdef HAVE_FFLAS_FFPACK
#include "aring-zzp-ffpack.hpp"
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

  /// @brief the rank of a matrix
  ///
  /// throws an engine_error for ring/matrix types where the function is not implemented.
  /// This version is deterministic.
  static size_t rank(const Mat& A)
  {
    throw exc::engine_error("'rank' not implemented for this kind of matrix over this ring");
    return 0;
  }

  /// @brief the determinant of a square matrix
  ///
  /// result_det should be a previously initialized ElementType.
  /// throws an engine_error for ring/matrix types where the function is not implemented.
  static void determinant(const Mat& A, ElementType& result_det)
  {
    throw exc::engine_error("'determinant' not implemented for this kind of matrix over this ring");
  }

  /// @brief the inverse of a square matrix
  ///
  /// result_inv is set to the inverse of the square matrix A, if A is invertible.
  /// result_inv should be a Mat, with the same ring/type as the input matrix A.
  ///   result_inv does not need to be the same size as A, it will be resized if needed.
  /// returns true exactly when the matrix is invertible, and result_inv has been set.
  ///
  /// throws an engine_error for ring/matrix types where the function is not implemented.
  /// throws an error if the matrix is not square.
  ///
  /// Note: the inverse of a 0 x 0 matrix is another 0 x 0 matrix.
  static bool inverse(const Mat& A, Mat& result_inv)
  {
    throw exc::engine_error("'invert' not implemented for this kind of matrix over this ring");
  }

  /// @brief the product of two matrices
  ///
  /// result_product is set to the product A*B
  /// result_product should be a Mat, with the same ring/type as the input matrices A,B.
  ///   result_product does not need to be the same size as A*B, it will be resized if needed.
  ///
  /// throws an engine_error for ring/matrix types where the function is not implemented.
  /// throws an error if the number of columns of A is not the number of rows of B.
  /// result_prod should not be the same as A or B (assertion error).
  static void mult(const Mat& A, const Mat& B, Mat& result_product)
  {
    throw exc::engine_error("'mult matrices' not implemented for this kind of matrix over this ring");
  }

  /// @brief the left or right null space of a matrix
  ///
  /// if right_side is true then 
  ///   result_nullspace is set to the matrix whose columns form a basis for {x | Ax = 0}.
  /// if right_side is false then
  ///   result_nullspace is set to the matrix whose rows form a basis for {x | xA = 0}.
  /// Returns the dimension of the nullspace.
  ///
  /// result_nullspace should be a Mat, with the same ring/type as the input matrix A.
  ///   result_nullspace does not need to be the correct size, it will be resized if needed.
  ///
  /// throws an engine_error for ring/matrix types where the function is not implemented.
  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace) 
  {
    throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
  }

  /// @brief solve a linear equation AX=B or XA=B
  ///
  /// if right_side is true then 
  ///   X is set to a matrix which solves AX=B.
  /// if right_side is false then
  ///   X is set to a matrix which solves XA=B.
  ///
  /// true is returned iff this equation has a solution.
  ///
  /// declare_A_is_invertible is a hint: if true, then A is assumed to be a square invertible matrix.
  ///   If A is not invertible, and declare_A_is_invertible is true, then the routine may either fail or crash.
  /// if declare_A_is_invertible is false, then no such assumption is made.
  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X, bool declare_A_is_invertible)
  {
    throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return false;
  }

  /// @brief solve AX=B, return true if the system has a solution.
  static bool solveLinear(const Mat& A, const Mat& B, Mat& X)
  {
    return solveLinear(A,B,true,X,false);
  }

  /// @brief Returns either the row or column rank profile of A
  ///
  /// if row_profile is true, then row profile is computed, otherwise
  /// the column profile is computed.
  ///
  /// The return value is an ascending sequence of non-negative integers
  /// with an entry a occuring iff the submatrix of A of the first
  /// (a-1) rows (resp columns) has lower rank than the submatrix of the 
  /// first a rows (resp columns).  
  ///
  /// Notice that if the matrix is non-zero and the first row is 
  /// non-zero, then the first entry will be 0.
  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
  {
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
  }

  /// @brief Set C += A*B
  /// 
  /// Throws an exception if not yet implementd for this ring/matrix type.
  /// The sizes of C,A,B must be compatible.  These are checked only via assertions.
  static void addMultipleTo(Mat& C, const Mat& A, const Mat& B)
  // C = C + A*B
  {
    throw exc::engine_error("'addMultipleTo' not implemented for this kind of matrix over this ring");
  }

  /// @brief Set C -= A*B
  /// 
  /// Throws an exception if not yet implementd for this ring/matrix type.
  /// The sizes of C,A,B must be compatible.  These are checked only via assertions.
  static void subtractMultipleTo(Mat& C, const Mat& A, const Mat& B)
  // C = C - A*B
  {
    throw exc::engine_error("'subtractMultipleTo' not implemented for this kind of matrix over this ring");
  }

  // Other functions possibly desired:
  // (1) rref
  // (2) LU = PA decomposition.
  //     other decompositions?
  // (3) char polynomial
  // (4) minimal polynomial
  
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

  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace);

  static bool solveLinear(const Mat& A, const Mat& B, Mat& X);

  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X, bool declare_A_is_invertible);

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile);

  static void addMultipleTo(Mat& C, const Mat& A, const Mat& B);

  static void subtractMultipleTo(Mat& C, const Mat& A, const Mat& B);
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
    std::cout << "calling flint rankZZ code" << std::endl;
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
    fmpz_mat_mul(result_product.fmpz_mat(), A.fmpz_mat(), B.fmpz_mat());
  }

  static size_t nullSpace(const Mat& A, Mat& result_nullspace) {
    long rank = fmpz_mat_nullspace(result_nullspace.fmpz_mat(), A.fmpz_mat());
    return (A.numColumns() - rank);
  }

  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace) 
  {
    if (not right_side)
      throw exc::engine_error("'nullSpace' not implemented for this kind of matrix over this ring");
    return nullSpace(A, result_nullspace);
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

  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X, bool declare_A_is_invertible)
  {
    //TODO: write this routine in the cases which are not handled
    if (not right_side or not declare_A_is_invertible)
      throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return solveLinear(A,B,X);
  }

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
  {
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
  }

  static void addMultipleTo(Mat& C, const Mat& A, const Mat& B)
  {
    Mat D(C.ring(), A.numRows(), B.numColumns());
    fmpz_mat_mul(D.fmpz_mat(), A.fmpz_mat(), B.fmpz_mat());
    fmpz_mat_add(C.fmpz_mat(), C.fmpz_mat(), D.fmpz_mat());
  }

  static void subtractMultipleTo(Mat& C, const Mat& A, const Mat& B)
  {
    Mat D(C.ring(), A.numRows(), B.numColumns());
    fmpz_mat_mul(D.fmpz_mat(), A.fmpz_mat(), B.fmpz_mat());
    fmpz_mat_sub(C.fmpz_mat(), C.fmpz_mat(), D.fmpz_mat());
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

  static size_t nullSpace(const Mat& A, Mat& result_nullspace) {
    Mat& A1 = const_cast<Mat&>(A); // needed because nmod_mat_solve doesn't declare params const
    long rank = nmod_mat_nullspace(result_nullspace.nmod_mat(), A1.nmod_mat());
    return (A.numColumns() - rank);
  }

  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace) 
  {
    //TODO: WRITE ME
    if (not right_side)
      throw exc::engine_error("'nullSpace' for left-side not implemented for this kind of matrix over this ring");
    return nullSpace(A,true,result_nullspace);
  }

  static bool solveLinear(const Mat& A, const Mat& B, Mat& X) {
    Mat& A1 = const_cast<Mat&>(A); // needed because nmod_mat_solve doesn't declare params const
    Mat& B1 = const_cast<Mat&>(B);
    return nmod_mat_solve(X.nmod_mat(), B1.nmod_mat(), A1.nmod_mat());
  }

  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X, bool declare_A_is_invertible)
  {
    //TODO: WRITE ME
    if (not right_side or not declare_A_is_invertible)
      throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return solveLinear(A,B,X);
  }

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
  {
    //TODO: WRITE ME
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
  }

  static void addMultipleTo(Mat& C, const Mat& A, const Mat& B)
  {
    Mat D(C.ring(), A.numRows(), B.numColumns());
    nmod_mat_mul(D.nmod_mat(), A.nmod_mat(), B.nmod_mat());
    nmod_mat_add(C.nmod_mat(), C.nmod_mat(), D.nmod_mat());
  }

  static void subtractMultipleTo(Mat& C, const Mat& A, const Mat& B)
  {
    Mat D(C.ring(), A.numRows(), B.numColumns());
    nmod_mat_mul(D.nmod_mat(), A.nmod_mat(), B.nmod_mat());
    nmod_mat_sub(C.nmod_mat(), C.nmod_mat(), D.nmod_mat());
  }
};
#endif

#ifdef HAVE_FLINT
template<>
class MatLinAlg< DMat<M2::ARingQQFlint> >
{
public:
  typedef M2::ARingQQFlint RT;
  typedef DMat<RT> Mat;
  typedef Mat::ElementType ElementType;

  static size_t rank(const Mat& A) { 
    std::cerr << "calling flint rank code" << std::endl;
    // fmpq_mat has no rank function.
    // So we clear denominators row-wise (or column-wise), and compute the rank of that matrix.
    fmpz_mat_t m1;
    fmpz_mat_init(m1, A.numRows(), A.numColumns());
    fmpq_mat_get_fmpz_mat_rowwise(m1, NULL, A.fmpq_mat());
    fmpz_mat_print_pretty(m1);
    std::cerr << "calling fmpz_mat_rank" << std::endl;
    size_t rk = fmpz_mat_rank(m1);
    std::cerr << "about to clear m1" << std::endl;
    fmpz_mat_clear(m1);
    std::cerr << "returning " << rk << std::endl;
    return rk;
  }

  static void determinant(const Mat& A, ElementType& result_det) {
    fmpq_mat_det(&result_det, A.fmpq_mat());
  }

  static bool inverse(const Mat& A, Mat& result_inv) {
    return fmpq_mat_inv(result_inv.fmpq_mat(), A.fmpq_mat());
  }

  static void mult(const Mat& A, const Mat& B, Mat& result_product) {
    std::cout << "calling flintQQ matrix mult code" << std::endl;
    // The A and B on the next line are switched because the memory layout expected
    // is the transpose of what we have for DMat.
    fmpq_mat_mul(result_product.fmpq_mat(), A.fmpq_mat(), B.fmpq_mat());
  }

  static size_t nullSpace(const Mat& A, Mat& result_nullspace) {
    //TODO: WRITE ME
    Mat& A1 = const_cast<Mat&>(A); // needed because fmpq_mat_solve doesn't declare params const
    //    long rank = fmpq_mat_nullspace(result_nullspace.fmpq_mat(), A1.fmpq_mat());
    //    return (A.numColumns() - rank);
    return 0;
  }
  static size_t nullSpace(const Mat& A, bool right_side, Mat& result_nullspace) 
  {
    //TODO: write this routine in the cases which are not handled
    if (not right_side)
      throw exc::engine_error("'nullSpace' for left-side not implemented for this kind of matrix over this ring");
    return nullSpace(A,true,result_nullspace);
  }


  static bool solveLinear(const Mat& A, const Mat& B, Mat& X) {
    Mat& A1 = const_cast<Mat&>(A); // needed because fmpq_mat_solve doesn't declare params const
    Mat& B1 = const_cast<Mat&>(B);
    //    return fmpq_mat_solve(X.fmpq_mat(), B1.fmpq_mat(), A1.fmpq_mat());
    return false;
  }
  static bool solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X, bool declare_A_is_invertible)
  {
    //TODO: write this routine in the cases which are not handled
    if (not right_side or not declare_A_is_invertible)
      throw exc::engine_error("'solveLinear' not implemented for this kind of matrix over this ring");
    return solveLinear(A,B,X);
  }

  static M2_arrayintOrNull rankProfile(const Mat& A, bool row_profile)
  {
    //TODO: WRITE ME
    throw exc::engine_error("'rankProfile' not implemented for this kind of matrix over this ring");
  }

  static void addMultipleTo(Mat& C, const Mat& A, const Mat& B)
  {
    Mat D(C.ring(), A.numRows(), B.numColumns());
    fmpq_mat_mul(D.fmpq_mat(), A.fmpq_mat(), B.fmpq_mat());
    fmpq_mat_add(C.fmpq_mat(), C.fmpq_mat(), D.fmpq_mat());
  }

  static void subtractMultipleTo(Mat& C, const Mat& A, const Mat& B)
  {
    Mat D(C.ring(), A.numRows(), B.numColumns());
    fmpq_mat_mul(D.fmpq_mat(), A.fmpq_mat(), B.fmpq_mat());
    fmpq_mat_sub(C.fmpq_mat(), C.fmpq_mat(), D.fmpq_mat());
  }
};
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
