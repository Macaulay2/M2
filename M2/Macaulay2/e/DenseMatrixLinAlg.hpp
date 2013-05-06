// Copyright 2013  Michael E. Stillman

#ifndef _dense_matrix_lin_alg_hpp_
#define _dense_matrix_lin_alg_hpp_

/**
 * \ingroup matrices
 */

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
    ERROR("not implemented for this ring yet");
    return static_cast<size_t>(-3);
  }

  static void determinant(const MatType& A, ElementType& result_det)
  {
    ERROR("not implemented for this ring yet");
    A.ring().set_zero(result_det);
  }

  static bool inverse(const MatType& A, MatType& result_inv)
  {
    ERROR("not implemented for this ring yet");
    return false;
  }
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

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
