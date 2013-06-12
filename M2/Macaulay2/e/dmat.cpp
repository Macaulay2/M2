// Copyright 2005  Michael E. Stillman

#include "exceptions.hpp"
#include "error.h"

#include "dmat.hpp"
#include "MatLinAlg.hpp"

////////////////////////////////////////////////////////////////////////////
// dmat code that might have alternate implementations, depending of type //
////////////////////////////////////////////////////////////////////////////

#ifdef HAVE_FFLAS_FFPACK
size_t MatLinAlg< DMat<M2::ARingZZpFFPACK> >::rank(const Mat& mat)
{
    std::cout << "Calling MatLinAlg::rank" << std::endl;
    /// @note 1. matrix data (N) is modified by FFPACK
    /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
    Mat N(mat); // copy of matrix mat.
    size_t result = FFPACK::Rank(mat.ring().field(), mat.numColumns(), mat.numRows(), N.array(), mat.numRows());
    return result;
}

void MatLinAlg< DMat<M2::ARingZZpFFPACK> >::determinant(const Mat& mat, ElementType& result_det)
{
    std::cout << "Calling MatLinAlg::determinant" << std::endl;
    /// @note 1. matrix data (N) is modified by FFPACK
    /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
    Mat N(mat);
    result_det = FFPACK::Det(mat.ring().field(), mat.numColumns(), mat.numRows(),  N.array(),  mat.numRows());
}

bool MatLinAlg< DMat<M2::ARingZZpFFPACK> >::inverse(const Mat& mat, Mat& result_inv)
{
    M2_ASSERT(mat.numRows() == mat.numColumns());
    Mat N(mat);
    size_t n = mat.numRows();
    int nullspacedim;
    FFPACK::Invert2(mat.ring().field(), n, N.array(), n, result_inv.array(), n, nullspacedim);
    return (nullspacedim == 0);
}

void MatLinAlg< DMat<M2::ARingZZpFFPACK> >::mult(const Mat& A, const Mat& B, Mat& C)
{
    // This one is a bit harder, as we need to be careful about rows/columns, and the ffpack routine
    // is so general.
    // We assume that result_product has been just created

    FFLAS::FFLAS_TRANSPOSE tA = FFLAS::FflasNoTrans;
    FFLAS::FFLAS_TRANSPOSE tB = FFLAS::FflasNoTrans;

    size_t m = B.numColumns();
    size_t n = A.numRows();
        
    size_t k = A.numColumns();
    //    size_t k2 = B.numRows();

    ElementType a;
    C.ring().init(a);
    C.ring().set_from_int(a, 1);
    FFLAS::fgemm( C.ring().field(),
                  tB, tA,
                  m,n,k,
                  a,
                  B.array(),
                  B.numRows(),
                  A.array(),
                  A.numRows(),
                  a,
                  C.array(),
                  C.numRows()
                  );
}

size_t MatLinAlg< DMat<M2::ARingZZpFFPACK> >::nullSpace(const Mat &mat, 
                                                       bool right_side,
                                                       Mat &nullspace)

{
  right_side = !right_side; // because FFPACK stores by rows, not by columns.

  Mat N(mat); // copy of mat
  size_t nr = mat.numRows();
  size_t nc = mat.numColumns();
  
  ElementType *nullspaceFFPACK = 0;
  
  size_t nullspace_dim;
  size_t nullspace_leading_dim;
  
  FFPACK::NullSpaceBasis(mat.ring().field(),
                         (right_side ? FFLAS::FflasRight : FFLAS::FflasLeft),
                         nc, nr, N.array(), nr, nullspaceFFPACK, nullspace_leading_dim, nullspace_dim);
  
  std::cerr << "leading dim = " << nullspace_leading_dim << " and dim = " << nullspace_dim << std::endl;
  if (right_side && nullspace_dim != nullspace_leading_dim)
    {
      std::cerr << "error: this should not happen!" << std::endl;
    }
  else if (!right_side && nullspace_leading_dim != nc)
    {
      std::cerr << "error: this should not happen either!" << std::endl;
    }
  
  if (right_side)
    nullspace.resize(nullspace_dim,nr);
  else
    nullspace.resize(nc,nullspace_dim);

  std::swap(nullspace.array(), nullspaceFFPACK);
  //  mat.copy_elems(nullspace.n_rows() * nullspace.n_cols(), nullspace.get_array(), 1, nullspaceFFPACK, 1); 

  delete [] nullspaceFFPACK;
  return nullspace_dim;
}

M2_arrayintOrNull MatLinAlg< DMat<M2::ARingZZpFFPACK> >::rankProfile(const Mat& mat,
                                                                     bool row_profile)
{
  // Note that FFPack stores matrices by row, not column, the opposite of what we do.
  // So row_profile true means use ffpack column rank profile!
  row_profile = not row_profile; // TODO: once matrices are stored row-major, this should be removed.
  Mat N(mat);
  
  size_t * prof; // this is where the result will be placed
  size_t rk;
  if (row_profile)
    rk = FFPACK::RowRankProfile(mat.ring().field(),
                                mat.numColumns(),mat.numRows(),
                                N.array(),mat.numRows(),
                                prof);
  else
    rk = FFPACK::ColumnRankProfile(mat.ring().field(),
                                   mat.numColumns(),mat.numRows(),
                                   N.array(),mat.numRows(),
                                   prof);
  
  M2_arrayint profile = M2_makearrayint(static_cast<int>(rk));
  for (size_t i=0; i<rk; i++)
    profile->array[i] = static_cast<int>(prof[i]);
  
  delete [] prof;
  return profile;
}

bool MatLinAlg< DMat<M2::ARingZZpFFPACK> >::solveLinear(const Mat& A, const Mat& B, bool right_side, Mat& X)
{
  std::cerr << "inside FFpackSolveLinear" << std::endl;

  size_t a_rows = A.numRows();
  size_t a_cols = A.numColumns();
  
  size_t b_rows = B.numRows();
  size_t b_cols = B.numColumns();

  Mat copyA(A);
  Mat copyB(B);

  // preallocate the space for the solutions:
  size_t x_rows = (right_side ? a_cols : b_rows);
  size_t x_cols = (right_side ? b_cols : a_rows);

  X.resize(x_rows, x_cols); // sets it to 0 too.
  
  int info; // >0 if the system is inconsistent, ==0 means success
  
  FFPACK::fgesv(A.ring().field(),
                (!right_side ? FFLAS::FflasLeft : FFLAS::FflasRight),
                a_cols, a_rows, 
                (!right_side ? b_cols : b_rows),
                copyA.array(),
                a_rows, // leading dim of A
                X.array(), x_rows,
                copyB.array(), b_rows,
                &info);
  
  if (info > 0)
    {
      // the system is inconsistent
      ERROR("the system is inconsistent");
      return false;
    }
  
  return true;
} 

bool MatLinAlg< DMat<M2::ARingZZpFFPACK> >::solveLinear(const Mat& A, const Mat& B, Mat& X)
{
  return solveLinear(A, B, true, X);
}

size_t MatLinAlg< DMat<M2::ARingZZpFFPACK> >::nullSpace(const Mat& mat, Mat& result_nullspace)
{
  return nullSpace(mat, true, result_nullspace);
}
#endif // HAVE_FFLAS_FFPACK

//template<> 
//size_t DMat<M2::ARingRRR>::rank() const
//{
//  return LUDecompositionRRR::rankRRR(*this);
//}

//template<>
//void DMat<M2::ARingRRR>::determinant(ElementType &result) const
//{
//  LUDecompositionRRR::determinantRRR(*this, result);
//}


#include "mutablemat.hpp"

template<typename MatT> 
inline MatT * MutableMatrix::coerce()
{
  MutableMat<MatT> *P = cast_to_MutableMat<MatT>();
  if (P == 0) return 0;
  return P->get_Mat();
}

template<typename MatT> 
inline const MatT * MutableMatrix::coerce() const
{
  const MutableMat<MatT> *P = cast_to_MutableMat<MatT>();
  if (P == 0) return 0;
  return P->get_Mat();
}

M2_arrayint stdvector_to_M2_arrayint(std::vector<size_t> &v)
{
  M2_arrayint result = M2_makearrayint(static_cast<int>(v.size()));
  for (size_t i = 0; i < v.size(); i++)
    result->array[i] = static_cast<int>(v[i]);
  return result;
}

engine_RawArrayIntPairOrNull rawLQUPFactorizationInPlace(MutableMatrix *A, M2_bool transpose)
{
#ifdef HAVE_FFLAS_FFPACK
  // Suppose A is m x n
  // P is n element permutation on columns
  // Qt is m element permutation on rows (inverse permutation)
  DMat<M2::ARingZZpFFPACK> *mat = A->coerce< DMat<M2::ARingZZpFFPACK> >();
  if (mat == 0) 
    {
      throw exc::engine_error("LUDivine not defined for this ring");
      //      ERROR("LUDivine not defined for this ring");
      //      return 0;
    }
  size_t nelems = mat->numColumns();
  if (mat->numRows() > mat->numColumns()) nelems = mat->numRows();

  std::vector<size_t> P(nelems, -1);
  std::vector<size_t> Qt(nelems, -1);

  // ignore return value (rank) of:
  LUdivine(mat->ring().field(),
                       FFLAS::FflasNonUnit,
                       (!transpose ? FFLAS::FflasTrans : FFLAS::FflasNoTrans),
                       mat->numColumns(),
                       mat->numRows(),
                       mat->array(),
                       mat->numRows(),
                       &P[0], 
                       &Qt[0]);

  engine_RawArrayIntPairOrNull result = new engine_RawArrayIntPair_struct;
  result->a = stdvector_to_M2_arrayint(Qt);
  result->b = stdvector_to_M2_arrayint(P);
  return result;
#endif
  return 0;
}


#include "dmat-LU.hpp"
#include "lapack.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-zzp.hpp"
#include "aring-tower.hpp"
#include "aring-m2-gf.hpp"
#include "aring-gf.hpp"
#include "aring-zz-gmp.hpp"
#include "coeffrings.hpp"

template class DMat<M2::ARingZZGMP>;
template class DMat<M2::ARingZZp>;
template class DMat<M2::ARingTower>;

template class DMat<CoefficientRingRRR>;
template class DMat<CoefficientRingCCC>;
template class DMat<CoefficientRingR>;

template class DMat<M2::ARingGFM2>;
template class DMat<M2::ARingZZpFFPACK>;

#ifdef HAVE_FLINT
template class DMat<M2::ARingZZpFlint>;
template class DMat<M2::ARingZZ>;
#endif

template class DMat<M2::ARingGF>;
template class DMat<M2::ARingRRR>;





// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
