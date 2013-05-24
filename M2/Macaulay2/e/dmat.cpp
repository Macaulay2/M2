// Copyright 2005  Michael E. Stillman

#include "exceptions.hpp"

#include "coeffrings.hpp"
#include "coeffrings-zz.hpp"
#include "ZZp.hpp"
#include "aring-RRR.hpp"
#include "aring-gf.hpp"
#include "aring-m2-gf.hpp"
#include "aring-tower.hpp"

#include "dmat.hpp"
#include "mat.hpp"
#include "mpfr.h"
#include <iostream>

#include "aring-zzp.hpp"
#include "aring-ffpack.hpp"

#include "dmat-RRR.hpp"
#include "aring-zzp-flint.hpp"

#include <typeinfo>

////////////////////////////////////////////////////////////////////////////
// dmat code that might have alternate implementations, depending of type //
////////////////////////////////////////////////////////////////////////////

#ifdef HAVE_FFLAS_FFPACK
size_t DenseMatrixLinAlg<M2::ARingZZpFFPACK>::rank(const MatType& mat)
{
    std::cout << "Calling DenseMatrixLinAlg::rank" << std::endl;
    /// @note 1. matrix data (N) is modified by FFPACK
    /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
    MatType N(mat); // copy of matrix mat.
    size_t result = FFPACK::Rank(mat.ring().field(), mat.numColumns(), mat.numRows(), N.array(), mat.numRows());
    return result;
}

void DenseMatrixLinAlg<M2::ARingZZpFFPACK>::determinant(const MatType& mat, ElementType& result_det)
{
    std::cout << "Calling DenseMatrixLinAlg::determinant" << std::endl;
    /// @note 1. matrix data (N) is modified by FFPACK
    /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
    MatType N(mat);
    result_det = FFPACK::Det(mat.ring().field(), mat.numColumns(), mat.numRows(),  N.array(),  mat.numRows());
}

bool DenseMatrixLinAlg<M2::ARingZZpFFPACK>::inverse(const MatType& mat, MatType& result_inv)
{
    M2_ASSERT(mat.numRows() == mat.numColumns());
    MatType N(mat);
    size_t n = mat.numRows();
    int nullspacedim;
    FFPACK::Invert2(mat.ring().field(), n, N.array(), n, result_inv.array(), n, nullspacedim);
    return (nullspacedim == 0);
}

void DenseMatrixLinAlg<M2::ARingZZpFFPACK>::mult(const MatType& A, const MatType& B, MatType& C)
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

bool DenseMatrixLinAlg<M2::ARingZZpFFPACK>::solveLinear(const MatType& A, const MatType& B, MatType& X)
{
  return false;
}

size_t DenseMatrixLinAlg<M2::ARingZZpFFPACK>::nullSpace(const MatType& mat, MatType& result_nullspace)
{
#if 0
    bool right_side = false; // because FFPACK stores by rows, not by columns.

    M2_ASSERT(mat.numRows() == mat.NumColumns());
    MatType N(mat);
    size_t nr = mat.numRows();
    size_t nc = mat.numColumns();

    ElementType *nullspaceFFPACK = 0;  //  FFPACK will allocate space and fill this in
    
    size_t nullspace_dim;
    size_t nullspace_leading_dim;
    
    FFPACK::NullSpaceBasis(mat.ring().field(),
                           (right_side ? FFLAS::FflasRight : FFLAS::FflasLeft),
                           nc, nr, N, nr, nullspaceFFPACK, nullspace_leading_dim, nullspace_dim);
    
    std::cerr << "leading dim = " << nullspace_leading_dim << " and dim = " << nullspace_dim << std::endl;

    //NOTUSED? size_t nullspace_nrows = (right_side ? nc : nullspace_dim);
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
    
    mat.copy_elems(nullspace.numRows() * nullspace.numColumns(), nullspace.get_array(), 1, nullspaceFFPACK, 1); 
    
    delete [] nullspaceFFPACK;
#endif
    return 0;
}
#endif // HAVE_FFLAS_FFPACK
///////////////////////////////////
/// Fast linear algebra routines //
///////////////////////////////////

template<typename CoeffRing>
size_t DMat<CoeffRing>::rank() const
{
  ERROR("not implemented for this ring yet");
  return static_cast<size_t>(-3);
}

///////////////////////////////////
/// Real linear algebra routines //
///////////////////////////////////

template<> 
size_t DMat<M2::ARingRRR>::rank() const
{
  return LUDecompositionRRR::rankRRR(*this);
}

template<>
void DMat<M2::ARingRRR>::determinant(ElementType &result) const
{
  LUDecompositionRRR::determinantRRR(*this, result);
}

//////////////////////////////////////
/// Generic linear algebra routines //
//////////////////////////////////////

template<typename CoeffRing>
void DMat<CoeffRing>::determinant(ElementType &result) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
M2_arrayintOrNull DMat<CoeffRing>::rankProfile(bool row_profile) const
{
  ERROR("not implemented for this ring yet");
  return 0;
}

template<typename CoeffRing>
void DMat<CoeffRing>::nullSpace(DMat<CoeffRing> &nullspace, bool right_side) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
bool DMat<CoeffRing>::solveLinear(DMat<CoeffRing> &X, const DMat<CoeffRing> &B, bool right_size) const
{
  ERROR("not implemented for this ring yet");
  return false;
}

template<typename CoeffRing>
void DMat<CoeffRing>::addMultipleTo(const DMat<CoeffRing> &A,
                                    const DMat<CoeffRing> &B,
                                    bool transposeA,
                                    bool transposeB,
                                    const ElementType& a,
                                    const ElementType& b)
{
  std::cerr << "DMat  addMultipleTo" << std::endl;
    std::cerr << "typeid: " << typeid(CoeffRing).name () << std::endl;
  ERROR("addMultipleTo not implemented for this ring yet");
}

////////////////////////////////////////////////////////////////////////////






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

template class DMat<CoefficientRingZZ_NTL>;
template class DMat<M2::ARingZZp>;
template class DMat<M2::ARingTower>;


template class DMat<CoefficientRingRRR>;
template class DMat<CoefficientRingCCC>;
template class DMat<CoefficientRingR>;

template class DMat<M2::ARingGFM2>;
template class DMat<M2::ARingZZpFFPACK>;

#ifdef HAVE_FFLAS_FFPACK
template class DMat<M2::ARingZZpFlint>;
template class DMat<M2::ARingZZ>;
#endif

template class DMat<M2::ARingGF>;
template class DMat<M2::ARingRRR>;





// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
