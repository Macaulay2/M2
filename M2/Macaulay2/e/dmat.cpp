// Copyright 2005  Michael E. Stillman

#include "exceptions.hpp"

#include "coeffrings.hpp"
#include "coeffrings-zz.hpp"
#include "ZZp.hpp"
#include "aring-gf.hpp"
#include "aring-m2-gf.hpp"
#include "aring-tower.hpp"

#include "dmat.hpp"
#include "mat.hpp"
#include "mpfr.h"
#ifdef HAVE_MPACK
#include <mpack/mblas_mpfr.h>
#include <mpack/mlapack_mpfr.h>
#endif
#include <iostream>

#include "aring-zzp.hpp"
#include "aring-ffpack.hpp"

 #include <typeinfo>




////////////////////////////////////////////////////////////////////////////
// dmat code that might have alternate implementations, depending of type //
////////////////////////////////////////////////////////////////////////////


///////////////////////////////////
/// Fast linear algebra routines //
///////////////////////////////////

template<typename CoeffRing>
size_t DMat<CoeffRing>::rank() const
{
  ERROR("not implemented for this ring yet");
  return static_cast<size_t>(-1);
}

template<typename CoeffRing>
void DMat<CoeffRing>::determinant(elem &result) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
bool DMat<CoeffRing>::invert(DMat<CoeffRing> &inverse) const
{
  ERROR("not implemented for this ring yet");
  return false;
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
engine_RawRingElementArrayOrNull DMat<CoeffRing>::characteristicPolynomial() const
{
  ERROR("not implemented for this ring yet");
  return 0;
}

template<typename CoeffRing>
engine_RawRingElementArrayOrNull DMat<CoeffRing>::minimalPolynomial() const
{
  ERROR("not implemented for this ring yet");
  return 0;
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







template <> double *DMat<CoefficientRingRRR>::make_lapack_array() const
{
  long len = n_rows() * n_cols();
  double *result = newarray_atomic(double, len);

  elem *a = array_;
  double *p = result;
  for (long i=0; i<len; i++)
    *p++ = mpfr_get_d(a++, GMP_RNDN);
  return result;
}

template <> void DMat<CoefficientRingRRR>::fill_from_lapack_array(double *lapack_array)
{
  long len = n_rows() * n_cols();

  elem *a = array_;
  double *p = lapack_array;
  for (long i=0; i<len; i++)
    mpfr_set_d(a++, *p++, GMP_RNDN);
}

template <> double *DMat<CoefficientRingCCC>::make_lapack_array() const
{
  long len = n_rows() * n_cols();
  double *result = newarray_atomic(double, 2*len);

  elem *a = array_;
  double *p = result;
  for (long i=0; i<len; i++)
    {
      *p++ = mpfr_get_d(a->re, GMP_RNDN);
      *p++ = mpfr_get_d(a->im, GMP_RNDN);
      a++;
    }
  return result;
}

template <> __mpfr_struct *DMat<CoefficientRingCCC>::make_mpack_array() const // why is this here???
{
  long len = n_rows() * n_cols();
  __mpfr_struct *result = new __mpfr_struct[2*len];

  elem *a = array_;
  __mpfr_struct *p = result;

 //std::cout<<"inside make_mpack"<<std::endl;
 for (long i=0; i<len; i++)
   {
     mpfr_init(p);
     mpfr_set(p, a->re, GMP_RNDN);
     p++;
     a++;
   }

 a =array_;
 for (long i=len; i< 2*len; i++)
   {
     mpfr_init(p);
     mpfr_set(p, a->im, GMP_RNDN);
     //*p= *(a->im);
     p++;
     a++;
   }
  return result;
}

template <> void DMat<CoefficientRingCCC>::fill_from_lapack_array(double *lapack_array)
{
  long len = n_rows() * n_cols();

  elem *a = array_;
  double *p = lapack_array;
  for (long i=0; i<len; i++)
    {
      mpfr_set_d(a->re, *p++, GMP_RNDN);
      mpfr_set_d(a->im, *p++, GMP_RNDN);
      a++;
    }
}



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
  M2_arrayint result = M2_makearrayint(v.size());
  for (size_t i = 0; i < v.size(); i++)
    result->array[i] = v[i];
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
  size_t nelems = mat->n_cols();
  if (mat->n_rows() > mat->n_cols()) nelems = mat->n_rows();

  std::vector<size_t> P(nelems, -1);
  std::vector<size_t> Qt(nelems, -1);

  size_t rk = LUdivine(mat->ring().field(),
                       FFLAS::FflasNonUnit,
                       (!transpose ? FFLAS::FflasTrans : FFLAS::FflasNoTrans),
                       mat->n_cols(),
                       mat->n_rows(),
                       mat->get_array(),
                       mat->n_rows(),
                       &P[0], 
                       &Qt[0]);

  engine_RawArrayIntPairOrNull result = new engine_RawArrayIntPair_struct;
  result->a = stdvector_to_M2_arrayint(Qt);
  result->b = stdvector_to_M2_arrayint(P);
  return result;
#endif
  return 0;
}


#include "dmat-ffpack.cpp"

template class DMat<CoefficientRingZZ_NTL>;
template class DMat<M2::ARingZZp>;
template class DMat<M2::ARingTower>;


template class DMat<CoefficientRingRRR>;
template class DMat<CoefficientRingCCC>;
template class DMat<CoefficientRingR>;

template class DMat<M2::ARingGFM2>;
template class DMat<M2::ARingZZpFFPACK>;
template class DMat<M2::ARingGF>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
