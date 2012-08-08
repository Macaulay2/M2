// Copyright 2005  Michael E. Stillman

#include "dmat.hpp"
#include "smat.hpp"
#include "mat.hpp"
#include "mutablemat.hpp"

#include "coeffrings.hpp"
#include "coeffrings-zz.hpp"

#include "matrix-con.hpp"
#include "matrix.hpp"

#include "aring-zzp.hpp"
#include "aring-ffpack.hpp"
#include "aring-m2-gf.hpp"
#include "aring-gf.hpp"
#include "aring-glue.hpp"
#include "aring-tower.hpp"

#include "lapack.hpp"
#include "dmat-LU.hpp"

#if 0
// Considering this kind of code
template <typename MatType>
inline MatType* coerceMatrix(MutableMatrix* A)
{
  MutableMat< MatType >* B = dynamic_cast< MutableMat< MatType > >(A);
  if (B == 0) return 0;
  return B->get_Mat();
}

template <typename MatType>
inline const MatType* coerceMatrix(const MutableMatrix* A)
{
  const MutableMat< MatType >* B = dynamic_cast< const MutableMat< MatType > >(A);
  if (B == 0) return 0;
  return B->get_Mat();
}



 MatType* coerceMatrix< MatType >(MutableMatrix* A);
  MatType* coerceMatrixWithError< MatType >(MutableMatrix* A, std::string msg);

#define COERCE_TO_MatT_or_ERROR(newName,MatT,A,ERRORMSG,retval) { \
  MutableMat< MatT > *P = dynamic_cast< MutableMat< MatT >* >(A); \
  if (P == 0) { \
    ERROR(ERRORMSG); \
    return retval; \
  } \
  MatT* newName = P->get_Mat(); \
  } 
#endif

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

MutableMatrix *MutableMatrix::zero_matrix(const Ring *R, 
						int nrows, 
						int ncols, 
						bool dense)
{
  if (nrows < 0 | ncols < 0)
    {
      ERROR("expected non-negative number of rows or columns");
      return 0;
    }
  MutableMatrix *result = R->makeMutableMatrix(nrows, ncols, dense);
  if (result != 0) return result;
  const Z_mod *KZZp = R->cast_to_Z_mod();
  if (KZZp != 0)
    {
      if (dense)
        return MutableMat< DMat<M2::ARingZZp> >
          ::zero_matrix(R,KZZp->get_ARing(), nrows,ncols);
      else
        return MutableMat< SMat<M2::ARingZZp> >
          ::zero_matrix(R,KZZp->get_ARing(),nrows,ncols);
    }
  if (R->ringID() == M2::ring_FFPACK)
    {
      std::cerr << "Should not get here" << std::endl;
      const M2::ConcreteRing<M2::ARingZZpFFPACK> *ffpackRing = 
        dynamic_cast< const M2::ConcreteRing<M2::ARingZZpFFPACK> * >(R);
      ASSERT(ffpackRing != 0);
      if (dense)
        return MutableMat< DMat<M2::ARingZZpFFPACK> >
          ::zero_matrix(R,&ffpackRing->ring(),nrows,ncols);
      else
        return MutableMat< SMat<M2::ARingZZpFFPACK> >
          ::zero_matrix(R,&ffpackRing->ring(),nrows,ncols);
    }
#if 0
  if (R->ringID() == M2::ring_GFM2)
    {
      const M2::ConcreteRing<M2::ARingGFM2> *AGF = dynamic_cast<const M2::ConcreteRing<M2::ARingGFM2> *>(R);
      ASSERT(AGF != 0);
      if (dense)
	{
	  return MutableMat< DMat<M2::ARingGFM2> >
	    ::zero_matrix(R,&AGF->ring(),nrows,ncols);
	}
      else
	return MutableMat< SMat<M2::ARingGFM2> >
          ::zero_matrix(R,&AGF->ring(),nrows,ncols);
    }
#endif
  if (R == globalZZ)
    {
      if (dense)
	{
	  return MutableMat< DMat<CoefficientRingZZ_NTL> >
	    ::zero_matrix(globalZZ,globalZZ->get_ARing(),nrows,ncols);
	}
      else
	  return MutableMat< SMat<CoefficientRingZZ_NTL> >
	    ::zero_matrix(globalZZ,globalZZ->get_ARing(),nrows,ncols);
    }
  if (R->is_RRR())
    {
      const RRR * ARRR = R->cast_to_RRR();
      ASSERT(ARRR != 0);
      if (ARRR->get_precision() <= 53)
	{
	  if (dense)
	    {
	      return MutableMat< DMat<CoefficientRingRRR> >
		::zero_matrix(R,ARRR->get_ARing(),nrows,ncols);
	    }
	  else
	    return MutableMat< SMat<CoefficientRingRRR> >
	      ::zero_matrix(R, ARRR->get_ARing(),nrows,ncols);
	  
	}
      // large precision after this
      if (dense)
	{
	  return MutableMat< DMat<CoefficientRingRRR> >
	    ::zero_matrix(R, ARRR->get_ARing(),nrows,ncols);
	}
      else
	return MutableMat< SMat<CoefficientRingRRR> >
	  ::zero_matrix(R, ARRR->get_ARing(),nrows,ncols);
}
  if (R->is_CCC())
    {
      const CCC *ACCC = R->cast_to_CCC();
      ASSERT(ACCC != 0);
      if (ACCC->get_precision() <= 53)
	{
	  if (dense)
	    {
	      return MutableMat< DMat<CoefficientRingCCC> >
		::zero_matrix(R, ACCC->get_ARing(),nrows,ncols);
	    }
	  else
	    return MutableMat< SMat<CoefficientRingCCC> >
	      ::zero_matrix(R, ACCC->get_ARing(),nrows,ncols);
	}
      // large precision after this
      if (dense)
	{
	  return MutableMat< DMat<CoefficientRingCCC> >
	    ::zero_matrix(R, ACCC->get_ARing(),nrows,ncols);
	}
      else
	return MutableMat< SMat<CoefficientRingCCC> >
	  ::zero_matrix(R, ACCC->get_ARing(),nrows,ncols);
    }
  // In this case, we just use ring elem arithmetic
  const CoefficientRingR *cR = new CoefficientRingR(R);
  if (dense)
    return MutableMat< DMat<CoefficientRingR> >
      ::zero_matrix(R,cR,nrows,ncols);
  else
    return MutableMat< SMat<CoefficientRingR> >
      ::zero_matrix(R,cR,nrows,ncols);
  ERROR("mutable matrices over this ring are not yet implemented");
  return 0;
}

MutableMatrix *MutableMatrix::identity(const Ring *R, int nrows, bool dense)
{
  MutableMatrix *result = MutableMatrix::zero_matrix(R,nrows,nrows,dense);
  for (int i=0; i<nrows; i++)
    result->set_entry(i,i,R->from_int(1));
  return result;
}

MutableMatrix *MutableMatrix::from_matrix(const Matrix *m, bool prefer_dense)
{
  MutableMatrix *result = zero_matrix(m->get_ring(),
                                         m->n_rows(),
                                         m->n_cols(),
                                         prefer_dense);
  Matrix::iterator i(m);
  for (int c=0; c<m->n_cols(); c++)
    {
      for (i.set(c); i.valid(); i.next())
        result->set_entry(i.row(), c, i.entry());
    }
  return result;
}

Matrix *MutableMatrix::to_matrix() const
{
  int nrows = n_rows();
  int ncols = n_cols();
  FreeModule *F = get_ring()->make_FreeModule(nrows);
  MatrixConstructor result(F,ncols);
  ring_elem f;
  iterator *i = begin();
  for (int c=0; c<ncols; c++)
    {
      ring_elem a;
      for (i->set(c); i->valid(); i->next())
        {
          i->copy_ring_elem(a);
          result.set_entry(i->row(), c, a);
        }
    }
  delete i;
  result.compute_column_degrees();
  return result.to_matrix();
}

void MutableMatrix::text_out(buffer &o) const
{
  const Ring *R = get_ring();
  int nrows = n_rows();
  int ncols = n_cols();
  buffer *p = new buffer[nrows];
  int r;
  for (int c=0; c<ncols; c++)
    {
      int maxcount = 0;
      for (r=0; r<nrows; r++)
        {
          ring_elem f;
          get_entry(r,c,f);
          if (!R->is_zero(f))
            R->elem_text_out(p[r], f);
          else
            p[r] << ".";
          if (p[r].size() > maxcount)
            maxcount = p[r].size();
        }
      for (r=0; r<nrows; r++)
        for (int k=maxcount+1-p[r].size(); k > 0; k--)
          p[r] << ' ';
    }
  for (r=0; r<nrows; r++)
    {
      p[r] << '\0';
      char *s = p[r].str();
      o << s << newline;
    }
  delete[] p;
}

bool MutableMatrix::set_values(M2_arrayint rows,
                                  M2_arrayint cols,
                                  engine_RawRingElementArray values)
{
  if (rows->len != cols->len || rows->len != values->len)
    return false;
  for (int i=rows->len-1; i>=0; i--)
    {
      if (!set_entry(rows->array[i], cols->array[i], values->array[i]->get_value()))
        return false;
    }
  return true;
}

#if 0
template<typename Mat>
MutableMat<Mat> *MutableMat<Mat>::grab_Mat(const Mat *m) {
    MutableMat *result = new MutableMat;
    Mat *copy_m = m->copy();
    result->mat.grab(copy_m);
    return result;
  }
#endif

///////////////////////////////////
//// Linear algebra routines //////
///////////////////////////////////

template<typename Mat>
engine_RawArrayIntPairOrNull MutableMat<Mat>::
  LQUPFactorizationInPlace(bool transpose)
{
  throw exc::engine_error("LU decomposition currently not implemented for this ring and matrix type");
}


template<typename Mat>
bool MutableMat<Mat>::solve(const MutableMatrix *b, MutableMatrix *x) const
  // resets x, find a solution of Ax=b, return false if no such exists.
{
  ERROR("solving linear equations is not implemented for this ring and matrix type");
  return false;
}

template<typename Mat>
bool MutableMat<Mat>::nullspaceU(MutableMatrix *x) const
{
  ERROR("finding the null-space is not implemented for this ring and matrix type");
  return false;
}

template<typename Mat>
M2_arrayintOrNull MutableMat<Mat>::LU(MutableMatrix *L,
				      MutableMatrix *U) const
{
  ERROR("LU decomposition currently not implemented for this ring and matrix type");
  return false;
}

template<typename Mat>
bool MutableMat<Mat>::eigenvalues(MutableMatrix *eigenvals, bool is_symm_or_hermitian) const
{
  ERROR("eigenvalues requires dense mutable matrices over RR or CC");
  return false;
}

template<typename Mat>
bool MutableMat<Mat>::SVD(MutableMatrix *Sigma,
                                    MutableMatrix *U,
                                    MutableMatrix *Vt,
                                    bool use_divide_and_conquer) const
{
  ERROR("SVD requires dense mutable matrices over RR or CC");
  return false;
}

template<typename Mat>
bool MutableMat<Mat>::eigenvectors(MutableMatrix *eigenvals,
                                             MutableMatrix *eigenvecs,
                                             bool is_symm_or_hermitian) const
{
  ERROR("eigenvectors requires dense mutable matrices over RR or CC");
  return false;
}

template<typename Mat>
bool MutableMat<Mat>::least_squares(const MutableMatrix *b,
                                              MutableMatrix *x,
                                              bool assume_full_rank) const
{
  ERROR("least squares requires dense mutable matrices over RR or CC");
  return false;
}

template <> M2_arrayintOrNull MutableMat< DMat<M2::ARingZZp> >::LU(MutableMatrix *L,
                                                              MutableMatrix *U) const
{
  MatType *L2 = L->coerce<MatType>();
  MatType *U2 = U->coerce<MatType>();
  if (L2 == 0 || U2 == 0)
    {
      ERROR("requires dense mutable matrices over ZZ/p");
      return false;
    }
  const MatType *A2 = get_Mat();
  return DMatLU<M2::ARingZZp>::LU(A2,L2,U2);
}

template <> M2_arrayintOrNull MutableMat< DMat<CoefficientRingRRR> >::LU(MutableMatrix *L,
                                                             MutableMatrix *U) const
{
  MatType *L2 = L->coerce<MatType>();
  MatType *U2 = U->coerce<MatType>();
  if (L2 == 0 || U2 == 0)
    {
      ERROR("requires dense mutable matrices over RR");
      return false;
    }
  const MatType *A2 = get_Mat();
  return Lapack::LU(A2,L2,U2);
}

template <> M2_arrayintOrNull MutableMat< DMat<CoefficientRingCCC> >::LU(MutableMatrix *L,
									 MutableMatrix *U) const
{
  MatType *L2 = L->coerce<MatType>();
  MatType *U2 = U->coerce<MatType>();
  if (L2 == 0 || U2 == 0)
    {
      ERROR("requires dense mutable matrices over CC");
      return false;
    }
  const MatType *A2 = get_Mat();
  return Lapack::LU(A2,L2,U2);
}

template <> bool MutableMat< DMat<CoefficientRingRRR> >::solve(const MutableMatrix *b, MutableMatrix *x) const
  // resets x, find a basis of solutions for Ax=b
{
  const MatType *b2 = b->coerce<MatType>();
  MatType *x2 = x->coerce<MatType>();
  if (b2 == 0 || x2 == 0)
    {
      ERROR("requires dense mutable matrices over RR");
      return false;
    }
  const MatType *A2 = get_Mat();
  return Lapack::solve(A2,b2,x2);
}

template <> bool MutableMat< DMat<CoefficientRingCCC> >::solve(const MutableMatrix *b, MutableMatrix *x) const
  // resets x, find a basis of solutions for Ax=b
{
  const MatType *b2 = b->coerce<MatType>();
  MatType *x2 = x->coerce<MatType>();
  if (b2 == 0 || x2 == 0)
    {
      ERROR("requires dense mutable matrices over CC");
      return false;
    }
  const MatType *A2 = get_Mat();
  return Lapack::solve(A2,b2,x2);
}

template <> bool MutableMat< DMat<M2::ARingZZp> >::solve(const MutableMatrix *b, MutableMatrix *x) const
  // resets x, find a basis of solutions for Ax=b
{
  const MatType *b2 = b->coerce<MatType>();
  MatType *x2 = x->coerce<MatType>();
  if (b2 == 0 || x2 == 0)
    {
      ERROR("requires dense mutable matrices over ZZ/p");
      return false;
    }
  const MatType *A2 = get_Mat();
  return DMatLU<M2::ARingZZp>::solve(A2,b2,x2);
}

template <> bool MutableMat< DMat<M2::ARingZZp> >::nullspaceU(MutableMatrix *x) const
{
  MatType *x2 = x->coerce<MatType>();
  if (x2 == 0)
    {
      ERROR("requires dense mutable matrices over ZZ/p");
      return false;
    }
  const MatType *A2 = get_Mat();
  DMatLU<M2::ARingZZp>::nullspaceU(A2,x2);
  return true;
}

template <> bool MutableMat< DMat<CoefficientRingRRR> >::eigenvalues(MutableMatrix *eigenvals, bool is_symm_or_hermitian) const
{
  const MatType *A2 = get_Mat();
  // First check that the matrix 'eigenvals' is correct type
  if (is_symm_or_hermitian)
    {
      MatType *eig = eigenvals->coerce<MatType>();
      if (eig == 0)
        {
          ERROR("requires a dense mutable matrix over RR");
          return false;
        }
      return Lapack::eigenvalues_symmetric(A2, eig);
    }
  else
    {
      DMat<CoefficientRingCCC> *eig = eigenvals->coerce< DMat<CoefficientRingCCC> >();
      if (eig == 0)
        {
          ERROR("requires a dense mutable matrix over CC");
          return false;
        }
      return Lapack::eigenvalues(A2, eig);
    }
}

template <> bool MutableMat< DMat<CoefficientRingCCC> >::eigenvalues(MutableMatrix *eigenvals, bool is_symm_or_hermitian) const
{
  const MatType *A2 = get_Mat();
  // First check that the matrix 'eigenvals' is correct type
  if (is_symm_or_hermitian)
    {
      DMat<CoefficientRingRRR> *eig = eigenvals->coerce< DMat<CoefficientRingRRR> >();
      if (eig == 0)
        {
          ERROR("requires a dense mutable matrix over RR");
          return false;
        }
      return Lapack::eigenvalues_hermitian(A2, eig);
    }
  else
    {
      MatType *eig = eigenvals->coerce<MatType>();
      if (eig == 0)
        {
          ERROR("requires a dense mutable matrix over CC");
          return false;
        }
      return Lapack::eigenvalues(A2, eig);
    }
}

template <> bool MutableMat< DMat<CoefficientRingRRR> >::eigenvectors(MutableMatrix *eigenvals,
                                                        MutableMatrix *eigenvecs,
                                                        bool is_symm_or_hermitian) const
{
  const MatType *A2 = get_Mat();
  if (is_symm_or_hermitian)
    {
      MatType *eig = eigenvals->coerce<MatType>();
      MatType *eigvecs = eigenvecs->coerce<MatType>();
      if (eig == 0 || eigvecs == 0)
      {
        ERROR("requires a dense mutable matrix over RR");
        return false;
      }
      return Lapack::eigenvectors_symmetric(A2, eig, eigvecs);
    }
  else
    {
      DMat<CoefficientRingCCC> *eig = eigenvals->coerce< DMat<CoefficientRingCCC> >();
      DMat<CoefficientRingCCC> *eigvecs = eigenvecs->coerce< DMat<CoefficientRingCCC> >();
      if (eig == 0 || eigvecs == 0)
	{
	  ERROR("requires a dense mutable matrix over CC");
	  return false;
	}
return Lapack::eigenvectors(A2, eig, eigvecs);
    }
}

template <> bool MutableMat< DMat<CoefficientRingCCC> >::eigenvectors(MutableMatrix *eigenvals,
                                                        MutableMatrix *eigenvecs,
                                                        bool is_symm_or_hermitian) const
{
  const MatType *A2 = get_Mat();
  if (is_symm_or_hermitian)
    {
      DMat<CoefficientRingRRR> *eig = eigenvals->coerce< DMat<CoefficientRingRRR> >();
      DMat<CoefficientRingCCC> *eigvecs = eigenvecs->coerce< DMat<CoefficientRingCCC> >();
      if (eig == 0)
	{
	  ERROR("requires a dense mutable matrix over RR");
	  return false;
	}
      if (eigvecs == 0)
	{
	  ERROR("requires a dense mutable matrix over CC");
	  return false;
	}

      return Lapack::eigenvectors_hermitian(A2, eig, eigvecs);
    }
  else
    {
      MatType *eig = eigenvals->coerce<MatType>();
      MatType *eigvecs = eigenvecs->coerce<MatType>();
      if (eig == 0)
	{
	  ERROR("requires a dense mutable matrix over CC");
	  return false;
	}
      return Lapack::eigenvectors(A2, eig, eigvecs);
    }
}

template <> bool MutableMat< DMat<CoefficientRingRRR> >::SVD(MutableMatrix *Sigma,
                                    MutableMatrix *U,
                                    MutableMatrix *VT,
                                    bool use_divide_and_conquer) const
{
  const MatType *A2 = get_Mat();
  MatType *Sigma2 = Sigma->coerce< DMat<CoefficientRingRRR> >();
  MatType *U2 = U->coerce< DMat<CoefficientRingRRR> >();
  MatType *VT2 = VT->coerce< DMat<CoefficientRingRRR> >();
  if (Sigma2 == 0 || U2 == 0 || VT2 == 0)
    {
      ERROR("requires dense mutable matrices over RR");
      return false;
    }

  if (use_divide_and_conquer)
    {
      return Lapack::SVD_divide_conquer(A2,Sigma2,U2,VT2);
    }
  else
    {
      return Lapack::SVD(A2,Sigma2,U2,VT2);
    }
}

template <> bool MutableMat< DMat<CoefficientRingCCC> >::SVD(MutableMatrix *Sigma,
                                    MutableMatrix *U,
                                    MutableMatrix *VT,
                                    bool use_divide_and_conquer) const
{
  const MatType *A2 = get_Mat();
  DMat<CoefficientRingRRR> *Sigma2 = Sigma->coerce< DMat<CoefficientRingRRR> >();
  MatType *U2 = U->coerce< MatType >();
  MatType *VT2 = VT->coerce< MatType >();
  if (Sigma2 == 0)
    {
      ERROR("requires dense mutable matrix over RR");
      return false;
    }
  if (U2 == 0 || VT2 == 0)
    {
      ERROR("requires dense mutable matrices over CC");
      return false;
    }

  if (use_divide_and_conquer)
    {
      return Lapack::SVD_divide_conquer(A2,Sigma2,U2,VT2);
    }
  else
    {
      return Lapack::SVD(A2,Sigma2,U2,VT2);
    }
}

template <> bool MutableMat< DMat<CoefficientRingRRR> >::least_squares(const MutableMatrix *b,
                                                         MutableMatrix *x,
                                                         bool assume_full_rank) const
{
  const MatType *A2 = get_Mat();
  const MatType *b2 = b->coerce<MatType>();
  MatType *x2 = x->coerce<MatType>();
  if (b2 == 0 || x2 == 0)
    {
      ERROR("requires dense mutable matrices over RR");
      return false;
    }

  if (assume_full_rank)
    {
      return Lapack::least_squares(A2,b2,x2);
    }
  else
    {
      return Lapack::least_squares_deficient(A2,b2,x2);
    }
}

template <> bool MutableMat< DMat<CoefficientRingCCC> >::least_squares(const MutableMatrix *b,
                                                         MutableMatrix *x,
                                                         bool assume_full_rank) const
{
  const MatType *A2 = get_Mat();
  const MatType *b2 = b->coerce<MatType>();
  MatType *x2 = x->coerce<MatType>();
  if (b2 == 0 || x2 == 0)
    {
      ERROR("requires dense mutable matrices over CC");
      return false;
    }

  if (assume_full_rank)
    {
      return Lapack::least_squares(A2,b2,x2);
    }
  else
    {
      return Lapack::least_squares_deficient(A2,b2,x2);
    }
}

template<class RingType>
MutableMatrix* M2::makeMutableZeroMatrix(const Ring* Rgeneral,
                                         const RingType* R,
                                         size_t nrows,
                                         size_t ncols,
                                         bool dense)
{
  if (dense)
    return MutableMat< DMat<RingType> >
      ::zero_matrix(Rgeneral,R,nrows,ncols);

  return MutableMat< SMat<RingType> >
    ::zero_matrix(Rgeneral,R,nrows,ncols);
}

#if 0
//TODO: Mike, Jakob.  Once we have sparse linbox matrices implemented, this
// might be how we create them.
template<>
MutableMatrix* M2::makeMutableZeroMatrix<M2::ARingZZpFFPACK>(const Ring* Rgeneral,
                                         const M2::ARingZZpFFPACK* R,
                                         size_t nrows,
                                         size_t ncols,
                                         bool dense)
{
  if (dense)
    return MutableMat< DMat<M2::ARingZZpFFPACK> >
      ::zero_matrix(Rgeneral,R,nrows,ncols);

  return MutableMat< SparseLinboxMat<M2::ARingZZpFFPACK> >
    ::zero_matrix(Rgeneral,R,nrows,ncols);
}
#endif

/////////////////////////////////////////
/// Fast Linear Algebra Routines ////////
/////////////////////////////////////////

template <typename T>
size_t MutableMat<T>::rank() const 
{
  return mat.rank();
}

template <typename T>
const RingElement* MutableMat<T>::determinant() const 
{
  ring_elem det;
  elem a;
  mat.determinant(a);
  mat.get_CoeffRing()->to_ring_elem(det, a);
  return RingElement::make_raw(mat.get_ring(), det);
}

template <typename T>
MutableMatrix* MutableMat<T>::invert() const
{
  MutableMat<T>*  result = makeZeroMatrix(n_rows(), n_cols());
  bool val = mat.invert(result->mat);
  if (!val)
    {
      delete result;
      return 0;
    }
  return result;
}

template <typename T>
M2_arrayintOrNull MutableMat<T>::rankProfile(bool row_profile) const
{
  return mat.rankProfile(row_profile);
}
  
template <typename T>
MutableMatrix* MutableMat<T>::nullSpace(bool right_side) const
{
  MutableMat<T>* ker = makeZeroMatrix(0,0);
  mat.nullSpace(ker->mat, right_side);
  return ker;
}

template <typename T>
std::pair<bool, MutableMatrix*> MutableMat<T>::solveLinear(const MutableMatrix* B, 
                                                           bool right_side) const 
{ 
  const MutableMat<T>* B1 = B->cast_to_MutableMat<T>();
  MutableMat<T>* solns = makeZeroMatrix(0,0);
  bool retval = mat.solveLinear(solns->mat, B1->mat, right_side);
  return std::pair<bool, MutableMatrix*>(retval, solns);
}

template <typename T>
void MutableMat<T>::addMultipleTo(const MutableMatrix* A,
                                            const MutableMatrix* B,
                                            bool transposeA,
                                            bool transposeB,
                                            const RingElement* a,
                                            const RingElement* b)
{
    std::cerr << "somewhere addMultipleTo" << std::endl;

  const MutableMat<T>* A1 = A->cast_to_MutableMat<T>();
  const MutableMat<T>* B1 = B->cast_to_MutableMat<T>();

  typename T::ElementType fa, fb;
  mat.get_CoeffRing()->from_ring_elem(fa, a->get_value());
  mat.get_CoeffRing()->from_ring_elem(fb, b->get_value());

  mat.addMultipleTo(A1->mat, B1->mat, transposeA, transposeB, fa, fb);
  
  return ;
}


/////////////////////////////////////////
#if 0
MutableMatrix *M2::ARingZZpFFPACK::makeMutableMatrix(const Ring* R, size_t nrows, size_t ncols, bool dense) const
{
  if (dense)
    return MutableMat< DMat<M2::ARingZZpFFPACK> >
      ::zero_matrix(R,this,nrows,ncols);

  return MutableMat< SMat<M2::ARingZZpFFPACK> >
    ::zero_matrix(R,this,nrows,ncols);
}

MutableMatrix *M2::ARingZZp::makeMutableMatrix(const Ring* R, size_t nrows, size_t ncols, bool dense) const
{
  if (dense)
    return MutableMat< DMat<M2::ARingZZp> >
      ::zero_matrix(R,this,nrows,ncols);

  return MutableMat< SMat<M2::ARingZZp> >
    ::zero_matrix(R,this,nrows,ncols);
}

MutableMatrix *M2::ARingGF::makeMutableMatrix(const Ring* R, size_t nrows, size_t ncols, bool dense) const
{
  if (dense)
    return MutableMat< DMat<M2::ARingGF> >
      ::zero_matrix(R,this,nrows,ncols);

  return MutableMat< SMat<M2::ARingGF> >
    ::zero_matrix(R,this,nrows,ncols);
}

MutableMatrix *M2::ARingGFM2::makeMutableMatrix(const Ring* R, size_t nrows, size_t ncols, bool dense) const
{
  if (dense)
    return MutableMat< DMat<M2::ARingGFM2> >
      ::zero_matrix(R,this,nrows,ncols);

  return MutableMat< SMat<M2::ARingGFM2> >
    ::zero_matrix(R,this,nrows,ncols);
}
#endif

template MutableMatrix* M2::makeMutableZeroMatrix<M2::ARingZZp>(const Ring* Rgeneral,
                                                 const M2::ARingZZp* R,
                                                 size_t nrows,
                                                 size_t ncols,
                                                 bool dense);
template MutableMatrix* M2::makeMutableZeroMatrix<M2::ARingTower>(const Ring* Rgeneral,
                                                 const M2::ARingTower* R,
                                                 size_t nrows,
                                                 size_t ncols,
                                                 bool dense);
template MutableMatrix* M2::makeMutableZeroMatrix<M2::ARingZZpFFPACK>(const Ring* Rgeneral,
                                                 const M2::ARingZZpFFPACK* R,
                                                 size_t nrows,
                                                 size_t ncols,
                                                 bool dense);
template MutableMatrix* M2::makeMutableZeroMatrix<M2::ARingGF>(const Ring* Rgeneral,
                                                 const M2::ARingGF* R,
                                                 size_t nrows,
                                                 size_t ncols,
                                                 bool dense);
template MutableMatrix* M2::makeMutableZeroMatrix<M2::ARingGFM2>(const Ring* Rgeneral,
                                                 const M2::ARingGFM2* R,
                                                 size_t nrows,
                                                 size_t ncols,
                                                 bool dense);

template class MutableMat< DMat<M2::ARingZZp> >;
template class MutableMat< DMat<CoefficientRingRRR> >;
template class MutableMat< DMat<CoefficientRingCCC> >;
template class MutableMat< DMat<CoefficientRingZZ_NTL> >;
template class MutableMat< DMat<CoefficientRingR> >;
template class MutableMat< DMat<M2::ARingTower> >;

template class MutableMat< SMat<M2::ARingZZp> >;
template class MutableMat< SMat<CoefficientRingRRR> >;
template class MutableMat< SMat<CoefficientRingCCC> >;
template class MutableMat< SMat<CoefficientRingZZ_NTL> >;
template class MutableMat< SMat<CoefficientRingR> >;
template class MutableMat< SMat<M2::ARingTower> >;

template class MutableMat< DMat<M2::ARingZZpFFPACK> >;
template class MutableMat< SMat<M2::ARingZZpFFPACK> >;

template class MutableMat< DMat<M2::ARingGF> >;
template class MutableMat< SMat<M2::ARingGF> >;

template class MutableMat< DMat<M2::ARingGFM2> >;
template class MutableMat< SMat<M2::ARingGFM2> >;


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
