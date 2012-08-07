// Copyright 2005  Michael E. Stillman

#include "dmat.hpp"
#include "smat.hpp"
#include "mat.hpp"

#include "coeffrings.hpp"
#include "matrix-con.hpp"
#include "matrix.hpp"

#include "lapack.hpp"
#include "dmat-LU.hpp"

#include "aring-zzp.hpp"

template<typename MatT> 
MatT * MutableMatrix::coerce()
{
  MutableMat<MatT> *P = cast_to_MutableMat<MatT>();
  if (P == 0) return 0;
  return P->get_Mat();
}

template<typename MatT> 
const MatT * MutableMatrix::coerce() const
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
  const Z_mod *KZZp = R->cast_to_Z_mod();
  if (KZZp != 0)
    {
      if (dense)
	{
	  return MutableMat< DMat<M2::ARingZZp> >
	    ::zero_matrix(KZZp,nrows,ncols);
	}
      else
	return MutableMat< SMat<M2::ARingZZp> >
	    ::zero_matrix(KZZp,nrows,ncols);
    }
  if (R == globalZZ)
    {
#ifdef DEVELOPMENT
 #warning "change to NTL mat_ZZ"
#endif
      if (dense)
	{
	  return MutableMat< DMat<CoefficientRingZZ_NTL> >
	    ::zero_matrix(globalZZ,nrows,ncols);
	}
      else
	  return MutableMat< SMat<CoefficientRingZZ_NTL> >
	    ::zero_matrix(globalZZ,nrows,ncols);
    }
  if (R->is_RRR())
    {
      if (R->cast_to_RRR()->get_precision() <= 53)
	{
	  if (dense)
	    {
	      return MutableMat< DMat<CoefficientRingRRR> >
		::zero_matrix(R->cast_to_RRR(),nrows,ncols);
	    }
	  else
	    return MutableMat< SMat<CoefficientRingRRR> >
	      ::zero_matrix(R->cast_to_RRR(),nrows,ncols);
	  
	}
      // large precision after this
      if (dense)
	{
	  return MutableMat< DMat<CoefficientRingRRR> >
	    ::zero_matrix(R->cast_to_RRR(),nrows,ncols);
	}
      else
	return MutableMat< SMat<CoefficientRingRRR> >
	  ::zero_matrix(R->cast_to_RRR(),nrows,ncols);
    }
  if (R->is_CCC())
    {
      if (R->cast_to_CCC()->get_precision() <= 53)
	{
	  if (dense)
	    {
	      return MutableMat< DMat<CoefficientRingCCC> >
		::zero_matrix(R->cast_to_CCC(),nrows,ncols);
	    }
	  else
	    return MutableMat< SMat<CoefficientRingCCC> >
	      ::zero_matrix(R->cast_to_CCC(),nrows,ncols);
	}
      // large precision after this
      if (dense)
	{
	  return MutableMat< DMat<CoefficientRingCCC> >
	    ::zero_matrix(R->cast_to_CCC(),nrows,ncols);
	}
      else
	return MutableMat< SMat<CoefficientRingCCC> >
	  ::zero_matrix(R->cast_to_CCC(),nrows,ncols);
    }
  // In this case, we just use ring elem arithmetic
  if (dense)
    return MutableMat< DMat<CoefficientRingR> >
            ::zero_matrix(R,nrows,ncols);
  else
    return MutableMat< SMat<CoefficientRingR> >
            ::zero_matrix(R,nrows,ncols);
  const GF *KGF = R->cast_to_GF();
  if (KGF != 0)
    {
    }
  ERROR("mutable matrices over this ring are not yet implemented");
  return 0;
}

MutableMatrix *MutableMatrix::identity(const Ring *R, int nrows, bool dense)
{
  MutableMatrix *result = MutableMatrix::zero_matrix(R,nrows,nrows,dense);
  ring_elem one = R->one();
  for (int i=0; i<nrows; i++)
    result->set_entry(i,i,one);
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

template<typename Mat>
MutableMat<Mat> *MutableMat<Mat>::grab_Mat(const Mat *m) {
    MutableMat *result = new MutableMat;
    Mat *copy_m = m->copy();
    result->mat.grab(copy_m);
    return result;
  }

///////////////////////////////////
//// Linear algebra routines //////
///////////////////////////////////

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

template class MutableMat< DMat<M2::ARingZZp> >;
template class MutableMat< DMat<CoefficientRingRRR> >;
template class MutableMat< DMat<CoefficientRingCCC> >;
template class MutableMat< DMat<CoefficientRingZZ_NTL> >;
template class MutableMat< DMat<CoefficientRingR> >;

template class MutableMat< SMat<M2::ARingZZp> >;
template class MutableMat< SMat<CoefficientRingRRR> >;
template class MutableMat< SMat<CoefficientRingCCC> >;
template class MutableMat< SMat<CoefficientRingZZ_NTL> >;
template class MutableMat< SMat<CoefficientRingR> >;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
