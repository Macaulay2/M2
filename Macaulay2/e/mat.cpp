// Copyright 2005  Michael E. Stillman

#include "mat.hpp"
#include "dmat.hpp"

#include "coeffrings.hpp"
#include "matrixcon.hpp"
#include "matrix.hpp"

template class Mat<CoefficientRingZZp>;
template class MutableMat< CoefficientRingZZp, Mat<CoefficientRingZZp> >;
template class MutableMat< CoefficientRingZZp, DMat<CoefficientRingZZp> >;

template class MutableMat< CoefficientRingRR, DMat<CoefficientRingRR> >;
template class MutableMat< CoefficientRingCC, DMat<CoefficientRingCC> >;

MutableMatrixXXX *MutableMatrixXXX::zero_matrix(const Ring *R, 
						long nrows, 
						long ncols, 
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
	  return MutableMat<CoefficientRingZZp, DMat<CoefficientRingZZp> >
	    ::zero_matrix(KZZp,nrows,ncols);
	}
      else
	return MutableMat<CoefficientRingZZp, DMat<CoefficientRingZZp> >
	    ::zero_matrix(KZZp,nrows,ncols);
    }
  if (R == globalRR)
    {
      if (dense)
	{
	  return MutableMat<CoefficientRingRR, DMat<CoefficientRingRR> >
	    ::zero_matrix(globalRR,nrows,ncols);
	}
      else
	return MutableMat<CoefficientRingRR, DMat<CoefficientRingRR> >
	    ::zero_matrix(globalRR,nrows,ncols);
    }
  if (R == globalCC)
    {
      if (dense)
	{
	  return MutableMat<CoefficientRingCC, DMat<CoefficientRingCC> >
	    ::zero_matrix(globalCC,nrows,ncols);
	}
      else
	return MutableMat<CoefficientRingCC, DMat<CoefficientRingCC> >
	    ::zero_matrix(globalCC,nrows,ncols);
    }
  // In this case, we just use ring elem arithmetic
  return MutableMat<CoefficientRingR, DMat<CoefficientRingR> >
            ::zero_matrix(R,nrows,ncols);
  const GF *KGF = R->cast_to_GF();
  if (KGF != 0)
    {
    }
  ERROR("mutable matrices over this ring are not yet implemented");
  return 0;
}

MutableMatrixXXX *MutableMatrixXXX::identity(const Ring *R, int nrows, bool dense)
{
  MutableMatrixXXX *result = MutableMatrixXXX::zero_matrix(R,nrows,nrows,dense);
  ring_elem one = R->one();
  for (int i=0; i<nrows; i++)
    result->set_entry(i,i,one);
  return result;
}

MutableMatrixXXX *MutableMatrixXXX::from_matrix(const Matrix *m, bool prefer_dense)
{
  MutableMatrixXXX *result = zero_matrix(m->get_ring(), 
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

Matrix *MutableMatrixXXX::to_matrix() const
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

void MutableMatrixXXX::text_out(buffer &o) const
{
  const Ring *R = get_ring();
  int nrows = n_rows();
  int ncols = n_cols();
  buffer *p = newarray(buffer,nrows);
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
  deletearray(p);
}

bool MutableMatrixXXX::set_values(M2_arrayint rows,
				  M2_arrayint cols,
				  RingElement_array *values)
{
  if (rows->len != cols->len || rows->len != values->len)
    return false;
  for (long i=rows->len-1; i>=0; i--)
    {
      if (!set_entry(rows->array[i], cols->array[i], values->array[i]->get_value()))
	return false;
    }
  return true;
}

template<typename CoeffRing, typename Mat>
void MutableMat<CoeffRing,Mat>::solve(MutableMatrixXXX *x, MutableMatrixXXX *b)
  // resets x, find a basis of solutions for Ax=b
{
}

template<typename CoeffRing, typename Mat>
void MutableMat<CoeffRing,Mat>::LU(MutableMatrixXXX *L, std::vector<int, gc_allocator<int> > &perm)
{
}

void MutableMat<CoefficientRingZZp, Mat<CoefficientRingZZp> >::LU(MutableMatrixXXX *L, std::vector<int, gc_allocator<int> > &perm)
{
}

template<typename CoeffRing, typename MatType>
Mat_ZZp *MutableMat<CoeffRing,MatType>::get_mat_ZZp()
{
  return 0;
}

Mat_ZZp *MutableMat<CoefficientRingZZp,Mat<CoefficientRingZZp> >
  ::get_mat_ZZp()
{
  return &mat;
}

template<typename CoeffRing, typename MatType>
Mat_RR *MutableMat<CoeffRing,MatType>::get_mat_RR()
{
  return 0;
}

Mat_RR *MutableMat<CoefficientRingRR,Mat<CoefficientRingRR> >
  ::get_mat_RR()
{
  return &mat;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
