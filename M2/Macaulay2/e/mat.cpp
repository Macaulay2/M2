// Copyright 2005  Michael E. Stillman

#include "mat.hpp"

#include "coeffrings.hpp"

template class Mat<CoefficientRingZZp>;
template class MutableMat< CoefficientRingZZp, Mat<CoefficientRingZZp> >;

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
