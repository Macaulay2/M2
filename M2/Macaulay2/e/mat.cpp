// Copyright 2005  Michael E. Stillman

#include "dmat.hpp"
#include "smat.hpp"
#include "mat.hpp"
#include "mutablemat.hpp"

#include "coeffrings.hpp"

#include "matrix-con.hpp"
#include "matrix.hpp"

#include "aring-RRR.hpp"
#include "aring-RR.hpp"
#include "aring-CCC.hpp"
#include "aring-zz-gmp.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp.hpp"
#include "aring-zzp-ffpack.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-m2-gf.hpp"
#include "aring-gf-givaro.hpp"
#include "aring-glue.hpp"
#include "aring-tower.hpp"
#include "aring-qq.hpp"

#include "QQ.hpp"

#include "lapack.hpp"
#include "dmat-LU.hpp"

MutableMatrix *MutableMatrix::zero_matrix(const Ring *R, 
						size_t nrows, 
						size_t ncols, 
						bool dense)
{
  if (nrows < 0 | ncols < 0)
    {
      ERROR("expected non-negative number of rows or columns");
      return 0;
    }
  MutableMatrix *result = R->makeMutableMatrix(nrows, ncols, dense);
  if (result != 0) return result;
  //  std::cout << "MutableMatrix::zero_matrix: R->makeMutableMatrix returned null" << std::endl;
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
  if (R->ringID() == M2::ring_ZZpFfpack)
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
  if (R->ringID() == M2::ring_GF)
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
	  return MutableMat< DMat<M2::ARingZZGMP> >
	    ::zero_matrix(globalZZ,globalZZ->get_ARing(),nrows,ncols);
	}
      else
	  return MutableMat< SMat<M2::ARingZZGMP> >
	    ::zero_matrix(globalZZ,globalZZ->get_ARing(),nrows,ncols);
    }
#if defined(HAVE_FLINT)
  if (R == globalQQ)
    {
      if (dense)
	{
	  return MutableMat< DMat<M2::ARingQQ> >
	    ::zero_matrix(globalQQ,globalQQ->get_ARing(),nrows,ncols);
	}
      else
	  return MutableMat< SMat<M2::ARingQQ> >
	    ::zero_matrix(globalQQ,globalQQ->get_ARing(),nrows,ncols);
    }
#endif
  if (R->is_RRR())
    {
      const RRR * ARRR = R->cast_to_RRR();
      ASSERT(ARRR != 0);
      if (ARRR->get_precision() <= 53)
	{
	  if (dense)
	    {
	      return MutableMat< DMat<Ring_RRR> >
		::zero_matrix(R,ARRR->get_ARing(),nrows,ncols);
	    }
	  else
	    return MutableMat< SMat<Ring_RRR> >
	      ::zero_matrix(R, ARRR->get_ARing(),nrows,ncols);
	  
	}
      // large precision after this
      if (dense)
	{
	  return MutableMat< DMat<Ring_RRR> >
	    ::zero_matrix(R, ARRR->get_ARing(),nrows,ncols);
	}
      else
	return MutableMat< SMat<Ring_RRR> >
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
  const CoefficientRingR *cR = R->getCoefficientRingR();
  if (dense)
    return MutableMat< DMat<CoefficientRingR> >
      ::zero_matrix(R,cR,nrows,ncols);
  else
    return MutableMat< SMat<CoefficientRingR> >
      ::zero_matrix(R,cR,nrows,ncols);
  ERROR("mutable matrices over this ring are not yet implemented");
  return 0;
}

MutableMatrix *MutableMatrix::identity(const Ring *R, size_t nrows, bool dense)
{
  MutableMatrix *result = MutableMatrix::zero_matrix(R,nrows,nrows,dense);
  for (size_t i=0; i<nrows; i++)
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
  for (unsigned int c=0; c<m->n_cols(); c++)
    {
      for (i.set(c); i.valid(); i.next())
        result->set_entry(i.row(), c, i.entry());
    }
  return result;
}

void MutableMatrix::text_out(buffer &o) const
{
  const Ring *R = get_ring();
  size_t nrows = n_rows();
  size_t ncols = n_cols();
  buffer *p = new buffer[nrows];
  size_t r;
  for (size_t c=0; c<ncols; c++)
    {
      size_t maxcount = 0;
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
        for (size_t k=maxcount+1-p[r].size(); k > 0; k--)
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
  for (size_t i=0; i<rows->len; i++)
    {
      if (!set_entry(rows->array[i], cols->array[i], values->array[i]->get_value()))
        return false;
    }
  return true;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
