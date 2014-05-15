// Copyright 2005  Michael E. Stillman

#include "util.hpp"
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
  // In this case, we just use ring elem arithmetic
  const CoefficientRingR *cR = R->getCoefficientRingR();
  if (dense)
    return new MutableMat< DMat<CoefficientRingR> >(R, cR, nrows, ncols);
  else
    return new MutableMat< SMat<CoefficientRingR> >(R, cR, nrows, ncols);
}

MutableMatrix *MutableMatrix::identity(const Ring *R, size_t nrows, bool dense)
{
  MutableMatrix *result = MutableMatrix::zero_matrix(R,nrows,nrows,dense);
  for (size_t i=0; i<nrows; i++)
    result->set_entry(i,i,R->from_long(1));
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
