// Copyright 2005  Michael E. Stillman

#include "coeffrings.hpp"
#include "z_mod_p.hpp"
#include "dmatrix.hpp"
#include "matrixcon.hpp"

template<typename CoeffRing>
DMatrix<CoeffRing>::DMatrix(const RingType *R0, 
			    int nrows0, 
			    int ncols0)
  : DenseMutableMatrix(R0)
{
  mat = new DMat<CoeffRing>(R0,nrows0,ncols0);
}

template<typename CoeffRing>
DMatrix<CoeffRing>::DMatrix(DMat<CoeffRing> *mat0) // grabs mat0
  : DenseMutableMatrix(mat0->get_ring()),
    mat(mat0)
{
}

template<typename CoeffRing>
DMatrix<CoeffRing> *
DMatrix<CoeffRing>::zero_matrix(const RingType *R, 
					   int nrows, 
					   int ncols)
{
  DMatrix<CoeffRing> *result = new DMatrix<CoeffRing>(R,nrows,ncols);
  return result;
}

template<typename CoeffRing>
DMatrix<CoeffRing> *DMatrix<CoeffRing>::copy(bool prefer_dense) const
{
  const RingType *A = mat->get_ring();
  DMatrix<CoeffRing> *result = new DMatrix<CoeffRing>(A, 0, 0);
  result->mat->set_matrix(mat);
  return result;
}

template<typename CoeffRing>
Matrix *DMatrix<CoeffRing>::to_matrix() const
{
  int nrows = n_rows();
  int ncols = n_cols();
  FreeModule *F = R->make_FreeModule(nrows);
  MatrixConstructor result(F,ncols);
  ring_elem f;
  for (int c=0; c<ncols; c++)
    for (int r=0; r<nrows; r++)
    {
      if (get_entry(r,c,f))
	result.set_entry(r,c,f);
    }
  result.compute_column_degrees();
  return result.to_matrix();
}




template<typename CoeffRing>
int DMatrix<CoeffRing>::lead_row(int col) const { 
  return mat->lead_row(col);
}

template<typename CoeffRing>
int DMatrix<CoeffRing>::lead_row(int col, ring_elem &result) const
{
  elem b;
  int ret = mat->lead_row(col, b);
  if (ret >= 0)
    mat->get_CoeffRing()->to_ring_elem(b, result);
  return ret;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::get_entry(int r, int c, ring_elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  if (r >= 0 && r < n_rows() && c >= 0 && c < n_cols())
    {
      elem a;
      bool ret = mat->get_entry(r,c,a);
      mat->get_CoeffRing()->to_ring_elem(a,result);
      return ret;
    }
  else
    {
      result = R->zero();
      return false;
    }
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::set_entry(int r, int c, const ring_elem a)
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  elem b;
  mat->get_CoeffRing()->from_ring_elem(a,b);
  mat->set_entry(r,c,b);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::interchange_rows(int i, int j, bool do_recording)
  /* swap rows: row(i) <--> row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;
  if (i == j) return true;
  mat->interchange_rows(i,j);
  if (do_recording && rowOps != 0)
    rowOps->interchange_columns(i,j,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::interchange_columns(int i, int j, bool do_recording)
  /* swap columns: column(i) <--> column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  if (i == j) return true;
  mat->interchange_columns(i,j);
  if (do_recording && colOps != 0)
    colOps->interchange_columns(i,j,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::scale_row(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* row(i) <- r * row(i) */
{
  if (error_row_bound(i)) return false;
  elem a;
  mat->get_CoeffRing()->from_ring_elem(r,a);
  mat->scale_row(a,i);
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,opposite_mult,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::scale_column(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* column(i) <- r * column(i) */
{
  if (error_column_bound(i)) return false;
  elem a;
  mat->get_CoeffRing()->from_ring_elem(r,a);
  mat->scale_column(a,i);
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,opposite_mult,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::divide_row(int i, ring_elem r, bool do_recording)
  /* row(i) <- row(i) / r */
{
  if (error_row_bound(i)) return false;
  elem a;
  mat->get_CoeffRing()->from_ring_elem(r,a);
  mat->divide_row(i,a);
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,false,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::divide_column(int i, ring_elem r, bool do_recording)
  /* column(i) <- column(i) / r */
{
  if (error_column_bound(i)) return false;
  elem a;
  mat->get_CoeffRing()->from_ring_elem(r,a);
  mat->divide_column(i,a);
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,false,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* row(i) <- row(i) + r * row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;
  elem a;
  mat->get_CoeffRing()->from_ring_elem(r,a);
  mat->row_op(i,a,j);
  if (do_recording && rowOps != 0)
    rowOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* column(i) <- column(i) + r * column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  elem a;
  mat->get_CoeffRing()->from_ring_elem(r,a);
  mat->column_op(i,a,j);
  if (do_recording && colOps != 0)
    colOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::column2by2(int c1, int c2, 
			  ring_elem a1, ring_elem a2,
			  ring_elem b1, ring_elem b2,
			  bool opposite_mult,
			  bool doRecording)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  if (error_column_bound(c1)) return false;
  if (error_column_bound(c2)) return false;
  elem aa1, aa2, bb1, bb2;
  mat->get_CoeffRing()->from_ring_elem(a1, aa1);
  mat->get_CoeffRing()->from_ring_elem(a2, aa2);
  mat->get_CoeffRing()->from_ring_elem(b1, bb1);
  mat->get_CoeffRing()->from_ring_elem(b2, bb2);
  mat->column2by2(c1,c2,aa1,aa2,bb1,bb2);
  if (doRecording && colOps != 0)
    colOps->column2by2(c1,c2,a1,a2,b1,b2,opposite_mult,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::row2by2(int r1, int r2, 
		       ring_elem a1, ring_elem a2,
		       ring_elem b1, ring_elem b2,
		       bool opposite_mult,
		       bool doRecording)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  if (error_row_bound(r1)) return false;
  if (error_row_bound(r2)) return false;
  elem aa1, aa2, bb1, bb2;
  mat->get_CoeffRing()->from_ring_elem(a1, aa1);
  mat->get_CoeffRing()->from_ring_elem(a2, aa2);
  mat->get_CoeffRing()->from_ring_elem(b1, bb1);
  mat->get_CoeffRing()->from_ring_elem(b2, bb2);
  mat->row2by2(r1,r2,aa1,aa2,bb1,bb2);
  if (doRecording && rowOps != 0)
    rowOps->column2by2(r1,r2,a1,a2,b1,b2,opposite_mult,false);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::dot_product(int i, int j, ring_elem &result) const
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  elem a;
  mat->dot_product(i,j,a);
  mat->get_CoeffRing()->to_ring_elem(a,result);
  return true;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::row_permute(int start_row, const M2_arrayint cols)
{
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::column_permute(int start_col, const M2_arrayint cols)
{
}

template<typename CoeffRing>
DMatrix<CoeffRing> * DMatrix<CoeffRing>::submatrix(const M2_arrayint rows, const M2_arrayint cols) const
{
}

template<typename CoeffRing>
DMatrix<CoeffRing> * DMatrix<CoeffRing>::submatrix(const M2_arrayint cols) const
{
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::set_submatrix(const M2_arrayint rows, 
				       const M2_arrayint cols,
				       const MutableMatrix *N)
{
}

///////////////////////////////
// Matrix operations //////////
///////////////////////////////

template<typename CoeffRing>
bool DMatrix<CoeffRing>::is_zero() const
{
  int nrows = n_rows();
  int ncols = n_cols();
  elem f;
  for (int r=0; r<nrows; r++)
    for (int c=0; c<ncols; c++)
      if (mat->get_entry(r,c,f))
	return false;
  return true;
}
 
template<typename CoeffRing>
bool DMatrix<CoeffRing>::is_equal(const MutableMatrix *B) const
{
#warning "write this"
  return false;
}

template<typename CoeffRing>
bool DMatrix<CoeffRing>::set_values(M2_arrayint rows,
			  M2_arrayint cols,
			  RingElement_array *values)
{
#warning "write this"
  return false;
}

template<typename CoeffRing>
MutableMatrixOrNull * DMatrix<CoeffRing>::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
#warning "write this"
  return 0;
}

template<typename CoeffRing>
MutableMatrixOrNull * DMatrix<CoeffRing>::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
#warning "write this"
  return 0;
}

template<typename CoeffRing>
MutableMatrixOrNull * DMatrix<CoeffRing>::mult(const MutableMatrix *B,
				     M2_bool opposite_mult) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
#warning "write this"
  return 0;
}

template<typename CoeffRing>
MutableMatrixOrNull * DMatrix<CoeffRing>::mult(const RingElement *f,
				     M2_bool opposite_mult) const
  // return f*this.  return NULL of sizes or types do not match.
{
#warning "write this"
  return 0;
}

template<typename CoeffRing>
MutableMatrix * DMatrix<CoeffRing>::negate() const
{
#warning "write this"
  return 0;
}

template class DMatrix<CoefficientRingZZp>;
template class DMatrix<CoefficientRingRR>;
template class DMatrix<CoefficientRingCC>;

DMat<CoefficientRingRR> *DMatrix<CoefficientRingRR>::get_DMatRR() const
{
  return mat;
}
DMat<CoefficientRingCC> *DMatrix<CoefficientRingCC>::get_DMatCC() const
{
  return mat;
}
DMat<CoefficientRingZZp> *DMatrix<CoefficientRingZZp>::get_DMatZZp() const
{
  return mat;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
