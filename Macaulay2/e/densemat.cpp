// Copyright 2004  Michael E. Stillman

#include "densemat.hpp"
#include "matrixcon.hpp"

DenseMutableMatrixRing::DenseMutableMatrixRing(const Ring *R0, int nrows0, int ncols0)
  : DenseMutableMatrix(R0)
{
  initialize(nrows0, ncols0, 0);
}

void DenseMutableMatrixRing::initialize(int nrows0, int ncols0, ring_elem *array)
{
  nrows = nrows0;
  ncols = ncols0;
  int len = nrows0 * ncols0;
  array_ = newarray(ring_elem,len);
  ring_elem zero = R->zero();
  if (array == 0)
    {
      for (int i=0; i<len; i++)
	array_[i] = zero;
    }
  else
    {
      for (int i=0; i<len; i++)
	array_[i] = array[i];
    }
}

Matrix *DenseMutableMatrixRing::to_matrix() const
{
  FreeModule *F = R->make_FreeModule(nrows);
  MatrixConstructor mat(F,ncols);
  ring_elem * loc = array_;
  for (int c=0; c<ncols; c++)
    for (int r=0; r<nrows; r++)
    {
      if (!R->is_zero(*loc))
	mat.set_entry(r,c,*loc);
      loc++;
    }
  mat.compute_column_degrees();
  return mat.to_matrix();
}

MutableMatrix *DenseMutableMatrixRing::copy(bool prefer_dense) const
{
}

bool DenseMutableMatrixRing::set_entry(int r, int c, const ring_elem a)
  // Returns false if (r,c) is out of range.  It is assumed that the ring of
  // 'a' is the same as the ring of this.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  int loc = c * nrows + r;
  array_[loc] = a;
  return true;
}

bool DenseMutableMatrixRing::interchange_rows(int i, int j, bool do_recording)
  /* swap rows: row(i) <--> row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;
  if (i == j) return true;
  ring_elem *loc1 = array_ + i;
  ring_elem *loc2 = array_ + j;

  for (int c=0; c<ncols; c++)
    {
      ring_elem tmp = *loc1;
      *loc1 = *loc2;
      *loc2 = tmp;
      loc1 += nrows;
      loc2 += nrows;
    }
  if (do_recording && rowOps != 0)
    rowOps->interchange_columns(i,j,false);
  return true;
}

bool DenseMutableMatrixRing::interchange_columns(int i, int j, bool do_recording)
  /* swap columns: column(i) <--> column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  ring_elem *loc1 = array_ + nrows*i;
  ring_elem *loc2 = array_ + nrows*j;
  for (int r=0; r<nrows; r++)
    {
      ring_elem tmp = *loc1;
      *loc1++ = *loc2;
      *loc2++ = tmp;
    }
  if (do_recording && colOps != 0)
    colOps->interchange_columns(i,j,false);
  return true;
}

bool DenseMutableMatrixRing::scale_row(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* row(i) <- r * row(i) */
{
  if (error_row_bound(i)) return false;
  ring_elem *loc = array_ + i;
  if (opposite_mult)
    for (int c=0; c<ncols; c++)
      {
	*loc = R->mult(*loc, r);
	loc += nrows;
      }
  else
    for (int c=0; c<ncols; c++)
      {
	*loc = R->mult(r, *loc);
	loc += nrows;
      }
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixRing::scale_column(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* column(i) <- r * column(i) */
{
  if (error_column_bound(i)) return false;
  ring_elem *loc = array_ + nrows*i;
  if (opposite_mult)
    for (int a=0; a<nrows; a++)
      {
	*loc = R->mult(*loc, r);
	loc++;
      }
  else
    for (int a=0; a<nrows; a++)
      {
	*loc = R->mult(r, *loc);
	loc++;
      }
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixRing::row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* row(i) <- row(i) + r * row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;

  ring_elem *loc1 = array_ + i;
  ring_elem *loc2 = array_ + j;

  if (opposite_mult)
    for (int c=0; c<ncols; c++)
      {
	ring_elem f = R->mult(*loc2,r);
	R->add_to(*loc1, f);
	loc1 += nrows;
	loc2 += nrows;
      }
  else
    for (int c=0; c<ncols; c++)
      {
	ring_elem f = R->mult(r,*loc2);
	R->add_to(*loc1, f);
	loc1 += nrows;
	loc2 += nrows;
      }

  if (do_recording && rowOps != 0)
    rowOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

bool DenseMutableMatrixRing::column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* column(i) <- column(i) + r * column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;

  ring_elem *loc1 = array_ + nrows*i;
  ring_elem *loc2 = array_ + nrows*j;

  if (opposite_mult)
    for (int a=0; a<nrows; a++)
      {
	ring_elem f = R->mult(*loc2,r);
	R->add_to(*loc1, f);
	loc1++;
	loc2++;
      }
  else
    for (int a=0; a<nrows; a++)
      {
	ring_elem f = R->mult(r,*loc2);
	R->add_to(*loc1, f);
	loc1++;
	loc2++;
      }

  if (do_recording && colOps != 0)
    colOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

bool DenseMutableMatrixRing::dot_product(int i, int j, ring_elem &result) const
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  ring_elem *loc1 = array_ + i;
  ring_elem *loc2 = array_ + j;
  result = R->zero();
  for (int r=0; r<nrows; r++)
    {
      ring_elem f = R->mult(*loc1++,*loc2++);
      R->add_to(result, f);
    }
  return true;
}

bool DenseMutableMatrixRing::get_entry(int r, int c, ring_elem &result) const
// Returns false if (r,c) is out of range.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  int loc = c * nrows + r;
  result = array_[loc];
  return true;
}

///////////////////////////////
// Matrix operations //////////
///////////////////////////////

bool DenseMutableMatrixRing::is_zero() const
{
}

bool DenseMutableMatrixRing::is_equal(const MutableMatrix *B) const
{
}

bool DenseMutableMatrixRing::set_values(M2_arrayint rows,
					M2_arrayint cols,
					RingElement_array *values)
{
}

MutableMatrixOrNull * DenseMutableMatrixRing::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
}

MutableMatrixOrNull * DenseMutableMatrixRing::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
}

MutableMatrixOrNull * DenseMutableMatrixRing::mult(const MutableMatrix *B,
						   M2_bool opposite_mult) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
}

MutableMatrixOrNull * DenseMutableMatrixRing::mult(const RingElement *f,
						   M2_bool opposite_mult) const
// return f*this.  return NULL of sizes or types do not match.
{
}

MutableMatrix * DenseMutableMatrixRing::negate() const
{
}

MutableMatrix * DenseMutableMatrixRing::submatrix(const M2_arrayint rows, 
						  const M2_arrayint cols) const
{
}

MutableMatrix * DenseMutableMatrixRing::submatrix(const M2_arrayint cols) const
{
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
