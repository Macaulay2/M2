// Copyright 2004  Michael E. Stillman

#include "densemat.hpp"
#include "matrixcon.hpp"

DenseMutableMatrixRing::DenseMutableMatrixRing(const Ring *R0)
  : DenseMutableMatrix(R0),
    array_(0)
{
}

DenseMutableMatrixRing *
DenseMutableMatrixRing::zero_matrix(const Ring *R, 
				    int nrows, 
				    int ncols)
{
  DenseMutableMatrixRing *result = new DenseMutableMatrixRing(R);
  result->initialize(nrows,ncols,0);
  return result;
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
  DenseMutableMatrixRing *result = new DenseMutableMatrixRing(R);
  result->initialize(nrows,ncols,array_);
  return result;
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

int DenseMutableMatrixRing::lead_row(int col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0, or if col is out of range (but: no error is flagged) */
{
  if (col < 0 || col >= ncols) return -1;
  ring_elem *loc = array_ + (nrows+1)*col - 1;

  for (int i=nrows-1; i>=0; i++, loc--)
    {
      if (!R->is_zero(*loc))
	return i;
    }
  return -1;
}

int DenseMutableMatrixRing::lead_row(int col, ring_elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range (but: no error is flagged) */
{
  if (col < 0 || col >= ncols) return -1;
  ring_elem *loc = array_ + (nrows+1)*col - 1;

  for (int i=nrows-1; i>=0; i++, loc--)
    {
      if (!R->is_zero(*loc))
	{
	  result = *loc;
	  return i;
	}
    }
  return -1;
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

bool DenseMutableMatrixRing::divide_row(int i, ring_elem r, bool do_recording)
  /* row(i) <- row(i) / r */
{
  if (error_row_bound(i)) return false;
  ring_elem *loc = array_ + i;
  for (int c=0; c<ncols; c++)
    {
      *loc = R->divide(*loc, r);
      loc += nrows;
    }
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,false,false);
  return true;
}

bool DenseMutableMatrixRing::divide_column(int i, ring_elem r, bool do_recording)
  /* column(i) <- column(i) / r */
{
  if (error_column_bound(i)) return false;
  ring_elem *loc = array_ + nrows*i;
  for (int a=0; a<nrows; a++)
    {
      *loc = R->divide(*loc, r);
      loc++;
    }
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,false,false);
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

bool DenseMutableMatrixRing::row2by2(int r1, int r2, 
				     ring_elem a1, ring_elem a2,
				     ring_elem b1, ring_elem b2,
				     bool opposite_mult,
				     bool doRecording)
{
  if (error_row_bound(r1)) return false;
  if (error_row_bound(r2)) return false;
  ring_elem *loc1 = array_ + r1;
  ring_elem *loc2 = array_ + r2;
  for (int i=0; i<ncols; i++)
    {
      ring_elem f1,f2,g1,g2;
      if (opposite_mult)
	{
	  f1 = R->mult(*loc1,a1);
	  f2 = R->mult(*loc2,a2);
	  g1 = R->mult(*loc1,b1);
	  g2 = R->mult(*loc2,b2);
	}
      else
	{
	  f1 = R->mult(a1,*loc1);
	  f2 = R->mult(a2,*loc2);
	  g1 = R->mult(b1,*loc1);
	  g2 = R->mult(b2,*loc2);
	}
      R->add_to(f1,f2);
      R->add_to(g1,g2);
      *loc1 = f1;
      *loc2 = g1;
      loc1 += nrows;
      loc2 += nrows;
    }

  if (doRecording && rowOps != 0)
    rowOps->column2by2(r1,r2,a1,a2,b1,b2,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixRing::column2by2(int c1, int c2, 
					ring_elem a1, ring_elem a2,
					ring_elem b1, ring_elem b2,
					bool opposite_mult,
					bool doRecording)
{
  if (error_column_bound(c1)) return false;
  if (error_column_bound(c2)) return false;
  ring_elem *loc1 = array_ + c1 * nrows;
  ring_elem *loc2 = array_ + c2 * nrows;

  for (int i=0; i<nrows; i++)
    {
      ring_elem f1,f2,g1,g2;
      if (opposite_mult)
	{
	  f1 = R->mult(*loc1,a1);
	  f2 = R->mult(*loc2,a2);
	  g1 = R->mult(*loc1,b1);
	  g2 = R->mult(*loc2,b2);
	}
      else
	{
	  f1 = R->mult(a1,*loc1);
	  f2 = R->mult(a2,*loc2);
	  g1 = R->mult(b1,*loc1);
	  g2 = R->mult(b2,*loc2);
	}
      R->add_to(f1,f2);
      R->add_to(g1,g2);
      *loc1++ = f1;
      *loc2++ = g1;
    }

  // Do the recording, if needed:
  if (doRecording && colOps != 0)
    colOps->column2by2(c1,c2,a1,a2,b1,b2,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixRing::dot_product(int i, int j, ring_elem &result) const
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  ring_elem *loc1 = array_ + nrows*i;
  ring_elem *loc2 = array_ + nrows*j;
  result = R->zero();
  for (int r=0; r<nrows; r++)
    {
      ring_elem f = R->mult(*loc1++,*loc2++);
      R->add_to(result, f);
    }
  return true;
}

bool DenseMutableMatrixRing::get_entry(int r, int c, ring_elem &result) const
{
  if (r >= 0 && r < nrows && c >= 0 && c < ncols)
    {
      int loc = c * nrows + r;
      result = array_[loc];
      return !R->is_zero(result);
    }
  else
    {
      result = R->zero();
      return false;
    }
}

///////////////////////////////
// Matrix operations //////////
///////////////////////////////

bool DenseMutableMatrixRing::is_zero() const
{
#warning "to be written"
  return 0;
}

bool DenseMutableMatrixRing::is_equal(const MutableMatrix *B) const
{
#warning "to be written"
  return 0;
}

bool DenseMutableMatrixRing::set_values(M2_arrayint rows,
					M2_arrayint cols,
					RingElement_array *values)
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRing::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRing::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRing::mult(const MutableMatrix *B,
						   M2_bool opposite_mult) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRing::mult(const RingElement *f,
						   M2_bool opposite_mult) const
// return f*this.  return NULL of sizes or types do not match.
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixRing::negate() const
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixRing::submatrix(const M2_arrayint rows, 
						  const M2_arrayint cols) const
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixRing::submatrix(const M2_arrayint cols) const
{
#warning "to be written"
  return 0;
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
