// Copyright 2004  Michael E. Stillman

#include "densemat.hpp"

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
  ring_elem *loc1 = array_ + i;
  ring_elem *loc2 = array_ + j;
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
  ring_elem *loc = array_ + i;
  if (opposite_mult)
    for (int a=0; a<nrows; a++)
      {
	*loc++ = R->mult(*loc, r);
      }
  else
    for (int a=0; a<nrows; a++)
      {
	*loc++ = R->mult(r, *loc);
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
	ring_elem tmp = *loc1;
	*loc1 = *loc2;
	ring_elem f = R->mult(*loc2,r);
	R->add_to(*loc1, f);
	loc1 += nrows;
	loc2 += nrows;
      }
  else
    for (int c=0; c<ncols; c++)
      {
	ring_elem tmp = *loc1;
	*loc1 = *loc2;
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

  ring_elem *loc1 = array_ + i;
  ring_elem *loc2 = array_ + j;

  if (opposite_mult)
    for (int a=0; a<nrows; a++)
      {
	ring_elem tmp = *loc1;
	*loc1 = *loc2;
	ring_elem f = R->mult(*loc2,r);
	R->add_to(*loc1, f);
	loc1++;
	loc2++;
      }
  else
    for (int a=0; a<nrows; a++)
      {
	ring_elem tmp = *loc1;
	*loc1 = *loc2;
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

bool DenseMutableMatrixRing::get_nonzero_entry(int r, int c, ring_elem &result) const
  // Returns false if (r,c) entry is either zero or out of range.
  // Otherwise, returns true, and sets result to be the matrix entry at (r,c)
{
}


#if 0
  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  bool DenseMutableMatrixRing::is_zero() const;

  bool DenseMutableMatrixRing::is_equal(const MutableMatrix *B) const;

  bool DenseMutableMatrixRing::set_values(M2_arrayint rows,
			  M2_arrayint cols,
			  RingElement_array *values);

  virtual MutableMatrixOrNull * add(const MutableMatrix *B) const; 
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * subtract(const MutableMatrix *B) const; 
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * mult(const MutableMatrix *B,
				     M2_bool opposite_mult) const; 
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * mult(const RingElement *f,
				     M2_bool opposite_mult) const; 
  // return f*this.  return NULL of sizes or types do not match.

  virtual MutableMatrix * negate() const;

  virtual MutableMatrix * submatrix(const M2_arrayint rows, const M2_arrayint cols) const;

  virtual MutableMatrix * submatrix(const M2_arrayint cols) const;
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
