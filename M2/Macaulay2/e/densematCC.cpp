// Copyright 2004  Michael E. Stillman

#include "densematCC.hpp"
#include "matrixcon.hpp"
#include "CC.hpp"
#include <cstdio>

DenseMutableMatrixCC::DenseMutableMatrixCC()
  : DenseMutableMatrix(globalCC),
    array_(0)
{
}

DenseMutableMatrixCC *
DenseMutableMatrixCC::zero_matrix(int nrows, 
				  int ncols)
{
  DenseMutableMatrixCC *result = new DenseMutableMatrixCC();
  result->initialize(nrows,ncols,0);
  return result;
}

void DenseMutableMatrixCC::initialize(int nrows0, int ncols0, double *array)
{
  fprintf(stderr, "made it to densematCC\n");
  nrows = nrows0;
  ncols = ncols0;
  int len = 2 * nrows0 * ncols0;
  array_ = newarray(double,len);
  if (array == 0)
    {
      for (int i=0; i<len; i++)
	array_[i] = 0.0;
    }
  else
    {
      for (int i=0; i<len; i++)
	array_[i] = array[i];
    }
}

Matrix *DenseMutableMatrixCC::to_matrix() const
{
  FreeModule *F = globalCC->make_FreeModule(nrows);
  MatrixConstructor mat(F,ncols);
  double * loc = array_;
  for (int c=0; c<ncols; c++)
    for (int r=0; r<nrows; r++)
    {
      if (*loc != 0.0)
	mat.set_entry(r,c,globalCC->from_doubles(*loc, loc[1]));
      loc += 2;
    }
  mat.compute_column_degrees();
  return mat.to_matrix();
}

MutableMatrix *DenseMutableMatrixCC::copy(bool prefer_dense) const
{
  DenseMutableMatrixCC *result = new DenseMutableMatrixCC;
  result->initialize(nrows,ncols,array_);
  return result;
}

bool DenseMutableMatrixCC::set_entry(int r, int c, const ring_elem a)
  // Returns false if (r,c) is out of range.  It is assumed that the ring of
  // 'a' is the same as the ring of this.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  int loc = 2 * (c * nrows + r);
  globalCC->to_doubles(a,re,im);
  array_[loc] = re;;
  array_[loc+1] = im;
  return true;
}

int DenseMutableMatrixCC::lead_row(int col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
{
  if (col < 0 || col >= ncols) return -1;
  double *loc = array_ + (nrows+1)*col - 1;

  for (int i=nrows-1; i>=0; i++, loc--)
    {
      if (*loc != 0.0)
	return i;
    }
  return -1;
}

int DenseMutableMatrixCC::lead_row(int col, ring_elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range (but: no error is flagged) */
{
  if (col < 0 || col >= ncols) return -1;
  double *loc = array_ + (nrows+1)*col - 1;

  for (int i=nrows-1; i>=0; i++, loc--)
    {
      if (*loc != 0.0)
	{
	  result = R->from_double(*loc);
	  return i;
	}
    }
  return -1;
}

bool DenseMutableMatrixCC::interchange_rows(int i, int j, bool do_recording)
  /* swap rows: row(i) <--> row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;
  if (i == j) return true;
  double *loc1 = array_ + i;
  double *loc2 = array_ + j;

  for (int c=0; c<ncols; c++)
    {
      double tmp = *loc1;
      *loc1 = *loc2;
      *loc2 = tmp;
      loc1 += nrows;
      loc2 += nrows;
    }
  if (do_recording && rowOps != 0)
    rowOps->interchange_columns(i,j,false);
  return true;
}

bool DenseMutableMatrixCC::interchange_columns(int i, int j, bool do_recording)
  /* swap columns: column(i) <--> column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  double *loc1 = array_ + nrows*i;
  double *loc2 = array_ + nrows*j;
  for (int r=0; r<nrows; r++)
    {
      double tmp = *loc1;
      *loc1++ = *loc2;
      *loc2++ = tmp;
    }
  if (do_recording && colOps != 0)
    colOps->interchange_columns(i,j,false);
  return true;
}

bool DenseMutableMatrixCC::scale_row(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* row(i) <- r * row(i) */
{
  if (error_row_bound(i)) return false;
  double rd = globalRR->to_double(r);
  double *loc = array_ + i;
  for (int c=0; c<ncols; c++)
    {
      *loc *= rd;
      loc += nrows;
    }
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixCC::scale_column(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* column(i) <- r * column(i) */
{
  if (error_column_bound(i)) return false;
  double rd = globalRR->to_double(r);
  double *loc = array_ + nrows*i;
  for (int a=0; a<nrows; a++)
      *loc++ *= rd;
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixCC::divide_row(int i, ring_elem r, bool do_recording)
  /* row(i) <- row(i) / r */
{
  if (error_row_bound(i)) return false;
  double rd = globalRR->to_double(r);
  double *loc = array_ + i;
  for (int c=0; c<ncols; c++)
    {
      *loc /= rd;
      loc += nrows;
    }
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,false,false);
  return true;
}

bool DenseMutableMatrixCC::divide_column(int i, ring_elem r, bool do_recording)
  /* column(i) <- column(i) / r */
{
  if (error_column_bound(i)) return false;
  double rd = globalRR->to_double(r);
  double *loc = array_ + nrows*i;
  for (int a=0; a<nrows; a++)
      *loc++ /= rd;
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,false,false);
  return true;
}

bool DenseMutableMatrixCC::row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* row(i) <- row(i) + r * row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;

  double rd = globalRR->to_double(r);
  double *loc1 = array_ + i;
  double *loc2 = array_ + j;

  for (int c=0; c<ncols; c++)
    {
      double f = *loc2 * rd;
      *loc1 += f;
      loc1 += nrows;
      loc2 += nrows;
    }
  
  if (do_recording && rowOps != 0)
    rowOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

bool DenseMutableMatrixCC::column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* column(i) <- column(i) + r * column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;

  double rd = globalRR->to_double(r);
  double *loc1 = array_ + nrows*i;
  double *loc2 = array_ + nrows*j;

  for (int a=0; a<nrows; a++)
    {
      double f = *loc2++ * rd;
      *loc1++ += f;
    }

  if (do_recording && colOps != 0)
    colOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

bool DenseMutableMatrixCC::dot_product(int i, int j, ring_elem &result) const
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  double *loc1 = array_ + nrows*i;
  double *loc2 = array_ + nrows*j;
  double f = 0.0;
  for (int r=0; r<nrows; r++)
    f += (*loc1++) * (*loc2++);
  result = globalRR->from_double(f);
  return true;
}

bool DenseMutableMatrixCC::get_entry(int r, int c, ring_elem &result) const
{
  if (r >= 0 && r < nrows && c >= 0 && c < ncols)
    {
      int loc = c * nrows + r;
      double f = array_[loc];
      result = R->from_double(f);
      return f != 0;
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

bool DenseMutableMatrixCC::is_zero() const
{
#warning "to be written"
  return 0;
}

bool DenseMutableMatrixCC::is_equal(const MutableMatrix *B) const
{
#warning "to be written"
  return 0;
}

bool DenseMutableMatrixCC::set_values(M2_arrayint rows,
					M2_arrayint cols,
					RingElement_array *values)
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixCC::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixCC::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixCC::mult(const MutableMatrix *B,
						   M2_bool opposite_mult) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixCC::mult(const RingElement *f,
						   M2_bool opposite_mult) const
// return f*this.  return NULL of sizes or types do not match.
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixCC::negate() const
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixCC::submatrix(const M2_arrayint rows, 
						  const M2_arrayint cols) const
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixCC::submatrix(const M2_arrayint cols) const
{
#warning "to be written"
  return 0;
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
