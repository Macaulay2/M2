// Copyright 2004  Michael E. Stillman

#include "densematRR.hpp"
#include "matrixcon.hpp"
#include "RR.hpp"
#include <cstdio>

DenseMutableMatrixRR::DenseMutableMatrixRR()
  : DenseMutableMatrix(globalRR),
    array_(0)
{
}

DenseMutableMatrixRR *
DenseMutableMatrixRR::zero_matrix(int nrows, 
				  int ncols)
{
  DenseMutableMatrixRR *result = new DenseMutableMatrixRR();
  result->initialize(nrows,ncols,0);
  return result;
}

void DenseMutableMatrixRR::initialize(int nrows0, int ncols0, double *array)
{
  fprintf(stderr, "made it to densematRR\n");
  nrows = nrows0;
  ncols = ncols0;
  int len = nrows0 * ncols0;
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

Matrix *DenseMutableMatrixRR::to_matrix() const
{
  FreeModule *F = R->make_FreeModule(nrows);
  MatrixConstructor mat(F,ncols);
  double * loc = array_;
  for (int c=0; c<ncols; c++)
    for (int r=0; r<nrows; r++)
    {
      if (*loc != 0.0)
	mat.set_entry(r,c,R->from_double(*loc));
      loc++;
    }
  mat.compute_column_degrees();
  return mat.to_matrix();
}

MutableMatrix *DenseMutableMatrixRR::copy(bool prefer_dense) const
{
  DenseMutableMatrixRR *result = new DenseMutableMatrixRR;
  result->initialize(nrows,ncols,array_);
  return result;
}

bool DenseMutableMatrixRR::set_entry(int r, int c, const ring_elem a)
  // Returns false if (r,c) is out of range.  It is assumed that the ring of
  // 'a' is the same as the ring of this.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  int loc = c * nrows + r;
  array_[loc] = globalRR->to_double(a);
  return true;
}

int DenseMutableMatrixRR::lead_row(int col) const
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

int DenseMutableMatrixRR::lead_row(int col, ring_elem &result) const
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

bool DenseMutableMatrixRR::interchange_rows(int i, int j, bool do_recording)
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

bool DenseMutableMatrixRR::interchange_columns(int i, int j, bool do_recording)
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

bool DenseMutableMatrixRR::scale_row(ring_elem r, int i, bool opposite_mult, bool do_recording)
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

bool DenseMutableMatrixRR::scale_column(ring_elem r, int i, bool opposite_mult, bool do_recording)
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

bool DenseMutableMatrixRR::divide_row(int i, ring_elem r, bool do_recording)
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

bool DenseMutableMatrixRR::divide_column(int i, ring_elem r, bool do_recording)
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

bool DenseMutableMatrixRR::row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
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

bool DenseMutableMatrixRR::column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
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

bool DenseMutableMatrixRR::row2by2(int r1, int r2, 
				   ring_elem ra1, ring_elem ra2,
				   ring_elem rb1, ring_elem rb2,
				   bool opposite_mult,
				   bool doRecording)
{
  if (error_row_bound(r1)) return false;
  if (error_row_bound(r2)) return false;
  double *loc1 = array_ + r1;
  double *loc2 = array_ + r2;
  double a1 = globalRR->to_double(ra1);
  double a2 = globalRR->to_double(ra2);
  double b1 = globalRR->to_double(rb1);
  double b2 = globalRR->to_double(rb2);
  for (int i=0; i<ncols; i++)
    {
      double f1 = a1 * *loc1;
      double f2 = a2 * *loc2;
      double g1 = b1 * *loc1;
      double g2 = b2 * *loc2;
      *loc1 = f1+f2;
      *loc2 = g1+g2;
      loc1 += nrows;
      loc2 += nrows;
    }

  if (doRecording && rowOps != 0)
    rowOps->column2by2(r1,r2,ra1,ra2,rb1,rb2,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixRR::column2by2(int c1, int c2, 
				      ring_elem ra1, ring_elem ra2,
				      ring_elem rb1, ring_elem rb2,
				      bool opposite_mult,
				      bool doRecording)
{
  if (error_column_bound(c1)) return false;
  if (error_column_bound(c2)) return false;
  double *loc1 = array_ + c1 * nrows;
  double *loc2 = array_ + c2 * nrows;
  double a1 = globalRR->to_double(ra1);
  double a2 = globalRR->to_double(ra2);
  double b1 = globalRR->to_double(rb1);
  double b2 = globalRR->to_double(rb2);
  for (int i=0; i<nrows; i++)
    {
      double f1 = a1 * *loc1;
      double f2 = a2 * *loc2;
      double g1 = b1 * *loc1;
      double g2 = b2 * *loc2;
      *loc1++ = f1+f2;
      *loc2++ = g1+g2;
    }

  // Do the recording, if needed:
  if (doRecording && colOps != 0)
    colOps->column2by2(c1,c2,ra1,ra2,rb1,rb2,opposite_mult,false);
  return true;
}

bool DenseMutableMatrixRR::dot_product(int i, int j, ring_elem &result) const
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

bool DenseMutableMatrixRR::get_entry(int r, int c, ring_elem &result) const
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

bool DenseMutableMatrixRR::is_zero() const
{
#warning "to be written"
  return 0;
}

bool DenseMutableMatrixRR::is_equal(const MutableMatrix *B) const
{
#warning "to be written"
  return 0;
}

bool DenseMutableMatrixRR::set_values(M2_arrayint rows,
					M2_arrayint cols,
					RingElement_array *values)
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRR::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRR::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRR::mult(const MutableMatrix *B,
						   M2_bool opposite_mult) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * DenseMutableMatrixRR::mult(const RingElement *f,
						   M2_bool opposite_mult) const
// return f*this.  return NULL of sizes or types do not match.
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixRR::negate() const
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixRR::submatrix(const M2_arrayint rows, 
						  const M2_arrayint cols) const
{
#warning "to be written"
  return 0;
}

MutableMatrix * DenseMutableMatrixRR::submatrix(const M2_arrayint cols) const
{
#warning "to be written"
  return 0;
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
