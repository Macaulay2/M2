// Copyright 2004  Michael E. Stillman

#include "sparsemat.hpp"
#include "matrixcon.hpp"

SparseMutableMatrix::SparseMutableMatrix(const Ring *R0)
  : MutableMatrix(R0),
    columns_(0)
{
}

SparseMutableMatrix *
SparseMutableMatrix::zero_matrix(const Ring *R, 
				    int nrows, 
				    int ncols)
{
  SparseMutableMatrix *result = new SparseMutableMatrix(R);
  result->initialize(nrows,ncols,0);
  return result;
}

void SparseMutableMatrix::initialize(int nrows0, int ncols0, vec * columns)
{
  nrows_ = nrows0;
  ncols_ = ncols0;
  columns_ = newarray(vec,ncols_);
  if (columns == 0)
    for (int i=0; i<ncols_; i++)
      columns_[i] = 0;
  else
    for (int i=0; i<ncols_; i++)
      columns_[i] = R->copy_vec(columns[i]);
}

Matrix *SparseMutableMatrix::to_matrix() const
{
  FreeModule *F = R->make_FreeModule(nrows_);
  MatrixConstructor mat(F,ncols_);
  for (int c=0; c<ncols_; c++)
    mat.set_column(c, R->copy_vec(columns_[c]));
  mat.compute_column_degrees();
  return mat.to_matrix();
}

MutableMatrix *SparseMutableMatrix::copy(bool prefer_dense) const
{
  SparseMutableMatrix *result = new SparseMutableMatrix(R);
  result->initialize(nrows_,ncols_,columns_);
  return result;
}




int SparseMutableMatrix::lead_row(int col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
{
  if (error_column_bound(col)) return -1;
  vec v = columns_[col];
  if (v == 0) return -1;
  return v->comp;
}

int SparseMutableMatrix::lead_row(int col, ring_elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */
{
  if (error_column_bound(col)) return -1;
  vec v = columns_[col];
  if (v == 0) return -1;
  result = v->coeff;
  return v->comp;
}

///////////////////////////////
// Row and column operations //
///////////////////////////////
// The following routines return false if one of the row or columns given
// is out of range.

bool SparseMutableMatrix::get_entry(int r, int c, ring_elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  bool isnonzero;
  if (r >= 0 && r < nrows_ && c >= 0 && c < ncols_)
    isnonzero = R->get_entry(columns_[c],r,result);
  else
    isnonzero = false;
  if (!isnonzero)
    result = R->zero();
  return isnonzero;
}

bool SparseMutableMatrix::set_entry(int r, int c, const ring_elem a)
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  R->set_entry(columns_[c], r, a);
  return true;
}

bool SparseMutableMatrix::interchange_rows(int i, int j, bool do_recording)
  /* swap rows: row(i) <--> row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;
  for (int c=0; c<n_cols(); c++)
    R->interchange_rows(columns_[c], i, j);
  if (do_recording && rowOps != 0)
    rowOps->interchange_columns(i,j,false);
  return true;
}

bool SparseMutableMatrix::interchange_columns(int i, int j, bool do_recording)
  /* swap columns: column(i) <--> column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  vec tmp = columns_[i];
  columns_[i] = columns_[j];
  columns_[j] = tmp;
  if (do_recording && colOps != 0)
    colOps->interchange_columns(i,j,false);
  return true;
}

bool SparseMutableMatrix::scale_row(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* row(i) <- r * row(i) */
{
  if (error_row_bound(i)) return false;
  for (int c=0; c<n_cols(); c++)
    R->mult_row(columns_[c], r, i, opposite_mult);
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool SparseMutableMatrix::scale_column(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* column(i) <- r * column(i) */
{
  if (error_column_bound(i)) return false;
  R->mult_vec_to(columns_[i], r, opposite_mult);
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool SparseMutableMatrix::divide_row(int i, ring_elem r, bool do_recording)
  /* row(i) <- row(i) / r */
{
  if (error_row_bound(i)) return false;
  for (int c=0; c<n_cols(); c++)
    R->divide_row(columns_[c], i, r);
  if (do_recording && rowOps != 0)
    rowOps->divide_column(i,r,false);
  return true;
}

bool SparseMutableMatrix::divide_column(int i, ring_elem r, bool do_recording)
  /* column(i) <- column(i) / r */
{
  if (error_column_bound(i)) return false;
  R->divide_vec_to(columns_[i], r);
  if (do_recording && colOps != 0)
    colOps->divide_column(i,r,false);
  return true;
}

bool SparseMutableMatrix::row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* row(i) <- row(i) + r * row(j) */
{
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;

  for (int c=0; c<n_cols(); c++)
    R->vec_row_op(columns_[c], i, r, j, opposite_mult);

  if (do_recording && rowOps != 0)
    rowOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

bool SparseMutableMatrix::column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* column(i) <- column(i) + r * column(j) */
{
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;

  vec tmp = R->copy_vec(columns_[j]);
  R->mult_vec_to(tmp, r, opposite_mult); // replaces tmp by r*tmp or tmp*r
  R->add_vec_to(columns_[i], tmp);

  if (do_recording && colOps != 0)
    colOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

void SparseMutableMatrix::vec_row2by2(vec &v, 
				      int r1, int r2,
				      ring_elem a1, ring_elem a2,
				      ring_elem b1, ring_elem b2,
				      bool opposite_mult) const
{
  // v[row r1] = a1 * v[r1] + a2 * v[r2]
  // v[row r2] = b1 * v[r1] + b2 * v[r2]
  ring_elem e1,e2, c1,c2,c3,c4;
  bool r1_nonzero = R->get_entry(v,r1,e1);
  bool r2_nonzero = R->get_entry(v,r2,e2);
  if (!r1_nonzero && !r2_nonzero) return;

  if (r1_nonzero)
    {
      if (opposite_mult)
	{
	  c1 = R->mult(e1,a1);
	  c3 = R->mult(e1,b1);
	}
      else
	{
	  c1 = R->mult(a1,e1);
	  c3 = R->mult(b1,e1);
	}
    }
  else
    {
      c1 = R->from_int(0);
      c3 = R->from_int(0);
    }
  if (r2_nonzero)
    {
      if (opposite_mult)
	{
	  c2 = R->mult(e2,a2);
	  c4 = R->mult(e2,b2);
	}
      else
	{
	  c2 = R->mult(a2,e2);
	  c4 = R->mult(b2,e2);
	}
    }
  else
    {
      c2 = R->from_int(0);
      c4 = R->from_int(0);
    }

  R->add_to(c1,c2);
  R->add_to(c3,c4);
  R->set_entry(v,r1,c1);
  R->set_entry(v,r2,c3);
}

bool SparseMutableMatrix::row2by2(int r1, int r2, 
				  ring_elem a1, ring_elem a2,
				  ring_elem b1, ring_elem b2,
				  bool opposite_mult,
				  bool doRecording)
{
  if (error_row_bound(r1)) return false;
  if (error_row_bound(r2)) return false;

  for (int i=0; i<ncols_; i++)
    vec_row2by2(columns_[i],r1,r2,a1,a2,b1,b2,opposite_mult);

  if (doRecording && rowOps != 0)
    rowOps->column2by2(r1,r2,a1,a2,b1,b2,opposite_mult,false);

  return true;
}

bool SparseMutableMatrix::column2by2(int c1, int c2, 
				     ring_elem a1, ring_elem a2,
				     ring_elem b1, ring_elem b2,
				     bool opposite_mult,
				     bool doRecording)
{
  // do the replacements:
  // new column c1 = a1 * column[c1] + a2 * column[c2]
  // new column c2 = b1 * column[c1] + b2 * column[c2]
  // Make first column: v1 = a1*c1+a2*c2
  if (error_column_bound(c1)) return false;
  if (error_column_bound(c2)) return false;

  vec v1 = R->copy_vec(columns_[c1]);
  vec v2 = R->copy_vec(columns_[c2]);
  R->mult_vec_to(v1,a1,opposite_mult);
  R->mult_vec_to(v2,a2,opposite_mult);
  R->add_vec_to(v1,v2);
  // Second column: w1 = b1*c1 + b2*c2
  vec w1 = R->copy_vec(columns_[c1]);
  vec w2 = R->copy_vec(columns_[c2]);
  R->mult_vec_to(w1,b1,opposite_mult);
  R->mult_vec_to(w2,b2,opposite_mult);
  R->add_vec_to(w1,w2);
  // Set the matrices:
  R->remove_vec(columns_[c1]);
  R->remove_vec(columns_[c2]);
  columns_[c1] = v1;
  columns_[c2] = w1;
  // Do the recording, if needed:
  if (doRecording && colOps != 0)
    colOps->column2by2(c1,c2,a1,a2,b1,b2,opposite_mult,false);
  return true;
}

bool SparseMutableMatrix::dot_product(int i, int j, ring_elem &result) const
{
  if (error_column_bound(i) || error_column_bound(j))
    result = R->zero();
  else
    result = R->dot_product(columns_[i], columns_[j]);
  return result;
}

bool SparseMutableMatrix::row_permute(int start_row, M2_arrayint perm)
{
#warning "write this"
  ERROR("not implemented yet");
  return false;
}

bool SparseMutableMatrix::column_permute(int start_col, M2_arrayint perm)
{
#warning "write this"
  ERROR("not implemented yet");
  return false;
}

bool SparseMutableMatrix::set_submatrix(M2_arrayint rows,
					M2_arrayint cols, 
					const MutableMatrix *N)
  // returns false iff there is an error
{
#warning "write this"
  ERROR("not implemented yet");
  return false;
}

///////////////////////////////
// Matrix operations //////////
///////////////////////////////

bool SparseMutableMatrix::is_zero() const
{
  for (int i=0; i<ncols_; i++)
    if (columns_[i] != 0) return false;
  return true;
}

bool SparseMutableMatrix::is_equal(const MutableMatrix *B) const
{
  if (R != B->get_ring()) return false;
  if (nrows_ != B->n_rows() || ncols_ != B->n_cols()) return false;
  const SparseMutableMatrix *B1 = B->cast_to_SparseMutableMatrix();
  if (B1 != 0)
    {
      for (int i=0; i<ncols_; i++)
	if (!R->is_equal(columns_[i], B1->columns_[i])) return false;
      return true;
    }
  // Otherwise B is dense. 
#warning "implement isequal for dense == sparse"
  return false;
}

bool SparseMutableMatrix::set_values(M2_arrayint rows,
					M2_arrayint cols,
					RingElement_array *values)
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * SparseMutableMatrix::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
  if (R != B->get_ring())
    {
      ERROR("matrices have different base rings");
      return 0;
    }
  if (nrows_ != B->n_rows() || ncols_ != B->n_cols())
    {
      ERROR("matrices have different shape");
      return 0;
    }
  const SparseMutableMatrix *B1 = B->cast_to_SparseMutableMatrix();
  SparseMutableMatrix *result = zero_matrix(R,nrows_,ncols_);
  if (B1 != 0)
    {  
      for (int i=0; i<ncols_; i++)
	{
	  vec v = R->copy_vec(columns_[i]);
	  vec w = R->copy_vec(B1->columns_[i]);
	  R->add_vec_to(v,w);
	  result->columns_[i] = v;
	}
    }
#warning "implement for dense + sparse"
  return 0;
}

MutableMatrixOrNull * SparseMutableMatrix::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * SparseMutableMatrix::mult(const MutableMatrix *B,
						   M2_bool opposite_mult) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
#warning "to be written"
  return 0;
}

MutableMatrixOrNull * SparseMutableMatrix::mult(const RingElement *f,
						   M2_bool opposite_mult) const
// return f*this.  return NULL of sizes or types do not match.
{
#warning "to be written"
  return 0;
}

MutableMatrix * SparseMutableMatrix::negate() const
{
  SparseMutableMatrix *result = SparseMutableMatrix::zero_matrix(R,nrows_,ncols_);
  for (int i=0; i<ncols_; i++)
    result->columns_[i] = R->negate_vec(columns_[i]);
  return result;
}

MutableMatrix * SparseMutableMatrix::submatrix(M2_arrayint rows, 
					       M2_arrayint cols) const
{
  int *trans = newarray(int,n_rows());

  for (int i=0; i<nrows_; i++)
    trans[i] = -1;

  for (unsigned j=0; j<rows->len; j++)
    if (rows->array[j] >= 0 && rows->array[j] < nrows_)
      trans[rows->array[j]] = j;

  SparseMutableMatrix *result = zero_matrix(R,rows->len,cols->len);

  for (unsigned int i=0; i<cols->len; i++)
    {
      if (cols->array[i] < 0 || cols->array[i] >= ncols_) continue;
      vec v = columns_[cols->array[i]];
      vec w = 0;
      for ( ; v != NULL; v = v->next)
	{
	  if (trans[v->comp] != -1)
	    R->set_entry(w, trans[v->comp], v->coeff);
	}
      result->columns_[i] = w;
    }
  deletearray(trans);
  return result;
}

MutableMatrix * SparseMutableMatrix::submatrix(M2_arrayint cols) const
{
  SparseMutableMatrix *result = zero_matrix(R,nrows_,cols->len);
  for (unsigned int i=0; i<cols->len; i++)
    if (cols->array[i] >= 0 && cols->array[i] < ncols_)
      result->columns_[i] = R->copy_vec(columns_[cols->array[i]]);
  return result;
}

void SparseMutableMatrix::setSizes(int c_lo, int c_hi, int *rowSize, int *colSize)
{
  assert(c_lo >= 0);
  assert(c_lo <= c_hi);
  assert(c_hi < n_cols());

  // Sets the arrays rowSize and colSize
  int i;
  for (i=0; i<ncols_; i++) colSize[i] = 0;
  for (i=0; i<nrows_; i++) rowSize[i] = 0;
  for (i=c_lo; i<=c_hi; i++)
    for (vec p=columns_[i]; p != 0; p=p->next)
      {
	colSize[i]++;
	rowSize[p->comp]++;
      }
#if 0
  buffer o;
  o << "Column sizes = ";
  for (i=0; i<ncols; i++)
    {
      if (i%10 == 0) o << newline;
      o << colSize[i] << " ";
    }
  o << newline;
  o << "Row sizes = ";
  for (i=0; i<nrows; i++)
    {
      if (i%10 == 0) o << newline;
      o << rowSize[i] << " ";
    }
  o << newline;
  emit(o.str());
#endif    
}


// Harry's routines

// this routine finds units (aka pivots) in a sparse matrix, 
// clears out other columns by the pivot, and then sticks the
//  pivot into the "current" last column and last row of the matrix.

// void SparseMutableMatrix::reduce_pivots()
// {
//   int nrows = n_rows();
//   if (nrows == 0) return;
//   int ncols = n_cols();
//   if (ncols == 0) return;
//   const Ring *K = get_ring();
//   ring_elem one = K->one();
//   ring_elem minus_one = K->minus_one();
//   int *rowSize = newarray(int, n_rows());
//   int *colSize = newarray(int, n_cols());
//   setSizes(0,n_cols()-1,rowSize,colSize);
// 
//   // After using the pivot element, it is moved to [nrows-1,ncols-1]
//   // and nrows and ncols are decremented.
// 
//   // first reduce 1's and -1's
//   int i;
//   for (i = 0; i < ncols; i++) 
//     { 
//       for (vec p = columns_[i]; p != 0; p = p->next) 
// 	{
// 	  bool oneFlag = K->is_equal(one, p->coeff);
// 	  
// 	  if ( oneFlag || K->is_equal(minus_one, p->coeff)) 
// 	    {
// 
// 	      int r = p->comp;
// 	      
// 	      // case 1: pivot is only elt in its column and row
// 	      if (colSize[i] == 1 && rowSize[r] == 1) 
// 		{
// 		  if (oneFlag == false)
// 		    {
// 		      K->remove(columns_[i]->coeff);
// 		      columns_[i]->coeff = K->copy(one);
// 		    }
// 		  if (i == ncols-1 && r == nrows-1)
// 		    {
// 		      nrows--;
// 		      ncols--;
// 		    }
// 		  else if (i < ncols-1 || r < nrows-1)
// 		    {
// 		      interchange_columns(i, --ncols);
// 		      interchange_rows(r, --nrows);
// 		      i = i-1;  //test column i again
// 		    }
// 		}
// 	      
// 	      // case 2: other elements exist in pivot's row or column
// 	      else
// 		{
// 		  for (int j = 0; j < ncols; j++) 
// 		    {
// 		      if (j != i) 
// 			{
// 			  for (vec q = columns_[j]; q != 0 
// 				 && q->comp >= r; q = q->next) 
// 			    {
// 			      if (q->comp == r)
// 				{
// 				  if (oneFlag) {
// 				    ring_elem f = K->negate(q->coeff);
// 				    column_op(i, f, j, false);
// 				    K->remove(f);
// 				  }
// 				  else {
// 				    column_op(i, q->coeff, j, false);
// 				  }
// 				  break;
// 				}
// 			    }
// 			}
// 		    }
// 		  p = columns_[i];
// 		  K->remove_vec(columns_[i]->next);
// 		  columns_[i]->next = 0;
// 		  columns_[i]->comp = r;
// 		  K->remove(columns_[i]->coeff);
// 		  columns_[i]->coeff = K->copy(one);
// 		  colSize[i] = 1;
// 		  rowSize[r] = 1;
// 		  interchange_columns(i, --ncols);
// 		  interchange_rows(r, --nrows);
// 		  i = -1; //start over at the 1st column
// 		}
// 	    }
// 	}
//     }
// 
//   // now reduce other units
//   for (i = 0; i < ncols; i++) 
//     { 
//       for (vec p = columns_[i]; p != 0; p = p->next) 
// 	{
// 	  if ( K->is_unit(p->coeff) && 
// 	       ( (colSize[i] > 1 || rowSize[p->comp] > 1) ||
// 		 (K->is_equal(one,p->coeff) == false) ) ) 
// 	    {
// 	      divide_column(i, p->coeff);
// 	      int r = p->comp;
// 	      for (int j = 0; j < ncols; j++) 
// 		{
// 		  if (j != i) 
// 		    {
// 		      for (vec q = columns_[j]; q != 0 
// 			     && q->comp >= r; q = q->next) 
// 			{
// 			  if (q->comp == r)
// 			    {
// 			      ring_elem f = K->negate(q->coeff);
// 			      column_op(i, f, j, false);
// 			      K->remove(f);
// 			      break;
// 			    }
// 			}
// 		    }
// 		}
// 	      p = columns_[i];
// 	      K->remove_vec(columns_[i]->next);
// 	      columns_[i]->next = 0;
// 	      columns_[i]->comp = r;
// 	      K->remove(columns_[i]->coeff);
// 	      columns_[i]->coeff = K->copy(one);
// 	      colSize[i] = 1;
// 	      rowSize[r] = 1;
// 	      interchange_columns(i, --ncols);
// 	      interchange_rows(r, --nrows);
// 	      i = -1; //start over at the 1st column
// 	    }
// 	}
//     }
// }
// 

void SparseMutableMatrix::perform_reduction(int r, int c, int nr, int nc,
					    int pivot_type)
  // Subroutine of reduce_pivots()
  // pivot_type: 1 means pivot is 1, -1 means pivot is -1, 0 means pivot is unit
{
  // Flip rows r, nr
  // Flip cols c, nc
  // Use (nr,nc) location to remove all terms in columns 0..nc-1
  //   and in row nr.
  // Replace column nc with all zeros, except 1 in nr row.
  interchange_columns(c,nc);
  interchange_rows(r,nr);
  ring_elem pivot = columns_[nc]->coeff;
  if (pivot_type == -1) // pivot is -1
    scale_column(pivot,nc,false);    
  else if (pivot_type == 0)
    divide_column(nc, pivot);
  for (int i=0; i<nc; i++)
    {
      vec p = columns_[i];
      if (p == 0) continue;
      if (p->comp == nr)
	{
	  // Do the reduction
	  ring_elem f = R->negate(p->coeff);
	  column_op(i, f, nc, false);
	}
    }
  R->remove_vec(columns_[nc]);
  columns_[nc] = R->e_sub_i(nr);
}

void SparseMutableMatrix::reduce_pivots()
{
  int nr = n_rows()-1;
  int nc = n_cols()-1;
  if (nr < 0 || nc < 0) return;
  const Ring *K = get_ring();
  ring_elem one = K->one();
  ring_elem minus_one = K->minus_one();

  // After using the pivot element, it is moved to [nrows-1,ncols-1]
  // and nrows and ncols are decremented.

  for (int i=0; i<=nc; i++)
    {
      for (vec p = columns_[i]; p != 0; p = p->next) 
	{
	  int pivot_type = 0;
	  if (K->is_equal(one, p->coeff))
	    pivot_type = 1;
	  else if (K->is_equal(minus_one, p->coeff))
	    pivot_type = -1;
	  if (pivot_type != 0)
	    {
	      perform_reduction(p->comp, i, nr--, nc--, pivot_type);
	      if (nr < 0 || nc < 0) return;
	      // restart loop with the (new) column i
	      i = -1;
	      break;
	    }
	}
    }

  // Now search for other possible pivots
  for (int i=0; i<=nc; i++)
    {
      for (vec p = columns_[i]; p != 0; p = p->next) 
	{
	  if (!K->is_unit(p->coeff)) continue;
	  int pivot_type = 0;
	  if (K->is_equal(one, p->coeff))
	    pivot_type = 1;
	  else if (K->is_equal(minus_one, p->coeff))
	    pivot_type = -1;

	  perform_reduction(p->comp, i, nr--, nc--, pivot_type);
	  if (nr < 0 || nc < 0) return;
	  // restart loop with the (new) column i
	  i = -1;
	  break;
	}
    }
}

#if 0
void SparseMutableMatrix::normalizeColumn(int c, bool doRecording)
{
  if (errorColumnBound(c)) return;
  if (matrix[c] == 0) return;
  if (K->is_ZZ())
    {
      ring_elem a = matrix[c]->coefficient;
      mpz_ptr b = MPZ_VAL(a);
      if (mpz_sgn(b) < 0)
	scaleColumn(c,minus_one,doRecording);
    }
  
}
void SparseMutableMatrix::columnReduce(int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;

  ring_elem a1 = matrix[c1]->coefficient; // Assumed to be non-zero...
  ring_elem a2, b;
  if (!getEntry(matrix[c1]->component,c2,a2))
    return;
  if (!K->is_equal(a1,one))
    {
      ring_elem rem;
      rem = K->remainderAndQuotient(a2,a1,b);
      K->negate_to(b);
      K->remove(rem);
    }
  else
    b = K->negate(a2);
  addColumnMultiple(c1,b,c2,doRecording);
  K->remove(b);
}
void SparseMutableMatrix::row2by2(int r1, int r2, 
				  ring_elem a1, ring_elem a2,
				  ring_elem b1, ring_elem b2,
				  bool doRecording)
{
  for (int i=0; i<ncols; i++)
    V->row2by2(matrix[i],r1,r2,a1,a2,b1,b2);

  if (doRecording && rowOps != 0)
    rowOps->column2by2(r1,r2,a1,a2,b1,b2,false);
}

void SparseMutableMatrix::column2by2(int c1, int c2, 
				     ring_elem a1, ring_elem a2,
				     ring_elem b1, ring_elem b2,
				     bool doRecording)
{
  // do the replacements:
  // new column c1 = a1 * column[c1] + a2 * column[c2]
  // new column c2 = b1 * column[c1] + b2 * column[c2]
  // Make first column: v1 = a1*c1+a2*c2
  sparse_vector *v1 = V->clone(matrix[c1]);
  sparse_vector *v2 = V->clone(matrix[c2]);
  V->scale(v1,a1);
  V->scale(v2,a2);
  V->add(v1,v2);
  // Second column: w1 = b1*c1 + b2*c2
  sparse_vector *w1 = V->clone(matrix[c1]);
  sparse_vector *w2 = V->clone(matrix[c2]);
  V->scale(w1,b1);
  V->scale(w2,b2);
  V->add(w1,w2);
  // Set the matrices:
  V->remove(matrix[c1]);
  V->remove(matrix[c2]);
  matrix[c1] = v1;
  matrix[c2] = w1;
  // Do the recording, if needed:
  if (doRecording && colOps != 0)
    colOps->column2by2(c1,c2,a1,a2,b1,b2,false);
}

void SparseMutableMatrix::gcdRowReduce(int c, int r1, int r2, bool doRecording)
{
  if (errorColumnBound(c)) return;
  if (errorRowBound(r1)) return;
  if (errorRowBound(r2)) return;

  if (!K->has_gcd()) return;
  ring_elem a1,a2,b,b1,b2,d,x,y;
  if (!getEntry(r2,c,a2)) return;
  if (!getEntry(r1,c,a1))
    interchangeRows(r1,r2,doRecording);
  else
    {
      b = K->negate(a2);
      d = K->gcd_extended(a1,a2,x,y);
      b1 = K->divide(a1,d); // exact
      b2 = K->divide(b,d); // exact
      row2by2(r1,r2,x,y,b1,b2,doRecording);
      K->remove(b);
      K->remove(d);
      K->remove(b1);
      K->remove(b2);
      K->remove(x);
      K->remove(y);
    }
}

void SparseMutableMatrix::gcdColumnReduce(int r, int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;
  if (errorRowBound(r)) return;

  if (!K->has_gcd()) return;
  ring_elem a1,a2,b,b1,b2,d,x,y;
  if (!getEntry(c2,r,a2)) return;
  if (!getEntry(c1,r,a1))
    interchangeColumns(c1,c2,doRecording);
  else
    {
      b = K->negate(a2);
      d = K->gcd_extended(a1,a2,x,y);
      b1 = K->divide(a1,d); //exact
      b2 = K->divide(b,d); // exact
      // Make first column: matrix[column c1] = v1 = x*c1+y*c2
      // Second column: matrix[column c2] = w1 = (a1/d) c2 - (a2/d) c1
      column2by2(c1,c2,x,y,b1,b2,doRecording);
      // Clean up
      K->remove(b);
      K->remove(d);
      K->remove(b1);
      K->remove(b2);
      K->remove(x);
      K->remove(y);
    }
}

void SparseMutableMatrix::gcdColumnReduce(int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;

  if (matrix[c1] == 0)
    interchangeColumns(c1,c2,doRecording);
  else
    {
      int r = matrix[c1]->component;
      gcdColumnReduce(r,c1,c2,doRecording);
    }
}

int SparseMutableMatrix::compare_sparse_vectors(sparse_vector *v, sparse_vector *w)
{
  // First compare: lead component.
  // Second: if ZZ: compare using mpz_cmp
  //         if k[x]: compare using simple_degree
  // Null sparse_vectors go first
  if (v == 0)
    {
      if (w == 0) return EQ;
      return LT;
    }
  if (w == 0) return GT;
  int cmp = v->component - w->component;
  if (cmp > 0) return GT;
  if (cmp < 0) return LT;
  if (K->is_ZZ())
    {
      bool negate_a = false;
      bool negate_b = false;
      mpz_ptr a = MPZ_VAL(v->coefficient);
      mpz_ptr b = MPZ_VAL(w->coefficient);
      if (mpz_sgn(a) < 0)
	{
	  negate_a = true;
	  mpz_neg(a,a);
	}
      if (mpz_sgn(b) < 0)
	{
	  negate_b = true;
	  mpz_neg(b,b);
	}
      int result = mpz_cmp(a,b);
      if (negate_a) mpz_neg(a,a);
      if (negate_b) mpz_neg(b,b);
      return -result;
    }
  else
    {
      cmp = K->primary_degree(v->coefficient) - K->primary_degree(w->coefficient);
      if (cmp > 0) return GT;
      if (cmp < 0) return LT;
      return EQ;
    }
}

int SparseMutableMatrix::sort_partition(int lo, int hi, int *sortvals)
{
  sparse_vector *pivot = matrix[sortvals[lo]];
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
      do { j--; }
      while (compare_sparse_vectors(matrix[sortvals[j]], pivot) > 0);

      do { i++; }
      while (compare_sparse_vectors(matrix[sortvals[i]], pivot) < 0);

      if (i < j)
	{
	  int tmp = sortvals[j];
	  sortvals[j] = sortvals[i];
	  sortvals[i] = tmp;
	}
      else
	return j;
    }
}

void SparseMutableMatrix::sort1(int lo, int hi, int *sortvals)
{
  if (lo < hi)
    {
      int q = sort_partition(lo, hi, sortvals);
      sort1(lo, q, sortvals);
      sort1(q+1, hi, sortvals);
    }
}

void SparseMutableMatrix::sortColumns(int lo, int hi, bool doRecording)
{
  if (errorColumnBound(lo)) return;
  if (errorColumnBound(hi)) return;
  // Sort columns.
  if (lo >= hi) return;
  int *sortvals = newarray(int,ncols);
  for (int i=lo; i<=hi; i++)
    sortvals[i] = i;
  sort1(lo,hi,sortvals);

  // Now permute the columns of 'matrix', and 'colOps'.
  permuteColumns(lo,hi,sortvals,doRecording);
  deletearray(sortvals);
}

void SparseMutableMatrix::permuteColumns(int lo, int hi, int *permutation, bool doRecording)
{
  int i;
  if (errorColumnBound(lo)) return;
  if (errorColumnBound(hi)) return;
  sparse_vector **old = newarray(sparse_vector *,hi-lo+1);
  for (i=lo; i<=hi; i++)
    old[i-lo] = matrix[i];
  for (i=lo; i<=hi; i++)
    matrix[i] = old[permutation[i]-lo];
  deletearray(old);

  if (doRecording && colOps != 0)
    colOps->permuteColumns(lo,hi,permutation,false);
}

void SparseMutableMatrix::text_out(buffer &o) const
{
  buffer *p = newarray(buffer,nrows);
  int r;
  for (int c=0; c<ncols; c++)
    {
      int maxcount = 0;
      for (r=0; r<nrows; r++)
	{
	  ring_elem f;
	  if (getEntry(r,c,f))
	    K->elem_text_out(p[r], f);
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

void SparseMutableMatrix::display() const
{
  buffer o;
  text_out(o);
  o << newline;
  emit(o.str());
}
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
