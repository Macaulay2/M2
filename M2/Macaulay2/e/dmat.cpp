// Copyright 2005  Michael E. Stillman

#include "coeffrings.hpp"
#include "z_mod_p.hpp"
#include "dmat.hpp"

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const RingType *R0, int nrows, int ncols)
  : R(R0),
    coeffR(R0->get_CoeffRing()),
    nrows_(nrows),
    ncols_(ncols)
{
  initialize(nrows,ncols,0);
}

template<typename CoeffRing>
void DMat<CoeffRing>::initialize(int nrows, int ncols, elem *array)
{
  nrows_ = nrows;
  ncols_ = ncols;
  int len = nrows * ncols;
  array_ = newarray(elem,len);
  if (array == 0)
    {
      for (int i=0; i<len; i++)
	coeffR->set_zero(array_[i]);
    }
  else
    for (int i=0; i<len; i++)
      coeffR->init_set(array_[i], array[i]);
}

template<typename CoeffRing>
void DMat<CoeffRing>::resize(int new_nrows, int new_ncols)
{
  int new_len = new_nrows * new_ncols;
  if (new_len != nrows_ * ncols_)
    initialize(new_nrows, new_ncols, 0);
  else
    for (int i=0; i<new_len; i++)
      coeffR->set_zero(array_[i]);
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_matrix(const DMat<CoeffRing> *mat0)
{
  initialize(mat0->n_rows(), mat0->n_cols(), mat0->get_array());
}


template<typename CoeffRing>
DMat<CoeffRing> *DMat<CoeffRing>::copy() const
{
  DMat<CoeffRing> *result = new DMat<CoeffRing>(get_ring(), 0, 0);
  result->initialize(nrows_, ncols_, array_);
  return result;
}

template<typename CoeffRing>
int DMat<CoeffRing>::lead_row(int col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
{
  elem *last = array_ + nrows_ * col;
  elem *loc = last + nrows_ - 1;
  for ( ; loc >= last; loc--)
    {
      if (!coeffR->is_zero(*loc))
	return loc-last;
    }
  return -1;
}

template<typename CoeffRing>
int DMat<CoeffRing>::lead_row(int col, elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
{
  elem *last = array_ + nrows_ * col;
  elem *loc = last + nrows_ - 1;
  for ( ; loc > last; loc--)
    {
      if (!coeffR->is_zero(*loc))
	{
	  result = *loc;
	  return loc-last;
	}
    }
  return -1;
}

///////////////////////////////
// Row and column operations //
///////////////////////////////

template<typename CoeffRing>
bool DMat<CoeffRing>::get_entry(int r, int c, elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  int loc = c * nrows_ + r;
  coeffR->init_set(result, array_[loc]);
  return !coeffR->is_zero(result);
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_entry(int r, int c, const elem a)
{
  int loc = c * nrows_ + r;
  array_[loc] = a;
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_rows(int i, int j)
  /* swap rows: row(i) <--> row(j) */
{
  elem *loc1 = array_ + i;
  elem *loc2 = array_ + j;

  for (int c=0; c<ncols_; c++)
    {
      elem tmp = *loc1;
      *loc1 = *loc2;
      *loc2 = tmp;
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_columns(int i, int j)
  /* swap columns: column(i) <--> column(j) */
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;
  for (int r=0; r<nrows_; r++)
    {
      elem tmp = *loc1;
      *loc1++ = *loc2;
      *loc2++ = tmp;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_row(elem r, int i)
  /* row(i) <- r * row(i) */
{
  elem *loc = array_ + i;
  for (int c=0; c<ncols_; c++)
    {
      coeffR->mult(*loc, r, *loc); // *loc = r * *loc
      loc += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_column(elem r, int i)
  /* column(i) <- r * column(i) */
{
  elem *loc = array_ + nrows_*i;
  for (int a=0; a<nrows_; a++)
    {
      coeffR->mult(*loc, r, *loc);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_row(int i, elem r)
  /* row(i) <- row(i) / r */
{
  elem *loc = array_ + i;
  for (int c=0; c<ncols_; c++)
    {
      coeffR->divide(*loc, *loc, r);
      loc += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_column(int i, elem r)
  /* column(i) <- column(i) / r */
{
  elem *loc = array_ + nrows_*i;
  for (int a=0; a<nrows_; a++)
    {
      coeffR->divide(*loc, *loc, r);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row_op(int i, elem r, int j)
  /* row(i) <- row(i) + r * row(j) */
{
  elem *loc1 = array_ + i;
  elem *loc2 = array_ + j;

  for (int c=0; c<ncols_; c++)
    {
      elem f;
      coeffR->mult(f,r,*loc2);
      coeffR->add(*loc1, f, *loc1);
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::column_op(int i, elem r, int j)
  /* column(i) <- column(i) + r * column(j) */
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;

  for (int a=0; a<nrows_; a++)
    {
      elem f;
      coeffR->mult(f,r,*loc2);
      coeffR->add(*loc1, *loc1, f);
      loc1++;
      loc2++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row2by2(int r1, int r2, 
	       elem a1, elem a2,
	       elem b1, elem b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  elem *loc1 = array_ + r1;
  elem *loc2 = array_ + r2;
  for (int i=0; i<ncols_; i++)
    {
      elem f1,f2,g1,g2;

      coeffR->mult(f1,a1,*loc1);
      coeffR->mult(f2,a2,*loc2);
      coeffR->mult(g1,b1,*loc1);
      coeffR->mult(g2,b2,*loc2);

      coeffR->add(f1,f1,f2);
      coeffR->add(g1,g1,g2);
      *loc1 = f1;
      *loc2 = g1;
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::column2by2(int c1, int c2,
		  elem a1, elem a2,
		  elem b1, elem b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  elem *loc1 = array_ + c1 * nrows_;
  elem *loc2 = array_ + c2 * nrows_;

  for (int i=0; i<nrows_; i++)
    {
      elem f1,f2,g1,g2;

      coeffR->mult(f1,a1,*loc1);
      coeffR->mult(f2,a2,*loc2);
      coeffR->mult(g1,b1,*loc1);
      coeffR->mult(g2,b2,*loc2);

      coeffR->add(f1,f1,f2);
      coeffR->add(g1,g1,g2);
      *loc1++ = f1;
      *loc2++ = g1;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::dot_product(int i, int j, elem &result) const
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;
  coeffR->set_zero(result);
  for (int r=0; r<nrows_; r++)
    {
      elem f;
      coeffR->mult(f,*loc1++,*loc2++);
      coeffR->add(result,result, f);
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::copy_elems(int n_to_copy, elem *target, int target_stride, elem *source, int stride)
{
  for (int i=0; i<n_to_copy; i++)
    {
      *target = *source;
      target += target_stride;
      source += stride;
    }
}

template<typename CoeffRing>
bool DMat<CoeffRing>::row_permute(int start_row, const M2_arrayint perm)
{
  // We copy one row to another location for each cycle in 'perm' of length > 1.
  int nrows_to_permute = perm->len;
  bool *done = newarray_atomic(bool,nrows_to_permute);
  for (int i=0; i<nrows_to_permute; i++)
    done[i] = true;
  for (int i=0; i<nrows_to_permute; i++)
    {
      int j = perm->array[i];
      if (!done[j])
	{
	  ERROR("expected permutation");
	  deletearray(done);
	  return false;
	}
      done[j] = false;
    }
  elem *tmp = newarray(elem,ncols_);
  int next = 0;
  elem *arr = array_ + start_row;

  while (next < nrows_to_permute)
    {
      if (done[next] || perm->array[next] == next)
	{
	  next++;
	}
      else
	{
	  // store row 'next' into tmp
	  copy_elems(ncols_,tmp,1,arr + next, nrows_);
	  
	  int r = next;
	  for (;;)
	    {
	      // copy row perm[r] to row r
	      copy_elems(ncols_, arr + r, nrows_, arr + perm->array[r], nrows_);
	      done[r] = true;
	      int next_r = perm->array[r];
	      if (next_r == next) break; // and so r is the previous one
	      r = perm->array[r];
	    }
	  // Now copy tmp back
	  copy_elems(ncols_, arr + r, nrows_, tmp, 1);
	  done[r] = true;
	}
    }
  deletearray(tmp);
  deletearray(done);
  return true;
}

template<typename CoeffRing>
bool DMat<CoeffRing>::column_permute(int start_col, const M2_arrayint perm)
{
  // We copy one column to another location for each cycle in 'perm' of length > 1.
  int ncols_to_permute = perm->len;
  bool *done = newarray_atomic(bool,ncols_to_permute);
  for (int i=0; i<ncols_to_permute; i++)
    done[i] = false;
  for (int i=0; i<ncols_to_permute; i++)
    {
      int j = perm->array[i];
      if (!done[j])
	{
	  ERROR("expected permutation");
	  deletearray(done);
	  return false;
	}
      done[j] = false;
    }
  elem *tmp = newarray(elem,nrows_);
  int next = 0;
  elem *arr = array_ + start_col * nrows_;

  while (next < ncols_to_permute)
    {
      if (done[next] || perm->array[next] == next)
	{
	  next++;
	}
      else
	{
	  // store col 'next' into tmp
	  copy_elems(nrows_,tmp,1,arr + next * nrows_, 1);
	  
	  int r = next;
	  for (;;)
	    {
	      // copy col perm[r] to col r
	      copy_elems(nrows_, arr + r * nrows_, 1, arr + perm->array[r] * nrows_, 1);
	      done[r] = true;
	      int next_r = perm->array[r];
	      if (next_r == next) break; // and so r is the previous one
	      r = perm->array[r];
	    }
	  // Now copy tmp back
	  copy_elems(nrows_, arr + r * nrows_, 1, tmp, 1);
	  done[r] = true;
	}
    }
  deletearray(tmp);
  deletearray(done);
  return true;
}


#if 0
template<typename CoeffRing>
bool DMat<CoeffRing>::is_zero() const
{
#warning "to be written"
  return 0;
}

template<typename CoeffRing>
bool DMat<CoeffRing>::set_values(M2_arrayint rows,
				 M2_arrayint cols,
				 RingElement_array *values)
{
#warning "to be written"
  return 0;
}

template<typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::submatrix(const M2_arrayint rows, 
					     const M2_arrayint cols) const
{
  DMat<CoeffRing> *result = DMat<CoeffRing>::zero_matrix(R,rows->len,cols->len);
  for (int r=0; r<rows->len; r++)
    for (int c=0; c<cols->len; c++)
      {
	elem f;
	get_entry(rows->array[r],cols->array[c],f);
	result->set_entry(r,c,f);
      }
  return result;
}

template<typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::submatrix(const M2_arrayint cols) const
{
#warning "to be written"
  return 0;
}



bool DenseMutableMatrixRR::is_equal(const MutableMatrix *B) const
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
#endif


template class DMat<CoefficientRingZZp>;
template class DMat<CoefficientRingRR>;
template class DMat<CoefficientRingCC>;

double *DMat<CoefficientRingRR>::get_lapack_array() const
{
  return array_;
}
double *DMat<CoefficientRingCC>::get_lapack_array() const
{
  return reinterpret_cast<double *>(array_);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
