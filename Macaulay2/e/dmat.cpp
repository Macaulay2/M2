// Copyright 2005  Michael E. Stillman

#include "coeffrings.hpp"
#include "ZZp.hpp"
#include "dmat.hpp"
#include "mat.hpp"

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const RingType *R0, long nrows, long ncols)
  : R(R0),
    coeffR(R0->get_CoeffRing()),
    nrows_(nrows),
    ncols_(ncols)
{
  initialize(nrows,ncols,0);
}

template <> DMat<CoefficientRingR>::DMat(const Ring *R0, long nrows, long ncols)
  : R(R0),
    coeffR(0),
    nrows_(nrows),
    ncols_(ncols)
{
  coeffR = new CoefficientRingR(R0);
  initialize(nrows,ncols,0);
}

template<typename CoeffRing>
void DMat<CoeffRing>::initialize(long nrows, long ncols, elem *array)
{
  nrows_ = nrows;
  ncols_ = ncols;
  long len = nrows * ncols;
  array_ = newarray_clear(elem,len);
  if (array == 0)
    {
      for (long i=0; i<len; i++)
	coeffR->set_zero(array_[i]);
    }
  else
    for (long i=0; i<len; i++)
      coeffR->init_set(array_[i], array[i]);
}

template<typename CoeffRing>
void DMat<CoeffRing>::resize(long new_nrows, long new_ncols)
{
  long new_len = new_nrows * new_ncols;
  if (new_len != nrows_ * ncols_)
    initialize(new_nrows, new_ncols, 0);
  else
    for (long i=0; i<new_len; i++)
      coeffR->set_zero(array_[i]);
}

template<typename CoeffRing>
double * DMat<CoeffRing>::get_lapack_array() const
{
  return 0;
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_matrix(const DMat<CoeffRing> *mat0)
{
  initialize(mat0->n_rows(), mat0->n_cols(), mat0->get_array());
}

template<typename CoeffRing>
void DMat<CoeffRing>::grab(DMat<CoeffRing> *M)
{
  std::swap(R,M->R);
  std::swap(coeffR,M->coeffR);
  std::swap(nrows_,M->nrows_);
  std::swap(ncols_,M->ncols_);
  std::swap(array_, M->array_);
}

template<typename CoeffRing>
DMat<CoeffRing> *DMat<CoeffRing>::copy() const
{
  DMat<CoeffRing> *result = new DMat<CoeffRing>(get_ring(), 0, 0);
  result->initialize(nrows_, ncols_, array_);
  return result;
}

template<typename CoeffRing>
long DMat<CoeffRing>::lead_row(long col) const
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
long DMat<CoeffRing>::lead_row(long col, elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
{
  elem *last = array_ + nrows_ * col;
  elem *loc = last + nrows_ - 1;
  for ( ; loc >= last; loc--)
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
bool DMat<CoeffRing>::get_entry(long r, long c, elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  long loc = c * nrows_ + r;
  coeffR->init_set(result, array_[loc]);
  return !coeffR->is_zero(result);
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_entry(long r, long c, const elem &a)
{
  long loc = c * nrows_ + r;
  array_[loc] = a;
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_rows(long i, long j)
  /* swap rows: row(i) <--> row(j) */
{
  elem *loc1 = array_ + i;
  elem *loc2 = array_ + j;

  for (long c=0; c<ncols_; c++)
    {
      elem tmp = *loc1;
      *loc1 = *loc2;
      *loc2 = tmp;
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_columns(long i, long j)
  /* swap columns: column(i) <--> column(j) */
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;
  for (long r=0; r<nrows_; r++)
    {
      elem tmp = *loc1;
      *loc1++ = *loc2;
      *loc2++ = tmp;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_row(long i, const elem &r)
  /* row(i) <- r * row(i) */
{
  elem *loc = array_ + i;
  for (long c=0; c<ncols_; c++)
    {
      coeffR->mult(*loc, r, *loc); // *loc = r * *loc
      loc += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_column(long i, const elem &r)
  /* column(i) <- r * column(i) */
{
  elem *loc = array_ + nrows_*i;
  for (long a=0; a<nrows_; a++)
    {
      coeffR->mult(*loc, r, *loc);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_row(long i, const elem &r)
  /* row(i) <- row(i) / r */
{
  elem *loc = array_ + i;
  for (long c=0; c<ncols_; c++)
    {
      coeffR->divide(*loc, *loc, r);
      loc += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_column(long i, const elem &r)
  /* column(i) <- column(i) / r */
{
  elem *loc = array_ + nrows_*i;
  for (long a=0; a<nrows_; a++)
    {
      coeffR->divide(*loc, *loc, r);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row_op(long i, const elem &r, long j)
  /* row(i) <- row(i) + r * row(j) */
{
  elem *loc1 = array_ + i;
  elem *loc2 = array_ + j;

  for (long c=0; c<ncols_; c++)
    {
      elem f;
      coeffR->mult(f,r,*loc2);
      coeffR->add(*loc1, f, *loc1);
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::column_op(long i, const elem &r, long j)
  /* column(i) <- column(i) + r * column(j) */
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;

  for (long a=0; a<nrows_; a++)
    {
      elem f;
      coeffR->mult(f,r,*loc2);
      coeffR->add(*loc1, *loc1, f);
      loc1++;
      loc2++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row2by2(long r1, long r2, 
	       const elem &a1, const elem &a2,
	       const elem &b1, const elem &b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  elem *loc1 = array_ + r1;
  elem *loc2 = array_ + r2;
  for (long i=0; i<ncols_; i++)
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
void DMat<CoeffRing>::column2by2(long c1, long c2,
		  const elem &a1, const elem &a2,
		  const elem &b1, const elem &b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  elem *loc1 = array_ + c1 * nrows_;
  elem *loc2 = array_ + c2 * nrows_;

  for (long i=0; i<nrows_; i++)
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
void DMat<CoeffRing>::dot_product(long i, long j, elem &result) const
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;
  coeffR->set_zero(result);
  for (long r=0; r<nrows_; r++)
    {
      elem f;
      coeffR->mult(f,*loc1++,*loc2++);
      coeffR->add(result,result, f);
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::copy_elems(long n_to_copy, elem *target, long target_stride, elem *source, long stride)
{
  for (long i=0; i<n_to_copy; i++)
    {
      *target = *source;
      target += target_stride;
      source += stride;
    }
}

template<typename CoeffRing>
bool DMat<CoeffRing>::row_permute(long start_row, M2_arrayint perm)
{
  // We copy one row to another location for each cycle in 'perm' of length > 1.
  long nrows_to_permute = perm->len;
  bool *done = newarray_atomic(bool,nrows_to_permute);
  for (long i=0; i<nrows_to_permute; i++)
    done[i] = true;
  for (long i=0; i<nrows_to_permute; i++)
    {
      long j = perm->array[i];
      if (!done[j])
	{
	  ERROR("expected permutation");
	  deletearray(done);
	  return false;
	}
      done[j] = false;
    }
  elem *tmp = newarray_clear(elem,ncols_);
  long next = 0;
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
	  
	  long r = next;
	  for (;;)
	    {
	      // copy row perm[r] to row r
	      copy_elems(ncols_, arr + r, nrows_, arr + perm->array[r], nrows_);
	      done[r] = true;
	      long next_r = perm->array[r];
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
bool DMat<CoeffRing>::column_permute(long start_col, M2_arrayint perm)
{
  // We copy one column to another location for each cycle in 'perm' of length > 1.
  long ncols_to_permute = perm->len;
  bool *done = newarray_atomic(bool,ncols_to_permute);
  for (long i=0; i<ncols_to_permute; i++)
    done[i] = true;
  for (long i=0; i<ncols_to_permute; i++)
    {
      long j = perm->array[i];
      if (!done[j])
	{
	  ERROR("expected permutation");
	  deletearray(done);
	  return false;
	}
      done[j] = false;
    }
  elem *tmp = newarray_clear(elem,nrows_);
  long next = 0;
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
	  
	  long r = next;
	  for (;;)
	    {
	      // copy col perm[r] to col r
	      copy_elems(nrows_, arr + r * nrows_, 1, arr + perm->array[r] * nrows_, 1);
	      done[r] = true;
	      long next_r = perm->array[r];
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

template<typename CoeffRing>
void DMat<CoeffRing>::insert_columns(long i, long n_to_add) 
/* Insert n_to_add columns directly BEFORE column i. */
{
  elem *tmp = array_;
  long nbefore = i * nrows_;
  long nadded = n_to_add * nrows_;
  long nelems = ncols_ * nrows_;

  ncols_ += n_to_add;
  long len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);
  for (long j=0; j<nbefore; j++)
    coeffR->swap(tmp[j], array_[j]);

  for (long j=0; j<nadded; j++)
    coeffR->set_zero(array_[j+nbefore]);
  for (long j=nbefore; j<nelems; j++)
    coeffR->swap(tmp[j], array_[j+nadded]);
  deletearray(tmp);
}

template<typename CoeffRing>
void DMat<CoeffRing>::insert_rows(long i, long n_to_add)
/* Insert n_to_add rows directly BEFORE row i. */
{
  elem *tmp = array_;
  elem zero;
  coeffR->set_zero(zero);
  int old_nrows = nrows_;

  nrows_ += n_to_add;
  long len = nrows_ * ncols_;
  array_ = newarray_clear(elem, len);
  for (long r=0; r<i; r++)
    copy_elems(ncols_, array_ + r, nrows_, tmp + r, old_nrows);
  for (long r=i; r<i+n_to_add; r++)
    copy_elems(ncols_, array_ + r, nrows_, &zero, 0);
  for (long r=i; r<old_nrows; r++)
    copy_elems(ncols_, array_ + r + n_to_add, nrows_, tmp + r, old_nrows);
  deletearray(tmp);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_columns(long i, long j)
/* Delete columns i .. j from M */
{
  elem *tmp = array_;
  long nbefore = i * nrows_;
  long ndeleted = (j-i+1) * nrows_;
  long nelems = ncols_ * nrows_;

  ncols_ -= j-i+1;
  long len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);
  for (long k=0; k<nbefore; k++)
    coeffR->swap(tmp[k], array_[k]);
  for (long k=nbefore+ndeleted; k<nelems; k++)
    coeffR->swap(tmp[k], array_[k-ndeleted]);
  deletearray(tmp);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_rows(long i, long j)
/* Delete rows i .. j from M */
{
  elem *tmp = array_;
  int ndeleted = j-i+1;
  int old_nrows = nrows_;

  nrows_ -= ndeleted;
  long len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);
  for (long r=0; r<i; r++)
    copy_elems(ncols_, array_ + r, nrows_, tmp + r, old_nrows);
  for (long r=j+1; r<old_nrows; r++)
    copy_elems(ncols_, array_ + r - ndeleted, nrows_, tmp + r, old_nrows);
  deletearray(tmp);
}

template<typename CoeffRing>
bool DMat<CoeffRing>::set_submatrix(M2_arrayint rows, 
				    M2_arrayint cols, 
				    const MutableMatrix *M)
{
  MutableMatrix::iterator *i = M->begin();
#ifdef DEVELOPMENT
#warning "set_submatrix"
#endif
#if 0
//   elem *first = array_ + first_row + nrows_ * first_col;
//   long ncols = M->n_cols();
//   for (long c=0; c<ncols; c++)
//     {
//       for (i->set(c); i->valid(); i->next())
// 	{
// 	  ring_elem a;
// 	  i->copy_ring_elem(a);
// 	  coeffR->from_ring_elem(*(first + i->row()), a);
// 	}
//       first += nrows_;
//     }
#endif
  delete i;
  return true;
}

template<typename CoeffRing>
bool DMat<CoeffRing>::is_zero() const
{
  for (elem *t = array_ + nrows_*ncols_ - 1; t >= array_; t--)
    if (!coeffR->is_zero(*t))
      return false;
  return true;
}

template<typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::submatrix(M2_arrayint rows, 
					     M2_arrayint cols) const
{
  DMat<CoeffRing> *result = new DMat<CoeffRing>(R,rows->len,cols->len);
  for (long r=0; r<rows->len; r++)
    for (long c=0; c<cols->len; c++)
      {
	elem f;
	get_entry(rows->array[r],cols->array[c],f);
	result->set_entry(r,c,f);
      }
  return result;
}

template<typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::submatrix(M2_arrayint cols) const
{
  DMat<CoeffRing> *result = new DMat<CoeffRing>(R,nrows_,cols->len);
  for (long r=0; r<nrows_; r++)
    for (long c=0; c<cols->len; c++)
      {
	elem f;
	get_entry(r,cols->array[c],f);
	result->set_entry(r,c,f);
      }
  return result;
}

template <typename CoeffRing>
bool DMat<CoeffRing>::is_equal(const MutableMatrix *B) const
{
  if (B->get_ring() != get_ring()) return false;
  if (B->n_rows() != n_rows()) return false;
  if (B->n_cols() != n_cols()) return false;
  MutableMatrix::iterator *i = B->begin();
  iterator j(this);

  for (long c=0; c<n_cols(); c++)
    {
      i->set(c);
      j.set(c);
      for (;;)
	{
	  if (!i->valid())
	    {
	      if (j.valid()) return false;
	      break;
	    }
	  else if (!j.valid()) return false;
	  // i->valid(), j.valid() are both true
	  if (i->row() != j.row())
	    return false;
	  ring_elem a, b;
	  i->copy_ring_elem(a);
	  coeffR->to_ring_elem(b, j.value());
	  if (!get_ring()->is_equal(a,b))
	    return false;
	  i->next();
	  j.next();
	}
    }
  return true;
}

template <typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
#ifdef DEVELOPMENT
  DMat<CoeffRing> *result = copy();
  for (long c=0; c<B->n_cols(); c++)
    {
    }
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::mult(const MutableMatrix *B) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::mult(const elem &f) const
// return f*this.  return NULL of sizes or types do not match.
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
DMat<CoeffRing> * DMat<CoeffRing>::negate() const
{
  elem zero;
  coeffR->set_zero(zero);
  DMat *result = copy();
  elem *a = result->get_array();
  elem *end = result->get_array() + n_rows() * n_cols();
  for ( ; a < end; a++)
    coeffR->subtract(*a, zero, *a);
  return result;
}

template <> double *DMat<CoefficientRingRRR>::make_lapack_array() const
{
  long len = n_rows() * n_cols();
  double *result = newarray_atomic(double, len);

  elem *a = array_;
  double *p = result;
  for (long i=0; i<len; i++)
    *p++ = mpfr_get_d(a++, GMP_RNDN);
  return result;
}

template <> void DMat<CoefficientRingRRR>::fill_from_lapack_array(double *lapack_array)
{
  long len = n_rows() * n_cols();

  elem *a = array_;
  double *p = lapack_array;
  for (long i=0; i<len; i++)
    mpfr_set_d(a++, *p++, GMP_RNDN);
}

template <> double *DMat<CoefficientRingCCC>::make_lapack_array() const
{
  long len = 2 * n_rows() * n_cols();
  double *result = newarray_atomic(double, len);

  elem *a = array_;
  double *p = result;
  for (long i=0; i<len; i++)
    {
      *p++ = mpfr_get_d(a->re, GMP_RNDN);
      *p++ = mpfr_get_d(a->im, GMP_RNDN);
      a++;
    }
  return result;
}

template <> void DMat<CoefficientRingCCC>::fill_from_lapack_array(double *lapack_array)
{
  long len = n_rows() * n_cols();

  elem *a = array_;
  double *p = lapack_array;
  for (long i=0; i<len; i++)
    {
      mpfr_set_d(a->re, *p++, GMP_RNDN);
      mpfr_set_d(a->im, *p++, GMP_RNDN);
      a++;
    }
}

template class DMat<CoefficientRingZZ_NTL>;
template class DMat<CoefficientRingZZp>;
template class DMat<CoefficientRingRRR>;
template class DMat<CoefficientRingCCC>;
template class DMat<CoefficientRingR>;


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
