// Copyright 2005  Michael E. Stillman

#include "coeffrings.hpp"
#include "coeffrings-zz.hpp"
#include "ZZp.hpp"
#include "aring-gf.hpp"
#include "aring-m2-gf.hpp"
#include "dmat.hpp"
#include "mat.hpp"
#include "mpfr.h"
#ifdef HAVE_MPACK
#include <mpack/mblas_mpfr.h>
#include <mpack/mlapack_mpfr.h>
#endif
#include <iostream>

#include "aring-zzp.hpp"
#include "aring-ffpack.hpp"
 #include <typeinfo>


template<typename CoeffRing>
DMat<CoeffRing>::DMat(const Ring *R0, const CoeffRing *coeffR0, int nrows, int ncols)
  : R(R0),
    coeffR(coeffR0),
    nrows_(nrows),
    ncols_(ncols)
{
  initialize(nrows,ncols,0);
}

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const DMat<CoeffRing> &m, size_t nrows, size_t ncols)
  : R(m.R),
    coeffR(m.coeffR),
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
void DMat<CoeffRing>::resize(int new_nrows, int new_ncols)
{
  long new_len = new_nrows * new_ncols;
  if (new_len == 0 || new_len != nrows_ * ncols_)
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
  DMat<CoeffRing> *result = new DMat<CoeffRing>(get_ring(), get_CoeffRing(), 0, 0);
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
        return static_cast<int>(loc-last);
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
  for ( ; loc >= last; loc--)
    {
      if (!coeffR->is_zero(*loc))
        {
          coeffR->set(result, *loc);
          return static_cast<int>(loc-last);
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
  long loc = c * nrows_ + r;
  coeffR->init_set(result, array_[loc]);
  return !coeffR->is_zero(result);
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_entry(int r, int c, const elem &a)
{
  long loc = c * nrows_ + r;
  coeffR->set(array_[loc], a);
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
void DMat<CoeffRing>::scale_row(int i, const elem &r)
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
void DMat<CoeffRing>::scale_column(int i, const elem &r)
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
void DMat<CoeffRing>::divide_row(int i, const elem &r)
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
void DMat<CoeffRing>::divide_column(int i, const elem &r)
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
void DMat<CoeffRing>::row_op(int i, const elem &r, int j)
  /* row(i) <- row(i) + r * row(j) */
{
  elem *loc1 = array_ + i;
  elem *loc2 = array_ + j;

  elem f;
  coeffR->set_zero(f);
  for (int c=0; c<ncols_; c++)
    {
      coeffR->mult(f,r,*loc2);
      coeffR->add(*loc1, f, *loc1);
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::column_op(int i, const elem &r, int j)
  /* column(i) <- column(i) + r * column(j) */
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;

  elem f;
  coeffR->set_zero(f);
  for (int a=0; a<nrows_; a++)
    {
      coeffR->mult(f,r,*loc2);
      coeffR->add(*loc1, *loc1, f);
      loc1++;
      loc2++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row2by2(int r1, int r2,
               const elem &a1, const elem &a2,
               const elem &b1, const elem &b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  elem *loc1 = array_ + r1;
  elem *loc2 = array_ + r2;

  elem f1,f2,g1,g2;
  coeffR->set_zero(f1);
  coeffR->set_zero(f2);
  coeffR->set_zero(g1);
  coeffR->set_zero(g2);
  for (int i=0; i<ncols_; i++)
    {
      coeffR->mult(f1,a1,*loc1);
      coeffR->mult(f2,a2,*loc2);
      coeffR->mult(g1,b1,*loc1);
      coeffR->mult(g2,b2,*loc2);

      coeffR->add(f1,f1,f2);
      coeffR->add(g1,g1,g2);
      coeffR->set(*loc1, f1);
      coeffR->set(*loc2, g1);
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::column2by2(int c1, int c2,
                  const elem &a1, const elem &a2,
                  const elem &b1, const elem &b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  elem *loc1 = array_ + c1 * nrows_;
  elem *loc2 = array_ + c2 * nrows_;

  elem f1,f2,g1,g2;
  coeffR->set_zero(f1);
  coeffR->set_zero(f2);
  coeffR->set_zero(g1);
  coeffR->set_zero(g2);
  for (int i=0; i<nrows_; i++)
    {
      coeffR->mult(f1,a1,*loc1);
      coeffR->mult(f2,a2,*loc2);
      coeffR->mult(g1,b1,*loc1);
      coeffR->mult(g2,b2,*loc2);

      coeffR->add(f1,f1,f2);
      coeffR->add(g1,g1,g2);
      coeffR->set(*loc1++, f1);
      coeffR->set(*loc2++, g1);
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::dot_product(int i, int j, elem &result) const
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;
  coeffR->set_zero(result);

  elem f;
  coeffR->set_zero(f);
  for (int r=0; r<nrows_; r++)
    {
      coeffR->mult(f,*loc1++,*loc2++);
      coeffR->add(result,result, f);
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::copy_elems(long n_to_copy, elem *target, int target_stride, const elem *source, int stride) const
{
  for (long i=0; i<n_to_copy; i++)
    {
      *target = *source;
      target += target_stride;
      source += stride;
    }
}

template<typename CoeffRing>
bool DMat<CoeffRing>::row_permute(int start_row, M2_arrayint perm)
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
  elem *tmp = newarray_clear(elem,ncols_);
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
bool DMat<CoeffRing>::column_permute(int start_col, M2_arrayint perm)
{
  // We copy one column to another location for each cycle in 'perm' of length > 1.
  int ncols_to_permute = perm->len;
  bool *done = newarray_atomic(bool,ncols_to_permute);
  for (int i=0; i<ncols_to_permute; i++)
    done[i] = true;
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
  elem *tmp = newarray_clear(elem,nrows_);
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

template<typename CoeffRing>
void DMat<CoeffRing>::insert_columns(int i, int n_to_add)
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
void DMat<CoeffRing>::insert_rows(int i, int n_to_add)
/* Insert n_to_add rows directly BEFORE row i. */
{
  elem *tmp = array_;
  elem zero;
  coeffR->set_zero(zero);
  int old_nrows = nrows_;

  nrows_ += n_to_add;
  long len = nrows_ * ncols_;
  array_ = newarray_clear(elem, len);
  for (int r=0; r<i; r++)
    copy_elems(ncols_, array_ + r, nrows_, tmp + r, old_nrows);
  for (int r=i; r<i+n_to_add; r++)
    copy_elems(ncols_, array_ + r, nrows_, &zero, 0);
  for (int r=i; r<old_nrows; r++)
    copy_elems(ncols_, array_ + r + n_to_add, nrows_, tmp + r, old_nrows);
  deletearray(tmp);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_columns(int i, int j)
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
void DMat<CoeffRing>::delete_rows(int i, int j)
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
//      {
//        ring_elem a;
//        i->copy_ring_elem(a);
//        coeffR->from_ring_elem(*(first + i->row()), a);
//      }
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
  DMat<CoeffRing> *result = new DMat<CoeffRing>(get_ring(), get_CoeffRing(),rows->len,cols->len);
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
DMat<CoeffRing> * DMat<CoeffRing>::submatrix(M2_arrayint cols) const
{
  DMat<CoeffRing> *result = new DMat<CoeffRing>(get_ring(), get_CoeffRing(),nrows_,cols->len);
  for (int r=0; r<nrows_; r++)
    for (int c=0; c<cols->len; c++)
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

  for (int c=0; c<n_cols(); c++)
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
  for (int c=0; c<B->n_cols(); c++)
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
  long len = n_rows() * n_cols();
  double *result = newarray_atomic(double, 2*len);

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

template <> __mpfr_struct *DMat<CoefficientRingCCC>::make_mpack_array() const // why is this here???
{
  long len = n_rows() * n_cols();
  __mpfr_struct *result = new __mpfr_struct[2*len];

  elem *a = array_;
  __mpfr_struct *p = result;

 //std::cout<<"inside make_mpack"<<std::endl;
 for (long i=0; i<len; i++)
   {
     mpfr_init(p);
     mpfr_set(p, a->re, GMP_RNDN);
     p++;
     a++;
   }

 a =array_;
 for (long i=len; i< 2*len; i++)
   {
     mpfr_init(p);
     mpfr_set(p, a->im, GMP_RNDN);
     //*p= *(a->im);
     p++;
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

///////////////////////////////////
/// Fast linear algebra routines //
///////////////////////////////////

template<typename CoeffRing>
size_t DMat<CoeffRing>::rank() const
{
  ERROR("not implemented for this ring yet");
  return static_cast<size_t>(-1);
}

template<typename CoeffRing>
void DMat<CoeffRing>::determinant(elem &result) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
bool DMat<CoeffRing>::invert(DMat<CoeffRing> &inverse) const
{
  ERROR("not implemented for this ring yet");
  return false;
}

template<typename CoeffRing>
M2_arrayintOrNull DMat<CoeffRing>::rankProfile(bool row_profile) const
{
  ERROR("not implemented for this ring yet");
  return 0;
}

template<typename CoeffRing>
void DMat<CoeffRing>::nullSpace(DMat<CoeffRing> &nullspace, bool right_side) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
bool DMat<CoeffRing>::solveLinear(DMat<CoeffRing> &X, const DMat<CoeffRing> &B, bool right_size) const
{
  ERROR("not implemented for this ring yet");
  return false;
}

template<typename CoeffRing>
void DMat<CoeffRing>::addMultipleTo(const DMat<CoeffRing> &A,
                                    const DMat<CoeffRing> &B,
                                    bool transposeA,
                                    bool transposeB,
                                    const ElementType& a,
                                    const ElementType& b)
{
  std::cerr << "DMat  addMultipleTo" << std::endl;
    std::cerr << "typeid: " << typeid(CoeffRing).name () << std::endl;
  ERROR("addMultipleTo not implemented for this ring yet");
}

#ifdef HAVE_FFLAS_FFPACK

    template<typename  CoeffRing >
    template<class RingType>
    size_t DMat < CoeffRing >::rank(typename enable_if<is_givaro_or_ffpack<RingType>::value >::type* dummy ) const
    {
        // assert not neccesary because the test is already done by  "enable_if<is_givaro_or_ffpack<RingType>::value >"
        // assert( typeid(CoeffRing) == typeid(M2::ARingZZpFFPACK) || typeid(CoeffRing) == typeid(M2::ARingGF ));
        std::cout << "Calling rankGF_or_FFPACK" << std::endl;
        ElementType *N = newarray(ElementType, n_rows() * n_cols() );
        /// @jakob: replace with memcopy or something fast.
        /// @jakob: potention problem: (  n_rows()*n_cols() ) - overflow for big matrices 
        copy_elems( n_rows()*n_cols(), N, 1, get_array(), 1); 
        /// @note 1. matrix data (N) is modified by FFPACK
        /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
    
        /* //debug
        typename MatrixType::ElementType *Npos=N;
        for ( int currRow=0; currRow < n_rows(); currRow++ )
        for ( int currCol =0; currCol < n_cols(); currCol++ )
        {
            typename MatrixType::ElementType entry;
                get_entry(currRow, currCol,entry)  ;
                ring().field().init(Npos, entry );
            //  mat.setEntry( currRow, currCol ,( (int)rand() ) % characteristic );
        }*/
    
        size_t result = FFPACK::Rank(ring().field(), n_cols(), n_rows(),  N,  n_rows() );
        deletearray(N);
        return result;
    }

    template<typename CoeffRing>
    void FFpackDeterminant(const DMat<CoeffRing>& mat, 
                                 typename CoeffRing::ElementType& result )
    {
        typedef typename CoeffRing::ElementType ElementType;
        std::cout << "Calling FFpackDeterminant" << std::endl;
        assert( typeid(CoeffRing) == typeid(M2::ARingZZpFFPACK) || typeid(CoeffRing) == typeid(M2::ARingGF ));
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
        /// @note 1. matrix data (N) is modified by FFPACK
        /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
        result = FFPACK::Det(mat.ring().field(), mat.n_cols(), mat.n_rows(),  N,  mat.n_rows());
        deletearray(N);
    }

    template<typename CoeffRing>
    bool FFpackInvert(const DMat<CoeffRing> &mat, DMat<CoeffRing> &inverse)
    {
        typedef typename CoeffRing::ElementType ElementType;
        ASSERT(mat.n_rows() == mat.n_cols());
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
    
        size_t n = mat.n_rows(); // same as n_cols()
        int nullspacedim;
        FFPACK::Invert2(mat.ring().field(), n, N, n, inverse.get_array(), n, nullspacedim);
    
        deletearray(N);
        return true;
    }

    template<typename CoeffRing>
    void FFpackNullSpace(const DMat<CoeffRing> &mat, 
                         DMat<CoeffRing> &nullspace, 
                         bool right_side)
    {
        right_side = !right_side; // because FFPACK stores by rows, not columns.

        typedef typename CoeffRing::ElementType ElementType;
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
    
        size_t nr = mat.n_rows();
        size_t nc = mat.n_cols();
    
        ElementType *nullspaceFFPACK = 0;
    
        size_t nullspace_dim;
        size_t nullspace_leading_dim;
    
        FFPACK::NullSpaceBasis(mat.ring().field(),
                               (right_side ? FFLAS::FflasRight : FFLAS::FflasLeft),
                               nc, nr, N, nr, nullspaceFFPACK, nullspace_leading_dim, nullspace_dim);
    
        std::cerr << "leading dim = " << nullspace_leading_dim << " and dim = " << nullspace_dim << std::endl;
        size_t nullspace_nrows = (right_side ? nc : nullspace_dim);
        if (right_side && nullspace_dim != nullspace_leading_dim)
          {
            std::cerr << "error: this should not happen!" << std::endl;
          }
        else if (!right_side && nullspace_leading_dim != nc)
          {
            std::cerr << "error: this should not happen either!" << std::endl;
          }
    
        if (right_side)
          nullspace.resize(nullspace_dim,nr);
        else
          nullspace.resize(nc,nullspace_dim);
    
        mat.copy_elems(nullspace.n_rows() * nullspace.n_cols(), nullspace.get_array(), 1, nullspaceFFPACK, 1); 
    
        delete [] nullspaceFFPACK;
    }

    template<typename CoeffRing>
    M2_arrayintOrNull FFpackRankProfile(const DMat<CoeffRing> &mat,
                                        bool row_profile)
    {
        // Note that FFPack stores matrices by row, not column, the opposite of what we do.
        // So row_profile true means use ffpack column rank profile!

        typedef typename CoeffRing::ElementType ElementType;
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
    
        size_t * prof; // this is where the result will be placed
    
        size_t rk;
        if (!row_profile)
          rk = FFPACK::RowRankProfile(mat.ring().field(),
                                      mat.n_cols(),mat.n_rows(),
                                      N,mat.n_rows(),
                                      prof);
        else
          rk = FFPACK::ColumnRankProfile(mat.ring().field(),
                                         mat.n_cols(),mat.n_rows(),
                                         N,mat.n_rows(),
                                         prof);
        
        M2_arrayint profile = M2_makearrayint(rk);
        for (size_t i=0; i<rk; i++)
          profile->array[i] = prof[i];
    
        delete [] prof;
        deletearray(N);

        return profile;
    }

    template<typename CoeffRing>
    bool FFpackSolveLinear(const DMat<CoeffRing> &mat, 
                           DMat<CoeffRing> &X, 
                           const DMat<CoeffRing> &B, 
                           bool right_side)
    {
        std::cerr << "inside FFpackSolveLinear" << std::endl;

        typedef typename CoeffRing::ElementType ElementType;
        size_t a_rows = mat.n_rows();
        size_t a_cols = mat.n_cols();
    
        size_t b_rows = B.n_rows();
        size_t b_cols = B.n_cols();
    
        ElementType* ffpackA = newarray(ElementType, mat.n_rows() * mat.n_cols());
        mat.copy_elems(mat.n_rows()*mat.n_cols(), ffpackA, 1, mat.get_array(), 1); 
    
        ElementType* ffpackB = newarray(ElementType, b_rows * b_cols);
        B.copy_elems(b_rows * b_cols, ffpackB, 1, B.get_array(), 1); 
    
        // preallocate the space for the solutions:
        size_t x_rows = (right_side ? a_cols : b_rows);
        size_t x_cols = (right_side ? b_cols : a_rows);
        size_t n_eqns = (right_side ? b_cols : b_rows);
    
        ElementType *ffpackX = newarray_clear(ElementType, x_rows * x_cols);
    
        int info; // >0 if the system is inconsistent, ==0 means success
    
        FFPACK::fgesv(mat.ring().field(),
                      (!right_side ? FFLAS::FflasLeft : FFLAS::FflasRight),
                      a_cols, a_rows, 
                      (!right_side ? b_cols : b_rows),
                      ffpackA,
                      a_rows, // leading dim of A
                      ffpackX, x_rows,
                      ffpackB, b_rows,
                      &info);
    
        if (info > 0)
          {
            // the system is inconsistent
            ERROR("the system is inconsistent");
            return false;
          }
    
        X.resize(x_rows, x_cols);
        X.copy_elems(x_rows * x_cols, X.get_array(), 1, ffpackX, 1); 
    
        delete [] ffpackX;
    
        return true;
    } 

    template<typename CoeffRing>
    void FFpackAddMultipleTo(DMat<CoeffRing>& C, 
                             const DMat<CoeffRing>& A,
                             const DMat<CoeffRing>& B,
                             bool transposeA,
                             bool transposeB,
                             const typename CoeffRing::ElementType& a,
                             const typename CoeffRing::ElementType& b)
    /* A,B,C should be mutable matrices over a finite prime field, and a,b
       elements of this field.
       C = b*C + a * op(A)*op(B),
       where op(A) = A or transpose(A), depending on transposeA
       where op(B) = B or transpose(B), depending on transposeB
    */
    { 
        std::cout << " FFpackAddMultipleTo " << std::endl;
        // set tA, tB
        FFLAS::FFLAS_TRANSPOSE tA = (transposeA ? FFLAS::FflasTrans : FFLAS::FflasNoTrans);
        FFLAS::FFLAS_TRANSPOSE tB = (transposeB ? FFLAS::FflasTrans : FFLAS::FflasNoTrans);

        // determine m,n,k
        //size_t m = (transposeA ? A.n_cols() : A.n_rows());
        //size_t n = (transposeB ? B.n_rows() : B.n_cols());
        //size_t k = (transposeA ? A.n_rows() : A.n_cols());
        //size_t k2 = (transposeB ? B.n_cols() : B.n_rows());

        size_t m = (transposeB ? B.n_rows() : B.n_cols());
        size_t n = (transposeA ? A.n_cols() : A.n_rows());
        
        size_t k = (transposeA ? A.n_rows() : A.n_cols());
        size_t k2 = (transposeB ? B.n_cols() : B.n_rows());

        std::cout <<"k  :" << k << std::endl;
        std::cout <<"k2 :" << k2 << std::endl;
        assert(k == k2); // The user of this function must insure that sizes are correct.
        if (k!=k2)
            ERROR("matrices are not composable");

        FFLAS::fgemm(C.ring().field(),
                     tA, tB,
                     m,n,k,
                     a,
                     B.get_array(),
                     B.n_rows(),
                     A.get_array(),
                     A.n_rows(),
                     b,
                     C.get_array(),
                     C.n_rows()
                     );
    }

    //////////////////////////////////////////////////////
    // ARingZZpFFPACK specific linear algebra functions //
    //////////////////////////////////////////////////////
    template<>
    size_t DMat<M2::ARingZZpFFPACK>::rank() const
    {
        std::cout << "DMat<M2::ARingZZpFFPACK>::rank()" << std::endl;
        return rank<M2::ARingZZpFFPACK>( );
    }

    template<>
    void DMat<M2::ARingZZpFFPACK>::determinant(elem &result) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::determinant" << std::endl;
        FFpackDeterminant<M2::ARingZZpFFPACK>(*this, result);
    }

    template<>
    bool DMat<M2::ARingZZpFFPACK>::invert(DMat<M2::ARingZZpFFPACK> &inverse) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::inverse" << std::endl;
        return FFpackInvert<M2::ARingZZpFFPACK>(*this, inverse);
    }

    template<>
    void DMat<M2::ARingZZpFFPACK>::nullSpace(DMat<M2::ARingZZpFFPACK> &nullspace, bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::nullspace" << std::endl;
        FFpackNullSpace<M2::ARingZZpFFPACK>(*this, nullspace, right_side);
    }

    template<>
    M2_arrayintOrNull DMat<M2::ARingZZpFFPACK>::rankProfile(bool row_profile) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::rankProfile" << std::endl;
        return FFpackRankProfile(*this, row_profile);
    }

    template<>
    bool DMat<M2::ARingZZpFFPACK>::solveLinear(DMat<M2::ARingZZpFFPACK> &X, 
                                               const DMat<M2::ARingZZpFFPACK> &B, 
                                               bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::solveLinear" << std::endl;
        return FFpackSolveLinear(*this, X, B, right_side);
    }

    template<>
    void DMat<M2::ARingZZpFFPACK>::addMultipleTo(const DMat<M2::ARingZZpFFPACK> &A,
                                                 const DMat<M2::ARingZZpFFPACK> &B,
                                                 bool transposeA,
                                                 bool transposeB,
                                                 const ElementType& a,
                                                 const ElementType& b)
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::addMultipleTo *" << std::endl;
        FFpackAddMultipleTo(*this, A, B, transposeA, transposeB, a, b);
    }

#ifdef HAVE_GIVARO

    //////////////////////////////////////////////////////
    // ARingGF specific linear algebra functions /////////
    //////////////////////////////////////////////////////
    
    template<>
    size_t DMat<M2::ARingGF>::rank() const
    {
        std::cout << "Calling  DMat<M2::ARingGF>::rank()" << std::endl;
        return rank<M2::ARingGF>( );
    }
    
    template<>
    void DMat<M2::ARingGF>::determinant(elem &result) const
    {
        std::cout << "Calling  DMat<M2::ARingGF>::determinant" << std::endl;
        FFpackDeterminant<M2::ARingGF>(*this, result );
    }
    
    template<>
    bool DMat<M2::ARingGF>::invert(DMat<M2::ARingGF> &inverse) const
    {
        std::cout << "Calling  DMat<M2::ARingGF>::inverse" << std::endl;
        return FFpackInvert<M2::ARingGF>(*this, inverse);
    }

    template<>
    void DMat<M2::ARingGF>::nullSpace(DMat<M2::ARingGF> &nullspace, bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingGF>::nullspace" << std::endl;
        FFpackNullSpace<M2::ARingGF>(*this, nullspace, right_side);
    }

    template<>
    M2_arrayintOrNull DMat<M2::ARingGF>::rankProfile(bool row_profile) const
    {
        std::cout << "Calling  DMat<M2::ARingGF>::rankProfile" << std::endl;
        return FFpackRankProfile(*this, row_profile);
    }

    template<>
    bool DMat<M2::ARingGF>::solveLinear(DMat<M2::ARingGF> &X, 
                                               const DMat<M2::ARingGF> &B, 
                                               bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingGF>::solveLinear" << std::endl;
        return FFpackSolveLinear(*this, X, B, right_side);
    }

    template<>
    void DMat<M2::ARingGF>::addMultipleTo(const DMat<M2::ARingGF> &A,
                                                 const DMat<M2::ARingGF> &B,
                                                 bool transposeA,
                                                 bool transposeB,
                                                 const ElementType& a,
                                                 const ElementType& b)
    {
        std::cout << "Calling  DMat<M2::ARingGF>::addMultipleTo" << std::endl;
        FFpackAddMultipleTo(*this, A, B, transposeA, transposeB, a, b);
    }

  #endif

#endif

#include "mutablemat.hpp"

template<typename MatT> 
inline MatT * MutableMatrix::coerce()
{
  MutableMat<MatT> *P = cast_to_MutableMat<MatT>();
  if (P == 0) return 0;
  return P->get_Mat();
}

template<typename MatT> 
inline const MatT * MutableMatrix::coerce() const
{
  const MutableMat<MatT> *P = cast_to_MutableMat<MatT>();
  if (P == 0) return 0;
  return P->get_Mat();
}

M2_arrayintOrNull rawLQUP(MutableMatrix *A, M2_bool transpose)
{
#ifdef HAVE_FFLAS_FFPACK
  // Suppose A is m x n
  // P is n element permutation on columns
  // Qt is m element permutation on rows (inverse permutation)
  DMat<M2::ARingZZpFFPACK> *mat = A->coerce< DMat<M2::ARingZZpFFPACK> >();
  if (mat == 0) 
    {
      ERROR("LUDivine not defined for this ring");
      return 0;
    }
  size_t nelems = mat->n_cols();
  if (mat->n_rows() > mat->n_cols()) nelems = mat->n_rows();
  size_t* P = newarray_atomic(size_t, nelems ); // initialize this (column perm
  size_t* Qt = newarray_atomic(size_t, nelems); // initialize this
  size_t rk = LUdivine(mat->ring().field(),
                       FFLAS::FflasNonUnit,
                       (!transpose ? FFLAS::FflasTrans : FFLAS::FflasNoTrans),
                       mat->n_cols(),
                       mat->n_rows(),
                       mat->get_array(),
                       mat->n_rows(),
                       P, 
                       Qt);
  std::cout << "P = [";
  for (size_t i=0; i<nelems; i++)
    std::cout << P[i] << " ";
  std::cout << "]" << std::endl;

  std::cout << "Qt = [";
  for (size_t i=0; i<nelems; i++)
    std::cout << Qt[i] << " ";
  std::cout << "]" << std::endl;
#endif
  return 0;
}

template class DMat<CoefficientRingZZ_NTL>;
template class DMat<M2::ARingZZp>;
template class DMat<M2::ARingZZpFFPACK>;

template class DMat<CoefficientRingRRR>;
template class DMat<CoefficientRingCCC>;
template class DMat<CoefficientRingR>;

template class DMat<M2::ARingGF>;
template class DMat<M2::ARingGFM2>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
