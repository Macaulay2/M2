// Copyright 2005-2012  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

union ring_elem;
class Ring;

#include "newdelete.hpp"
#include "ring.hpp"
#include "coeffrings.hpp"

#if 0
#include "ZZp.hpp"
#include "aring-ffpack.hpp"
#include "aring-gf.hpp"
#endif

#if 0
// enable_if is probably only available for never standard, e.g. compiler flag -std=c++0x
template<bool, typename T = void> 
  struct enable_if {};

template<typename T>
  struct enable_if<true, T> {
    typedef T type;
  };

template< typename T > 
struct is_givaro_or_ffpack{ 
  static const bool value = false;
};

template<> 
struct is_givaro_or_ffpack< M2::ARingZZpFFPACK >{ 
  static const bool value = true; 
};

template<> 
struct is_givaro_or_ffpack< M2::ARingGF >{ 
  static const bool value = true; 
};
#endif

class MutableMatrix;

template <typename RingType>
class EigenvalueType
{
public:
  typedef RingType Ring;
};

template<>
class EigenvalueType<Ring_RRR>
{
public:
  typedef CoefficientRingCCC Ring;
};

/**
 * \ingroup matrices
 */
template<typename ACoeffRing>
class DMat : public our_new_delete
{
public:
  typedef ACoeffRing CoeffRing;
  typedef typename CoeffRing::elem elem;
  typedef elem ElementType; // same as elem.  Will possibly remove 'elem' later.

  typedef DMat<typename EigenvalueType<ACoeffRing>::Ring> EigenvalueMatrixType;

  DMat():R(0), coeffR(0), nrows_(0), ncols_(0), array_(0) {} // Makes a zero matrix

  DMat(const Ring *R, const ACoeffRing *R0, size_t nrows, size_t ncols); // Makes a zero matrix

  DMat(const DMat<ACoeffRing> &M, size_t nrows, size_t ncols); // Makes a zero matrix, same ring.

  DMat(const DMat<ACoeffRing> &M); // Copies (clones) M

  void grab(DMat *M);// swaps M and this.

  //  DMat<CoeffRing> *copy() const;

  bool is_dense() const { return true; }

  size_t n_rows() const { return nrows_; }
  size_t n_cols() const { return ncols_; }
  const Ring * get_ring() const { return R; }
  const CoeffRing * get_CoeffRing() const { return coeffR; }
  const CoeffRing& ring() const { return *coeffR; }

  void set_matrix(const DMat<CoeffRing> *mat0);
  void initialize(size_t nrows, size_t ncols, const elem *array);
  void resize(size_t nrows, size_t ncols);

  // These functions are used for interface with e.g. lapack, ffpack.
  const elem * get_array() const { return array_; }
  elem * get_array() { return array_; }

  double *get_lapack_array() const; // redefined by RR,CC
  double *make_lapack_array() const; // creates an array of doubles (or 0, if not applicable)
  void fill_from_lapack_array(double *lapack_array);  // The size of the array should match the size of this.

  class iterator : public our_new_delete
  {
    const DMat<CoeffRing> *M;
    size_t col;
    elem *begin;
    elem *end;
    void to_next_valid() {
      const CoeffRing *R = M->get_CoeffRing();
      --begin;
      while (begin >= end && R->is_zero(*begin)) --begin;
    }
  public:
    void set(size_t col0) {
      col = col0;
      begin = M->array_ + (col0+1) * (M->n_rows());
      end = begin-M->n_rows();
      to_next_valid();
    }
    iterator(const DMat<CoeffRing> *M0) : M(M0),
                                          col(-1),
                                          begin(0),
                                          end(0) { }
    const elem &value() { return *begin; }
    void next() { to_next_valid(); }
    bool valid() { return begin >= end; }
    size_t row() { return (begin-end); }

    void copy_elem(ring_elem &result) {
      M->get_CoeffRing()->to_ring_elem(result, value());
    }
  };


public:
  size_t lead_row(size_t col) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  size_t lead_row(size_t col, elem &result) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  bool get_entry(size_t r, size_t c, elem &result) const;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  void set_entry(size_t r, size_t c, const elem &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  void interchange_rows(size_t i, size_t j);
  /* swap rows: row(i) <--> row(j) */

  void interchange_columns(size_t i, size_t j);
  /* swap columns: column(i) <--> column(j) */

  void scale_row(size_t i, const elem &r);
  /* row(i) <- r * row(i) */

  void scale_column(size_t i, const elem &r);
  /* column(i) <- r * column(i) */

  void divide_row(size_t i, const elem &r);
  /* row(i) <- row(i) / r */

  void divide_column(size_t i, const elem &r);
  /* column(i) <- column(i) / r */

  void row_op(size_t i, const elem &r, size_t j);
  /* row(i) <- row(i) + r * row(j) */

  void column_op(size_t i, const elem &r, size_t j);
  /* column(i) <- column(i) + r * column(j) */

  void column2by2(size_t c1, size_t c2,
                  const elem &a1, const elem &a2,
                  const elem &b1, const elem &b2);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  void row2by2(size_t r1, size_t r2,
               const elem &a1, const elem &a2,
               const elem &b1, const elem &b2);
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  void dot_product(size_t i, size_t j, elem &result) const;

  bool row_permute(size_t start_row, M2_arrayint perm);

  bool column_permute(size_t start_col, M2_arrayint perm);

  void insert_columns(size_t i, size_t n_to_add);
  /* Insert n_to_add columns directly BEFORE column i. */

  void insert_rows(size_t i, size_t n_to_add);
  /* Insert n_to_add rows directly BEFORE row i. */

  void delete_columns(size_t i, size_t j);
  /* Delete columns i .. j from M */

  void delete_rows(size_t i, size_t j);
  /* Delete rows i .. j from M */

  bool set_submatrix(M2_arrayint rows,
                     M2_arrayint cols,
                     const MutableMatrix *N);

  bool is_zero() const;

  bool is_equal(const DMat& B) const;

  DMat * mult(const MutableMatrix *B) const;
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  DMat * mult(const elem &f) const;
  // return f*this.  return NULL of sizes or types do not match.

  ///////////////////////////////////
  /// Fast linear algebra routines //
  ///////////////////////////////////

  // this += B, assertion failure on bad ring or bad sizes
  void addInPlace(const DMat& B);

  // this -= B, assertion failure on bad ring or bad sizes
  void subtractInPlace(const DMat& B);

  // this = -this
  void negateInPlace();

  // this = f * this
  void scalarMultInPlace(const elem &f);

  void setFromSubmatrix(const DMat &A, M2_arrayint rows, M2_arrayint cols);

  void setFromSubmatrix(const DMat &A, M2_arrayint cols);

  // this += A*B
  //  void addMultipleInPlace(const DMat& A, const DMat& B);

  //  void fillFrom(const DMat &m);
  //  void fillFrom(const SMat &m);

  /// fillFromMatrix initializes 'this' with a submatrix of 'm'.
  // if 'rows' or 'cols' is NULL, then that argument means take all rows (resp. columns).
  //  void fillFromSubmatrix(const DMat &m, M2_arrayint rows, M2_arrayint cols);
  //  void fillFromSubmatrix(const SMat &m, M2_arrayint rows, M2_arrayint cols);

#if 0
  template<class RingType>
  size_t rank(typename enable_if<is_givaro_or_ffpack<RingType>::value >::type* dummy = 0) const;
#endif

  size_t rank() const;
 
  void determinant(elem &result) const;

  // Set 'inverse' with the inverse of 'this'.  If the matrix is not square, or 
  // the matrix is not invertible, or
  // the ring is one in which the matrix cannot be inverted,
  // then false is returned, and an error message is set.
  bool invert(DMat<ACoeffRing> &inverse) const;

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  M2_arrayintOrNull rankProfile(bool row_profile) const;
  
  // Find a spanning set for the null space.  If M = this,
  // and right_side is true, return a matrix whose rows span {x |  xM = 0},
  // otherwise return a matrix whose columns span {x | Mx = 0}
  void nullSpace(DMat<ACoeffRing> &nullspace, bool right_side) const;

  // X is set to  a matrix whose rows or columns solve either AX = B (right_side=true)
  // or XA = B (right_side=false). If no solutions are found, false is returned.
  bool solveLinear(DMat<ACoeffRing> &X, const DMat<ACoeffRing> &B, bool right_side) const;

  /** C=this,A,B should be mutable matrices over the same ring, and a,b
     elements of this ring. AND of the same density type.
     C = b*C + a * op(A)*op(B),
     where op(A) = A or transpose(A), depending on transposeA
     where op(B) = B or transpose(B), depending on transposeB
  */  
  void addMultipleTo(const DMat<ACoeffRing> &A,
                     const DMat<ACoeffRing> &B,
                     bool transposeA,
                     bool transposeB,
                     const ElementType& a,
                     const ElementType& b);

  /** if 'this' is a square n x n matrix, return
      an array, {a0,a1,a2,...,an} such that
      the characteristic polynomial is
      det(t*I - this) = a0 + a1 * t + ... + an * t^n .
  */
  engine_RawRingElementArrayOrNull characteristicPolynomial() const;

  /** if 'this' is a square n x n matrix, return
      an array, {a0,a1,a2,...,am} such that
      the minimal monic polynomial of 'this' is
      a0 + a1 * t + ... + am * t^m .
  */
  engine_RawRingElementArrayOrNull minimalPolynomial() const;

  void copy_elems(size_t n_to_copy, elem *target, size_t target_stride, const elem *source, size_t stride) const;
private:
  const Ring *R; // To interface to the outside world
  const CoeffRing * coeffR; // Same as R, optimized for speed.  R->get_CoeffRing()
  size_t nrows_;
  size_t ncols_;
  elem *array_; // array has length nrows*ncols
                // columns stored one after another
};

/////////////////////////
// Implementation ///////
/////////////////////////

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const Ring *R0, const CoeffRing *coeffR0, size_t nrows, size_t ncols)
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
DMat<CoeffRing>::DMat(const DMat<CoeffRing> &m)
  : R(m.R),
    coeffR(m.coeffR),
    nrows_(m.nrows_),
    ncols_(m.ncols_)
{
  initialize(nrows_,ncols_,m.array_);
}

template<typename CoeffRing>
void DMat<CoeffRing>::initialize(size_t nrows, size_t ncols, const elem *array)
{
  nrows_ = nrows;
  ncols_ = ncols;
  size_t len = nrows * ncols;
  array_ = newarray_clear(elem,len);
  if (array == 0)
    {
      for (size_t i=0; i<len; i++)
        ring().set_zero(array_[i]);
    }
  else
    for (size_t i=0; i<len; i++)
      ring().init_set(array_[i], array[i]);
}

template<typename CoeffRing>
void DMat<CoeffRing>::resize(size_t new_nrows, size_t new_ncols)
{
  size_t new_len = new_nrows * new_ncols;
  if (new_len == 0 || new_len != nrows_ * ncols_)
    initialize(new_nrows, new_ncols, 0);
  else
    for (size_t i=0; i<new_len; i++)
      ring().set_zero(array_[i]);
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

#if 0
template<typename CoeffRing>
DMat<CoeffRing> *DMat<CoeffRing>::copy() const
{
  DMat<CoeffRing> *result = new DMat<CoeffRing>(get_ring(), get_CoeffRing(), 0, 0);
  result->initialize(nrows_, ncols_, array_);
  return result;
}
#endif

template<typename CoeffRing>
size_t DMat<CoeffRing>::lead_row(size_t col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
{
  elem *last = array_ + nrows_ * col;
  elem *loc = last + nrows_ - 1;
  for ( ; loc >= last; loc--)
    {
      if (!ring().is_zero(*loc))
        return (loc-last);
    }
  return -1;
}

template<typename CoeffRing>
size_t DMat<CoeffRing>::lead_row(size_t col, elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
{
  elem *last = array_ + nrows_ * col;
  elem *loc = last + nrows_ - 1;
  for ( ; loc >= last; loc--)
    {
      if (!ring().is_zero(*loc))
        {
          ring().set(result, *loc);
          return static_cast<size_t>(loc-last);
        }
    }
  return -1;
}

///////////////////////////////
// Row and column operations //
///////////////////////////////

template<typename CoeffRing>
bool DMat<CoeffRing>::get_entry(size_t r, size_t c, elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  size_t loc = c * nrows_ + r;
  ring().init_set(result, array_[loc]);
  return !ring().is_zero(result);
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_entry(size_t r, size_t c, const elem &a)
{
  size_t loc = c * nrows_ + r;
  ring().set(array_[loc], a);
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_rows(size_t i, size_t j)
  /* swap rows: row(i) <--> row(j) */
{
  elem *loc1 = array_ + i;
  elem *loc2 = array_ + j;

  for (size_t c=0; c<ncols_; c++)
    {
      elem tmp = *loc1;
      *loc1 = *loc2;
      *loc2 = tmp;
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_columns(size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;
  for (size_t r=0; r<nrows_; r++)
    {
      elem tmp = *loc1;
      *loc1++ = *loc2;
      *loc2++ = tmp;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_row(size_t i, const elem &r)
  /* row(i) <- r * row(i) */
{
  elem *loc = array_ + i;
  for (size_t c=0; c<ncols_; c++)
    {
      ring().mult(*loc, r, *loc); // *loc = r * *loc
      loc += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_column(size_t i, const elem &r)
  /* column(i) <- r * column(i) */
{
  elem *loc = array_ + nrows_*i;
  for (size_t a=0; a<nrows_; a++)
    {
      ring().mult(*loc, r, *loc);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_row(size_t i, const elem &r)
  /* row(i) <- row(i) / r */
{
  elem *loc = array_ + i;
  for (size_t c=0; c<ncols_; c++)
    {
      ring().divide(*loc, *loc, r);
      loc += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_column(size_t i, const elem &r)
  /* column(i) <- column(i) / r */
{
  elem *loc = array_ + nrows_*i;
  for (size_t a=0; a<nrows_; a++)
    {
      ring().divide(*loc, *loc, r);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row_op(size_t i, const elem &r, size_t j)
  /* row(i) <- row(i) + r * row(j) */
{
  elem *loc1 = array_ + i;
  elem *loc2 = array_ + j;

  elem f;
  ring().set_zero(f);
  for (size_t c=0; c<ncols_; c++)
    {
      ring().mult(f,r,*loc2);
      ring().add(*loc1, f, *loc1);
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::column_op(size_t i, const elem &r, size_t j)
  /* column(i) <- column(i) + r * column(j) */
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;

  elem f;
  ring().set_zero(f);
  for (size_t a=0; a<nrows_; a++)
    {
      ring().mult(f,r,*loc2);
      ring().add(*loc1, *loc1, f);
      loc1++;
      loc2++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row2by2(size_t r1, size_t r2,
               const elem &a1, const elem &a2,
               const elem &b1, const elem &b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  elem *loc1 = array_ + r1;
  elem *loc2 = array_ + r2;

  elem f1,f2,g1,g2;
  ring().set_zero(f1);
  ring().set_zero(f2);
  ring().set_zero(g1);
  ring().set_zero(g2);
  for (size_t i=0; i<ncols_; i++)
    {
      ring().mult(f1,a1,*loc1);
      ring().mult(f2,a2,*loc2);
      ring().mult(g1,b1,*loc1);
      ring().mult(g2,b2,*loc2);

      ring().add(f1,f1,f2);
      ring().add(g1,g1,g2);
      ring().set(*loc1, f1);
      ring().set(*loc2, g1);
      loc1 += nrows_;
      loc2 += nrows_;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::column2by2(size_t c1, size_t c2,
                  const elem &a1, const elem &a2,
                  const elem &b1, const elem &b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  elem *loc1 = array_ + c1 * nrows_;
  elem *loc2 = array_ + c2 * nrows_;

  elem f1,f2,g1,g2;
  ring().set_zero(f1);
  ring().set_zero(f2);
  ring().set_zero(g1);
  ring().set_zero(g2);
  for (size_t i=0; i<nrows_; i++)
    {
      ring().mult(f1,a1,*loc1);
      ring().mult(f2,a2,*loc2);
      ring().mult(g1,b1,*loc1);
      ring().mult(g2,b2,*loc2);

      ring().add(f1,f1,f2);
      ring().add(g1,g1,g2);
      ring().set(*loc1++, f1);
      ring().set(*loc2++, g1);
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::dot_product(size_t i, size_t j, elem &result) const
{
  elem *loc1 = array_ + nrows_*i;
  elem *loc2 = array_ + nrows_*j;
  ring().set_zero(result);

  elem f;
  ring().set_zero(f);
  for (size_t r=0; r<nrows_; r++)
    {
      ring().mult(f,*loc1++,*loc2++);
      ring().add(result,result, f);
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::copy_elems(size_t n_to_copy, elem *target, size_t target_stride, const elem *source, size_t stride) const
{
  for (size_t i=0; i<n_to_copy; i++)
    {
      *target = *source;
      target += target_stride;
      source += stride;
    }
}


template<typename CoeffRing>
bool DMat<CoeffRing>::row_permute(size_t start_row, M2_arrayint perm)
{
  // We copy one row to another location for each cycle in 'perm' of length > 1.
  size_t nrows_to_permute = perm->len;
  bool *done = newarray_atomic(bool,nrows_to_permute);
  for (size_t i=0; i<nrows_to_permute; i++)
    done[i] = true;
  for (size_t i=0; i<nrows_to_permute; i++)
    {
      size_t j = perm->array[i];
      if (!done[j])
        {
          ERROR("expected permutation");
          deletearray(done);
          return false;
        }
      done[j] = false;
    }
  elem *tmp = newarray_clear(elem,ncols_);
  size_t next = 0;
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

          size_t r = next;
          for (;;)
            {
              // copy row perm[r] to row r
              copy_elems(ncols_, arr + r, nrows_, arr + perm->array[r], nrows_);
              done[r] = true;
              size_t next_r = perm->array[r];
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
bool DMat<CoeffRing>::column_permute(size_t start_col, M2_arrayint perm)
{
  // We copy one column to another location for each cycle in 'perm' of length > 1.
  size_t ncols_to_permute = perm->len;
  bool *done = newarray_atomic(bool,ncols_to_permute);
  for (size_t i=0; i<ncols_to_permute; i++)
    done[i] = true;
  for (size_t i=0; i<ncols_to_permute; i++)
    {
      size_t j = perm->array[i];
      if (!done[j])
        {
          ERROR("expected permutation");
          deletearray(done);
          return false;
        }
      done[j] = false;
    }
  elem *tmp = newarray_clear(elem,nrows_);
  size_t next = 0;
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

          size_t r = next;
          for (;;)
            {
              // copy col perm[r] to col r
              copy_elems(nrows_, arr + r * nrows_, 1, arr + perm->array[r] * nrows_, 1);
              done[r] = true;
              size_t next_r = perm->array[r];
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
void DMat<CoeffRing>::insert_columns(size_t i, size_t n_to_add)
/* Insert n_to_add columns directly BEFORE column i. */
{
  elem *tmp = array_;
  size_t nbefore = i * nrows_;
  size_t nadded = n_to_add * nrows_;
  size_t nelems = ncols_ * nrows_;

  ncols_ += n_to_add;
  size_t len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);
  for (size_t j=0; j<nbefore; j++)
    ring().swap(tmp[j], array_[j]);

  for (size_t j=0; j<nadded; j++)
    ring().set_zero(array_[j+nbefore]);
  for (size_t j=nbefore; j<nelems; j++)
    ring().swap(tmp[j], array_[j+nadded]);
  deletearray(tmp);
}

template<typename CoeffRing>
void DMat<CoeffRing>::insert_rows(size_t i, size_t n_to_add)
/* Insert n_to_add rows directly BEFORE row i. */
{
  elem *tmp = array_;
  elem zero;
  ring().set_zero(zero);
  size_t old_nrows = nrows_;

  nrows_ += n_to_add;
  size_t len = nrows_ * ncols_;
  array_ = newarray_clear(elem, len);
  for (size_t r=0; r<i; r++)
    copy_elems(ncols_, array_ + r, nrows_, tmp + r, old_nrows);
  for (size_t r=i; r<i+n_to_add; r++)
    copy_elems(ncols_, array_ + r, nrows_, &zero, 0);
  for (size_t r=i; r<old_nrows; r++)
    copy_elems(ncols_, array_ + r + n_to_add, nrows_, tmp + r, old_nrows);
  deletearray(tmp);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_columns(size_t i, size_t j)
/* Delete columns i .. j from M */
{
  elem *tmp = array_;
  size_t nbefore = i * nrows_;
  size_t ndeleted = (j-i+1) * nrows_;
  size_t nelems = ncols_ * nrows_;

  ncols_ -= j-i+1;
  size_t len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);
  for (size_t k=0; k<nbefore; k++)
    ring().swap(tmp[k], array_[k]);
  for (size_t k=nbefore+ndeleted; k<nelems; k++)
    ring().swap(tmp[k], array_[k-ndeleted]);
  deletearray(tmp);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_rows(size_t i, size_t j)
/* Delete rows i .. j from M */
{
  elem *tmp = array_;
  size_t ndeleted = j-i+1;
  size_t old_nrows = nrows_;

  nrows_ -= ndeleted;
  size_t len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);
  for (size_t r=0; r<i; r++)
    copy_elems(ncols_, array_ + r, nrows_, tmp + r, old_nrows);
  for (size_t r=j+1; r<old_nrows; r++)
    copy_elems(ncols_, array_ + r - ndeleted, nrows_, tmp + r, old_nrows);
  deletearray(tmp);
}

template<typename CoeffRing>
bool DMat<CoeffRing>::is_zero() const
{
  for (elem *t = array_ + nrows_*ncols_ - 1; t >= array_; t--)
    if (!ring().is_zero(*t))
      return false;
  return true;
}

template<typename CoeffRing>
bool DMat<CoeffRing>::set_submatrix(M2_arrayint rows,
                                    M2_arrayint cols,
                                    const MutableMatrix *M)
{
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
//        ring().from_ring_elem(*(first + i->row()), a);
//      }
//       first += nrows_;
//     }
#endif
  return true;
}

template<typename CoeffRing>
void DMat<CoeffRing>::setFromSubmatrix(const DMat& A, 
                                       M2_arrayint rows,
                                       M2_arrayint cols)
{
  R = A.R;
  coeffR = A.coeffR;
  nrows_ = rows->len;
  ncols_ = cols->len;
  size_t len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);

  elem *target = get_array();
  for (size_t c=0; c<cols->len; c++)
    {
      const elem* src = A.get_array() + A.n_rows() * cols->array[c];
      ASSERT(cols->array[c] >= 0 && cols->array[c] < A.n_cols());

      for (size_t r=0; r<rows->len; r++)
        {
          ASSERT(rows->array[r] >= 0 && rows->array[r] < A.n_rows());
          ring().init_set(*target++, src[rows->array[r]]);
        }
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::setFromSubmatrix(const DMat& A, 
                                       M2_arrayint cols)
{
  R = A.R;
  coeffR = A.coeffR;
  nrows_ = A.nrows_;
  ncols_ = cols->len;
  size_t len = nrows_ * ncols_;
  array_ = newarray_clear(elem,len);

  elem *target = get_array();
  for (size_t c=0; c<cols->len; c++)
    {
      const elem* src = A.get_array() + A.n_rows() * cols->array[c];
      ASSERT(cols->array[c] >= 0 && cols->array[c] < A.n_cols());

      for (size_t r=0; r<nrows_; r++)
        ring().init_set(*target++, *src++);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::addInPlace(const DMat<CoeffRing>& B)
  // this += B.
  // assumption:the assert statements below:
{
  ASSERT(&B.ring() == &ring());
  ASSERT(B.n_rows() == n_rows());
  ASSERT(B.n_cols() == n_cols());
  
  for (size_t i=0; i<n_rows()*n_cols(); i++)
    {
      ring().add(array_[i], array_[i], B.array_[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::subtractInPlace(const DMat<CoeffRing>& B)
  // this -= B.
  // assumption:the assert statements below:
{
  ASSERT(&B.ring() == &ring());
  ASSERT(B.n_rows() == n_rows());
  ASSERT(B.n_cols() == n_cols());
  
  for (size_t i=0; i<n_rows()*n_cols(); i++)
    {
      ring().subtract(array_[i], array_[i], B.array_[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::negateInPlace()
  // this = -this
{
  for (size_t i=0; i<n_rows()*n_cols(); i++)
    {
      ring().negate(array_[i], array_[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::scalarMultInPlace(const elem &f)
  // this = f * this
{
  for (size_t i=0; i<n_rows()*n_cols(); i++)
    {
      ring().mult(array_[i], f, array_[i]);
    }
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
bool DMat<CoeffRing>::is_equal(const DMat& B) const
{
  ASSERT(&ring() == &B.ring())
  if (B.n_rows() != n_rows()) return false;
  if (B.n_cols() != n_cols()) return false;
  size_t top = n_rows() * n_cols();
  const elem * elemsA = get_array();
  const elem * elemsB = B.get_array();
  for (size_t i = 0; i < top; i++)
    if (!ring().is_equal(*elemsA++, *elemsB++))
      return false;
  return true;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
