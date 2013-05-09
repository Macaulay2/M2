// Copyright 2005-2012  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

union ring_elem;
class Ring;

#include "newdelete.hpp"
#include "ring.hpp"
#include "coeffrings.hpp"

#include "DenseMatrixDef.hpp"
#include "DenseMatrixLinAlg.hpp"

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
  typedef typename EigenvalueType<ACoeffRing>::Ring EigenvalueRing;
  typedef DenseMatrixLinAlg<ACoeffRing> LinAlg;
  //  typedef DenseMatrixArithmetic<CoeffRing> Arithmetic;
public:
  typedef ACoeffRing CoeffRing;
  typedef typename CoeffRing::elem ElementType;
  typedef ElementType elem; // same as ElementType.  Will possibly remove 'elem' later.

  typedef DMat<EigenvalueRing> EigenvalueMatrixType;

  DMat() {} // Makes an unusable matrix, over no ring.
  DMat(const Ring *R, const ACoeffRing *R0, size_t nrows, size_t ncols); // Makes a zero matrix
  DMat(const DMat<ACoeffRing> &M, size_t nrows, size_t ncols); // Makes a zero matrix, same ring.
  DMat(const DMat<ACoeffRing> &M); // Copies (clones) M into 'this'

  const Ring * get_ring() const { return mGeneralRing; }
  const CoeffRing& ring() const { return mMatrix.ring(); }
  const CoeffRing* get_CoeffRing() const { return & ring(); }

  size_t numRows() const { return mMatrix.numRows(); }
  size_t numColumns() const { return mMatrix.numColumns(); }
  size_t n_rows() const { return numRows(); }
  size_t n_cols() const { return numColumns(); }

  const ElementType* array() const { return mMatrix.array(); }
  ElementType* array() { return mMatrix.array(); }
  // These functions are used for interface with e.g. lapack, ffpack.
  const ElementType * get_array() const { return array(); }
  ElementType * get_array() { return array(); }

  bool is_dense() const { return true; }

  void text_out(buffer& o) const;

  void set_matrix(const DMat<CoeffRing>& mat0);
  void resize(size_t nrows, size_t ncols);

  double *get_lapack_array() const; // redefined by RR,CC
  double *make_lapack_array() const; // creates an array of doubles (or 0, if not applicable)
  void fill_from_lapack_array(double *lapack_array);  // The size of the array should match the size of this.

  class iterator : public our_new_delete
  {
    const DMat<CoeffRing> *M;
    size_t col;
    const ElementType *begin;
    const ElementType *end;
    void to_next_valid() {
      const CoeffRing *R = M->get_CoeffRing();
      --begin;
      while (begin >= end && R->is_zero(*begin)) --begin;
    }
  public:
    void set(size_t col0) {
      col = col0;
      begin = M->array() + (col0+1) * (M->numRows());
      end = begin-M->numRows();
      to_next_valid();
    }
    iterator(const DMat<CoeffRing> *M0) : M(M0),
                                          col(-1),
                                          begin(0),
                                          end(0) { }
    const ElementType &value() { return *begin; }
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

  size_t lead_row(size_t col, ElementType &result) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  bool get_entry(size_t r, size_t c, ElementType &result) const;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  void set_entry(size_t r, size_t c, const ElementType &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  void interchange_rows(size_t i, size_t j);
  /* swap rows: row(i) <--> row(j) */

  void interchange_columns(size_t i, size_t j);
  /* swap columns: column(i) <--> column(j) */

  void scale_row(size_t i, const ElementType &r);
  /* row(i) <- r * row(i) */

  void scale_column(size_t i, const ElementType &r);
  /* column(i) <- r * column(i) */

  void divide_row(size_t i, const ElementType &r);
  /* row(i) <- row(i) / r */

  void divide_column(size_t i, const ElementType &r);
  /* column(i) <- column(i) / r */

  void row_op(size_t i, const ElementType &r, size_t j);
  /* row(i) <- row(i) + r * row(j) */

  void column_op(size_t i, const ElementType &r, size_t j);
  /* column(i) <- column(i) + r * column(j) */

  void column2by2(size_t c1, size_t c2,
                  const ElementType &a1, const ElementType &a2,
                  const ElementType &b1, const ElementType &b2);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  void row2by2(size_t r1, size_t r2,
               const ElementType &a1, const ElementType &a2,
               const ElementType &b1, const ElementType &b2);
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  void dot_product(size_t i, size_t j, ElementType &result) const;

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

  bool is_zero() const;

  bool is_equal(const DMat& B) const;

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
  void scalarMultInPlace(const ElementType &f);

  void setFromSubmatrix(const DMat &A, M2_arrayint rows, M2_arrayint cols);

  void setFromSubmatrix(const DMat &A, M2_arrayint cols);

  size_t rank() const;
  void determinant(ElementType &result) const;

  size_t new_rank() const
  {
    return LinAlg::rank(mMatrix);
  }

  void new_determinant(ElementType &result_det) const
  {
    LinAlg::determinant(mMatrix, result_det);
  }

  // Set 'inverse' with the inverse of 'this'.  If the matrix is not square, or 
  // the matrix is not invertible, or
  // the ring is one in which the matrix cannot be inverted,
  // then false is returned, and an error message is set.
  bool invert(DMat<ACoeffRing> &result_inverse) const
  {
    return LinAlg::inverse(mMatrix, result_inverse.mMatrix);
  }

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

  void mult(const DMat<ACoeffRing>& B,
            DMat<ACoeffRing>& result_product) const
  {
    return DenseMatrixLinAlg<CoeffRing>::mult(mMatrix, B.mMatrix, result_product.mMatrix);
  }

  engine_RawRingElementArrayOrNull characteristicPolynomial() const;

  /** if 'this' is a square n x n matrix, return
      an array, {a0,a1,a2,...,am} such that
      the minimal monic polynomial of 'this' is
      a0 + a1 * t + ... + am * t^m .
  */
  engine_RawRingElementArrayOrNull minimalPolynomial() const;

  void copy_elems(size_t n_to_copy, ElementType *target, size_t target_stride, const ElementType *source, size_t stride) const;
private:
  const Ring* mGeneralRing; // To interface to the outside world
  DenseMatrixDef<CoeffRing> mMatrix;
};

/////////////////////////
// Implementation ///////
/////////////////////////

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const Ring *R0, const CoeffRing *coeffR0, size_t nrows, size_t ncols)
  : mGeneralRing(R0),
    mMatrix(*coeffR0, nrows, ncols)
{
}

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const DMat<CoeffRing> &m, size_t nrows, size_t ncols)
  : mGeneralRing(m.get_ring()),
    mMatrix(m.ring(), nrows, ncols)
{
}

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const DMat<CoeffRing> &m)
  : mGeneralRing(m.get_ring()),
    mMatrix(m.mMatrix)
{
}

template<typename CoeffRing>
void DMat<CoeffRing>::resize(size_t new_nrows, size_t new_ncols)
{
  DenseMatrixDef<CoeffRing> newMatrix(ring(), new_nrows, new_ncols);
  mMatrix.swap(newMatrix); // when newMatrix is removed, it frees elements of matrix too
}

template<typename CoeffRing>
double * DMat<CoeffRing>::get_lapack_array() const
{
  return 0;
}

template<typename CoeffRing>
void DMat<CoeffRing>::text_out(buffer& o) const
{
  buffer *p = new buffer[numRows()];
  for (size_t c=0; c<numColumns(); c++)
    {
      size_t maxcount = 0;
      for (size_t r=0; r<numRows(); r++)
        {
          const ElementType& f = mMatrix.entry(r,c);
          if (!ring().is_zero(f))
            {
#warning "don't forget to write elem_text_out for CoeffRing's"
              //              ring().elem_text_out(p[r], f);
            }
          else
            p[r] << ".";
          if (p[r].size() > maxcount)
            maxcount = p[r].size();
        }
      for (size_t r=0; r<numRows(); r++)
        for (size_t k=maxcount+1-p[r].size(); k > 0; k--)
          p[r] << ' ';
    }
  for (size_t r=0; r<numRows(); r++)
    {
      p[r] << '\0';
      char *s = p[r].str();
      o << s << newline;
    }
  delete[] p;
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_matrix(const DMat<CoeffRing>& mat0)
{
  DenseMatrixDef<CoeffRing> newMatrix(mat0.mMatrix);
  mMatrix.swap(newMatrix); // when newMatrix is removed, it frees elements of matrix too
}

template<typename CoeffRing>
size_t DMat<CoeffRing>::lead_row(size_t col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
{
  const ElementType *last = array() + numRows() * col;
  const ElementType *loc = last + numRows() - 1;
  for ( ; loc >= last; loc--)
    {
      if (!ring().is_zero(*loc))
        return (loc-last);
    }
  return -1;
}

template<typename CoeffRing>
size_t DMat<CoeffRing>::lead_row(size_t col, ElementType &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
{
  const ElementType *last = array() + numRows() * col;
  const ElementType *loc = last + numRows() - 1;
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
bool DMat<CoeffRing>::get_entry(size_t r, size_t c, ElementType &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  ring().set(result, mMatrix.entry(r,c));
  return !ring().is_zero(result);
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_entry(size_t r, size_t c, const ElementType &a)
{
  ring().set(mMatrix.entry(r,c), a);
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_rows(size_t i, size_t j)
  /* swap rows: row(i) <--> row(j) */
{
  ElementType *loc1 = array() + i;
  ElementType *loc2 = array() + j;

  for (size_t c=0; c<numColumns(); c++)
    {
      ElementType tmp = *loc1;
      *loc1 = *loc2;
      *loc2 = tmp;
      loc1 += numRows();
      loc2 += numRows();
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_columns(size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
{
  ElementType *loc1 = array() + numRows()*i;
  ElementType *loc2 = array() + numRows()*j;
  for (size_t r=0; r<numRows(); r++)
    {
      ElementType tmp = *loc1;
      *loc1++ = *loc2;
      *loc2++ = tmp;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_row(size_t i, const ElementType &r)
  /* row(i) <- r * row(i) */
{
  ElementType *loc = array() + i;
  for (size_t c=0; c<numColumns(); c++)
    {
      ring().mult(*loc, r, *loc); // *loc = r * *loc
      loc += numRows();
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_column(size_t i, const ElementType &r)
  /* column(i) <- r * column(i) */
{
  ElementType *loc = array() + numRows()*i;
  for (size_t a=0; a<numRows(); a++)
    {
      ring().mult(*loc, r, *loc);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_row(size_t i, const ElementType &r)
  /* row(i) <- row(i) / r */
{
  ElementType *loc = array() + i;
  for (size_t c=0; c<numColumns(); c++)
    {
      ring().divide(*loc, *loc, r);
      loc += numRows();
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::divide_column(size_t i, const ElementType &r)
  /* column(i) <- column(i) / r */
{
  ElementType *loc = array() + numRows()*i;
  for (size_t a=0; a<numRows(); a++)
    {
      ring().divide(*loc, *loc, r);
      loc++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::row_op(size_t i, const ElementType &r, size_t j)
  /* row(i) <- row(i) + r * row(j) */
{
  ElementType *loc1 = array() + i;
  ElementType *loc2 = array() + j;

  ElementType f;
  ring().init(f);
  ring().set_zero(f);
  for (size_t c=0; c<numColumns(); c++)
    {
      ring().mult(f,r,*loc2);
      ring().add(*loc1, f, *loc1);
      loc1 += numRows();
      loc2 += numRows();
    }
  ring().clear(f);
}

template<typename CoeffRing>
void DMat<CoeffRing>::column_op(size_t i, const ElementType &r, size_t j)
  /* column(i) <- column(i) + r * column(j) */
{
  ElementType *loc1 = array() + numRows()*i;
  ElementType *loc2 = array() + numRows()*j;

  ElementType f;
  ring().init(f);
  ring().set_zero(f);
  for (size_t a=0; a<numRows(); a++)
    {
      ring().mult(f,r,*loc2);
      ring().add(*loc1, *loc1, f);
      loc1++;
      loc2++;
    }
  ring().clear(f);
}

template<typename CoeffRing>
void DMat<CoeffRing>::row2by2(size_t r1, size_t r2,
               const ElementType &a1, const ElementType &a2,
               const ElementType &b1, const ElementType &b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  ElementType *loc1 = array() + r1;
  ElementType *loc2 = array() + r2;

  ElementType f1,f2,g1,g2;
  ring().init(f1);
  ring().init(f2);
  ring().init(g1);
  ring().init(g2);
  ring().set_zero(f1);
  ring().set_zero(f2);
  ring().set_zero(g1);
  ring().set_zero(g2);
  for (size_t i=0; i<numColumns(); i++)
    {
      ring().mult(f1,a1,*loc1);
      ring().mult(f2,a2,*loc2);
      ring().mult(g1,b1,*loc1);
      ring().mult(g2,b2,*loc2);

      ring().add(f1,f1,f2);
      ring().add(g1,g1,g2);
      ring().set(*loc1, f1);
      ring().set(*loc2, g1);
      loc1 += numRows();
      loc2 += numRows();
    }
  ring().clear(f1);
  ring().clear(f2);
  ring().clear(g1);
  ring().clear(g2);
}

template<typename CoeffRing>
void DMat<CoeffRing>::column2by2(size_t c1, size_t c2,
                  const ElementType &a1, const ElementType &a2,
                  const ElementType &b1, const ElementType &b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  ElementType *loc1 = array() + c1 * numRows();
  ElementType *loc2 = array() + c2 * numRows();

  ElementType f1,f2,g1,g2;
  ring().init(f1);
  ring().init(f2);
  ring().init(g1);
  ring().init(g2);
  ring().set_zero(f1);
  ring().set_zero(f2);
  ring().set_zero(g1);
  ring().set_zero(g2);
  for (size_t i=0; i<numRows(); i++)
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
  ring().clear(f1);
  ring().clear(f2);
  ring().clear(g1);
  ring().clear(g2);
}

template<typename CoeffRing>
void DMat<CoeffRing>::dot_product(size_t i, size_t j, ElementType &result) const
{
  const ElementType *loc1 = array() + numRows()*i;
  const ElementType *loc2 = array() + numRows()*j;
  ring().set_zero(result);

  ElementType f;
  ring().init(f);
  ring().set_zero(f);
  for (size_t r=0; r<numRows(); r++)
    {
      ring().mult(f,*loc1++,*loc2++);
      ring().add(result,result, f);
    }
  ring().clear(f);
}

template<typename CoeffRing>
void DMat<CoeffRing>::copy_elems(size_t n_to_copy, 
                                 ElementType *target, 
                                 size_t target_stride, 
                                 const ElementType *source, 
                                 size_t stride) const
{
  for (size_t i=0; i<n_to_copy; i++)
    {
      ring().set(*target, *source);
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
  ElementType *tmp = newarray_clear(ElementType,numColumns());
  for (size_t c=0; c<numColumns(); c++)
    ring().init(tmp[c]);
  size_t next = 0;
  ElementType *arr = array() + start_row;

  while (next < nrows_to_permute)
    {
      if (done[next] || perm->array[next] == next)
        {
          next++;
        }
      else
        {
          // store row 'next' into tmp
          copy_elems(numColumns(),tmp,1,arr + next, numRows());

          size_t r = next;
          for (;;)
            {
              // copy row perm[r] to row r
              copy_elems(numColumns(), arr + r, numRows(), arr + perm->array[r], numRows());
              done[r] = true;
              size_t next_r = perm->array[r];
              if (next_r == next) break; // and so r is the previous one
              r = perm->array[r];
            }
          // Now copy tmp back
          copy_elems(numColumns(), arr + r, numRows(), tmp, 1);
          done[r] = true;
        }
    }
  for (size_t c=0; c<numColumns(); c++)
    ring().clear(tmp[c]);
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
  ElementType *tmp = newarray_clear(ElementType,numRows());
  for (size_t r=0; r<numRows(); r++)
    ring().init(tmp[r]);
  size_t next = 0;
  ElementType *arr = array() + start_col * numRows();

  while (next < ncols_to_permute)
    {
      if (done[next] || perm->array[next] == next)
        {
          next++;
        }
      else
        {
          // store col 'next' into tmp
          copy_elems(numRows(),tmp,1,arr + next * numRows(), 1);

          size_t r = next;
          for (;;)
            {
              // copy col perm[r] to col r
              copy_elems(numRows(), arr + r * numRows(), 1, arr + perm->array[r] * numRows(), 1);
              done[r] = true;
              size_t next_r = perm->array[r];
              if (next_r == next) break; // and so r is the previous one
              r = perm->array[r];
            }
          // Now copy tmp back
          copy_elems(numRows(), arr + r * numRows(), 1, tmp, 1);
          done[r] = true;
        }
    }
  for (size_t r=0; r<numRows(); r++)
    ring().clear(tmp[r]);
  deletearray(tmp);
  deletearray(done);
  return true;
}

template<typename CoeffRing>
void DMat<CoeffRing>::insert_columns(size_t i, size_t n_to_add)
/* Insert n_to_add columns directly BEFORE column i. */
{
  size_t new_ncols = numColumns() + n_to_add;
  DenseMatrixDef<CoeffRing> newMatrix(ring(), numRows(), new_ncols);

  for (size_t r=0; r<numRows(); r++)
    {
      for (size_t c=0; c<i; c++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c));
      for (size_t c=i; c<numColumns(); c++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c+n_to_add));
    }
  mMatrix.swap(newMatrix);
}

template<typename CoeffRing>
void DMat<CoeffRing>::insert_rows(size_t i, size_t n_to_add)
/* Insert n_to_add rows directly BEFORE row i. */
{
  size_t new_nrows = numRows() + n_to_add;
  DenseMatrixDef<CoeffRing> newMatrix(ring(), new_nrows, numColumns());

  for (size_t c=0; c<numColumns(); c++)
    {
      for (size_t r=0; r<i; r++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c));
      for (size_t r=i; r<numRows(); r++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r+n_to_add,c));
    }
  mMatrix.swap(newMatrix);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_columns(size_t i, size_t j)
/* Delete columns i .. j from M */
{
  size_t n_to_delete = j-i+1;
  size_t new_ncols = numColumns() - n_to_delete;
  DenseMatrixDef<CoeffRing> newMatrix(ring(), numRows(), new_ncols);

  for (size_t r=0; r<numRows(); r++)
    {
      for (size_t c=0; c<i; c++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c));
      for (size_t c=i; c<=j; c++)
        ring().clear(mMatrix.entry(r,c));
      for (size_t c=j+1; c<numColumns(); c++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c-n_to_delete));
    }
  mMatrix.swap(newMatrix);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_rows(size_t i, size_t j)
/* Delete rows i .. j from M */
{
  size_t n_to_delete = j-i+1;
  size_t new_nrows = numRows() - n_to_delete;
  DenseMatrixDef<CoeffRing> newMatrix(ring(), new_nrows, numColumns());

  for (size_t c=0; c<numColumns(); c++)
    {
      for (size_t r=0; r<i; r++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c));
      for (size_t r=i; r<=j; r++)
        ring().clear(mMatrix.entry(r,c));
      for (size_t r=j+1; r<numRows(); r++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r-n_to_delete,c));
    }
  mMatrix.swap(newMatrix);
}

template<typename CoeffRing>
bool DMat<CoeffRing>::is_zero() const
{
  for (const ElementType *t = array() + numRows()*numColumns() - 1; t >= array(); t--)
    if (!ring().is_zero(*t))
      return false;
  return true;
}

template<typename CoeffRing>
void DMat<CoeffRing>::setFromSubmatrix(const DMat& A, 
                                       M2_arrayint rows,
                                       M2_arrayint cols)
{
  resize(rows->len, cols->len); // resets 'this' to zero matrix
  for (size_t r = 0; r < numRows(); r++)
    for (size_t c = 0; c < cols->len; c++)
      ring().set(mMatrix.entry(r,c), A.mMatrix.entry(rows->array[r],cols->array[c]));
}

template<typename CoeffRing>
void DMat<CoeffRing>::setFromSubmatrix(const DMat& A, 
                                       M2_arrayint cols)
{
  resize(numRows(), cols->len); // resets 'this' to zero matrix
  for (size_t r = 0; r < numRows(); r++)
    for (size_t c = 0; c < cols->len; c++)
      ring().set(mMatrix.entry(r,c), A.mMatrix.entry(r,cols->array[c]));
}

template <typename CoeffRing>
void DMat<CoeffRing>::addInPlace(const DMat<CoeffRing>& B)
  // this += B.
  // assumption:the assert statements below:
{
  M2_ASSERT(&B.ring() == &ring());
  M2_ASSERT(B.numRows() == numRows());
  M2_ASSERT(B.numColumns() == numColumns());
  
  for (size_t i=0; i<numRows()*numColumns(); i++)
    {
      ring().add(array()[i], array()[i], B.array()[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::subtractInPlace(const DMat<CoeffRing>& B)
  // this -= B.
  // assumption:the assert statements below:
{
  M2_ASSERT(&B.ring() == &ring());
  M2_ASSERT(B.numRows() == numRows());
  M2_ASSERT(B.numColumns() == numColumns());
  
  for (size_t i=0; i<numRows()*numColumns(); i++)
    {
      ring().subtract(array()[i], array()[i], B.array()[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::negateInPlace()
  // this = -this
{
  for (size_t i=0; i<numRows()*numColumns(); i++)
    {
      ring().negate(array()[i], array()[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::scalarMultInPlace(const ElementType &f)
  // this = f * this
{
  for (size_t i=0; i<numRows()*numColumns(); i++)
    {
      ring().mult(array()[i], f, array()[i]);
    }
}

template <typename CoeffRing>
bool DMat<CoeffRing>::is_equal(const DMat& B) const
{
  M2_ASSERT(&ring() == &B.ring());
  if (B.numRows() != numRows()) return false;
  if (B.numColumns() != numColumns()) return false;
  size_t top = numRows() * numColumns();
  const ElementType * elemsA = get_array();
  const ElementType * elemsB = B.get_array();
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
