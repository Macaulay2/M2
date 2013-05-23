// Copyright 2013  Michael E. Stillman

#ifndef _mat_elementary_ops_hpp_
#define _mat_elementary_ops_hpp_

template <typename MT> class MatElementaryOps;
#include "dmat.hpp"
#include "smat.hpp"

#if 0
template <typename MT>
class MatElementaryOps
{
public:
  typedef MT Mat;
  typedef typename Mat::ElementType ElementType;

  static size_t lead_row(const Mat& mat, size_t col)
  {
    throw exc::engine_error("not implemented yet");
  }
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
  
  static size_t lead_row(const Mat& mat, size_t col, ElementType &result);
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

#if 0
  static bool get_entry(const Mat& mat, size_t r, size_t c, ElementType &result);
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  static void set_entry(Mat& mat, size_t r, size_t c, const ElementType &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
#endif

  static void interchange_rows(Mat& mat, size_t i, size_t j);
  /* swap rows: row(i) <--> row(j) */

  static void interchange_columns(Mat& mat, size_t i, size_t j);
  /* swap columns: column(i) <--> column(j) */

  static void scale_row(Mat& mat, size_t i, const ElementType &r);
  /* row(i) <- r * row(i) */

  static void scale_column(Mat& mat, size_t i, const ElementType &r);
  /* column(i) <- r * column(i) */

  static void divide_row(Mat& mat, size_t i, const ElementType &r);
  /* row(i) <- row(i) / r */

  static void divide_column(Mat& mat, size_t i, const ElementType &r);
  /* column(i) <- column(i) / r */

  static void row_op(Mat& mat, size_t i, const ElementType &r, size_t j);
  /* row(i) <- row(i) + r * row(j) */

  static void column_op(Mat& mat, size_t i, const ElementType &r, size_t j);
  /* column(i) <- column(i) + r * column(j) */

  static void column2by2(Mat& mat, 
                  size_t c1, size_t c2,
                  const ElementType &a1, const ElementType &a2,
                  const ElementType &b1, const ElementType &b2);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  static void row2by2(Mat& mat, 
               size_t r1, size_t r2,
               const ElementType &a1, const ElementType &a2,
               const ElementType &b1, const ElementType &b2);
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  static void dot_product(const Mat& mat, size_t i, size_t j, ElementType &result);

  static bool row_permute(Mat& mat, size_t start_row, M2_arrayint perm);

  static bool column_permute(Mat& mat, size_t start_col, M2_arrayint perm);

  static void insert_columns(Mat& mat, size_t i, size_t n_to_add);
  /* Insert n_to_add columns directly BEFORE column i. */

  static void insert_rows(Mat& mat, size_t i, size_t n_to_add);
  /* Insert n_to_add rows directly BEFORE row i. */

  static void delete_columns(Mat& mat, size_t i, size_t j);
  /* Delete columns i .. j from M */

  static void delete_rows(Mat& mat, size_t i, size_t j);
  /* Delete rows i .. j from M */
};
#endif

#if 0

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
  M2_ASSERT(i < numRows());
  M2_ASSERT(j < numRows());
  if (i == j) return;
  ElementType *loc1 = array() + i;
  ElementType *loc2 = array() + j;

  for (size_t c=0; c<numColumns(); c++)
    {
      ring().swap(*loc1, *loc2);
      loc1 += numRows();
      loc2 += numRows();
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::interchange_columns(size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
{
  M2_ASSERT(i < numColumns());
  M2_ASSERT(j < numColumns());
  if (i == j) return;
  ElementType *loc1 = array() + numRows()*i;
  ElementType *loc2 = array() + numRows()*j;
  for (size_t r=0; r<numRows(); r++)
    {
      ring().swap(*loc1, *loc2);
      loc1++;
      loc2++;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::scale_row(size_t i, const ElementType &r)
  /* row(i) <- r * row(i) */
{
  M2_ASSERT(i < numRows());
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
  M2_ASSERT(i < numColumns());
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
      for (size_t c=j+1; c<numColumns(); c++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c-n_to_delete));
    }
  mMatrix.swap(newMatrix);
}

template<typename CoeffRing>
void DMat<CoeffRing>::delete_rows(size_t i, size_t j)
/* Delete rows i .. j from M */
{
  M2_ASSERT(i < numRows());
  M2_ASSERT(j < numRows());
  M2_ASSERT(i <= j);
  size_t n_to_delete = j-i+1;
  size_t new_nrows = numRows() - n_to_delete;
  DenseMatrixDef<CoeffRing> newMatrix(ring(), new_nrows, numColumns());

  for (size_t c=0; c<numColumns(); c++)
    {
      for (size_t r=0; r<i; r++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r,c));
      for (size_t r=j+1; r<numRows(); r++)
        ring().swap(mMatrix.entry(r,c), newMatrix.entry(r-n_to_delete,c));
    }
  mMatrix.swap(newMatrix);
}
#endif

template <typename RT>
class MatElementaryOps< DMat<RT> >
{
public:
  typedef DMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;

  static size_t lead_row(const Mat& mat, size_t col)
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column has no non-zero entries */
  {
    const ElementType *last = mat.array() + mat.numRows() * col;
    const ElementType *loc = last + mat.numRows() - 1;
    for ( ; loc >= last; loc--)
      {
        if (!mat.ring().is_zero(*loc))
          return (loc-last);
      }
    return -1;
  }

  static size_t lead_row(const Mat& mat, size_t col, ElementType &result)
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
  {
    const ElementType *last = mat.array() + mat.numRows() * col;
    const ElementType *loc = last + mat.numRows() - 1;
    for ( ; loc >= last; loc--)
      {
        if (!mat.ring().is_zero(*loc))
          {
            mat.ring().set(result, *loc);
            return static_cast<size_t>(loc-last);
          }
      }
    return -1;
  }

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

#if 0
  static bool get_entry(const Mat& mat, size_t r, size_t c, ElementType &result)
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
  {
    mat.ring().set(result, mat.mMatrix.entry(r,c));
    return !mat.ring().is_zero(result);
  }

  static void set_entry(Mat& mat, size_t r, size_t c, const ElementType &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
#endif

  static void interchange_rows(Mat& mat, size_t i, size_t j)
  /* swap rows: row(i) <--> row(j) */
  {
    M2_ASSERT(i < mat.numRows());
    M2_ASSERT(j < mat.numRows());
    if (i == j) return;
    ElementType *loc1 = mat.array() + i;
    ElementType *loc2 = mat.array() + j;
    
    for (size_t c=0; c < mat.numColumns(); c++)
      {
        mat.ring().swap(*loc1, *loc2);
        loc1 += mat.numRows();
        loc2 += mat.numRows();
      }
  }

  static void interchange_columns(Mat& mat, size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
  {
    M2_ASSERT(i < mat.numColumns());
    M2_ASSERT(j < mat.numColumns());
    if (i == j) return;
    ElementType *loc1 = mat.array() + mat.numRows()*i;
    ElementType *loc2 = mat.array() + mat.numRows()*j;
    for (size_t r=0; r<mat.numRows(); r++)
      {
        mat.ring().swap(*loc1, *loc2);
        loc1++;
        loc2++;
      }
  }

  static void scale_row(Mat& mat, size_t i, const ElementType &r)
  /* row(i) <- r * row(i) */
  {
    M2_ASSERT(i < mat.numRows());
    ElementType *loc = mat.array() + i;
    for (size_t c=0; c<mat.numColumns(); c++)
      {
        mat.ring().mult(*loc, r, *loc); // *loc = r * *loc
        loc += mat.numRows();
      }
  }

  static void scale_column(Mat& mat, size_t i, const ElementType &r)
  /* column(i) <- r * column(i) */
  {
    M2_ASSERT(i < mat.numColumns());
    ElementType *loc = mat.array() + mat.numRows()*i;
    for (size_t a=0; a<mat.numRows(); a++)
      {
        mat.ring().mult(*loc, r, *loc);
        loc++;
      }
  }

  static void divide_row(Mat& mat, size_t i, const ElementType &r)
  /* row(i) <- row(i) / r */
  {
    ElementType *loc = mat.array() + i;
    for (size_t c=0; c<mat.numColumns(); c++)
      {
        mat.ring().divide(*loc, *loc, r);
        loc += mat.numRows();
      }
  }

  static void divide_column(Mat& mat, size_t i, const ElementType &r)
  /* column(i) <- column(i) / r */
  {
    ElementType *loc = mat.array() + mat.numRows()*i;
    for (size_t a=0; a<mat.numRows(); a++)
      {
        mat.ring().divide(*loc, *loc, r);
        loc++;
      }
  }

  static void row_op(Mat& mat, size_t i, const ElementType &r, size_t j)
  /* row(i) <- row(i) + r * row(j) */
  {
    ElementType *loc1 = mat.array() + i;
    ElementType *loc2 = mat.array() + j;
    
    ElementType f;
    mat.ring().init(f);
    mat.ring().set_zero(f);
    for (size_t c=0; c<mat.numColumns(); c++)
      {
        mat.ring().mult(f,r,*loc2);
        mat.ring().add(*loc1, f, *loc1);
        loc1 += mat.numRows();
        loc2 += mat.numRows();
      }
    mat.ring().clear(f);
  }

  static void column_op(Mat& mat, size_t i, const ElementType &r, size_t j)
  /* column(i) <- column(i) + r * column(j) */
  {
    ElementType *loc1 = mat.array() + mat.numRows()*i;
    ElementType *loc2 = mat.array() + mat.numRows()*j;
    
    ElementType f;
    mat.ring().init(f);
    mat.ring().set_zero(f);
    for (size_t a=0; a<mat.numRows(); a++)
      {
        mat.ring().mult(f,r,*loc2);
        mat.ring().add(*loc1, *loc1, f);
        loc1++;
        loc2++;
      }
    mat.ring().clear(f);
  }

  static void column2by2(Mat& mat, 
                  size_t c1, size_t c2,
                  const ElementType &a1, const ElementType &a2,
                  const ElementType &b1, const ElementType &b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    ElementType *loc1 = mat.array() + c1 * mat.numRows();
    ElementType *loc2 = mat.array() + c2 * mat.numRows();
    
    ElementType f1,f2,g1,g2;
    mat.ring().init(f1);
    mat.ring().init(f2);
    mat.ring().init(g1);
    mat.ring().init(g2);
    mat.ring().set_zero(f1);
    mat.ring().set_zero(f2);
    mat.ring().set_zero(g1);
    mat.ring().set_zero(g2);
    for (size_t i=0; i<mat.numRows(); i++)
      {
        mat.ring().mult(f1,a1,*loc1);
        mat.ring().mult(f2,a2,*loc2);
        mat.ring().mult(g1,b1,*loc1);
        mat.ring().mult(g2,b2,*loc2);
        
        mat.ring().add(f1,f1,f2);
        mat.ring().add(g1,g1,g2);
        mat.ring().set(*loc1++, f1);
        mat.ring().set(*loc2++, g1);
      }
    mat.ring().clear(f1);
    mat.ring().clear(f2);
    mat.ring().clear(g1);
    mat.ring().clear(g2);
  }

  static void row2by2(Mat& mat, 
               size_t r1, size_t r2,
               const ElementType &a1, const ElementType &a2,
               const ElementType &b1, const ElementType &b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    ElementType *loc1 = mat.array() + r1;
    ElementType *loc2 = mat.array() + r2;
    
    ElementType f1,f2,g1,g2;
    mat.ring().init(f1);
    mat.ring().init(f2);
    mat.ring().init(g1);
    mat.ring().init(g2);
    mat.ring().set_zero(f1);
    mat.ring().set_zero(f2);
    mat.ring().set_zero(g1);
    mat.ring().set_zero(g2);
    for (size_t i=0; i<mat.numColumns(); i++)
      {
        mat.ring().mult(f1,a1,*loc1);
        mat.ring().mult(f2,a2,*loc2);
        mat.ring().mult(g1,b1,*loc1);
        mat.ring().mult(g2,b2,*loc2);
        
        mat.ring().add(f1,f1,f2);
        mat.ring().add(g1,g1,g2);
        mat.ring().set(*loc1, f1);
        mat.ring().set(*loc2, g1);
        loc1 += mat.numRows();
        loc2 += mat.numRows();
      }
    mat.ring().clear(f1);
    mat.ring().clear(f2);
    mat.ring().clear(g1);
    mat.ring().clear(g2);
  }

  static void dot_product(const Mat& mat, size_t i, size_t j, ElementType &result)
  {
    const ElementType *loc1 = mat.array() + mat.numRows()*i;
    const ElementType *loc2 = mat.array() + mat.numRows()*j;
    mat.ring().set_zero(result);
    
    ElementType f;
    mat.ring().init(f);
    mat.ring().set_zero(f);
    for (size_t r=0; r < mat.numRows(); r++)
      {
        mat.ring().mult(f,*loc1++,*loc2++);
        mat.ring().add(result,result, f);
      }
    mat.ring().clear(f);
  }

  static bool row_permute(Mat& mat, size_t start_row, M2_arrayint perm)
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
    ElementType *tmp = newarray_clear(ElementType,mat.numColumns());
    for (size_t c=0; c<mat.numColumns(); c++)
      mat.ring().init(tmp[c]);
    size_t next = 0;
    ElementType *arr = mat.array() + start_row;
    
    while (next < nrows_to_permute)
      {
        if (done[next] || perm->array[next] == next)
          {
            next++;
          }
        else
          {
            // store row 'next' into tmp
            mat.copy_elems(mat.numColumns(),tmp,1,arr + next, mat.numRows());
            
            size_t r = next;
            for (;;)
              {
                // copy row perm[r] to row r
                mat.copy_elems(mat.numColumns(), arr + r, mat.numRows(), arr + perm->array[r], mat.numRows());
                done[r] = true;
                size_t next_r = perm->array[r];
                if (next_r == next) break; // and so r is the previous one
                r = perm->array[r];
              }
            // Now copy tmp back
            mat.copy_elems(mat.numColumns(), arr + r, mat.numRows(), tmp, 1);
            done[r] = true;
          }
      }
    for (size_t c=0; c<mat.numColumns(); c++)
      mat.ring().clear(tmp[c]);
    deletearray(tmp);
    deletearray(done);
    return true;
  }

  static bool column_permute(Mat& mat, size_t start_col, M2_arrayint perm)
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
    ElementType *tmp = newarray_clear(ElementType,mat.numRows());
    for (size_t r=0; r<mat.numRows(); r++)
      mat.ring().init(tmp[r]);
    size_t next = 0;
    ElementType *arr = mat.array() + start_col * mat.numRows();
    
    while (next < ncols_to_permute)
      {
        if (done[next] || perm->array[next] == next)
          {
            next++;
          }
        else
          {
            // store col 'next' into tmp
            mat.copy_elems(mat.numRows(),tmp,1,arr + next * mat.numRows(), 1);
            
            size_t r = next;
            for (;;)
              {
                // copy col perm[r] to col r
                mat.copy_elems(mat.numRows(), arr + r * mat.numRows(), 1, 
                               arr + perm->array[r] * mat.numRows(), 1);
                done[r] = true;
                size_t next_r = perm->array[r];
                if (next_r == next) break; // and so r is the previous one
                r = perm->array[r];
              }
            // Now copy tmp back
            mat.copy_elems(mat.numRows(), arr + r * mat.numRows(), 1, tmp, 1);
            done[r] = true;
          }
      }
    for (size_t r=0; r<mat.numRows(); r++)
      mat.ring().clear(tmp[r]);
    deletearray(tmp);
    deletearray(done);
    return true;
  }

  static void insert_columns(Mat& mat, size_t i, size_t n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. */
  {
    size_t new_ncols = mat.numColumns() + n_to_add;
    DenseMatrixDef<RT> newMatrix(mat.ring(), mat.numRows(), new_ncols);
    
    for (size_t r=0; r<mat.numRows(); r++)
      {
        for (size_t c=0; c<i; c++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r,c));
        for (size_t c=i; c<mat.numColumns(); c++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r,c+n_to_add));
      }
    mat.getInternalMatrix().swap(newMatrix);
  }

  static void insert_rows(Mat& mat, size_t i, size_t n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
  {
    size_t new_nrows = mat.numRows() + n_to_add;
    DenseMatrixDef<RT> newMatrix(mat.ring(), new_nrows, mat.numColumns());
    
    for (size_t c=0; c<mat.numColumns(); c++)
      {
        for (size_t r=0; r<i; r++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r,c));
        for (size_t r=i; r<mat.numRows(); r++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r+n_to_add,c));
      }
    mat.getInternalMatrix().swap(newMatrix);
  }

  static void delete_columns(Mat& mat, size_t i, size_t j)
  /* Delete columns i .. j from M */
  {
    M2_ASSERT(i < mat.numColumns());
    M2_ASSERT(j < mat.numColumns());
    M2_ASSERT(i <= j);
    size_t n_to_delete = j-i+1;
    size_t new_ncols = mat.numColumns() - n_to_delete;
    DenseMatrixDef<RT> newMatrix(mat.ring(), mat.numRows(), new_ncols);
    
    for (size_t r=0; r<mat.numRows(); r++)
      {
        for (size_t c=0; c<i; c++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r,c));
        for (size_t c=j+1; c<mat.numColumns(); c++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r,c-n_to_delete));
      }
    mat.getInternalMatrix().swap(newMatrix);
  }

  static void delete_rows(Mat& mat, size_t i, size_t j)
  /* Delete rows i .. j from M */
  {
    M2_ASSERT(i < mat.numRows());
    M2_ASSERT(j < mat.numRows());
    M2_ASSERT(i <= j);
    size_t n_to_delete = j-i+1;
    size_t new_nrows = mat.numRows() - n_to_delete;
    DenseMatrixDef<RT> newMatrix(mat.ring(), new_nrows, mat.numColumns());
    
    for (size_t c=0; c<mat.numColumns(); c++)
      {
        for (size_t r=0; r<i; r++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r,c));
        for (size_t r=j+1; r<mat.numRows(); r++)
          mat.ring().swap(mat.getInternalMatrix().entry(r,c), newMatrix.entry(r-n_to_delete,c));
      }
    mat.getInternalMatrix().swap(newMatrix);
  }
};

template <typename RT>
class MatElementaryOps< SMat<RT> >
{
public:
  typedef SMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;

  static size_t lead_row(const Mat& mat, size_t col)
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
  {
    return mat.lead_row(col);
  }  

  static size_t lead_row(const Mat& mat, size_t col, ElementType &result)
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
  {
    return mat.lead_row(col, result);
  }
  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

#if 0
  static bool get_entry(const Mat& mat, size_t r, size_t c, ElementType &result);
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  static void set_entry(Mat& mat, size_t r, size_t c, const ElementType &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
#endif

  static void interchange_rows(Mat& mat, size_t i, size_t j)
  /* swap rows: row(i) <--> row(j) */
  {
    mat.interchange_rows(i,j);
  }

  static void interchange_columns(Mat& mat, size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
  {
    mat.interchange_columns(i,j);
  }
  static void scale_row(Mat& mat, size_t i, const ElementType &r)
  /* row(i) <- r * row(i) */
  {
    mat.scale_row(i,r);
  }

  static void scale_column(Mat& mat, size_t i, const ElementType &r)
  /* column(i) <- r * column(i) */
  {
    mat.scale_column(i,r);
  }

  static void divide_row(Mat& mat, size_t i, const ElementType &r)
  /* row(i) <- row(i) / r */
  {
    mat.divide_row(i,r);
  }

  static void divide_column(Mat& mat, size_t i, const ElementType &r)
  /* column(i) <- column(i) / r */
  {
    mat.divide_column(i,r);
  }

  static void row_op(Mat& mat, size_t i, const ElementType &r, size_t j)
  /* row(i) <- row(i) + r * row(j) */
  {
    mat.row_op(i,r,j);
  }

  static void column_op(Mat& mat, size_t i, const ElementType &r, size_t j)
  /* column(i) <- column(i) + r * column(j) */
  {
    mat.column_op(i,r,j);
  }

  static void column2by2(Mat& mat, 
                  size_t c1, size_t c2,
                  const ElementType &a1, const ElementType &a2,
                  const ElementType &b1, const ElementType &b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    mat.column2by2(c1,c2,a1,a2,b1,b2);
  }

  static void row2by2(Mat& mat, 
               size_t r1, size_t r2,
               const ElementType &a1, const ElementType &a2,
               const ElementType &b1, const ElementType &b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    mat.row2by2(r1,r2,a1,a2,b1,b2);
  }

  static void dot_product(const Mat& mat, size_t i, size_t j, ElementType &result)
  {
    mat.dot_product(i,j,result);
  }

  static bool row_permute(Mat& mat, size_t start_row, M2_arrayint perm)
  {
    return mat.row_permute(start_row, perm);
  }

  static bool column_permute(Mat& mat, size_t start_col, M2_arrayint perm)
  {
    return mat.column_permute(start_col, perm);
  }

  static void insert_columns(Mat& mat, size_t i, size_t n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. */
  {
    mat.insert_columns(i, n_to_add);
  }

  static void insert_rows(Mat& mat, size_t i, size_t n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
  {
    mat.insert_rows(i, n_to_add);
  }

  static void delete_columns(Mat& mat, size_t i, size_t j)
  /* Delete columns i .. j from M */
  {
    mat.delete_columns(i, j);
  }

  static void delete_rows(Mat& mat, size_t i, size_t j)
  /* Delete rows i .. j from M */
  {
    mat.delete_rows(i,j);
  }
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
