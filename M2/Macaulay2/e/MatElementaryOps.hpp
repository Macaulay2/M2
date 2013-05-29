// Copyright 2013  Michael E. Stillman

#ifndef _mat_elementary_ops_hpp_
#define _mat_elementary_ops_hpp_

template <typename MT> class MatElementaryOps;
template <typename RT> class DMat;
template <typename RT> class SMat;

template <typename RT>
class MatElementaryOps< DMat<RT> >
{
public:
  typedef DMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;
private:
  static void copy_elems(const RT& R, 
                         size_t n_to_copy, 
                         ElementType *target, size_t target_stride, 
                         const ElementType *source, size_t stride)
  {
    for (size_t i=0; i<n_to_copy; i++)
      {
        R.set(*target, *source);
        target += target_stride;
        source += stride;
      }
  }
public:

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
            copy_elems(mat.ring(), mat.numColumns(),tmp,1,arr + next, mat.numRows());
            
            size_t r = next;
            for (;;)
              {
                // copy row perm[r] to row r
                copy_elems(mat.ring(), mat.numColumns(), arr + r, mat.numRows(), arr + perm->array[r], mat.numRows());
                done[r] = true;
                size_t next_r = perm->array[r];
                if (next_r == next) break; // and so r is the previous one
                r = perm->array[r];
              }
            // Now copy tmp back
            copy_elems(mat.ring(), mat.numColumns(), arr + r, mat.numRows(), tmp, 1);
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
            copy_elems(mat.ring(), mat.numRows(),tmp,1,arr + next * mat.numRows(), 1);
            
            size_t r = next;
            for (;;)
              {
                // copy col perm[r] to col r
                copy_elems(mat.ring(), mat.numRows(), arr + r * mat.numRows(), 1, 
                           arr + perm->array[r] * mat.numRows(), 1);
                done[r] = true;
                size_t next_r = perm->array[r];
                if (next_r == next) break; // and so r is the previous one
                r = perm->array[r];
              }
            // Now copy tmp back
            copy_elems(mat.ring(), mat.numRows(), arr + r * mat.numRows(), 1, tmp, 1);
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
    Mat newMatrix(mat.ring(), mat.numRows(), new_ncols);
    
    for (size_t r=0; r<mat.numRows(); r++)
      {
        for (size_t c=0; c<i; c++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r,c));
        for (size_t c=i; c<mat.numColumns(); c++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r,c+n_to_add));
      }
    mat.swap(newMatrix);
  }

  static void insert_rows(Mat& mat, size_t i, size_t n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
  {
    size_t new_nrows = mat.numRows() + n_to_add;
    Mat newMatrix(mat.ring(), new_nrows, mat.numColumns());
    
    for (size_t c=0; c<mat.numColumns(); c++)
      {
        for (size_t r=0; r<i; r++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r,c));
        for (size_t r=i; r<mat.numRows(); r++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r+n_to_add,c));
      }
    mat.swap(newMatrix);
  }

  static void delete_columns(Mat& mat, size_t i, size_t j)
  /* Delete columns i .. j from M */
  {
    M2_ASSERT(i < mat.numColumns());
    M2_ASSERT(j < mat.numColumns());
    M2_ASSERT(i <= j);
    size_t n_to_delete = j-i+1;
    size_t new_ncols = mat.numColumns() - n_to_delete;
    Mat newMatrix(mat.ring(), mat.numRows(), new_ncols);
    
    for (size_t r=0; r<mat.numRows(); r++)
      {
        for (size_t c=0; c<i; c++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r,c));
        for (size_t c=j+1; c<mat.numColumns(); c++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r,c-n_to_delete));
      }
    mat.swap(newMatrix);
  }

  static void delete_rows(Mat& mat, size_t i, size_t j)
  /* Delete rows i .. j from M */
  {
    M2_ASSERT(i < mat.numRows());
    M2_ASSERT(j < mat.numRows());
    M2_ASSERT(i <= j);
    size_t n_to_delete = j-i+1;
    size_t new_nrows = mat.numRows() - n_to_delete;
    Mat newMatrix(mat.ring(), new_nrows, mat.numColumns());
    
    for (size_t c=0; c<mat.numColumns(); c++)
      {
        for (size_t r=0; r<i; r++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r,c));
        for (size_t r=j+1; r<mat.numRows(); r++)
          mat.ring().swap(mat.entry(r,c), newMatrix.entry(r-n_to_delete,c));
      }
    mat.swap(newMatrix);
  }

  static void setFromSubmatrix(const Mat& mat, M2_arrayint rows, M2_arrayint cols, Mat& result)
  /* Set 'result' with the given submatrix of 'mat' */
  {
    result.resize(rows->len, cols->len); // resets to a zero matrix
    for (size_t r = 0; r < rows->len; r++)
      for (size_t c = 0; c < cols->len; c++)
        mat.ring().set(result.entry(r,c), mat.entry(rows->array[r],cols->array[c]));
  }

  static void setFromSubmatrix(const Mat& mat, M2_arrayint cols, Mat& result)
  /* Set 'result' with the given submatrix of 'mat' */
  {
    result.resize(mat.numRows(), cols->len); // resets to a zero matrix
    for (size_t r = 0; r < mat.numRows(); r++)
      for (size_t c = 0; c < cols->len; c++)
        mat.ring().set(result.entry(r,c), mat.entry(r,cols->array[c]));
  }

  static void getEntry(const Mat& mat, size_t r, size_t c, ElementType& result)
  {
    mat.ring().set(result, mat.entry(r,c));
  }

  static void setEntry(Mat& mat, size_t r, size_t c, const ElementType& a)
  {
    mat.ring().set(mat.entry(r,c), a);
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

  static void setFromSubmatrix(const Mat& mat, M2_arrayint rows, M2_arrayint cols, Mat& result)
  /* Set 'result' with the given submatrix of 'mat' */
  {
    result.setFromSubmatrix(mat, rows, cols);
  }

  static void setFromSubmatrix(const Mat& mat, M2_arrayint cols, Mat& result)
  /* Set 'result' with the given submatrix of 'mat' */
  {
    result.setFromSubmatrix(mat,cols);
  }
  
  static void getEntry(const Mat& mat, size_t r, size_t c, ElementType& result)
  {
    mat.get_entry(r,c,result);
  }

  static void setEntry(Mat& mat, size_t r, size_t c, const ElementType& a)
  {
    mat.set_entry(r,c,a);
  }

};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
