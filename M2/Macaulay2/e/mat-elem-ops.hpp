// Copyright 2013  Michael E. Stillman

#ifndef _mat_elementary_ops_hpp_
#define _mat_elementary_ops_hpp_

#include <memory>

template <typename MT>
class MatElementaryOps;
template <typename RT>
class DMat;
template <typename RT>
class SMat;

template <typename RT>
class MatElementaryOps<DMat<RT> >
{
 public:
  typedef DMat<RT> Mat;
  typedef typename Mat::ElementType ElementType;
  typedef typename RT::Element Element;

  // remove
 // private:
 //  template <typename It1, typename It2, typename It3>
 //  static void copy_from_iter(const RT& R, It1 loc1, It2 end1, It3 loc2)
 //  {
 //    for (; loc1 != end1; ++loc1, ++loc2) R.set(*loc1, *loc2);
 //  }

 public:
  static size_t lead_row(const Mat& mat, size_t col)
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column has no non-zero entries */
  {
    size_t row = mat.numRows();
    while (row != 0)
      {
        --row;
        if (!mat.ring().is_zero(mat.entry(row, col))) return row;
      }
    return static_cast<size_t>(-1);
  }

  static size_t lead_row(const Mat& mat, size_t col, ElementType& result)
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
  {
    size_t row = mat.numRows();
    while (row != 0)
      {
        --row;
        if (!mat.ring().is_zero(mat.entry(row, col)))
          {
            mat.ring().set(result, mat.entry(row, col));
            return row;
          }
      }
    return static_cast<size_t>(-1);
  }

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  static void interchange_rows(Mat& mat, size_t i, size_t j)
  /* swap rows: row(i) <--> row(j) */
  {
    assert(i < mat.numRows());
    assert(j < mat.numRows());
    if (i == j) return;

    for (size_t c = 0; c < mat.numColumns(); ++c)
      mat.ring().swap(mat.entry(i,c), mat.entry(j,c));
  }

  static void interchange_columns(Mat& mat, size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
  {
    assert(i < mat.numColumns());
    assert(j < mat.numColumns());
    if (i == j) return;

    for (size_t r = 0; r < mat.numRows(); ++r)
      mat.ring().swap(mat.entry(r,i), mat.entry(r,j));
  }

  static void scale_row(Mat& mat, size_t i, const ElementType& a)
  /* row(i) <- a * row(i) */
  {
    assert(i < mat.numRows());

    for (size_t c = 0; c < mat.numColumns(); ++c)
      mat.ring().mult(mat.entry(i,c), a, mat.entry(i,c));
  }

  static void scale_column(Mat& mat, size_t i, const ElementType& a)
  /* column(i) <- a * column(i) */
  {
    assert(i < mat.numColumns());

    for (size_t r = 0; r < mat.numRows(); ++r)
      mat.ring().mult(mat.entry(r,i), a, mat.entry(r,i));
  }

  static void divide_row(Mat& mat, size_t i, const ElementType& a)
  /* row(i) <- row(i) / a */
  {
    assert(i < mat.numRows());

    for (size_t c = 0; c < mat.numColumns(); ++c)
      mat.ring().divide(mat.entry(i,c), mat.entry(i,c), a);
  }

  static void divide_column(Mat& mat, size_t i, const ElementType& a)
  /* column(i) <- column(i) / a */
  {
    assert(i < mat.numColumns());

    for (size_t r = 0; r < mat.numRows(); ++r)
      mat.ring().divide(mat.entry(r,i), mat.entry(r,i), a);
  }

  static void row_op(Mat& mat, size_t i, const ElementType& a, size_t j)
  /* row(i) <- row(i) + a * row(j) */
  {
    assert(i < mat.numRows());
    assert(j < mat.numRows());
    assert(i != j);

    Element f(mat.ring());
    mat.ring().set_zero(f);

    for (size_t c = 0; c < mat.numColumns(); ++c)
      {
        mat.ring().mult(f, a, mat.entry(j,c));
        mat.ring().add(mat.entry(i,c), f, mat.entry(i,c));
      }
  }

  static void column_op(Mat& mat, size_t i, const ElementType& a, size_t j)
  /* column(i) <- column(i) + a * column(j) */
  {
    assert(i < mat.numColumns());
    assert(j < mat.numColumns());
    assert(i != j);

    Element f(mat.ring());
    mat.ring().set_zero(f);

    for (size_t r = 0; r < mat.numRows(); ++r)
      {
        mat.ring().mult(f, a, mat.entry(r,j));
        mat.ring().add(mat.entry(r,i), f, mat.entry(r,i));
      }
  }

 public:
  static void row2by2(Mat& mat,
                      size_t r1,
                      size_t r2,
                      const ElementType& a1,
                      const ElementType& a2,
                      const ElementType& b1,
                      const ElementType& b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    assert(r1 < mat.numRows());
    assert(r2 < mat.numRows());
    assert(r1 != r2);

    const RT& ring { mat.ring() };
    Element f1(ring), f2(ring), g1(ring), g2(ring);
    ring.set_zero(f1);
    ring.set_zero(f2);
    ring.set_zero(g1);
    ring.set_zero(g2);

    for (size_t c = 0; c < mat.numColumns(); ++c)
      {
        ring.mult(f1, a1, mat.entry(r1,c));
        ring.mult(f2, a2, mat.entry(r2,c));
        ring.add(f1, f1, f2);
        
        ring.mult(g1, b1, mat.entry(r1,c));
        ring.mult(g2, b2, mat.entry(r2,c));
        ring.add(g1, g1, g2);

        ring.set(mat.entry(r1,c), f1);
        ring.set(mat.entry(r2,c), g1);
      }
  }

  static void column2by2(Mat& mat,
                         size_t c1,
                         size_t c2,
                         const ElementType& a1,
                         const ElementType& a2,
                         const ElementType& b1,
                         const ElementType& b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    assert(c1 < mat.numColumns());
    assert(c2 < mat.numColumns());
    assert(c1 != c2);

    const RT& ring { mat.ring() };
    Element f1(ring), f2(ring), g1(ring), g2(ring);
    ring.set_zero(f1);
    ring.set_zero(f2);
    ring.set_zero(g1);
    ring.set_zero(g2);

    for (size_t r = 0; r < mat.numRows(); ++r)
      {
        ring.mult(f1, a1, mat.entry(r, c1));
        ring.mult(f2, a2, mat.entry(r, c2));
        ring.add(f1, f1, f2);
        
        ring.mult(g1, b1, mat.entry(r, c1));
        ring.mult(g2, b2, mat.entry(r, c2));
        ring.add(g1, g1, g2);

        ring.set(mat.entry(r, c1), f1);
        ring.set(mat.entry(r, c2), g1);
      }
  }

  // dot product of *columns* i,j
  static void dot_product(const Mat& mat,
                          size_t i,
                          size_t j,
                          ElementType& result)
  {
    assert(i < mat.numColumns());
    assert(j < mat.numColumns());

    Element f(mat.ring());
    mat.ring().set_zero(f);
    mat.ring().set_zero(result);
    
    for (size_t r = 0; r < mat.numRows(); ++r)
      {
        mat.ring().mult(f, mat.entry(r,i), mat.entry(r,j));
        mat.ring().add(result, result, f);
      }
  }

  static bool row_permute(Mat& mat, size_t start_row, M2_arrayint perm)
  {
    // We copy one row to another location for each cycle in 'perm' of length > 1
    size_t nrows_to_permute = perm->len;
    std::unique_ptr<bool[]> done(new bool[nrows_to_permute]);
    for (size_t i = 0; i < nrows_to_permute; i++) done[i] = true;
    // We check that this is a permutation
    for (size_t i = 0; i < nrows_to_permute; i++)
      {
        size_t j = perm->array[i];
        if (!done[j])
          {
            ERROR("expected permutation");
            return false;
          }
        done[j] = false;
      }
    typename RT::ElementArray tmp(mat.ring(), mat.numColumns());
    size_t next = 0;

    while (next < nrows_to_permute)
      {
        if (done[next] || perm->array[next] == next)
          {
            next++;
          }
        else
          {
            // store row 'next' into tmp
            for (int i=0; i<mat.numColumns(); ++i) std::swap(tmp[i], mat.entry(start_row + next, i));
            
            size_t r = next;
            while (perm->array[r] != next)
              {
                // copy row perm[r] to row r
                for (int i = 0; i < mat.numColumns(); ++i)
                  std::swap(mat.entry(start_row + perm->array[r], i),
                            mat.entry(start_row + r, i));
                done[r] = true;
                r = perm->array[r];
              }
            // we swap tmp and r
            for (int i=0; i<mat.numColumns(); ++i) std::swap(tmp[i], mat.entry(start_row + r, i));
            done[r] = true;
          }
      }
    return true;
  }

  static bool column_permute(Mat& mat, size_t start_col, M2_arrayint perm)
  {
    // We copy one col to another location for each cycle in 'perm' of length > 1
    size_t ncols_to_permute = perm->len;
    std::unique_ptr<bool[]> done(new bool[ncols_to_permute]);
    for (size_t i = 0; i < ncols_to_permute; i++) done[i] = true;
    // We check that this is a permutation
    for (size_t i = 0; i < ncols_to_permute; i++)
      {
        size_t j = perm->array[i];
        if (!done[j])
          {
            ERROR("expected permutation");
            return false;
          }
        done[j] = false;
      }
    typename RT::ElementArray tmp(mat.ring(), mat.numRows());
    size_t next = 0;

    while (next < ncols_to_permute)
      {
        if (done[next] || perm->array[next] == next)
          {
            next++;
          }
        else
          {
            // store col 'next' into tmp
            for (int i=0; i<mat.numRows(); ++i) std::swap(tmp[i], mat.entry(i, start_col + next));
            
            size_t c = next;
            while (perm->array[c] != next)
              {
                // swap col perm[c] to col c
                for (int i = 0; i < mat.numRows(); ++i)
                  std::swap(mat.entry(i, start_col + perm->array[c]),
                            mat.entry(i, start_col + c));
                done[c] = true;
                c = perm->array[c];
              }
            // we swap tmp and c
            for (int i=0; i<mat.numRows(); ++i) std::swap(tmp[i], mat.entry(i, start_col + c));
            done[c] = true;
          }
      }
    return true;
  }
  
  static void insert_columns(Mat& mat, size_t i, size_t n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. TODO: use iterators */
  {
    size_t new_ncols = mat.numColumns() + n_to_add;
    Mat newMatrix(mat.ring(), mat.numRows(), new_ncols);

    for (size_t r = 0; r < mat.numRows(); r++)
      {
        for (size_t c = 0; c < i; c++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r, c));
        for (size_t c = i; c < mat.numColumns(); c++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r, c + n_to_add));
      }
    mat.swap(newMatrix);
  }

  static void insert_rows(Mat& mat, size_t i, size_t n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. TODO: use iterators */
  {
    size_t new_nrows = mat.numRows() + n_to_add;
    Mat newMatrix(mat.ring(), new_nrows, mat.numColumns());

    for (size_t c = 0; c < mat.numColumns(); c++)
      {
        for (size_t r = 0; r < i; r++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r, c));
        for (size_t r = i; r < mat.numRows(); r++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r + n_to_add, c));
      }
    mat.swap(newMatrix);
  }

  static void delete_columns(Mat& mat, size_t i, size_t j)
  /* Delete columns i .. j from M. TODO: use iterators */
  {
    assert(i < mat.numColumns());
    assert(j < mat.numColumns());
    assert(i <= j);
    size_t n_to_delete = j - i + 1;
    size_t new_ncols = mat.numColumns() - n_to_delete;
    Mat newMatrix(mat.ring(), mat.numRows(), new_ncols);

    for (size_t r = 0; r < mat.numRows(); r++)
      {
        for (size_t c = 0; c < i; c++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r, c));
        for (size_t c = j + 1; c < mat.numColumns(); c++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r, c - n_to_delete));
      }
    mat.swap(newMatrix);
  }

  static void delete_rows(Mat& mat, size_t i, size_t j)
  /* Delete rows i .. j from M. TODO: use iterators */
  {
    assert(i < mat.numRows());
    assert(j < mat.numRows());
    assert(i <= j);
    size_t n_to_delete = j - i + 1;
    size_t new_nrows = mat.numRows() - n_to_delete;
    Mat newMatrix(mat.ring(), new_nrows, mat.numColumns());

    for (size_t c = 0; c < mat.numColumns(); c++)
      {
        for (size_t r = 0; r < i; r++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r, c));
        for (size_t r = j + 1; r < mat.numRows(); r++)
          mat.ring().swap(mat.entry(r, c), newMatrix.entry(r - n_to_delete, c));
      }
    mat.swap(newMatrix);
  }

  /////////////////////////////
  // reduce_by_pivots /////////
  /////////////////////////////
 private:
  // An internal function for reduceby_pivots
  static void perform_reduction(Mat& M,
                                size_t r,
                                size_t c,
                                size_t nr,
                                size_t nc,
                                int pivot_type)
  // Subroutine of reduce_pivots()
  // pivot_type: 1 means pivot is 1, -1 means pivot is -1, 0 means pivot is unit
  {
    // Flip rows r, nr
    // Flip cols c, nc
    // Use (nr,nc) location to remove all terms in columns 0..nc-1
    //   and in row nr.
    // Replace column nc with all zeros, except 1 in nr row.

    Element pivot(M.ring()), coef(M.ring()), f(M.ring()), zero(M.ring()),
        one(M.ring());
    M.ring().set_from_long(zero, 0);
    M.ring().set_from_long(one, 1);

    interchange_columns(M, c, nc);
    interchange_rows(M, r, nr);
    long pivotrow = lead_row(M, nc, pivot);
    if (pivot_type == -1)  // pivot is -1
      scale_column(M, nc, pivot);
    else if (pivot_type == 0)
      divide_column(M, nc, pivot);
    for (int i = 0; i < nc; i++)
      {
        pivotrow = lead_row(M, i, coef);
        if (pivotrow < 0) continue;
        if (pivotrow == nr)
          {
            // Do the reduction
            M.ring().negate(f, coef);
            column_op(M, i, f, nc);
          }
      }

    scale_column(M, nc, zero);
    setEntry(M, nr, nc, one);
  }

 public:
  static void reduce_by_pivots(Mat& M)
  {
    if (M.numRows() == 0 or M.numColumns() == 0) return;
    size_t nr = M.numRows() - 1;
    size_t nc = M.numColumns() - 1;

    Element one(M.ring()), minus_one(M.ring());
    M.ring().set_from_long(one, 1);
    M.ring().set_from_long(minus_one, -1);

    // After using the pivot element, it is moved to [nrows-1,ncols-1]
    // and nrows and ncols are decremented.

    for (size_t i = 0; i <= nc; i++)
      {
        for (size_t j = 0; j < M.numRows(); ++j)
          {
            auto& p = M.entry(j,i);
            if (M.ring().is_zero(p)) continue;
            int pivot_type = 0;
            if (M.ring().is_equal(one, p))
              pivot_type = 1;
            else if (M.ring().is_equal(minus_one, p))
              pivot_type = -1;
            if (pivot_type != 0)
              {
                // printf("before reduction: j=%lu i=%lu:\n",j,i);
                // displayMat(M);
                perform_reduction(M, j, i, nr--, nc--, pivot_type);
                // printf("after reduction: j=%lu i=%lu:\n",j,i);
                // displayMat(M);
                if (nr == static_cast<size_t>(-1) or
                    nc == static_cast<size_t>(-1))
                  return;
                // restart loop with the (new) column i
                i = -1;
                break;
              }
          }
      }

    // Now search for other possible pivots
    for (size_t i = 0; i <= nc; i++)
      {
        for (size_t j = 0; j < M.numRows(); ++j)
          {
            auto& p = M.entry(j,i);
            if (M.ring().is_zero(p)) continue;
            if (!M.ring().is_unit(p)) continue;
            int pivot_type = 0;
            if (M.ring().is_equal(one, p))
              pivot_type = 1;
            else if (M.ring().is_equal(minus_one, p))
              pivot_type = -1;

            // printf("before general reduction: j=%lu i=%lu:\n",j,i);
            // displayMat(M);
            perform_reduction(M, j, i, nr--, nc--, pivot_type);
            // printf("after general reduction: j=%lu i=%lu:\n",j,i);
            // displayMat(M);
            if (nr == static_cast<size_t>(-1) or nc == static_cast<size_t>(-1))
              return;
            // restart loop with the (new) column i
            i = -1;
            break;
          }
      }
  }
  //////////////////////////////////

  static void setFromSubmatrix(const Mat& mat,
                               size_t r0,
                               size_t r1,
                               size_t c0,
                               size_t c1,
                               Mat& result)
  /* Set 'result' with the given submatrix of 'mat'. TODO: use iterator on
   * result */
  {
    // assert(r1-r0+1<=result.numRows());
    // assert(c1-c0+1<=result.numColumns());
    for (size_t r = r0; r <= r1; r++)
      for (size_t c = c0; c <= c1; c++)
        mat.ring().set(result.entry(r - r0, c - c0), mat.entry(r, c));
  }

  static void setFromSubmatrix(const Mat& mat,
                               M2_arrayint rows,
                               M2_arrayint cols,
                               Mat& result)
  /* Set 'result' with the given submatrix of 'mat'. TODO: use iterator on
   * result */
  {
    result.resize(rows->len, cols->len);  // resets to a zero matrix
    for (size_t r = 0; r < rows->len; r++)
      for (size_t c = 0; c < cols->len; c++)
        mat.ring().set(result.entry(r, c),
                       mat.entry(rows->array[r], cols->array[c]));
  }

  static void setFromSubmatrix(const Mat& mat, M2_arrayint cols, Mat& result)
  /* Set 'result' with the given submatrix of 'mat'. TODO: use iterator on
   * result */
  {
    result.resize(mat.numRows(), cols->len);  // resets to a zero matrix
    for (size_t r = 0; r < mat.numRows(); r++)
      for (size_t c = 0; c < cols->len; c++)
        mat.ring().set(result.entry(r, c), mat.entry(r, cols->array[c]));
  }

  static void getEntry(const Mat& mat, size_t r, size_t c, ElementType& result)
  {
    mat.ring().set(result, mat.entry(r, c));
  }

  static void setEntry(Mat& mat, size_t r, size_t c, const ElementType& a)
  {
    mat.ring().set(mat.entry(r, c), a);
  }

 private:
#if 0
  // MES June 2013: working on this code
  // Internal function for reduceByPivots
  static void perform_reduction(Mat& M,
                                size_t r, size_t c,
                                size_t nr, size_t nc,
                                int pivot_type)
  // Subroutine of reduceByPivots()
  // pivot_type: 1 means pivot is 1, -1 means pivot is -1, 0 means pivot is unit
  {
    // Swap rows r, nr
    // Swap cols c, nc
    // Use (nr,nc) location to remove all terms in columns 0..nc-1
    //   and in row nr.
    // Replace column nc with all zeros, except 1 in nr row.
    const Ring *R = M->get_ring();
    M->interchange_columns(c,nc);
    M->interchange_rows(r,nr);
    ring_elem pivot;
    long pivotrow = M->lead_row(nc, pivot);
    if (pivot_type == -1) // pivot is -1
      M->scale_column(nc,pivot);
    else if (pivot_type == 0)
      M->divide_column(nc, pivot);
    for (int i=0; i<nc; i++)
      {
        ring_elem coef;
        pivotrow = M->lead_row(i,coef);
        if (pivotrow < 0) continue;
        if (pivotrow == nr)
          {
            // Do the reduction
            //            M.ring().negate(M.entry(pivotrow, i));
            ring_elem f = R->negate(coef);
            M->column_op(i, f, nc);
          }
      }
    M->scale_column(nc, R->zero());
    M->set_entry(nr,nc,R->one());
  }
  
  static void reduceByPivots(Mat& M)
  {
    if (M.numRows() == 0 or M.numColumns() == 0) return;
    size_t nr = M.numRows()-1;
    size_t nc = M.numColumns()-1;

    const Ring *K = M->get_ring();
    ring_elem one = K->one();
    ring_elem minus_one = K->minus_one();
    
    // After using the pivot element, it is moved to [nrows-1,ncols-1]
    // and nrows and ncols are decremented.

    MutableMatrix::iterator *p = M->begin();
    for (int i=0; i<=nc; i++)
      {
        p->set(i);
        for (; p->valid(); p->next())
          {
            int pivot_type = 0;
            ring_elem coef;
            p->copy_ring_elem(coef);
            if (K->is_equal(one, coef))
              pivot_type = 1;
            else if (K->is_equal(minus_one, coef))
              pivot_type = -1;
            if (pivot_type != 0)
              {
                perform_reduction(M, static_cast<int>(p->row()), i, nr, nc, pivot_type);
                if (nr == 0 or nc == 0) return;
                --nr;
                --nc;
                // restart loop with the (new) column i
                i = -1;
                break;
              }
          }
      }
    
    // Now search for other possible pivots
    for (int i=0; i<=nc; i++)
      {
        p->set(i);
        for (; p->valid(); p->next())
          {
            ring_elem coef;
            p->copy_ring_elem(coef);
            if (!K->is_unit(coef)) continue;
            int pivot_type = 0;
            if (K->is_equal(one, coef))
              pivot_type = 1;
            else if (K->is_equal(minus_one, coef))
              pivot_type = -1;
            
            perform_reduction(M, static_cast<int>(p->row()), i, nr, nc, pivot_type);
            if (nr == 0 or nc == 0) return;
            --nr;
            --nc;
            // restart loop with the (new) column i
            i = -1;
            break;
          }
      }
}
#endif
};

template <typename RT>
class MatElementaryOps<SMat<RT> >
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

  static size_t lead_row(const Mat& mat, size_t col, ElementType& result)
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
  /* swap rows: row(i) <--> row(j) */ { mat.interchange_rows(i, j); }
  static void interchange_columns(Mat& mat, size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
  {
    mat.interchange_columns(i, j);
  }
  static void scale_row(Mat& mat, size_t i, const ElementType& r)
  /* row(i) <- r * row(i) */ { mat.scale_row(i, r); }
  static void scale_column(Mat& mat, size_t i, const ElementType& r)
  /* column(i) <- r * column(i) */ { mat.scale_column(i, r); }
  static void divide_row(Mat& mat, size_t i, const ElementType& r)
  /* row(i) <- row(i) / r */ { mat.divide_row(i, r); }
  static void divide_column(Mat& mat, size_t i, const ElementType& r)
  /* column(i) <- column(i) / r */ { mat.divide_column(i, r); }
  static void row_op(Mat& mat, size_t i, const ElementType& r, size_t j)
  /* row(i) <- row(i) + r * row(j) */ { mat.row_op(i, r, j); }
  static void column_op(Mat& mat, size_t i, const ElementType& r, size_t j)
  /* column(i) <- column(i) + r * column(j) */ { mat.column_op(i, r, j); }
  static void column2by2(Mat& mat,
                         size_t c1,
                         size_t c2,
                         const ElementType& a1,
                         const ElementType& a2,
                         const ElementType& b1,
                         const ElementType& b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    mat.column2by2(c1, c2, a1, a2, b1, b2);
  }

  static void row2by2(Mat& mat,
                      size_t r1,
                      size_t r2,
                      const ElementType& a1,
                      const ElementType& a2,
                      const ElementType& b1,
                      const ElementType& b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    mat.row2by2(r1, r2, a1, a2, b1, b2);
  }

  static void dot_product(const Mat& mat,
                          size_t i,
                          size_t j,
                          ElementType& result)
  {
    mat.dot_product(i, j, result);
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
  /* Delete columns i .. j from M */ { mat.delete_columns(i, j); }
  static void delete_rows(Mat& mat, size_t i, size_t j)
  /* Delete rows i .. j from M */ { mat.delete_rows(i, j); }
  static void reduce_by_pivots(Mat& M)
  {
    throw exc::engine_error(
        "reduce_py_pivots not yet implemented for sparse mutable matrices");
    // TODO: write this!!
  }

  static void setFromSubmatrix(const Mat& mat,
                               M2_arrayint rows,
                               M2_arrayint cols,
                               Mat& result)
  /* Set 'result' with the given submatrix of 'mat' */
  {
    result.setFromSubmatrix(mat, rows, cols);
  }

  static void setFromSubmatrix(const Mat& mat, M2_arrayint cols, Mat& result)
  /* Set 'result' with the given submatrix of 'mat' */
  {
    result.setFromSubmatrix(mat, cols);
  }

  static void getEntry(const Mat& mat, size_t r, size_t c, ElementType& result)
  {
    mat.get_entry(r, c, result);
  }

  static void setEntry(Mat& mat, size_t r, size_t c, const ElementType& a)
  {
    mat.set_entry(r, c, a);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
