// Copyright 2005-2012  Michael E. Stillman

#ifndef _mutable_mat_hpp_
#define _mutable_mat_hpp_

#include "mat.hpp"

inline bool error_column_bound(int c, int ncols)
{
  if (c < 0 || c >= ncols)
    {
      ERROR("column out of range");
      return true;
    }
  return false;
}

inline bool error_row_bound(int r, int nrows)
{
  if (r < 0 || r >= nrows)
    {
      ERROR("row out of range");
      return true;
    }
  return false;
}

/**
 * \ingroup matrices
 */

template<typename Mat>
class MutableMat : public MutableMatrix
{
public:
  typedef Mat MatType;
  typedef typename Mat::CoeffRing CoeffRing;
  typedef typename CoeffRing::elem elem;

private:
  Mat mat;
  // This class wraps the operations for Mat to make a MutableMatrix

  // Same operations as in MutableMatrix.  Almost nothing is
  // done except to call the Mat routines.

  // Linear algebra routines: Default is to return 0.
  // Caller must then make error message?
  // Specific template instances must provide these functions
  MutableMat() {}

  MutableMat(const Ring *R, const CoeffRing *coeffR, int nrows, int ncols)
    : mat(R,coeffR,nrows,ncols) {}

  MutableMat(Mat &m) : mat(m) {}

public:
  // Make a zero matrix, using the same ring and density taken from 'mat'.
  MutableMat* makeZeroMatrix(size_t nrows, size_t ncols) const {
    return zero_matrix(mat.get_ring(), &mat.ring(), nrows, ncols);
  }

  Mat * get_Mat() { return &mat; }
  const Mat * get_Mat() const { return &mat; }

  Mat& getMat() { return mat; }
  const Mat& getMat() const { return mat; }

  class iterator : public MutableMatrix::iterator
  {
    typename Mat::iterator i;
  public:
    iterator(const Mat *M0) : i(M0) {}
    void set(int col0) { i.set(col0); }
    void next() { i.next(); }
    bool valid() { return i.valid(); }
    int row() { return i.row(); }
    elem value() { return i.value(); }
    void copy_ring_elem(ring_elem &result) { i.copy_elem(result); }
  };

  static MutableMat *zero_matrix(const Ring *R, const CoeffRing *coeffR, int nrows, int ncols)
  {
    return new MutableMat(R,coeffR,nrows,ncols);
  }

  //  static MutableMat *grab_Mat(const Mat *m);

  virtual iterator * begin() const { return new iterator(&mat); }

  virtual const Ring * get_ring() const { return mat.get_ring(); }

  virtual int n_rows() const { return mat.n_rows(); }

  virtual int n_cols() const { return mat.n_cols(); }

  virtual bool is_dense() const { return mat.is_dense(); }

  virtual MutableMat *copy(bool prefer_dense) const
  {
#if 0
    MutableMat *result = new MutableMat;
    Mat *m = mat.copy();
    result->mat.grab(m);
    return result;
#endif
    return clone();
  }

  virtual MutableMat *clone() const
  {
    Mat *m = new Mat(mat); // copies mat
    return new MutableMat(*m); // places copy into result
  }

  virtual int lead_row(int col) const { return mat.lead_row(col); }
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual int lead_row(int col, ring_elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */
  {
    elem b;
    mat.get_CoeffRing()->set_zero(b);
    int ret = mat.lead_row(col, b);
    if (ret >= 0)
      mat.get_CoeffRing()->to_ring_elem(result, b);
    return ret;
  }

  virtual bool get_entry(int r, int c, ring_elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
  {
    if (r >= 0 && r < n_rows() && c >= 0 && c < n_cols())
      {
        elem a;
        mat.get_CoeffRing()->set_zero(a);
        if (mat.get_entry(r,c,a))
          {
            mat.get_CoeffRing()->to_ring_elem(result,a);
            return true;
          }
      }

    result = mat.get_ring()->zero();
    return false;
  }

  virtual bool set_entry(int r, int c, const ring_elem a)
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
  {
    if (error_row_bound(r,n_rows())) return false;
    if (error_column_bound(c,n_cols())) return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,a);
    mat.set_entry(r,c,b);
    return true;
  }

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual bool interchange_rows(int i, int j)
  /* swap rows: row(i) <--> row(j) */
  {
    int nrows = n_rows();
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      return false;
    mat.interchange_rows(i,j);
    return true;
  }

  virtual bool interchange_columns(int i, int j)
  /* swap columns: column(i) <--> column(j) */
  {
    int ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    mat.interchange_columns(i,j);
    return true;
  }

  virtual bool scale_row(int i, ring_elem r)
  /* row(i) <- r * row(i) */
  {
    int nrows = n_rows();
    if (error_row_bound(i,nrows))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.scale_row(i,b);
    return true;
  }

  virtual bool scale_column(int i, ring_elem r)
  /* column(i) <- r * column(i) */
  {
    int ncols = n_cols();
    if (error_column_bound(i,ncols))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.scale_column(i,b);
    return true;
  }

  virtual bool divide_row(int i, ring_elem r)
  /* row(i) <- row(i) / r */
  {
    int nrows = n_rows();
    if (error_row_bound(i,nrows))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.divide_row(i,b);
    return true;
  }

  virtual bool divide_column(int i, ring_elem r)
  /* column(i) <- column(i) / r */
  {
    int ncols = n_cols();
    if (error_column_bound(i,ncols))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.divide_column(i,b);
    return true;
  }

  virtual bool row_op(int i, ring_elem r, int j)
  /* row(i) <- row(i) + r * row(j) */
  {
    int nrows = n_rows();
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      return false;
    if (i == j) return true;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.row_op(i,b,j);
    return true;
  }

  virtual bool column_op(int i, ring_elem r, int j)
  /* column(i) <- column(i) + r * column(j) */
  {
    int ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    if (i == j) return true;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.column_op(i,b,j);
    return true;
  }

  virtual bool column2by2(int c1, int c2,
                          ring_elem a1, ring_elem a2,
                          ring_elem b1, ring_elem b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    int ncols = n_cols();
    if (error_column_bound(c1,ncols) || error_column_bound(c2,ncols))
      return false;
    if (c1 == c2) return true;
    elem aa1, aa2, bb1, bb2;
    mat.get_CoeffRing()->from_ring_elem(aa1,a1);
    mat.get_CoeffRing()->from_ring_elem(aa2,a2);
    mat.get_CoeffRing()->from_ring_elem(bb1,b1);
    mat.get_CoeffRing()->from_ring_elem(bb2,b2);
    mat.column2by2(c1,c2,aa1,aa2,bb1,bb2);
    return true;
  }

  virtual bool row2by2(int r1, int r2,
                       ring_elem a1, ring_elem a2,
                       ring_elem b1, ring_elem b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    int nrows = n_rows();
    if (error_row_bound(r1,nrows) || error_row_bound(r2,nrows))
      return false;
    if (r1 == r2) return true;
    elem aa1, aa2, bb1, bb2;
    mat.get_CoeffRing()->from_ring_elem(aa1,a1);
    mat.get_CoeffRing()->from_ring_elem(aa2,a2);
    mat.get_CoeffRing()->from_ring_elem(bb1,b1);
    mat.get_CoeffRing()->from_ring_elem(bb2,b2);
    mat.row2by2(r1,r2,aa1,aa2,bb1,bb2);
    return true;
  }

  virtual bool dot_product(int c1, int c2, ring_elem &result) const
  {
    int ncols = n_cols();
    if (error_column_bound(c1,ncols) || error_column_bound(c2,ncols))
      return false;
    elem a;
    mat.get_CoeffRing()->set_zero(a);
    mat.dot_product(c1,c2,a);
    mat.get_CoeffRing()->to_ring_elem(result,a);
    return true;
  }

  virtual bool row_permute(int start_row, M2_arrayint perm)
  {
    return mat.row_permute(start_row, perm);
  }

  virtual bool column_permute(int start_col, M2_arrayint perm)
  {
    return mat.column_permute(start_col, perm);
  }

  virtual bool insert_columns(int i, int n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. */
  {
    if (i < 0 || i > n_cols() || n_to_add < 0)
      {
        ERROR("cannot insert %l columns before column %ln",n_to_add,i);
        return false;
      }
    mat.insert_columns(i, n_to_add);
    return true;
  }

  virtual bool insert_rows(int i, int n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
  {
    if (i < 0 || i > n_rows() || n_to_add < 0)
      {
        ERROR("cannot insert %l rows before row %ln",n_to_add,i);
        return false;
      }
    mat.insert_rows(i, n_to_add);
    return true;
  }

  virtual bool delete_columns(int i, int j)
  /* Delete columns i .. j from M */
  {
    int ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      {
        ERROR("column index out of range");
        return false;
      }

    mat.delete_columns(i, j);
    return true;
  }

  virtual bool delete_rows(int i, int j)
  /* Delete rows i .. j from M */
  {
    int nrows = n_rows();
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      {
        ERROR("row index out of range");
        return false;
      }
    mat.delete_rows(i, j);
    return true;
  }

  virtual MutableMatrix * submatrix(M2_arrayint rows, M2_arrayint cols) const
  {
    for (int r = 0; r<rows->len; r++)
      if (rows->array[r] < 0 || rows->array[r] >= n_rows())
        {
          ERROR("row index %d out of bounds 0..%d", rows->array[r], n_rows()-1);
          return 0;
        }
    for (int c = 0; c<cols->len; c++)
      if (cols->array[c] < 0 || cols->array[c] >= n_cols())
        {
          ERROR("column index %d out of bounds 0..%d", cols->array[c], n_cols()-1);
          return 0;
        }
    MutableMat *result = new MutableMat(*this); // zero matrix, over the same ring
    result->getMat().setFromSubmatrix(getMat(), rows, cols);
    return result;
  }

  virtual MutableMatrix * submatrix(M2_arrayint cols) const
  {
    for (int c = 0; c<cols->len; c++)
      if (cols->array[c] < 0 || cols->array[c] >= n_cols())
        {
          ERROR("column index %d out of bounds 0..%d", cols->array[c], n_cols()-1);
          return 0;
        }
    MutableMat *result = new MutableMat(*this); // zero matrix, over the same ring
    result->getMat().setFromSubmatrix(getMat(), cols);
    return result;
  }

  virtual bool set_submatrix(M2_arrayint rows,
                             M2_arrayint cols,
                             const MutableMatrix *N)
  // returns false iff there is an error
  {
    return mat.set_submatrix(rows,cols,N);
  }

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual bool is_zero() const
  {
    return mat.is_zero();
  }

  virtual bool is_equal(const MutableMatrix *B) const
  {
    const MutableMat *B1 = dynamic_cast<const MutableMat *>(B);
    if (B1 == NULL || &B1->getMat().ring() != &getMat().ring())
      return false;
    return getMat().is_equal(B1->getMat());
  }

  virtual MutableMat * add(const MutableMatrix *B) const
  // return this + B.  return NULL if sizes or types do not match.
  {
    const Mat *B1 = B->coerce<Mat>();
    if (B1 == NULL) 
      {
        ERROR("expected matrices with the same ring and sparsity");
        return NULL;
      }
    if (B->get_ring() != get_ring())
      {
        ERROR("expected matrices with the same ring");
        return NULL;
      }
    if (B1->n_rows() != n_rows() || B1->n_cols() != n_cols())
      {
        ERROR("expected matrices of the same shape");
        return NULL;
      }

    MutableMat* result = clone();
    result->getMat().addInPlace(*B1);
    return result;
  }

  virtual MutableMatrix * negate() const
  {
    MutableMat *result = clone();
    result->getMat().negateInPlace();
    return result;
  }

  virtual MutableMat * subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  {
    const MutableMat *B1 = dynamic_cast<const MutableMat *>(B);
    if (B1 == NULL || &B1->getMat().ring() != &getMat().ring())
      {
        ERROR("expected matrices with same sparsity and same base ring");
        return 0;
      }
    MutableMat *result = clone();
    result->getMat().subtractInPlace(B1->getMat());
    return result;
  }

  virtual MutableMat * mult(const MutableMatrix *B) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
  {
    MutableMat *result = new MutableMat;
    result->mat.grab(mat.mult(B));
    return result;
  }

  virtual MutableMat * mult(const RingElement *f) const
  // return f*this.  return NULL of sizes or types do not match.
  {
    if (f->get_ring() != get_ring())
      {
        ERROR("expected same ring");
        return 0;
      }
    elem a;
    mat.ring().from_ring_elem(a, f->get_value());

    MutableMat *result = clone();
    result->mat.scalarMultInPlace(a);

    return result;
  }

  ///////////////////////////////
  // Linear algebra /////////////
  ///////////////////////////////

  virtual bool solve(const MutableMatrix *b, MutableMatrix *x) const;
  // resets x, find a basis of solutions for Ax=b
  // assumes that 'this' is full rank and a square matrix

  virtual bool nullspaceU(MutableMatrix *x) const;
  // resets x, find a basis of solutions for Ux=0, U upper triangular

  virtual M2_arrayintOrNull LU(MutableMatrix *L,
                                MutableMatrix *U) const;

  virtual bool eigenvalues(MutableMatrix *eigenvals, bool is_symm_or_hermitian) const;

  virtual bool eigenvectors(MutableMatrix *eigenvals,
                            MutableMatrix *eigenvecs,
                            bool is_symm_or_hermitian) const;

  virtual bool SVD(MutableMatrix *Sigma,
                   MutableMatrix *U,
                   MutableMatrix *Vt,
                   bool use_divide_and_conquer) const;

  virtual bool least_squares(const MutableMatrix *b,
                             MutableMatrix *x,
                             bool assume_full_rank) const;



  virtual engine_RawArrayIntPairOrNull LQUPFactorizationInPlace(bool transpose);

  /// Fast linear algebra routines (well, fast for some rings)

  virtual size_t rank() const;

  virtual const RingElement* determinant() const;

  // Find the inverse matrix.  If the matrix is not square, or 
  // the ring is one in which th matrix cannot be inverted,
  // then NULL is returned, and an error message is set.
  virtual MutableMatrix* invert() const;

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  virtual M2_arrayintOrNull rankProfile(bool row_profile) const;
  
  // Find a spanning set for the null space.  If M = this,
  // and right_side is true, return a matrix whose rows span {x |  xM = 0},
  // otherwise return a matrix whose columns span {x | Mx = 0}
  virtual MutableMatrix* nullSpace(bool right_side) const;

  // Return a matrix whose rows or columns solve either Ax = B (right_side=true)
  // or xA = B (right_side=false).  The first argument returned is false
  // in this case.
  virtual std::pair<bool, MutableMatrix*> solveLinear(const MutableMatrix* B, 
                                              bool right_side) const;

  /** C=this,A,B should be mutable matrices over the same ring, and a,b
     elements of this ring. AND of the same density type.
     C = b*C + a * op(A)*op(B),
     where op(A) = A or transpose(A), depending on transposeA
     where op(B) = B or transpose(B), depending on transposeB
  */
  virtual void addMultipleTo(const MutableMatrix* A,
                                             const MutableMatrix* B,
                                             bool transposeA,
                                             bool transposeB,
                                             const RingElement* a,
                                             const RingElement* b);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
