// Copyright 2005-2012  Michael E. Stillman

#ifndef _mutable_mat_defs_hpp_
#define _mutable_mat_defs_hpp_

#include <iostream>
#include "mat.hpp"

namespace M2 {
class ARingZZp;
class ARingRR;
class ARingCC;
class ARingRRR;
class ARingCCC;
};

template <typename RT>
class DMat;
template <typename RT>
class SMat;
template <typename MT>
bool isDense(const MT& mat);
template <typename RT>
bool isDense(const DMat<RT>& mat)
{
  return true;
}
template <typename RT>
bool isDense(const SMat<RT>& mat)
{
  return false;
}

template <typename RT>
struct EigenTypes
{
  typedef RT EigenvalueType;
  typedef RT EigenvectorType;
  typedef RT HermitianEigenvalueType;
  typedef RT HermitianEigenvectorType;
};

template <>
struct EigenTypes<M2::ARingRR>
{
  typedef M2::ARingCC EigenvalueType;
  typedef M2::ARingCC EigenvectorType;
  typedef M2::ARingRR HermitianEigenvalueType;
  typedef M2::ARingRR HermitianEigenvectorType;
};

template <>
struct EigenTypes<M2::ARingCC>
{
  typedef M2::ARingCC EigenvalueType;
  typedef M2::ARingCC EigenvectorType;
  typedef M2::ARingRR HermitianEigenvalueType;
  typedef M2::ARingCC HermitianEigenvectorType;
};

template <>
struct EigenTypes<M2::ARingRRR>
{
  typedef M2::ARingCCC EigenvalueType;
  typedef M2::ARingCCC EigenvectorType;
  typedef M2::ARingRRR HermitianEigenvalueType;
  typedef M2::ARingRRR HermitianEigenvectorType;
};

template <>
struct EigenTypes<M2::ARingCCC>
{
  typedef M2::ARingCCC EigenvalueType;
  typedef M2::ARingCCC EigenvectorType;
  typedef M2::ARingRRR HermitianEigenvalueType;
  typedef M2::ARingCCC HermitianEigenvectorType;
};

// The following include file is for creating a Matrix, in "toMatrix"
#include "matrix-con.hpp"

#include "dmat.hpp"
#include "smat.hpp"
#include "mat-elem-ops.hpp"
#include "mat-arith.hpp"
#include "mat-linalg.hpp"

template <typename CoeffRing>
Matrix* toMatrix(const Ring* R, const DMat<CoeffRing>& A)
{
  int nrows = static_cast<int>(A.numRows());
  int ncols = static_cast<int>(A.numColumns());
  FreeModule* F = R->make_FreeModule(nrows);
  MatrixConstructor result(F, ncols);
  if (nrows == 0 || ncols == 0) return result.to_matrix();

  for (int c = 0; c < ncols; c++)
    {
      int r = 0;
      auto end = A.columnEnd(c);
      for (auto j = A.columnBegin(c); not(j == end); ++j, ++r)
        {
          if (not A.ring().is_zero(*j))
            {
              ring_elem a;
              A.ring().to_ring_elem(a, *j);
              result.set_entry(r, c, a);
            }
        }
    }
  result.compute_column_degrees();
  return result.to_matrix();
}
template <typename CoeffRing>
Matrix* toMatrix(const Ring* R, const SMat<CoeffRing>& A)
{
  int nrows = static_cast<int>(A.numRows());
  int ncols = static_cast<int>(A.numColumns());
  FreeModule* F = R->make_FreeModule(nrows);
  MatrixConstructor result(F, ncols);
  if (nrows == 0 || ncols == 0) return result.to_matrix();
  ring_elem f;
  auto i = A.begin();
  for (int c = 0; c < ncols; c++)
    {
      ring_elem a;
      for (i.set(c); i.valid(); i.next())
        {
          i.copy_elem(a);
          int r = static_cast<int>(i.row());
          result.set_entry(r, c, a);
        }
    }
  result.compute_column_degrees();
  return result.to_matrix();
}

inline bool error_column_bound(size_t c, size_t ncols)
{
  if (c >= ncols)
    {
      ERROR("column out of range");
      return true;
    }
  return false;
}

inline bool error_row_bound(size_t r, size_t nrows)
{
  if (r >= nrows)
    {
      ERROR("row out of range");
      return true;
    }
  return false;
}

/**
 * \ingroup matrices
 */

template <typename Mat>
class MutableMat : public MutableMatrix
{
 public:
  typedef Mat MatType;
  typedef typename Mat::CoeffRing CoeffRing;
  typedef typename CoeffRing::elem elem;

  typedef MatElementaryOps<Mat> MatOps;

  typedef typename EigenTypes<CoeffRing>::EigenvalueType EigenvalueType;
  typedef typename EigenTypes<CoeffRing>::EigenvectorType EigenvectorType;
  typedef typename EigenTypes<CoeffRing>::HermitianEigenvalueType
      HermitianEigenvalueType;
  typedef typename EigenTypes<CoeffRing>::HermitianEigenvectorType
      HermitianEigenvectorType;

 private:
  const Ring* mRing;
  Mat mat;
  // This class wraps the operations for Mat to make a MutableMatrix

  // Same operations as in MutableMatrix.  Almost nothing is
  // done except to call the Mat routines.

  MutableMat() {}
  MutableMat(const Ring* R, const Mat& m) : mRing(R), mat(m) {}
 public:
  // This constructor makes a zero matrix
  MutableMat(const Ring* R, const CoeffRing* coeffR, size_t nrows, size_t ncols)
      : mRing(R), mat(*coeffR, nrows, ncols)
  {
  }

  // Make a zero matrix, using the same ring and density taken from 'mat'.
  MutableMat* makeZeroMatrix(size_t nrows, size_t ncols) const
  {
    return new MutableMat(get_ring(), &mat.ring(), nrows, ncols);
  }

  Mat* get_Mat() { return &mat; }
  const Mat* get_Mat() const { return &mat; }
  Mat& getMat() { return mat; }
  const Mat& getMat() const { return mat; }
#if 0
  // MESXXX
  class iterator : public MutableMatrix::iterator
  {
    typename Mat::iterator i;
  public:
    iterator(const Mat *M0) : i(M0) {}
    void set(size_t col0) { i.set(col0); }
    void next() { i.next(); }
    bool valid() { return i.valid(); }
    size_t row() { return i.row(); }
    const elem& value() { return i.value(); }
    void copy_ring_elem(ring_elem &result) { i.copy_elem(result); }
  };
#endif
#if 0
  // MESXXX
  virtual iterator * begin() const { return new iterator(&mat); }
#endif
  virtual const Ring* get_ring() const { return mRing; }
  virtual size_t n_rows() const { return mat.numRows(); }
  virtual size_t n_cols() const { return mat.numColumns(); }
  virtual bool is_dense() const { return isDense(mat); }
  virtual Matrix* to_matrix() const { return toMatrix(get_ring(), mat); }
  virtual MutableMat* copy(bool prefer_dense) const
  {
#if 0
    MutableMat *result = new MutableMat;
    Mat *m = mat.copy();
    result->mat.grab(m);
    return result;
#endif
    return clone();
  }

  virtual MutableMat* clone() const
  {
    MutableMat* result = new MutableMat(mRing, mat);
    return result;
    //    result->mat.copy(mat);
    //    Mat *m = new Mat(mat); // copies mat
    //    return new MutableMat(*m); // places copy into result
  }

  virtual size_t lead_row(size_t col) const
  {
    return MatOps::lead_row(mat, col);
  }
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual size_t lead_row(size_t col, ring_elem& result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */
  {
    elem b;
    mat.ring().init(b);
    mat.ring().set_zero(b);

    size_t ret = MatOps::lead_row(mat, col, b);
    if (ret != static_cast<size_t>(-1)) mat.ring().to_ring_elem(result, b);

    mat.ring().clear(b);
    return ret;
  }

  virtual bool get_entry(size_t r, size_t c, ring_elem& result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
  {
    if (r < n_rows() && c < n_cols())
      {
        elem a;

        mat.ring().init(a);
        MatOps::getEntry(mat, r, c, a);
        bool is_nonzero = not mat.ring().is_zero(a);
        mat.ring().to_ring_elem(result, a);
        mat.ring().clear(a);
        return is_nonzero;
      }

    result = get_ring()->zero();
    return false;
  }

  virtual bool set_entry(size_t r, size_t c, const ring_elem a)
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
  {
    if (error_row_bound(r, n_rows())) return false;
    if (error_column_bound(c, n_cols())) return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b, a);
    MatOps::setEntry(mat, r, c, b);
    mat.ring().clear(b);
    return true;
  }

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual bool interchange_rows(size_t i, size_t j)
  /* swap rows: row(i) <--> row(j) */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i, nrows) || error_row_bound(j, nrows)) return false;
    MatOps::interchange_rows(mat, i, j);
    return true;
  }

  virtual bool interchange_columns(size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i, ncols) || error_column_bound(j, ncols))
      return false;
    MatOps::interchange_columns(mat, i, j);
    return true;
  }

  virtual bool scale_row(size_t i, ring_elem r)
  /* row(i) <- r * row(i) */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i, nrows)) return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b, r);
    MatOps::scale_row(mat, i, b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool scale_column(size_t i, ring_elem r)
  /* column(i) <- r * column(i) */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i, ncols)) return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b, r);
    MatOps::scale_column(mat, i, b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool divide_row(size_t i, ring_elem r)
  /* row(i) <- row(i) / r */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i, nrows)) return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b, r);
    MatOps::divide_row(mat, i, b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool divide_column(size_t i, ring_elem r)
  /* column(i) <- column(i) / r */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i, ncols)) return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b, r);
    MatOps::divide_column(mat, i, b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool row_op(size_t i, ring_elem r, size_t j)
  /* row(i) <- row(i) + r * row(j) */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i, nrows) || error_row_bound(j, nrows)) return false;
    if (i == j) return true;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b, r);
    MatOps::row_op(mat, i, b, j);
    mat.ring().clear(b);
    return true;
  }

  virtual bool column_op(size_t i, ring_elem r, size_t j)
  /* column(i) <- column(i) + r * column(j) */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i, ncols) || error_column_bound(j, ncols))
      return false;
    if (i == j) return true;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b, r);
    MatOps::column_op(mat, i, b, j);
    mat.ring().clear(b);
    return true;
  }

  virtual bool column2by2(size_t c1,
                          size_t c2,
                          ring_elem a1,
                          ring_elem a2,
                          ring_elem b1,
                          ring_elem b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    size_t ncols = n_cols();
    if (error_column_bound(c1, ncols) || error_column_bound(c2, ncols))
      return false;
    if (c1 == c2) return true;
    elem aa1, aa2, bb1, bb2;
    mat.ring().init(aa1);
    mat.ring().init(aa2);
    mat.ring().init(bb1);
    mat.ring().init(bb2);
    mat.ring().from_ring_elem(aa1, a1);
    mat.ring().from_ring_elem(aa2, a2);
    mat.ring().from_ring_elem(bb1, b1);
    mat.ring().from_ring_elem(bb2, b2);
    MatOps::column2by2(mat, c1, c2, aa1, aa2, bb1, bb2);
    mat.ring().clear(aa1);
    mat.ring().clear(aa2);
    mat.ring().clear(bb1);
    mat.ring().clear(bb2);
    return true;
  }

  virtual bool row2by2(size_t r1,
                       size_t r2,
                       ring_elem a1,
                       ring_elem a2,
                       ring_elem b1,
                       ring_elem b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    size_t nrows = n_rows();
    if (error_row_bound(r1, nrows) || error_row_bound(r2, nrows)) return false;
    if (r1 == r2) return true;
    elem aa1, aa2, bb1, bb2;
    mat.ring().init(aa1);
    mat.ring().init(aa2);
    mat.ring().init(bb1);
    mat.ring().init(bb2);
    mat.ring().from_ring_elem(aa1, a1);
    mat.ring().from_ring_elem(aa2, a2);
    mat.ring().from_ring_elem(bb1, b1);
    mat.ring().from_ring_elem(bb2, b2);
    MatOps::row2by2(mat, r1, r2, aa1, aa2, bb1, bb2);
    mat.ring().clear(aa1);
    mat.ring().clear(aa2);
    mat.ring().clear(bb1);
    mat.ring().clear(bb2);
    return true;
  }

  virtual bool dot_product(size_t c1, size_t c2, ring_elem& result) const
  {
    size_t ncols = n_cols();
    if (error_column_bound(c1, ncols) || error_column_bound(c2, ncols))
      return false;
    elem a;
    mat.ring().init(a);
    mat.ring().set_zero(a);
    MatOps::dot_product(mat, c1, c2, a);
    mat.ring().to_ring_elem(result, a);
    mat.ring().clear(a);
    return true;
  }

  virtual bool row_permute(size_t start_row, M2_arrayint perm)
  {
    return MatOps::row_permute(mat, start_row, perm);
  }

  virtual bool column_permute(size_t start_col, M2_arrayint perm)
  {
    return MatOps::column_permute(mat, start_col, perm);
  }

  virtual bool insert_columns(size_t i, size_t n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. */
  {
    if (i > n_cols())
      {
        ERROR("cannot insert %l columns before column %ln", n_to_add, i);
        return false;
      }
    MatOps::insert_columns(mat, i, n_to_add);
    return true;
  }

  virtual bool insert_rows(size_t i, size_t n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
  {
    if (i > n_rows())
      {
        ERROR("cannot insert %l rows before row %ln", n_to_add, i);
        return false;
      }
    MatOps::insert_rows(mat, i, n_to_add);
    return true;
  }

  virtual bool delete_columns(size_t i, size_t j)
  /* Delete columns i .. j from M */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i, ncols) || error_column_bound(j, ncols))
      {
        ERROR("column index out of range");
        return false;
      }

    MatOps::delete_columns(mat, i, j);
    return true;
  }

  virtual bool delete_rows(size_t i, size_t j)
  /* Delete rows i .. j from M */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i, nrows) || error_row_bound(j, nrows))
      {
        ERROR("row index out of range");
        return false;
      }
    MatOps::delete_rows(mat, i, j);
    return true;
  }

  virtual void reduce_by_pivots() { MatOps::reduce_by_pivots(mat); }
  virtual MutableMatrix* submatrix(M2_arrayint rows, M2_arrayint cols) const
  {
    for (size_t r = 0; r < rows->len; r++)
      if (rows->array[r] < 0 || rows->array[r] >= n_rows())
        {
          ERROR(
              "row index %d out of bounds 0..%d", rows->array[r], n_rows() - 1);
          return 0;
        }
    for (size_t c = 0; c < cols->len; c++)
      if (cols->array[c] < 0 || cols->array[c] >= n_cols())
        {
          ERROR("column index %d out of bounds 0..%d",
                cols->array[c],
                n_cols() - 1);
          return 0;
        }
    MutableMat* result =
        new MutableMat(*this);  // zero matrix, over the same ring
    MatOps::setFromSubmatrix(getMat(), rows, cols, result->getMat());
    return result;
  }

  virtual MutableMatrix* submatrix(M2_arrayint cols) const
  {
    for (size_t c = 0; c < cols->len; c++)
      if (cols->array[c] < 0 || cols->array[c] >= n_cols())
        {
          ERROR("column index %d out of bounds 0..%d",
                cols->array[c],
                n_cols() - 1);
          return 0;
        }
    MutableMat* result =
        new MutableMat(*this);  // zero matrix, over the same ring
    MatOps::setFromSubmatrix(getMat(), cols, result->getMat());
    return result;
  }

  ///////////////////////////////
  // promote, lift, eval ////////
  ///////////////////////////////

  virtual MutableMatrix* promote(const Ring* S) const { return 0; }
  virtual MutableMatrix* lift(const Ring* R) const { return 0; }
  virtual MutableMatrix* eval(const RingMap* F) const { return 0; }
  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual bool is_zero() const { return MatrixOps::isZero(getMat()); }
  virtual bool is_equal(const MutableMatrix* B) const
  {
    const MutableMat* B1 = dynamic_cast<const MutableMat*>(B);
    if (B1 == NULL || &B1->getMat().ring() != &getMat().ring()) return false;
    return MatrixOps::isEqual(getMat(), B1->getMat());
  }

  virtual MutableMat* add(const MutableMatrix* B) const
  // return this + B.  return NULL if sizes or types do not match.
  {
    const Mat* B1 = B->coerce_const<Mat>();
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
    if (B1->numRows() != n_rows() || B1->numColumns() != n_cols())
      {
        ERROR("expected matrices of the same shape");
        return NULL;
      }

    MutableMat* result = clone();
    MatrixOps::addInPlace(result->getMat(), *B1);
    return result;
  }

  virtual MutableMatrix* negate() const
  {
    MutableMat* result = clone();
    MatrixOps::negateInPlace(result->getMat());
    return result;
  }

  virtual MutableMat* subtract(const MutableMatrix* B) const
  // return this - B.  return NULL of sizes or types do not match.
  {
    const Mat* B1 = B->coerce_const<Mat>();
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
    if (B1->numRows() != n_rows() || B1->numColumns() != n_cols())
      {
        ERROR("expected matrices of the same shape");
        return NULL;
      }

    MutableMat* result = clone();
    MatrixOps::subtractInPlace(result->getMat(), *B1);
    return result;
  }

  virtual MutableMat* mult(const RingElement* f) const
  // return f*this.  return NULL of sizes or types do not match.
  {
    if (f->get_ring() != get_ring())
      {
        ERROR("expected same ring");
        return 0;
      }
    elem a;
    mat.ring().init(a);
    mat.ring().from_ring_elem(a, f->get_value());

    MutableMat* result = clone();
    MatrixOps::scalarMultInPlace(result->mat, a);

    mat.ring().clear(a);
    return result;
  }

  virtual MutableMat /* or null */* transpose() const
  {
    MutableMat* result = makeZeroMatrix(n_cols(), n_rows());
    MatrixOps::transpose(getMat(), result->getMat());
    return result;
  }

  ///////////////////////////////
  // Linear algebra /////////////
  ///////////////////////////////

  virtual M2_arrayintOrNull LU(MutableMatrix* L, MutableMatrix* U) const;

  virtual M2_arrayintOrNull LUincremental(std::vector<size_t>& P, const MutableMatrix* v, int i);

  virtual void triangularSolve(MutableMatrix* x, int m, int strategy);

  virtual bool eigenvalues(MutableMatrix* eigenvals,
                           bool is_symm_or_hermitian) const;

  virtual bool eigenvectors(MutableMatrix* eigenvals,
                            MutableMatrix* eigenvecs,
                            bool is_symm_or_hermitian) const;

  virtual bool SVD(MutableMatrix* Sigma,
                   MutableMatrix* U,
                   MutableMatrix* Vt,
                   bool use_divide_and_conquer) const;

  virtual bool least_squares(const MutableMatrix* b,
                             MutableMatrix* x,
                             bool assume_full_rank) const;

  virtual bool QR(MutableMatrix* Q, MutableMatrix* R, bool return_QR) const;

  virtual engine_RawArrayIntPairOrNull LQUPFactorizationInPlace(bool transpose);

  /// Fast linear algebra routines (well, fast for some rings)

  virtual size_t rank() const;

  virtual const RingElement* determinant() const;

  // Find the inverse matrix.  If the matrix is not square, or
  // the ring is one in which th matrix cannot be inverted,
  // then NULL is returned, and an error message is set.
  virtual MutableMatrix* invert() const;

  virtual MutableMatrix* rowReducedEchelonForm() const;

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is
  // false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  virtual M2_arrayintOrNull rankProfile(bool row_profile) const;

  // Find a spanning set for the null space.  If M = this,
  // return a matrix whose columns span {x | Mx = 0}
  virtual MutableMatrix* nullSpace() const;

  // Returns X, if (this=A) AX=B has a solution.
  // Returns NULL, if not.
  // Throws an exception if any other usage issues arise (bad rings, sizes, not
  // implemented...)
  virtual MutableMatrix* solveLinear(const MutableMatrix* B) const;

  // Returns X, if this=A is invertible, and AX=B. (so X is uniquely determined)
  // Returns NULL, if A is not invertible.
  // Throws an exception if any other usage issues arise.
  virtual MutableMatrix* solveInvertible(const MutableMatrix* B) const;

  virtual void addMultipleTo(const MutableMatrix* A, const MutableMatrix* B);

  virtual void subtractMultipleTo(const MutableMatrix* A,
                                  const MutableMatrix* B);

  virtual MutableMatrix /* or null */* mult(const MutableMatrix* B) const;

  // Special routines for approximate fields
  virtual void clean(gmp_RR epsilon);  // modifies 'this'
  virtual gmp_RRorNull norm() const;

  virtual M2SLEvaluator* createSLEvaluator(
      M2SLProgram* P,
      M2_arrayint constsPos,
      M2_arrayint varsPos) const;  // this = const matrix
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
