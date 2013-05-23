// Copyright 2005-2012  Michael E. Stillman

#ifndef _mutable_mat_hpp_
#define _mutable_mat_hpp_

#include <iostream>
#include "mat.hpp"

class CoefficientRingCCC;
class Ring_RRR;
template <typename RT> class DMat;
namespace M2 {
  class ARingZZp;
};

#include "linalg.hpp"
#include "MatElementaryOps.hpp"
#include "MatArithmetic.hpp"

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

template<typename Mat>
class MutableMat : public MutableMatrix
{
public:
  typedef Mat MatType;
  typedef typename Mat::CoeffRing CoeffRing;
  typedef typename CoeffRing::elem elem;

  typedef MatElementaryOps<Mat> MatOps;
  typedef MatArithmetic<Mat> MatArith;
private:
  const Ring* mRing;
  Mat mat;
  // This class wraps the operations for Mat to make a MutableMatrix

  // Same operations as in MutableMatrix.  Almost nothing is
  // done except to call the Mat routines.

  // Linear algebra routines: Default is to return 0.
  // Caller must then make error message?
  // Specific template instances must provide these functions
  MutableMat() {}

  MutableMat(const Ring *R, const CoeffRing *coeffR, size_t nrows, size_t ncols)
    : mRing(R), mat(coeffR,nrows,ncols) {}

  MutableMat(const Ring* R, const Mat &m) : mRing(R), mat(m) {}

public:
  // Make a zero matrix, using the same ring and density taken from 'mat'.
  MutableMat* makeZeroMatrix(size_t nrows, size_t ncols) const {
    return zero_matrix(get_ring(), &mat.ring(), nrows, ncols);
  }

  Mat * get_Mat() { return &mat; }
  const Mat * get_Mat() const { return &mat; }

  Mat& getMat() { return mat; }
  const Mat& getMat() const { return mat; }

#if 1
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
  static MutableMat *zero_matrix(const Ring *R, const CoeffRing *coeffR, size_t nrows, size_t ncols)
  {
    return new MutableMat(R,coeffR,nrows,ncols);
  }

  //  static MutableMat *grab_Mat(const Mat *m);

#if 1
  // MESXXX
  virtual iterator * begin() const { return new iterator(&mat); }
#endif
  virtual const Ring * get_ring() const { return mRing; }

  virtual size_t n_rows() const { return mat.numRows(); }

  virtual size_t n_cols() const { return mat.numColumns(); }

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
    MutableMat* result = new MutableMat(mRing, mat);
    return result;
    //    result->mat.copy(mat);
    //    Mat *m = new Mat(mat); // copies mat
    //    return new MutableMat(*m); // places copy into result
  }

  virtual size_t lead_row(size_t col) const { return MatOps::lead_row(mat,col); }
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual size_t lead_row(size_t col, ring_elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */
  {
    elem b;
    mat.ring().init(b);
    mat.ring().set_zero(b);

    size_t ret = MatOps::lead_row(mat, col, b);
    if (ret >= 0)
      mat.ring().to_ring_elem(result, b);

    mat.ring().clear(b);
    return ret;
  }

  virtual bool get_entry(size_t r, size_t c, ring_elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
  {
    if (r >= 0 && r < n_rows() && c >= 0 && c < n_cols())
      {
        elem a;
        
        mat.ring().init(a);
        mat.ring().set_zero(a);
        if (mat.get_entry(r,c,a))
          {
            mat.ring().to_ring_elem(result,a);
            return true;
          }
      }

    result = get_ring()->zero();
    return false;
  }

  virtual bool set_entry(size_t r, size_t c, const ring_elem a)
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
  {
    if (error_row_bound(r,n_rows())) return false;
    if (error_column_bound(c,n_cols())) return false;
    elem b;
    mat.ring().from_ring_elem(b,a);
    mat.set_entry(r,c,b);
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
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      return false;
    MatOps::interchange_rows(mat,i,j);
    return true;
  }

  virtual bool interchange_columns(size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    MatOps::interchange_columns(mat,i,j);
    return true;
  }

  virtual bool scale_row(size_t i, ring_elem r)
  /* row(i) <- r * row(i) */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i,nrows))
      return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b,r);
    MatOps::scale_row(mat,i,b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool scale_column(size_t i, ring_elem r)
  /* column(i) <- r * column(i) */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i,ncols))
      return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b,r);
    MatOps::scale_column(mat,i,b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool divide_row(size_t i, ring_elem r)
  /* row(i) <- row(i) / r */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i,nrows))
      return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b,r);
    MatOps::divide_row(mat,i,b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool divide_column(size_t i, ring_elem r)
  /* column(i) <- column(i) / r */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i,ncols))
      return false;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b,r);
    MatOps::divide_column(mat,i,b);
    mat.ring().clear(b);
    return true;
  }

  virtual bool row_op(size_t i, ring_elem r, size_t j)
  /* row(i) <- row(i) + r * row(j) */
  {
    size_t nrows = n_rows();
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      return false;
    if (i == j) return true;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b,r);
    MatOps::row_op(mat,i,b,j);
    mat.ring().clear(b);
    return true;
  }

  virtual bool column_op(size_t i, ring_elem r, size_t j)
  /* column(i) <- column(i) + r * column(j) */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    if (i == j) return true;
    elem b;
    mat.ring().init(b);
    mat.ring().from_ring_elem(b,r);
    MatOps::column_op(mat,i,b,j);
    mat.ring().clear(b);
    return true;
  }

  virtual bool column2by2(size_t c1, size_t c2,
                          ring_elem a1, ring_elem a2,
                          ring_elem b1, ring_elem b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    size_t ncols = n_cols();
    if (error_column_bound(c1,ncols) || error_column_bound(c2,ncols))
      return false;
    if (c1 == c2) return true;
    elem aa1, aa2, bb1, bb2;
    mat.ring().init(aa1);
    mat.ring().init(aa2);
    mat.ring().init(bb1);
    mat.ring().init(bb2);
    mat.ring().from_ring_elem(aa1,a1);
    mat.ring().from_ring_elem(aa2,a2);
    mat.ring().from_ring_elem(bb1,b1);
    mat.ring().from_ring_elem(bb2,b2);
    MatOps::column2by2(mat,c1,c2,aa1,aa2,bb1,bb2);
    mat.ring().clear(aa1);
    mat.ring().clear(aa2);
    mat.ring().clear(bb1);
    mat.ring().clear(bb2);
    return true;
  }

  virtual bool row2by2(size_t r1, size_t r2,
                       ring_elem a1, ring_elem a2,
                       ring_elem b1, ring_elem b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    size_t nrows = n_rows();
    if (error_row_bound(r1,nrows) || error_row_bound(r2,nrows))
      return false;
    if (r1 == r2) return true;
    elem aa1, aa2, bb1, bb2;
    mat.ring().from_ring_elem(aa1,a1);
    mat.ring().from_ring_elem(aa2,a2);
    mat.ring().from_ring_elem(bb1,b1);
    mat.ring().from_ring_elem(bb2,b2);
    mat.ring().from_ring_elem(aa1,a1);
    mat.ring().from_ring_elem(aa2,a2);
    mat.ring().from_ring_elem(bb1,b1);
    mat.ring().from_ring_elem(bb2,b2);
    MatOps::row2by2(mat,r1,r2,aa1,aa2,bb1,bb2);
    mat.ring().clear(aa1);
    mat.ring().clear(aa2);
    mat.ring().clear(bb1);
    mat.ring().clear(bb2);
    return true;
  }

  virtual bool dot_product(size_t c1, size_t c2, ring_elem &result) const
  {
    size_t ncols = n_cols();
    if (error_column_bound(c1,ncols) || error_column_bound(c2,ncols))
      return false;
    elem a;
    mat.ring().init(a);
    mat.ring().set_zero(a);
    MatOps::dot_product(mat,c1,c2,a);
    mat.ring().to_ring_elem(result,a);
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
    if (i < 0 || i > n_cols() || n_to_add < 0)
      {
        ERROR("cannot insert %l columns before column %ln",n_to_add,i);
        return false;
      }
    MatOps::insert_columns(mat, i, n_to_add);
    return true;
  }

  virtual bool insert_rows(size_t i, size_t n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
  {
    if (i < 0 || i > n_rows() || n_to_add < 0)
      {
        ERROR("cannot insert %l rows before row %ln",n_to_add,i);
        return false;
      }
    MatOps::insert_rows(mat, i, n_to_add);
    return true;
  }

  virtual bool delete_columns(size_t i, size_t j)
  /* Delete columns i .. j from M */
  {
    size_t ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
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
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      {
        ERROR("row index out of range");
        return false;
      }
    MatOps::delete_rows(mat, i, j);
    return true;
  }

  virtual MutableMatrix * submatrix(M2_arrayint rows, M2_arrayint cols) const
  {
    for (size_t r = 0; r<rows->len; r++)
      if (rows->array[r] < 0 || rows->array[r] >= n_rows())
        {
          ERROR("row index %d out of bounds 0..%d", rows->array[r], n_rows()-1);
          return 0;
        }
    for (size_t c = 0; c<cols->len; c++)
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
    for (size_t c = 0; c<cols->len; c++)
      if (cols->array[c] < 0 || cols->array[c] >= n_cols())
        {
          ERROR("column index %d out of bounds 0..%d", cols->array[c], n_cols()-1);
          return 0;
        }
    MutableMat *result = new MutableMat(*this); // zero matrix, over the same ring
    result->getMat().setFromSubmatrix(getMat(), cols);
    return result;
  }

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual bool is_zero() const
  {
    return MatArith::isZero(getMat());
  }

  virtual bool is_equal(const MutableMatrix *B) const
  {
    const MutableMat *B1 = dynamic_cast<const MutableMat *>(B);
    if (B1 == NULL || &B1->getMat().ring() != &getMat().ring())
      return false;
    return MatArith::isEqual(getMat(), B1->getMat());
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
    if (B1->numRows() != n_rows() || B1->numColumns() != n_cols())
      {
        ERROR("expected matrices of the same shape");
        return NULL;
      }

    MutableMat* result = clone();
    MatArith::addInPlace(result->getMat(), *B1);
    return result;
  }

  virtual MutableMatrix * negate() const
  {
    MutableMat *result = clone();
    MatArith::negateInPlace(result->getMat());
    return result;
  }

  virtual MutableMat * subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
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
    if (B1->numRows() != n_rows() || B1->numColumns() != n_cols())
      {
        ERROR("expected matrices of the same shape");
        return NULL;
      }

    MutableMat* result = clone();
    MatArith::subtractInPlace(result->getMat(), *B1);
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
    mat.ring().init(a);
    mat.ring().from_ring_elem(a, f->get_value());

    MutableMat *result = clone();
    MatArith::scalarMultInPlace(result->mat, a);

    mat.ring().clear(a);
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

  virtual MutableMatrix /* or null */ * mult(const MutableMatrix *B) const;
};






template <typename T>
size_t MutableMat<T>::rank() const 
{
  return mat.new_rank();
}

template <typename T>
const RingElement* MutableMat<T>::determinant() const 
{
  ring_elem det;
  elem a;
  mat.ring().init(a);
  mat.new_determinant(a);
  mat.ring().to_ring_elem(det, a);
  mat.ring().clear(a);
  return RingElement::make_raw(get_ring(), det);
}

template <typename T>
MutableMatrix* MutableMat<T>::invert() const
{
  MutableMat<T>*  result = makeZeroMatrix(n_rows(), n_cols());
  bool val = mat.invert(result->mat);
  if (!val)
    {
      delete result;
      return 0;
    }
  return result;
}

template <typename T>
MutableMatrix /* or null */ * MutableMat<T>::mult(const MutableMatrix *B) const
{
  // First, make sure B has the same ring/type as 'this'.
  const MutableMat<T>* B1 = B->cast_to_MutableMat<T>();
  if (B1 == 0)
    {
      ERROR("mutable matrix/ring type for (mutable) matrix multiplication required to be the same");
      return 0;
    }
  // Second, make sure the sizes are correct.
  if (mat.numColumns() != B1->n_rows())
    {
      ERROR("matrix sizes do not match in matrix multiplication");
      return 0;
    }
  // create the result matrix
  MutableMat<T>*  result = makeZeroMatrix(n_rows(), B->n_cols());

  // Call the resulting matrix routine.
  mat.mult(B1->mat, result->mat);

  return result;
}

template <typename T>
M2_arrayintOrNull MutableMat<T>::rankProfile(bool row_profile) const
{
  return mat.rankProfile(row_profile);
}

#if 0
// MES, 19 May 2013: Do we want to do it like this?
template <typename T> 
bool MutableMat<T>::SVD(MutableMatrix *Sigma,
                        MutableMatrix *U,
                        MutableMatrix *VT,
                        bool use_divide_and_conquer) const
{
  const MatType *A2 = get_Mat();
  MatType *Sigma2 = Sigma->coerce<MatType>();
  MatType *U2 = U->coerce<MatType>();
  MatType *VT2 = VT->coerce<MatType>();
  if (Sigma2 == 0 || U2 == 0 || VT2 == 0)
    {
      ERROR("requires dense mutable matrices over the same ring");
      return false;
    }

  int strategy = (use_divide_and_conquer ? 1 : 0);
  return DenseApproxLinAlg::SVD(A2,Sigma,U2,VT2,strategy);
}
#endif

///////////////////////////////////////////////
#if 0
#include "dmat.hpp"
template<typename MT>
bool solve1(const MT &A, const MT &b, MT& x)
{
  std::cout << "calling base template for solve1" << std::endl;
  ERROR("'solve' not implemented for this ring amd matrix type");
  return false;
}

template<typename CoeffRing>
bool solve1< DMat<CoeffRing> >(const DMat<CoeffRing> &A, const DMat<CoeffRing> &b, DMat<CoeffRing>& x)
{
  std::cout << "calling DMat template for solve1" << std::endl;
  return DMatLU<typename MT::CoeffRing>::solve(&A, &b, &x);
}

bool solve1(const DMat<Ring_RRR> &A, const DMat<Ring_RRR> &b, DMat<Ring_RRR> &x);
//bool solve1(const DMat<M2::ARingZZp> &A, const DMat<M2::ARingZZp> &b, DMat<M2::ARingZZp> &x);
#endif

template<typename MT>
bool eigenvalues1(const MT &A, typename MT::EigenvalueMatrixType &eigenvals)
{
  std::cout << "calling base template for eigenvalues1" << std::endl;
  ERROR("'eigenvalues' not implemented for this ring amd matrix type");
  return false;
}

bool eigenvalues1(const DMat<Ring_RRR> &A, DMat<Ring_RRR> &eigenvalues);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
