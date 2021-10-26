// Copyright 2005  Michael E. Stillman

#ifndef _mat_hpp_
#define _mat_hpp_

#include "exceptions.hpp"
#include "hash.hpp"
#include "relem.hpp"

#include <iostream>

class buffer;
class RingElement;
class Ring;
class Matrix;
template <class T>
class MutableMat;

/**
 * \ingroup matrices
 */

class MutableMatrix : public MutableEngineObject
{
 protected:
  MutableMatrix() {}
 public:
  virtual ~MutableMatrix()
  {
  }

#if 0
  // MESXXX
  class iterator : public our_new_delete
  {
  public:
    virtual ~iterator() {}
    virtual void set(size_t col) = 0;
    virtual void next() = 0;
    virtual bool valid() = 0;
    virtual size_t row() = 0;
    virtual void copy_ring_elem(ring_elem &a) = 0;
  };
#endif
#if 0
  // MESXX
  virtual iterator * begin() const = 0;
#endif
  virtual const Ring *get_ring() const = 0;

  virtual size_t n_rows() const = 0;

  virtual size_t n_cols() const = 0;

  virtual bool is_dense() const = 0;

  static MutableMatrix *zero_matrix(const Ring *R,
                                    size_t nrows,
                                    size_t ncols,
                                    bool dense);
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or
  // MutableMatrixCC will be used.

  static MutableMatrix *identity(const Ring *R, size_t nrows, bool dense);

  static MutableMatrix *from_matrix(const Matrix *N, bool is_dense);
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or
  // MutableMatrixCC will be used.

  virtual Matrix *to_matrix() const = 0;

  void text_out(buffer &o) const;

  virtual MutableMatrix *copy(bool prefer_dense) const = 0;

  bool set_values(M2_arrayint rows,
                  M2_arrayint cols,
                  engine_RawRingElementArray values);

  //////////////////////////////
  // Casts down the hierarchy //
  //////////////////////////////
  template <typename MatType>
  MutableMat<MatType> *cast_to_MutableMat()
  {
    return dynamic_cast<MutableMat<MatType> *>(this);
  }

  template <typename MatType>
  const MutableMat<MatType> *cast_to_MutableMat() const
  {
    return dynamic_cast<const MutableMat<MatType> *>(this);
  }

  template <typename MatT>
  MatT *coerce()
  {
    MutableMat<MatT> *P = cast_to_MutableMat<MatT>();
    if (P == 0) return 0;
    return P->get_Mat();
  }

  template <typename MatT>
  const MatT *coerce_const() const
  {
    const MutableMat<MatT> *P = cast_to_MutableMat<MatT>();
    if (P == 0) return 0;
    return P->get_Mat();
  }

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual size_t lead_row(size_t col) const = 0;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual size_t lead_row(size_t col, ring_elem &result) const = 0;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */

  virtual bool get_entry(size_t r, size_t c, ring_elem &result) const = 0;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  virtual bool set_entry(size_t r, size_t c, const ring_elem a) = 0;
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  virtual bool interchange_rows(size_t i, size_t j) = 0;
  /* swap rows: row(i) <--> row(j) */

  virtual bool interchange_columns(size_t i, size_t j) = 0;
  /* swap columns: column(i) <--> column(j) */

  virtual bool scale_row(size_t i, ring_elem r) = 0;
  /* row(i) <- r * row(i) */

  virtual bool scale_column(size_t i, ring_elem r) = 0;
  /* column(i) <- r * column(i) */

  virtual bool divide_row(size_t i, ring_elem r) = 0;
  /* row(i) <- row(i) / r */

  virtual bool divide_column(size_t i, ring_elem r) = 0;
  /* column(i) <- column(i) / r */

  virtual bool row_op(size_t i, ring_elem r, size_t j) = 0;
  /* row(i) <- row(i) + r * row(j) */

  virtual bool column_op(size_t i, ring_elem r, size_t j) = 0;
  /* column(i) <- column(i) + r * column(j) */

  virtual bool column2by2(size_t c1,
                          size_t c2,
                          ring_elem a1,
                          ring_elem a2,
                          ring_elem b1,
                          ring_elem b2) = 0;
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  virtual bool row2by2(size_t r1,
                       size_t r2,
                       ring_elem a1,
                       ring_elem a2,
                       ring_elem b1,
                       ring_elem b2) = 0;
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  virtual bool dot_product(size_t i, size_t j, ring_elem &result) const = 0;

  virtual bool row_permute(size_t start_row, M2_arrayint perm) = 0;

  virtual bool column_permute(size_t start_col, M2_arrayint perm) = 0;

  virtual bool insert_columns(size_t i, size_t n_to_add) = 0;
  /* Insert n_to_add columns directly BEFORE column i. */

  virtual bool insert_rows(size_t i, size_t n_to_add) = 0;
  /* Insert n_to_add rows directly BEFORE row i. */

  virtual bool delete_columns(size_t i, size_t j) = 0;
  /* Delete columns i .. j from M */

  virtual bool delete_rows(size_t i, size_t j) = 0;
  /* Delete rows i .. j from M */

  virtual void reduce_by_pivots() = 0;
  /* Finds units (starting with 1 and -1, then moving to other units)
     in the matrix 'this', and performs row and column operations to
     create a matrix with isomorphic cokernel
  */

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual MutableMatrix *submatrix(M2_arrayint rows,
                                   M2_arrayint cols) const = 0;

  virtual MutableMatrix *submatrix(M2_arrayint cols) const = 0;

  virtual bool is_zero() const = 0;

  virtual bool is_equal(const MutableMatrix *B) const = 0;

  virtual MutableMatrix /* or null */ *add(const MutableMatrix *B) const = 0;
  // return this + B.  return NULL of sizes or types do not match.

  virtual MutableMatrix /* or null */ *subtract(
      const MutableMatrix *B) const = 0;
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrix /* or null */ *mult(const RingElement *f) const = 0;
  // return f*this.  return NULL of sizes or types do not match.

  virtual MutableMatrix *negate() const = 0;

  virtual MutableMatrix /* or null */ *transpose() const = 0;

  ///////////////////////////////
  // Linear algebra /////////////
  ///////////////////////////////

  virtual M2_arrayintOrNull LU(MutableMatrix *L, MutableMatrix *U) const = 0;

  virtual M2_arrayintOrNull LUincremental(std::vector<size_t>& P, const MutableMatrix* v, int m) = 0;

  virtual void triangularSolve(MutableMatrix* x, int m, int strategy) = 0;

  // replace 'this=A' with a matrix which encodes both 'L' and 'U', returning a
  // permutation P
  // of 0..numRows A-1 s.t. LU = PA
  //  virtual M2_arrayintOrNull LUInPlace() const = 0;

  virtual bool eigenvalues(MutableMatrix *eigenvals,
                           bool is_symm_or_hermitian) const = 0;

  virtual bool eigenvectors(MutableMatrix *eigenvals,
                            MutableMatrix *eigenvecs,
                            bool is_symm_or_hermitian) const = 0;

  virtual bool SVD(MutableMatrix *Sigma,
                   MutableMatrix *U,
                   MutableMatrix *Vt,
                   bool use_divide_and_conquer) const = 0;

  virtual bool least_squares(const MutableMatrix *b,
                             MutableMatrix *x,
                             bool assume_full_rank) const = 0;

  virtual bool QR(MutableMatrix *Q, MutableMatrix *R, bool return_QR) const = 0;

  ///////////////////////////////////
  /// LU decomposition routines /////
  ///////////////////////////////////

  // A = m x n 'this' is factored as A = LQUP, where
  //   L is m x m lower unit triangular
  //   U is m x n upper triangular
  //   Q is a m x m permutation matrix
  //   P is an n x n permutation matrix
  // However, the result is constructed in an abbreviated, encoded format:
  // First, the entries of A are modified:
  //   L and U are placed into A
  // Two integer arrays are returned:
  // The first represents Q, giving the rows of U ??
  // The second represents P, which tells which columns of A have non-zero
  // pivots
  //   (and, the elements below the main diagonal in these columns form the
  //   lower part of L).
  // If the ring or matrix type does not support this computation, an
  // engine_error is thrown.
  virtual engine_RawArrayIntPairOrNull LQUPFactorizationInPlace(bool transpose)
  {
    throw exc::engine_error("not implemented for this ring or matrix type");
  }

  /// Fast linear algebra routines (well, fast for some rings)

  virtual size_t rank() const = 0;

  virtual const RingElement *determinant() const = 0;

  // Find the inverse matrix.  If the matrix is not square, or
  // the ring is one in which th matrix cannot be inverted,
  // then NULL is returned, and an error message is set.
  virtual MutableMatrix *invert() const = 0;

  // Find the row reduced echelon form of 'this'. If
  // the ring is one in which the rref cannot be computed,
  // then NULL is returned, and an error message is set.
  virtual MutableMatrix *rowReducedEchelonForm() const = 0;

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is
  // false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  virtual M2_arrayintOrNull rankProfile(bool row_profile) const = 0;

  // Find a spanning set for the null space.  If M = this,
  // return a matrix whose columns span {x | Mx = 0}
  virtual MutableMatrix *nullSpace() const = 0;

  // Returns X, if (this=A) AX=B has a solution.
  // Returns NULL, if not.
  // Throws an exception if any other usage issues arise (bad rings, sizes, not
  // implemented...)
  virtual MutableMatrix *solveLinear(const MutableMatrix *B) const = 0;

  // Returns X, if this=A is invertible, and AX=B. (so X is uniquely determined)
  // Returns NULL, if A is not invertible.
  // Throws an exception if any other usage issues arise.
  virtual MutableMatrix *solveInvertible(const MutableMatrix *B) const = 0;

  virtual void addMultipleTo(const MutableMatrix *A,
                             const MutableMatrix *B) = 0;

  virtual void subtractMultipleTo(const MutableMatrix *A,
                                  const MutableMatrix *B) = 0;

  virtual MutableMatrix /* or null */ *mult(const MutableMatrix *B) const = 0;

  // return this * B.
  // both matrices must be of the same type.
  // If not, or sizes don't match, NULL is returned.

  virtual void clean(gmp_RR epsilon) = 0;  // modifies 'this'
  virtual gmp_RRorNull norm() const = 0;

  virtual M2SLEvaluator *createSLEvaluator(
      M2SLProgram *P,
      M2_arrayint constsPos,
      M2_arrayint varsPos) const = 0;  // this = const matrix
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
