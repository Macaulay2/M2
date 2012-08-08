// Copyright 2005  Michael E. Stillman

#ifndef _mat_hpp_
#define _mat_hpp_

#include "exceptions.hpp"
#include "hash.hpp"
#include "relem.hpp"

class buffer;
class RingElement;
class Ring;
class Matrix;
template<class T> class MutableMat;

/**
 * \ingroup matrices
 */

class MutableMatrix : public mutable_object
{
protected:
  MutableMatrix() {}
  virtual ~MutableMatrix() {}
public:
  class iterator : public our_new_delete
  {
  public:
    virtual ~iterator() {}
    virtual void set(int col) = 0;
    virtual void next() = 0;
    virtual bool valid() = 0;
    virtual int row() = 0;
    virtual void copy_ring_elem(ring_elem &a) = 0;
  };

  virtual iterator * begin() const = 0;

  virtual const Ring * get_ring() const = 0;

  virtual int n_rows() const = 0;

  virtual int n_cols() const = 0;

  virtual bool is_dense() const = 0;

  static MutableMatrix *zero_matrix(const Ring *R, int nrows, int ncols, bool dense);
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or
  // MutableMatrixCC will be used.

  static MutableMatrix *identity(const Ring *R, int nrows, bool dense);

  static MutableMatrix *from_matrix(const Matrix *N, bool is_dense);
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or
  // MutableMatrixCC will be used.

  Matrix *to_matrix() const;

  void text_out(buffer &o) const;

  virtual MutableMatrix *copy(bool prefer_dense) const = 0;

  bool set_values(M2_arrayint rows,
                  M2_arrayint cols,
                  engine_RawRingElementArray values);

  //////////////////////////////
  // Casts down the hierarchy //
  //////////////////////////////
  template< typename MatType>
  MutableMat<MatType> * cast_to_MutableMat() { 
    return dynamic_cast< MutableMat<MatType> *>(this); 
  }

  template< typename MatType>
  const MutableMat<MatType> * cast_to_MutableMat() const { 
    return dynamic_cast< const MutableMat<MatType> *>(this); 
  }

  template<typename MatT> MatT * coerce();

  template<typename MatT> const MatT * coerce() const;

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual int lead_row(int col) const = 0;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual int lead_row(int col, ring_elem &result) const = 0;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */

  virtual bool get_entry(int r, int c, ring_elem &result) const = 0;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  virtual bool set_entry(int r, int c, const ring_elem a) = 0;
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  virtual bool interchange_rows(int i, int j) = 0;
  /* swap rows: row(i) <--> row(j) */

  virtual bool interchange_columns(int i, int j) = 0;
  /* swap columns: column(i) <--> column(j) */

  virtual bool scale_row(int i, ring_elem r) = 0;
  /* row(i) <- r * row(i) */

  virtual bool scale_column(int i, ring_elem r) = 0;
  /* column(i) <- r * column(i) */

  virtual bool divide_row(int i, ring_elem r) = 0;
  /* row(i) <- row(i) / r */

  virtual bool divide_column(int i, ring_elem r) = 0;
  /* column(i) <- column(i) / r */

  virtual bool row_op(int i, ring_elem r, int j) = 0;
  /* row(i) <- row(i) + r * row(j) */

  virtual bool column_op(int i, ring_elem r, int j) = 0;
  /* column(i) <- column(i) + r * column(j) */

  virtual bool column2by2(int c1, int c2,
                          ring_elem a1, ring_elem a2,
                          ring_elem b1, ring_elem b2) = 0;
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  virtual bool row2by2(int r1, int r2,
                       ring_elem a1, ring_elem a2,
                       ring_elem b1, ring_elem b2) = 0;
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  virtual bool dot_product(int i, int j, ring_elem &result) const = 0;

  virtual bool row_permute(int start_row, M2_arrayint perm) = 0;

  virtual bool column_permute(int start_col, M2_arrayint perm) = 0;

  virtual bool insert_columns(int i, int n_to_add) = 0;
  /* Insert n_to_add columns directly BEFORE column i. */

  virtual bool insert_rows(int i, int n_to_add) = 0;
  /* Insert n_to_add rows directly BEFORE row i. */

  virtual bool delete_columns(int i, int j) = 0;
  /* Delete columns i .. j from M */

  virtual bool delete_rows(int i, int j) = 0;
  /* Delete rows i .. j from M */

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual MutableMatrix * submatrix(M2_arrayint rows, M2_arrayint cols) const = 0;

  virtual MutableMatrix * submatrix(M2_arrayint cols) const = 0;

  virtual bool set_submatrix(M2_arrayint rows,
                             M2_arrayint cols,
                             const MutableMatrix *N) = 0;
  // returns false iff there is an error

  virtual bool is_zero() const = 0;


  virtual bool is_equal(const MutableMatrix *B) const = 0;

  virtual MutableMatrix /* or null */ * add(const MutableMatrix *B) const = 0;
  // return this + B.  return NULL of sizes or types do not match.

  virtual MutableMatrix /* or null */ * subtract(const MutableMatrix *B) const = 0;
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrix /* or null */ * mult(const MutableMatrix *B) const = 0;
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrix /* or null */ * mult(const RingElement *f) const = 0;
  // return f*this.  return NULL of sizes or types do not match.

  virtual MutableMatrix * negate() const = 0;

  ///////////////////////////////
  // Linear algebra /////////////
  ///////////////////////////////
  virtual bool solve(const MutableMatrix *b, MutableMatrix *x) const = 0;
  // resets x, find a solution for Ax=b.  Returns false if no such solution exists

  virtual bool nullspaceU(MutableMatrix *x) const = 0;
  // resets x, find a basis of solutions for Ux=0, where U is
  // 'this', and is in upper triangular form from an LU decomp.  Returns true if
  // this matrix type implements this algorith,

  virtual M2_arrayintOrNull LU(MutableMatrix *L,
                                MutableMatrix *U) const = 0;

  virtual bool eigenvalues(MutableMatrix *eigenvals, bool is_symm_or_hermitian) const = 0;

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
  // The second represents P, which tells which columns of A have non-zero pivots
  //   (and, the elements below the main diagonal in these columns form the lower part of L).
  // If the ring or matrix type does not support this computation, an engine_error is thrown.
  virtual engine_RawArrayIntPairOrNull LQUPFactorizationInPlace(bool transpose) { 
    throw exc::engine_error("not implemented for this ring or matrix type");
  }

  // Return a permutation P of 0..nrows-1 and place into LU
  // the encoded L and U matrices, such that this = A = PLU
  virtual M2_arrayintOrNull LU1(MutableMatrix *LU) const { return 0; }

  // If X = this, and A = PLU, then replace X with a solution to AX=B
  // If the sizes don't match, return null, otherwise return 'this'
  // ASSUMPTION: (LU, row_permutation) is the result of an LU
  // decomposition of A.  (LU is encoded as in LU1).
  virtual MutableMatrix *LUSolve(const MutableMatrix *LU,
                                 const M2_arrayint row_permutation,
                                 const MutableMatrix *B) const { return 0; }

  /// Fast linear algebra routines (well, fast for some rings)

  virtual size_t rank() const { return static_cast<size_t>(-1); }

  virtual const RingElement* determinant() const { return NULL; }

  // Find the inverse matrix.  If the matrix is not square, or 
  // the ring is one in which th matrix cannot be inverted,
  // then NULL is returned, and an error message is set.
  virtual MutableMatrix* invert() const { return 0; }

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  virtual M2_arrayintOrNull rankProfile(bool row_profile) const { return 0; }
  
  // Find a spanning set for the null space.  If M = this,
  // and right_side is true, return a matrix whose rows span {x |  xM = 0},
  // otherwise return a matrix whose columns span {x | Mx = 0}
  virtual MutableMatrix* nullSpace(bool right_side) const { return 0; }

  // Return a matrix whose rows or columns solve either Ax = B (right_side=true)
  // or xA = B (right_side=false).  The first argument returned is false
  // in this case.
  virtual std::pair<bool, MutableMatrix*> solveLinear(const MutableMatrix* B, 
                                                      bool right_side) const { 
    return std::pair<bool, MutableMatrix*>(0,NULL); 
  }

  /** C=this,A,B should be mutable matrices over the same ring, and a,b
     elements of this ring. AND of the same density type.
     C = b*C + a * op(A)*op(B),
     where op(A) = A or transpose(A), depending on transposeA
     where op(B) = B or transpose(B), depending on transposeB
  */
  virtual void               addMultipleTo(const MutableMatrix* A,
                                             const MutableMatrix* B,
                                             bool transposeA,
                                             bool transposeB,
                                             const RingElement* a,
                                             const RingElement* b)
  {
    //std::cerr << "MutableMatrix : rawLinAlgAddMultipleTo" << std::endl;
    return ;
  }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
