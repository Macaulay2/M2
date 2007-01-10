// Copyright 2005  Michael E. Stillman

#ifndef _mat_hpp_
#define _mat_hpp_

#include "hash.hpp"
#include "error.h"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "coeffrings.hpp"
#include "relem.hpp"
#include <vector>

#include "dmat.hpp"
#include "smat.hpp"

inline bool error_column_bound(long c, long ncols)
{
  if (c < 0 || c >= ncols)
    {
      ERROR("column out of range");
      return true;
    }
  return false;
}

inline bool error_row_bound(long r, long nrows)
{
  if (r < 0 || r >= nrows)
    {
      ERROR("row out of range");
      return true;
    }
  return false;
}

class Ring;
class Matrix;
class MutableMatrix;
typedef MutableMatrix MutableMatrixOrNull;


// We define the following classes for mutable matrices
// MutableMatrix
//   no row/column change
//   virtual functions
//   creation of iden, zero and random matrices, sparse<-->dense form
//   iterators for each column: and going to/from dense and sparse columns
//   ringelem routines
//   cast functions to get to underlying matrices (densematZZ, sparsematZZ, etc.)
//   query: dense?  which ring?
//   special linear algebra operations, not defined for every ring, dense type.
//     solve
//     LU
//     eigenvalues
//     eigenvectors
//     LLL
//     HermiteNF
//     Smith NF
//     SVD
//     least squares
//     others?
// Mat
//   not a real class: these are just the routines expected for the base
//     matrix classes
//   classes that follow this interface:
//   DMat<R>
//   SMat<R>
//   NTLMat_ZZ ? Do we really need this, or should we just copy when using?
//   NTLMat_RR, ... others too.  Are these needed?
// template<R:CoefficientRing,T:Mat over R> Mat_T : public MutableMatrix wrapper class
//   This has functions: Mat_T::solve, etc... that simply return 0 (can't compute).
//   Each situation then needs to put in a definition:
//    template... Mat_T<...>::solve(...) { }


template<typename CoeffRing>
class Mat
{
  // This class is a shell of a class which describes what needs to
  // be provided in order for MutableMat<CoeffRing,Mat> to work.

  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
public:
  Mat() {}

  Mat(const RingType *R, long nrows, long ncols) {}

  Mat *copy() const { return 0; }
  void grab(Mat<CoeffRing> *M) { }  // Switches this and M.

  class iterator : public our_new_delete
  {
    const Mat<CoeffRing> *M;
    long col;
    elem zero;
  public:
    iterator(const Mat<CoeffRing> *M0) : M(M0), col(0) {}
    void set(long col0) { col = col0; }
    void next() { }
    bool valid() { return false; }
    long row() { return 0; }
    const elem &value() { return zero; }
    void copy_elem(ring_elem &result) { M->get_CoeffRing()->to_ring_elem(result, value()); }
  };

  long n_rows() const { return 0; }

  long n_cols() const { return 0; }

  bool is_dense() const { return true; }

  long lead_row(long col, elem &result) const { return -1; }

  long lead_row(long col) const { return -1; }

  const CoeffRing *get_CoeffRing() const { return 0; }

  const Ring *get_ring() const { return 0; }

  bool get_entry(long i, long j, elem &result) const { return false; }

  bool set_entry(long i, long j, const elem &result) const { return false; }

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////

  void interchange_rows(long i, long j) const { }

  void interchange_columns(long i, long j) const { }

  bool scale_row(long i, const elem &r) { return false; }

  bool scale_column(long i, const elem &r) { return false; }

  bool divide_row(long i, const elem &r) { return false; }

  bool divide_column(long i, const elem &r) { return false; }

  void row_op(long i, const elem &b, long j) {}

  void column_op(long i, const elem &b, long j) {}

  void column2by2(long c1, long c2, 
		  const elem &a1, const elem &a2,
		  const elem &b1, const elem &b2) {}
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  void row2by2(long r1, long r2, 
	       const elem &a1, const elem &a2,
	       const elem &b1, const elem &b2) {}
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  bool row_permute(long start_row, M2_arrayint perm) { return false; }

  bool column_permute(long start_col, M2_arrayint perm) { return false; }

  void insert_columns(long i, long n_to_add) {}
  /* Insert n_to_add columns directly BEFORE column i. */

  void insert_rows(long i, long n_to_add) {}
  /* Insert n_to_add rows directly BEFORE row i. */

  void delete_columns(long i, long j) {}
  /* Delete columns i .. j from M */

  void delete_rows(long i, long j) {}
  /* Delete rows i .. j from M */

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////


  void dot_product(long i, long j, elem &result) const { } 

  bool set_submatrix(M2_arrayint rows,
		     M2_arrayint cols,
		     const MutableMatrix *N) { return false; }

  Mat<CoeffRing> * submatrix(M2_arrayint rows, M2_arrayint cols) const
  { return 0; }

  Mat<CoeffRing> * submatrix(M2_arrayint cols) const { return 0; }

  bool is_zero() const { return false; }

  bool is_equal(const MutableMatrix *B) const { return false; }

  Mat * add(const MutableMatrix *B) const { return 0; }

  Mat * subtract(const MutableMatrix *B) const { return 0; }
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  Mat * mult(const MutableMatrix *B) const { return 0; }
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  Mat * mult(const elem &f) const { return 0; }
  // return f*this.  return NULL of sizes or types do not match.

  Mat * negate() const { return 0; }
};

typedef Mat<CoefficientRingZZp> Mat_ZZp;
typedef Mat<CoefficientRingRR> Mat_RR;

typedef DMat<CoefficientRingZZp> DMatZZp;
typedef DMat<CoefficientRingRR> DMatRR;
typedef DMat<CoefficientRingCC> DMatCC;
typedef DMat<CoefficientRingZZ_NTL> DMatZZ;
typedef DMat<CoefficientRingR> DMatR;

typedef SMat<CoefficientRingZZp> SMatZZp;
typedef SMat<CoefficientRingRR> SMatRR;
typedef SMat<CoefficientRingCC> SMatCC;
typedef SMat<CoefficientRingZZ_NTL> SMatZZ;
typedef SMat<CoefficientRingR> SMatR;



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
    virtual void set(long col) = 0;
    virtual void next() = 0;
    virtual bool valid() = 0;
    virtual long row() = 0;
    virtual void copy_ring_elem(ring_elem &a) = 0;
  };

  virtual iterator * begin() const = 0;
  
  virtual const Ring * get_ring() const = 0;

  virtual long n_rows() const = 0;

  virtual long n_cols() const = 0;

  virtual bool is_dense() const = 0;

  static MutableMatrix *zero_matrix(const Ring *R, long nrows, long ncols, bool dense);
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
		  RingElement_array *values);

  //////////////////////////////
  // Casts down the hierarchy //
  //////////////////////////////

  virtual Mat_ZZp *get_mat_ZZp() { return 0; }
  virtual Mat_RR *get_mat_RR() { return 0; }

  virtual DMatRR * get_DMatRR() = 0;
  virtual DMatCC * get_DMatCC() = 0;
  virtual DMatZZp * get_DMatZZp() = 0;
  virtual DMatZZ * get_DMatZZ() = 0;
  virtual DMatR * get_DMatR() = 0;

  virtual SMatRR * get_SMatRR() = 0;
  virtual SMatCC * get_SMatCC() = 0;
  virtual SMatZZp * get_SMatZZp() = 0;
  virtual SMatZZ * get_SMatZZ() = 0;
  virtual SMatR * get_SMatR() = 0;

  virtual const DMatRR * get_DMatRR() const = 0;
  virtual const DMatCC * get_DMatCC() const = 0;
  virtual const DMatZZp * get_DMatZZp() const = 0;
  virtual const DMatZZ * get_DMatZZ() const = 0;
  virtual const DMatR * get_DMatR() const = 0;

  virtual const SMatRR * get_SMatRR() const = 0;
  virtual const SMatCC * get_SMatCC() const = 0;
  virtual const SMatZZp * get_SMatZZp() const = 0;
  virtual const SMatZZ * get_SMatZZ() const = 0;
  virtual const SMatR * get_SMatR() const = 0;

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual long lead_row(long col) const = 0;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual long lead_row(long col, ring_elem &result) const = 0;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */

  virtual bool get_entry(long r, long c, ring_elem &result) const = 0;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  virtual bool set_entry(long r, long c, const ring_elem a) = 0;
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  virtual bool interchange_rows(long i, long j) = 0;
  /* swap rows: row(i) <--> row(j) */

  virtual bool interchange_columns(long i, long j) = 0;
  /* swap columns: column(i) <--> column(j) */

  virtual bool scale_row(long i, ring_elem r) = 0;
  /* row(i) <- r * row(i) */

  virtual bool scale_column(long i, ring_elem r) = 0;
  /* column(i) <- r * column(i) */

  virtual bool divide_row(long i, ring_elem r) = 0;
  /* row(i) <- row(i) / r */

  virtual bool divide_column(long i, ring_elem r) = 0;
  /* column(i) <- column(i) / r */

  virtual bool row_op(long i, ring_elem r, long j) = 0;
  /* row(i) <- row(i) + r * row(j) */

  virtual bool column_op(long i, ring_elem r, long j) = 0;
  /* column(i) <- column(i) + r * column(j) */

  virtual bool column2by2(long c1, long c2, 
			  ring_elem a1, ring_elem a2,
			  ring_elem b1, ring_elem b2) = 0;
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  virtual bool row2by2(long r1, long r2, 
		       ring_elem a1, ring_elem a2,
		       ring_elem b1, ring_elem b2) = 0;
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  virtual bool dot_product(long i, long j, ring_elem &result) const = 0;

  virtual bool row_permute(long start_row, M2_arrayint perm) = 0;

  virtual bool column_permute(long start_col, M2_arrayint perm) = 0;

  virtual bool insert_columns(long i, long n_to_add) = 0;
  /* Insert n_to_add columns directly BEFORE column i. */

  virtual bool insert_rows(long i, long n_to_add) = 0;
  /* Insert n_to_add rows directly BEFORE row i. */

  virtual bool delete_columns(long i, long j) = 0;
  /* Delete columns i .. j from M */

  virtual bool delete_rows(long i, long j) = 0;
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

  virtual MutableMatrixOrNull * add(const MutableMatrix *B) const = 0; 
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * subtract(const MutableMatrix *B) const = 0; 
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * mult(const MutableMatrix *B) const = 0; 
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * mult(const RingElement *f) const = 0; 
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

  virtual M2_arrayint_OrNull LU(MutableMatrix *L,
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
};

///////////////////////////////////////////////////
// Wrapper class for a type which implements Mat //
///////////////////////////////////////////////////

template<typename CoeffRing, typename Mat>
class MutableMat : public MutableMatrix
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
  Mat mat;
  // This class wraps the operations for Mat to make a MutableMatrix

  // Same operations as in MutableMatrix.  Almost nothing is
  // done except to call the Mat routines.

  // Linear algebra routines: Default is to return 0.
  // Caller must then make error message?
  // Specific template instances must provide these functions
  MutableMat() {}

  MutableMat(const RingType *R, long nrows, long ncols)
    : mat(R,nrows,ncols) {}
public:
  virtual DMatRR * get_DMatRR();
  virtual DMatCC * get_DMatCC();
  virtual DMatZZp * get_DMatZZp();
  virtual DMatZZ * get_DMatZZ();
  virtual DMatR * get_DMatR();

  virtual SMatRR * get_SMatRR();
  virtual SMatCC * get_SMatCC();
  virtual SMatZZp * get_SMatZZp();
  virtual SMatZZ * get_SMatZZ();
  virtual SMatR * get_SMatR();

  virtual const DMatRR * get_DMatRR() const;
  virtual const DMatCC * get_DMatCC() const;
  virtual const DMatZZp * get_DMatZZp() const;
  virtual const DMatZZ * get_DMatZZ() const;
  virtual const DMatR * get_DMatR() const;

  virtual const SMatRR * get_SMatRR() const;
  virtual const SMatCC * get_SMatCC() const;
  virtual const SMatZZp * get_SMatZZp() const;
  virtual const SMatZZ * get_SMatZZ() const;
  virtual const SMatR * get_SMatR() const;

  virtual Mat_ZZp *get_mat_ZZp();
  virtual Mat_RR *get_mat_RR();

  Mat * get_Mat() { return &mat; }
  const Mat * get_Mat() const { return &mat; }

  class iterator : public MutableMatrix::iterator
  {
    typename Mat::iterator i;
  public:
    iterator(const Mat *M0) : i(M0) {}
    void set(long col0) { i.set(col0); }
    void next() { i.next(); }
    bool valid() { return i.valid(); }
    long row() { return i.row(); }
    elem value() { return i.value(); }
    void copy_ring_elem(ring_elem &result) { i.copy_elem(result); }
  };

  static MutableMat *zero_matrix(const RingType *R, long nrows, long ncols)
  {
    return new MutableMat(R,nrows,ncols);
  }

  static MutableMat *grab_Mat(const Mat *m);

  virtual iterator * begin() const { return new iterator(&mat); }

  virtual const Ring * get_ring() const { return mat.get_ring(); }

  virtual long n_rows() const { return mat.n_rows(); }

  virtual long n_cols() const { return mat.n_cols(); }

  virtual bool is_dense() const { return mat.is_dense(); }

  virtual MutableMat *copy(bool prefer_dense) const
  {
    MutableMat *result = new MutableMat;
    Mat *m = mat.copy();
    result->mat.grab(m);
    return result;
  }

  virtual long lead_row(long col) const { return mat.lead_row(col); }
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual long lead_row(long col, ring_elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */
  {
    elem b;
    mat.get_CoeffRing()->set_zero(b);
    long ret = mat.lead_row(col, b);
    if (ret >= 0)
      mat.get_CoeffRing()->to_ring_elem(result, b);
    return ret;
  }    

  virtual bool get_entry(long r, long c, ring_elem &result) const
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

  virtual bool set_entry(long r, long c, const ring_elem a)
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

  virtual bool interchange_rows(long i, long j)
  /* swap rows: row(i) <--> row(j) */
  {
    long nrows = n_rows();
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      return false;
    mat.interchange_rows(i,j);
    return true;
  }

  virtual bool interchange_columns(long i, long j)
  /* swap columns: column(i) <--> column(j) */
  {
    long ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    mat.interchange_columns(i,j);
    return true;
  }

  virtual bool scale_row(long i, ring_elem r)
  /* row(i) <- r * row(i) */
  {
    long nrows = n_rows();
    if (error_row_bound(i,nrows))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.scale_row(i,b);
    return true;
  }

  virtual bool scale_column(long i, ring_elem r)
  /* column(i) <- r * column(i) */
  {
    long ncols = n_cols();
    if (error_column_bound(i,ncols))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.scale_column(i,b);
    return true;
  }

  virtual bool divide_row(long i, ring_elem r)
  /* row(i) <- row(i) / r */
  {
    long nrows = n_rows();
    if (error_row_bound(i,nrows))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.divide_row(i,b);
    return true;
  }

  virtual bool divide_column(long i, ring_elem r)
  /* column(i) <- column(i) / r */
  {
    long ncols = n_cols();
    if (error_column_bound(i,ncols))
      return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.divide_column(i,b);
    return true;
  }

  virtual bool row_op(long i, ring_elem r, long j)
  /* row(i) <- row(i) + r * row(j) */
  {
    long nrows = n_rows();
    if (error_row_bound(i,nrows) || error_row_bound(j,nrows))
      return false;
    if (i == j) return true;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.row_op(i,b,j);
    return true;
  }

  virtual bool column_op(long i, ring_elem r, long j)
  /* column(i) <- column(i) + r * column(j) */
  {
    long ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    if (i == j) return true;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(b,r);
    mat.column_op(i,b,j);
    return true;
  }

  virtual bool column2by2(long c1, long c2, 
			  ring_elem a1, ring_elem a2,
			  ring_elem b1, ring_elem b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
  {
    long ncols = n_cols();
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

  virtual bool row2by2(long r1, long r2, 
		       ring_elem a1, ring_elem a2,
		       ring_elem b1, ring_elem b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
  {
    long nrows = n_rows();
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

  virtual bool dot_product(long c1, long c2, ring_elem &result) const
  {
    long ncols = n_cols();
    if (error_column_bound(c1,ncols) || error_column_bound(c2,ncols))
      return false;
    elem a;
    mat.get_CoeffRing()->set_zero(a);
    mat.dot_product(c1,c2,a);
    mat.get_CoeffRing()->to_ring_elem(result,a);
    return true;
  }

  virtual bool row_permute(long start_row, M2_arrayint perm)
  {
    return mat.row_permute(start_row, perm);
  }

  virtual bool column_permute(long start_col, M2_arrayint perm)
  {
    return mat.column_permute(start_col, perm);
  }

  virtual bool insert_columns(long i, long n_to_add)
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

  virtual bool insert_rows(long i, long n_to_add)
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

  virtual bool delete_columns(long i, long j)
  /* Delete columns i .. j from M */
  {
    long ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      {
	ERROR("column index out of range");
	return false;
      }

    mat.delete_columns(i, j);
    return true;
  }

  virtual bool delete_rows(long i, long j)
  /* Delete rows i .. j from M */
  {
    long nrows = n_rows();
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
    MutableMat *M = new MutableMat;
    M->mat.grab(mat.submatrix(rows,cols));
    return M;
  }

  virtual MutableMatrix * submatrix(M2_arrayint cols) const
  {
    MutableMat *M = new MutableMat;
    M->mat.grab(mat.submatrix(cols));
    return M;
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
    return mat.is_equal(B);
  }

  virtual MutableMat * add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
  {
    MutableMat *result = new MutableMat;
    result->mat.grab(mat.add(B));
    return result;
  }

  virtual MutableMat * subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
  {
    MutableMat *result = new MutableMat;
    result->mat.grab(mat.subtract(B));
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
    elem a;
    mat.get_CoeffRing()->from_ring_elem(a, f->get_value());
    MutableMat *result = new MutableMat;
    result->mat.grab(mat.mult(a));
    return result;
  }

  virtual MutableMatrix * negate() const
  {
    MutableMat *result = new MutableMat;
    result->mat.grab(mat.negate());
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

  virtual M2_arrayint_OrNull LU(MutableMatrix *L,
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
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

