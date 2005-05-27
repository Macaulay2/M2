// Copyright 2005  Michael E. Stillman

#ifndef _mat_hpp_
#define _mat_hpp_

#include "hash.hpp"
#include "error.h"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "coeffrings.hpp"
#include <vector>

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
class Vecs : public our_new_delete
{
public:
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;

  struct vec : public our_new_delete
  {
    vec *next;
    long comp;
    elem coeff;
  };
};

template<typename CoeffRing>
class Mat
{
  // This class is a shell of a class which describes what needs to
  // be provided in order for MutableMat<CoeffRing,Mat> to work.

  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
  typedef typename Vecs<CoeffRing>::vec sparsevec;
public:
  long n_rows() const { return 0; }

  long n_cols() const { return 0; }

  bool is_dense() const { return true; }

  long lead_row(long col, elem &result) const { return -1; }

  const CoeffRing *get_CoeffRing() const { return 0; }

  const Ring *get_ring() const { return 0; }

  bool get_entry(long i, long j, elem &result) const { return false; }

  bool set_entry(long i, long j, elem result) const { return false; }

  void interchange_columns(long i, long j) const { }

  void column_op(long i, elem b, long j, bool opposite_mult) {}

  // To and from vectors
  void add_to_dense_vector(long col, int lo, int hi, elem *result);
  // Adds this[lo,col], ..., this[hi,col]  into result[0], ..., result[hi-lo]

  void add_to_sparse_vector(long col, int lo, int hi, sparsevec *&result);
  // Adds this[lo,col], ..., this[hi,col]  into result

};

template<typename CoeffRing, typename Mat>
class DMat : public our_new_delete
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
};

template<typename CoeffRing, typename Mat>
class SMat : public our_new_delete
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;

  
};

class GBMatrix : public our_new_delete
{
};

//////////////////////////////////////
// The wrapper MutableMatrix type ////
//////////////////////////////////////
class matrix_iterator : public our_new_delete
{
  virtual void set(int col) = 0;
  virtual void next() = 0;
  virtual bool valid() = 0;
  virtual int row() = 0;
  virtual void copy_ringelem(ring_elem &a) = 0;
};


//matrix_iterator *i = M->iterator();
//for (i->set(c); i->valid(); i->next())
//{
//  use i->row(), i->copy_ringelem(a)
//}
//delete i;

typedef Mat<CoefficientRingZZp> Mat_ZZp;
typedef Mat<CoefficientRingRR> Mat_RR;

class MutableMatrixXXX;
typedef MutableMatrixXXX MutableMatrixXXXOrNull;

class MutableMatrixXXX : public mutable_object
{
protected:
  MutableMatrixXXX() {}
  virtual ~MutableMatrixXXX() {}
public:
  virtual const Ring * get_ring() const = 0;

  virtual long n_rows() const = 0;

  virtual long n_cols() const = 0;

  static MutableMatrixXXX *zero_matrix(const Ring *R, long nrows, long ncols, bool dense);
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or 
  // MutableMatrixCC will be used.

  static MutableMatrixXXX *from_matrix(const Matrix *N, bool is_dense);
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or 
  // MutableMatrixCC will be used.

  virtual Matrix *to_matrix() const = 0;

  virtual MutableMatrixXXX *copy(bool prefer_dense) const = 0;

  //////////////////////////////
  // Casts down the hierarchy //
  //////////////////////////////

  virtual Mat_ZZp *get_mat_ZZp() { return 0; }
  virtual Mat_RR *get_mat_RR() { return 0; }

  virtual bool is_dense() const = 0;

  void text_out(buffer &o) const;

  virtual long lead_row(long col, ring_elem &result) const = 0;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual bool get_entry(long r, long c, ring_elem &result) const = 0;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  virtual bool set_entry(long r, long c, const ring_elem a) = 0;
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  virtual bool interchange_columns(long i, long j, bool do_recording=true) = 0;
  /* swap columns: column(i) <--> column(j) */

  virtual bool column_op(long i, ring_elem r, long j, bool opposite_mult, bool do_recording=true) = 0;
  /* column(i) <- column(i) + r * column(j) */

  virtual MutableMatrixXXX * submatrix(const M2_arrayint rows, const M2_arrayint cols) const = 0;

  virtual bool set_submatrix(const M2_arrayint rows,
			     const M2_arrayint cols, 
			     const MutableMatrixXXX *N) = 0;
  // returns false iff there is an error

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual bool is_zero() const = 0;

  virtual bool is_equal(const MutableMatrixXXX *B) const = 0;

  virtual bool set_values(M2_arrayint rows,
			  M2_arrayint cols,
			  RingElement_array *values) = 0;

  virtual MutableMatrixXXXOrNull * add(const MutableMatrixXXX *B) const = 0; 
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.

  ///////////////////////////////
  // Linear algebra /////////////
  ///////////////////////////////
  
  virtual void solve(MutableMatrixXXX *x, MutableMatrixXXX *b) = 0;
  // resets x, find a basis of solutions for Ax=b

  virtual void LU(MutableMatrixXXX *L, std::vector<int, gc_allocator<int> > &perm) = 0;
};

template<typename CoeffRing, typename Mat>
class MutableMat : public MutableMatrixXXX
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
  Mat mat;
  // This class wraps the operations for Mat to make a MutableMatrixXXX

  // Same operations as in MutableMatrixXXX.  Almost nothing is
  // done except to call the Mat routines.

  // Linear algebra routines: Default is to return 0.
  // Caller must then make error message?
  // Specific template instances must provide these functions
public:
  virtual Mat_ZZp *get_mat_ZZp();
  virtual Mat_RR *get_mat_RR();

  virtual const Ring * get_ring() const { return mat.get_ring(); }

  virtual long n_rows() const { return mat.n_rows(); }

  virtual long n_cols() const { return mat.n_cols(); }

  virtual bool is_dense() const { return mat.is_dense(); }

  virtual long lead_row(long col, ring_elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */
  {
    elem b;
    long ret = mat.lead_row(col, b);
    if (ret >= 0)
      mat.get_CoeffRing()->to_ring_elem(b, result);
    return ret;
  }    

  virtual bool get_entry(long r, long c, ring_elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
  {
    if (r >= 0 && r < n_rows() && c >= 0 && c < n_cols())
      {
	elem a;
	bool ret = mat.get_entry(r,c,a);
	mat.get_CoeffRing()->to_ring_elem(a,result);
	return ret;
      }
    else
      {
	result = mat.get_ring()->zero();
	return false;
      }
  }

  virtual bool set_entry(long r, long c, const ring_elem a)
  // Returns false if (r,c) is out of range, or the ring of a is wrong.
  {
    if (error_row_bound(r,n_rows())) return false;
    if (error_column_bound(c,n_cols())) return false;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(a,b);
    mat.set_entry(r,c,b);
    return true;
  }


  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual bool interchange_columns(long i, long j)
  /* swap columns: column(i) <--> column(j) */
  {
    int ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    mat.interchange_columns(i,j);
    return true;
  }

  virtual bool column_op(long i, ring_elem r, long j, bool opposite_mult)
  /* column(i) <- column(i) + r * column(j) */
  {
    int ncols = n_cols();
    if (error_column_bound(i,ncols) || error_column_bound(j,ncols))
      return false;
    if (i == j) return true;
    elem b;
    mat.get_CoeffRing()->from_ring_elem(r,b);
    mat.column_op(i,b,j,opposite_mult);
    return true;
  }

  virtual MutableMatrixXXX * submatrix(const M2_arrayint rows, const M2_arrayint cols) const
  {
    return 0;
  }

  virtual bool set_submatrix(const M2_arrayint rows,
			     const M2_arrayint cols, 
			     const MutableMatrixXXX *N)
  // returns false iff there is an error
  {
    return false;
  }

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual bool is_zero() const
  {
    return false;
  }

  virtual bool is_equal(const MutableMatrixXXX *B) const
  {
    return false;
  }

  virtual bool set_values(M2_arrayint rows,
			  M2_arrayint cols,
			  RingElement_array *values)
  {
    return false;
  }

  virtual MutableMatrixXXXOrNull * add(const MutableMatrixXXX *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
  {
    return 0;
  }

  ///////////////////////////////
  // Linear algebra /////////////
  ///////////////////////////////
  
  virtual void solve(MutableMatrixXXX *x, MutableMatrixXXX *b);
  // resets x, find a basis of solutions for Ax=b

  virtual void LU(MutableMatrixXXX *L, std::vector<int, gc_allocator<int> > &perm);

};


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

#endif
