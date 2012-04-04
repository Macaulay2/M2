// Copyright 2005  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

union ring_elem;
#include "ZZp.hpp"

#include "aring-ffpack.hpp"
#include "aring-gf.hpp"


//#include <mblas_mpfr.h>
//#include <mlapack_mpfr.h>
//#include "mpreal.h"
// This is the low level dense matrix class.
// The only reason "RingType" is present is to more easily
//   communicate with the rest of Macaulay2

// enable_if is probably only available for never standard, e.g. compiler flag -std=c++0x
template<bool, typename T = void> 
  struct enable_if {};

template<typename T>
  struct enable_if<true, T> {
    typedef T type;
  };

template< typename T > 
struct is_givaro_or_ffpack{ 
  static const bool value = false;
};

template<> 
struct is_givaro_or_ffpack< M2::ARingZZpFFPACK >{ 
  static const bool value = true; 
};

template<> 
struct is_givaro_or_ffpack< M2::ARingGF >{ 
  static const bool value = true; 
};




class MutableMatrix;

/**
 * \ingroup matrices
 */
template<typename ACoeffRing>
class DMat : public our_new_delete
{
public:
  typedef ACoeffRing CoeffRing;
  typedef typename CoeffRing::elem elem;
  typedef elem ElementType; // same as elem.  Will possibly remove 'elem' later.

  DMat():R(0), coeffR(0), nrows_(0), ncols_(0), array_(0) {} // Makes a zero matrix

  DMat(const Ring *R, const ACoeffRing *R0, int nrows, int ncols); // Makes a zero matrix

  DMat(const DMat<ACoeffRing> &M, size_t nrows, size_t ncols); // Makes a zero matrix, same ring.

  void grab(DMat *M);// swaps M and this.

  DMat<CoeffRing> *copy() const;

  bool is_dense() const { return true; }

  int n_rows() const { return nrows_; }
  int n_cols() const { return ncols_; }
  const Ring * get_ring() const { return R; }
  const CoeffRing * get_CoeffRing() const { return coeffR; }
  const CoeffRing& ring() const { return *coeffR; }

  void set_matrix(const DMat<CoeffRing> *mat0);
  void initialize(int nrows, int ncols, elem *array);
  void resize(int nrows, int ncols);

  ///@todo potential trouble if array is modivied by the caller... either return copy  or introduce a second non-const function .
  elem * get_array() const { return array_; } // Used for lapack type routines

  double *get_lapack_array() const; // redefined by RR,CC

  double *make_lapack_array() const; // creates an array of doubles (or 0, if not applicable)
  __mpfr_struct *make_mpack_array() const;
  void fill_from_lapack_array(double *lapack_array);  // The size of the array should match the size of this.
  //void fill_from_mpack_array(mpreal *mparray);

  class iterator : public our_new_delete
  {
    const DMat<CoeffRing> *M;
    int col;
    elem *begin;
    elem *end;
    void to_next_valid() {
      const CoeffRing *R = M->get_CoeffRing();
      --begin;
      while (begin >= end && R->is_zero(*begin)) --begin;
    }
  public:
    void set(int col0) {
      col = col0;
      begin = M->array_ + (col0+1) * (M->n_rows());
      end = begin-M->n_rows();
      to_next_valid();
    }
    iterator(const DMat<CoeffRing> *M0) : M(M0),
                                          col(-1),
                                          begin(0),
                                          end(0) { }
    const elem &value() { return *begin; }
    void next() { to_next_valid(); }
    bool valid() { return begin >= end; }
    int row() { return static_cast<int>(begin-end); }

    void copy_elem(ring_elem &result) {
      M->get_CoeffRing()->to_ring_elem(result, value());
    }
  };


public:
  int lead_row(int col) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  int lead_row(int col, elem &result) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  bool get_entry(int r, int c, elem &result) const;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  void set_entry(int r, int c, const elem &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  void interchange_rows(int i, int j);
  /* swap rows: row(i) <--> row(j) */

  void interchange_columns(int i, int j);
  /* swap columns: column(i) <--> column(j) */

  void scale_row(int i, const elem &r);
  /* row(i) <- r * row(i) */

  void scale_column(int i, const elem &r);
  /* column(i) <- r * column(i) */

  void divide_row(int i, const elem &r);
  /* row(i) <- row(i) / r */

  void divide_column(int i, const elem &r);
  /* column(i) <- column(i) / r */

  void row_op(int i, const elem &r, int j);
  /* row(i) <- row(i) + r * row(j) */

  void column_op(int i, const elem &r, int j);
  /* column(i) <- column(i) + r * column(j) */

  void column2by2(int c1, int c2,
                  const elem &a1, const elem &a2,
                  const elem &b1, const elem &b2);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  void row2by2(int r1, int r2,
               const elem &a1, const elem &a2,
               const elem &b1, const elem &b2);
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  void dot_product(int i, int j, elem &result) const;

  bool row_permute(int start_row, M2_arrayint perm);

  bool column_permute(int start_col, M2_arrayint perm);

  void insert_columns(int i, int n_to_add);
  /* Insert n_to_add columns directly BEFORE column i. */

  void insert_rows(int i, int n_to_add);
  /* Insert n_to_add rows directly BEFORE row i. */

  void delete_columns(int i, int j);
  /* Delete columns i .. j from M */

  void delete_rows(int i, int j);
  /* Delete rows i .. j from M */

  bool set_submatrix(M2_arrayint rows,
                     M2_arrayint cols,
                     const MutableMatrix *N);

  DMat<CoeffRing> *submatrix(M2_arrayint rows, M2_arrayint cols) const;

  DMat<CoeffRing> *submatrix(M2_arrayint cols) const;

  bool is_zero() const;

  bool is_equal(const MutableMatrix *B) const;

  DMat * add(const MutableMatrix *B) const;

  DMat * subtract(const MutableMatrix *B) const;
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  DMat * mult(const MutableMatrix *B) const;
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  DMat * mult(const elem &f) const;
  // return f*this.  return NULL of sizes or types do not match.

  DMat * negate() const;

  ///////////////////////////////////
  /// Fast linear algebra routines //
  ///////////////////////////////////

  
  template<class RingType>
  size_t rank(typename enable_if<is_givaro_or_ffpack<RingType>::value >::type* dummy = 0) const;
  size_t rank() const;
 
  
  template<class RingType>
  void determinantGF_or_FFPACK(elem &result) const;

  void determinant(elem &result) const;

  // Set 'inverse' with the inverse of 'this'.  If the matrix is not square, or 
  // the matrix is not invertible, or
  // the ring is one in which the matrix cannot be inverted,
  // then false is returned, and an error message is set.
  bool invert(DMat<ACoeffRing> &inverse) const;

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  M2_arrayintOrNull rankProfile(bool row_profile) const;
  
  // Find a spanning set for the null space.  If M = this,
  // and right_side is true, return a matrix whose rows span {x |  xM = 0},
  // otherwise return a matrix whose columns span {x | Mx = 0}
  void nullSpace(DMat<ACoeffRing> &nullspace, bool right_side) const;

  // X is set to  a matrix whose rows or columns solve either AX = B (right_side=true)
  // or XA = B (right_side=false). If no solutions are found, false is returned.
  bool solveLinear(DMat<ACoeffRing> &X, const DMat<ACoeffRing> &B, bool right_side) const;

  /** C=this,A,B should be mutable matrices over the same ring, and a,b
     elements of this ring. AND of the same density type.
     C = b*C + a * op(A)*op(B),
     where op(A) = A or transpose(A), depending on transposeA
     where op(B) = B or transpose(B), depending on transposeB
  */
  void addMultipleTo(DMat<ACoeffRing> &C,
                     const DMat<ACoeffRing> &A,
                     const DMat<ACoeffRing> &B,
                     bool transposeA,
                     bool transposeB,
                     ring_elem a,
                     ring_elem b) const;

private:
  const Ring *R; // To interface to the outside world
  const CoeffRing * coeffR; // Same as R, optimized for speed.  R->get_CoeffRing()
  int nrows_;
  int ncols_;
  elem *array_; // array has length nrows*ncols
                // columns stored one after another


  void copy_elems(long n_to_copy, elem *target, int target_stride, const elem *source, int stride) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
