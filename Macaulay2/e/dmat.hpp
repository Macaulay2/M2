// Copyright 2005  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

union ring_elem;
#include "ZZp.hpp"
// This is the low level dense matrix class.
// The only reason "RingType" is present is to more easily
//   communicate with the rest of Macaulay2

class MutableMatrix;

template<typename CoeffRing>
class DMat : public our_new_delete
{
  typedef typename CoeffRing::elem elem;
  typedef typename CoeffRing::ring_type RingType;
  const RingType *R; // To interface to the outside world
  CoeffRing * coeffR; // Same as R, optimized for speed.  R->get_CoeffRing()
  long nrows_;
  long ncols_;
  elem *array_; // array has length nrows*ncols
                // columns stored one after another


  void copy_elems(long n_to_copy, elem *target, long target_stride, elem *source, long stride);
public:
  DMat():R(0), coeffR(0), nrows_(0), ncols_(0), array_(0) {} // Makes a zero matrix

  DMat(const RingType *R0, long nrows, long ncols); // Makes a zero matrix

  void grab(DMat *M);// swaps M and this.

  DMat<CoeffRing> *copy() const;

  bool is_dense() const { return true; }

  long n_rows() const { return nrows_; }
  long n_cols() const { return ncols_; }
  const RingType * get_ring() const { return R; }
  const CoeffRing * get_CoeffRing() const { return coeffR; }

  void set_matrix(const DMat<CoeffRing> *mat0);
  void initialize(long nrows, long ncols, elem *array);
  void resize(long nrows, long ncols);
  elem * get_array() const { return array_; } // Used for lapack type routines
  double *get_lapack_array() const; // redefined by RR,CC

  class iterator : public our_new_delete
  {
    const DMat<CoeffRing> *M;
    long col;
    elem *begin;
    elem *end;
    void to_next_valid() { 
      const CoeffRing *R = M->get_CoeffRing();
      --begin;
      while (begin >= end && R->is_zero(*begin)) --begin;
    }
  public:
    void set(long col0) { 
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
    long row() { return begin-end; }

    void copy_elem(ring_elem &result) { 
      M->get_CoeffRing()->to_ring_elem(result, value());
    }
  };


public:
  long lead_row(long col) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  long lead_row(long col, elem &result) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  bool get_entry(long r, long c, elem &result) const;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  void set_entry(long r, long c, const elem &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  void interchange_rows(long i, long j); 
  /* swap rows: row(i) <--> row(j) */

  void interchange_columns(long i, long j); 
  /* swap columns: column(i) <--> column(j) */

  void scale_row(long i, const elem &r); 
  /* row(i) <- r * row(i) */

  void scale_column(long i, const elem &r); 
  /* column(i) <- r * column(i) */

  void divide_row(long i, const elem &r);
  /* row(i) <- row(i) / r */

  void divide_column(long i, const elem &r);
  /* column(i) <- column(i) / r */

  void row_op(long i, const elem &r, long j); 
  /* row(i) <- row(i) + r * row(j) */

  void column_op(long i, const elem &r, long j); 
  /* column(i) <- column(i) + r * column(j) */

  void column2by2(long c1, long c2, 
		  const elem &a1, const elem &a2,
		  const elem &b1, const elem &b2);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  void row2by2(long r1, long r2, 
	       const elem &a1, const elem &a2,
	       const elem &b1, const elem &b2);
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  void dot_product(long i, long j, elem &result) const; 

  bool row_permute(long start_row, M2_arrayint perm);

  bool column_permute(long start_col, M2_arrayint perm);

  void insert_columns(long i, long n_to_add);
  /* Insert n_to_add columns directly BEFORE column i. */

  void insert_rows(long i, long n_to_add);
  /* Insert n_to_add rows directly BEFORE row i. */

  void delete_columns(long i, long j);
  /* Delete columns i .. j from M */

  void delete_rows(long i, long j);
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
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
