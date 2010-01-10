// Copyright 2005  Michael E. Stillman

#ifndef _smat_hpp_
#define _smat_hpp_

union ring_elem;
#include "ZZp.hpp"
// This is the low level dense matrix class.
// The only reason "RingType" is present is to more easily
//   communicate with the rest of Macaulay2

class MutableMatrix;

template<typename CoeffRing>
class SMat : public our_new_delete
{
  typedef typename CoeffRing::elem elem;
  typedef typename CoeffRing::ring_type RingType;

  struct sparsevec : public our_new_delete
  {
    sparsevec *next;
    int row;
    elem coeff;
  };

  const RingType *R; // To interface to the outside world
  CoeffRing * coeffR; // Same as R, optimized for speed.  R->get_CoeffRing()
  long nrows_;
  long ncols_;
  sparsevec **columns_; // array has length nrows*ncols
                // columns stored one after another

  ////////////////////////
  // sparsevec routines //
  ////////////////////////
  sparsevec *vec_new() const;
  void vec_remove_node(sparsevec *&v) const;
  void vec_remove(sparsevec *&v) const;
  sparsevec *vec_copy(const sparsevec *v) const;
  bool vec_get_entry(const sparsevec *v, long r, elem &result) const;
  void vec_set_entry(sparsevec *&v, long r, const elem &result) const;
  void vec_interchange_rows(sparsevec *&v, long r1, long r2) const;
  void vec_scale_row(sparsevec *&v, long r, const elem &a) const;
  void vec_scale(sparsevec *&v, const elem &a) const;
  void vec_divide_row(sparsevec *&v, long r, const elem &a) const;
  void vec_divide(sparsevec *&v, const elem &a) const;
  void vec_add_to(sparsevec *&v, sparsevec *&w) const;
    // v := v+w, w := 0
  void vec_row_op(sparsevec *&v, long r1, const elem &a, long r2) const;
    // row(r1 in v) := row(r1 in v) + a * row(r2 in v)
  void vec_row_op2(sparsevec *&v, 
		   long r1, long r2, 
		   const elem &a1, const elem &a2,
		   const elem &b1, const elem &b2) const;
    // row(r1 in v) := a1 * row(r1 in v) + a2 * row(r2 in v)
    // row(r2 in v) := b1 * row(r1 in v) + b2 * row(r2 in v) (RHS refers to previous values)
  void vec_column_op(sparsevec *&v, const elem &a, sparsevec *w) const;
    // v := v + a*w
  void vec_dot_product(sparsevec *v, sparsevec *w, elem &result) const;
  void vec_sort(sparsevec *&v) const;
  void vec_permute(sparsevec *&v, long start_row, M2_arrayint perm) const;
  void vec_insert_rows(sparsevec *&v, long i, long n_to_add) const;
  void vec_delete_rows(sparsevec *&v, long i, long j) const;
public:
  SMat():R(0), coeffR(0), nrows_(0), ncols_(0), columns_(0) {} // Makes a zero matrix

  SMat(const RingType *R0, long nrows, long ncols); // Makes a zero matrix

  void grab(SMat *M);// swaps M and this.

  SMat<CoeffRing> *copy() const;

  bool is_dense() const { return false; }

  long n_rows() const { return nrows_; }
  long n_cols() const { return ncols_; }
  const RingType * get_ring() const { return R; }
  const CoeffRing * get_CoeffRing() const { return coeffR; }

  //  void set_matrix(const SMat<CoeffRing> *mat0);
  void initialize(long nrows, long ncols, sparsevec **cols);
  //  void resize(long nrows, long ncols);

  class iterator : public our_new_delete
  {
    const SMat<CoeffRing> *M;
    long col;
    sparsevec *v;
  public:
    void set(long col0) { 
      col = col0;
      v = M->columns_[col];
    }
    iterator(const SMat<CoeffRing> *M0) : M(M0), 
					  col(-1), 
					  v(0) {}
    const elem &value() { return v->coeff; }
    void next() { v = v->next; }
    bool valid() { return v != 0; }
    long row() { return v->row; }

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

  void set_entry(long r, long c, const elem a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  void interchange_rows(long i, long j); 
  /* swap rows: row(i) <--> row(j) */

  void interchange_columns(long i, long j); 
  /* swap columns: column(i) <--> column(j) */

  void scale_row(long i, elem r); 
  /* row(i) <- r * row(i) */

  void scale_column(long i, elem r); 
  /* column(i) <- r * column(i) */

  void divide_row(long i, elem r);
  /* row(i) <- row(i) / r */

  void divide_column(long i, elem r);
  /* column(i) <- column(i) / r */

  void row_op(long i, elem r, long j); 
  /* row(i) <- row(i) + r * row(j) */

  void column_op(long i, elem r, long j); 
  /* column(i) <- column(i) + r * column(j) */

  void column2by2(long c1, long c2, 
		  elem a1, elem a2,
		  elem b1, elem b2);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  void row2by2(long r1, long r2, 
	       elem a1, elem a2,
	       elem b1, elem b2);
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
		     const MutableMatrix *M);

  SMat<CoeffRing> *submatrix(M2_arrayint rows, M2_arrayint cols) const;

  SMat<CoeffRing> *submatrix(M2_arrayint cols) const;

  bool is_zero() const;

  bool is_equal(const MutableMatrix *B) const;

  SMat * add(const MutableMatrix *B) const;

  SMat * subtract(const MutableMatrix *B) const;
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  SMat * mult(const MutableMatrix *B) const;
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  SMat * mult(const elem &f) const;
  // return f*this.  return NULL of sizes or types do not match.

  SMat * negate() const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
