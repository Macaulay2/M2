// Copyright 2005  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

// This is the low level dense matrix class.
// The only reason "RingType" is present is to more easily
//   communicate with the rest of Macaulay2

template<typename CoeffRing>
class DMat : public our_new_delete
{
  typedef typename CoeffRing::elem elem;
  typedef typename CoeffRing::ring_type RingType;
  const RingType *R; // To interface to the outside world
  CoeffRing * coeffR; // Same as R, optimized for speed.  R->get_CoeffRing()
  int nrows_;
  int ncols_;
  elem *array_; // array has length nrows*ncols
                // columns stored one after another


public:
  DMat(const RingType *R0, int nrows, int ncols); // Makes a zero matrix

  DMat<CoeffRing> *copy() const;

  static DMat<CoeffRing> *zero_matrix(const RingType *R0, int nrows, int ncols);

  int n_rows() const { return nrows_; }
  int n_cols() const { return ncols_; }
  const RingType * get_ring() const { return R; }
  const CoeffRing * get_CoeffRing() const { return coeffR; }

  void set_matrix(const DMat<CoeffRing> *mat0);
  void initialize(int nrows, int ncols, elem *array);
  void resize(int nrows, int ncols);
  elem * get_array() const { return array_; } // Used for lapack type routines
  double *get_lapack_array() const { return 0; } // redefined by RR,CC
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

  void set_entry(int r, int c, const elem a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  void interchange_rows(int i, int j); 
  /* swap rows: row(i) <--> row(j) */

  void interchange_columns(int i, int j); 
  /* swap columns: column(i) <--> column(j) */

  void scale_row(elem r, int i); 
  /* row(i) <- r * row(i) */

  void scale_column(elem r, int i); 
  /* column(i) <- r * column(i) */

  void divide_row(int i, elem r);
  /* row(i) <- row(i) / r */

  void divide_column(int i, elem r);
  /* column(i) <- column(i) / r */

  void row_op(int i, elem r, int j); 
  /* row(i) <- row(i) + r * row(j) */

  void column_op(int i, elem r, int j); 
  /* column(i) <- column(i) + r * column(j) */

  void column2by2(int c1, int c2, 
		  elem a1, elem a2,
		  elem b1, elem b2);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  void row2by2(int r1, int r2, 
	       elem a1, elem a2,
	       elem b1, elem b2);
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  void dot_product(int i, int j, elem &result) const; 

  bool row_permute(int start_row, const M2_arrayint perm);

  bool column_permute(int start_col, const M2_arrayint perm);

  bool set_submatrix(const M2_arrayint rows,
		     const M2_arrayint cols, 
		     const DMat<CoeffRing> *N);
  // returns false iff there is an error
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
