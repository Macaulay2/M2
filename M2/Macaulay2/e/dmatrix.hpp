// Copyright 2005  Michael E. Stillman

#ifndef _dmatrix_hpp_
#define _dmatrix_hpp_

#include "mutablemat.hpp"
#include "dmat.hpp"

template<typename CoeffRing>
class DMatrix : public DenseMutableMatrix
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
  DMat<CoeffRing> *mat;

  DMatrix(const RingType *R, int nrows, int ncols);

  DMatrix(DMat<CoeffRing> *mat0); // grabs this matrix

  virtual ~DMatrix() {}
public:
  static DMatrix<CoeffRing> *zero_matrix(const RingType *R0, int nrows, int ncols);

  virtual DMatrix<CoeffRing> * cast_to_DMatrix() { return this; }
  virtual const DMatrix<CoeffRing> * cast_to_DMatrix() const { return this; }

  DMat<CoeffRing> * get_DMat() { return mat; }

  virtual DMat<CoefficientRingRR> * get_DMatRR() const { return 0; }
  virtual DMat<CoefficientRingCC> * get_DMatCC() const { return 0; }
  virtual DMat<CoefficientRingZZp> * get_DMatZZp() const { return 0; }
public:
  virtual Matrix *to_matrix() const;
  
  virtual DMatrix<CoeffRing> *copy(bool prefer_dense) const;

  virtual int n_rows() const { return mat->n_rows(); }

  virtual int n_cols() const { return mat->n_cols(); }
public:
  virtual int lead_row(int col) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual int lead_row(int col, ring_elem &result) const;

  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */

  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual bool get_entry(int r, int c, ring_elem &result) const;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  virtual bool set_entry(int r, int c, const ring_elem a); // DONE
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  virtual bool interchange_rows(int i, int j, bool do_recording=true); // DONE
  /* swap rows: row(i) <--> row(j) */

  virtual bool interchange_columns(int i, int j, bool do_recording=true); // DONE
  /* swap columns: column(i) <--> column(j) */

  virtual bool scale_row(ring_elem r, int i, bool opposite_mult, bool do_recording=true); // DONE
  /* row(i) <- r * row(i) */

  virtual bool scale_column(ring_elem r, int i, bool opposite_mult, bool do_recording=true); // DONE
  /* column(i) <- r * column(i) */

  virtual bool divide_row(int i, ring_elem r, bool do_recording=true);
  /* row(i) <- row(i) / r */

  virtual bool divide_column(int i, ring_elem r, bool do_recording=true);
  /* column(i) <- column(i) / r */

  virtual bool row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording=true); // DONE
  /* row(i) <- row(i) + r * row(j) */

  virtual bool column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording=true); // DONE
  /* column(i) <- column(i) + r * column(j) */

  virtual bool column2by2(int c1, int c2, 
			  ring_elem a1, ring_elem a2,
			  ring_elem b1, ring_elem b2,
			  bool opposite_mult,
			  bool doRecording=true);
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  virtual bool row2by2(int r1, int r2, 
		       ring_elem a1, ring_elem a2,
		       ring_elem b1, ring_elem b2,
		       bool opposite_mult,
		       bool doRecording=true);
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  virtual bool dot_product(int i, int j, ring_elem &result) const; // DONE

  virtual bool row_permute(int start_row, const M2_arrayint perm);

  virtual bool column_permute(int start_col, const M2_arrayint perm);

  virtual bool set_submatrix(const M2_arrayint rows,
			     const M2_arrayint cols, 
			     const MutableMatrix *N);
  // returns false iff there is an error

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual bool is_zero() const;

  virtual bool is_equal(const MutableMatrix *B) const;

  virtual bool set_values(M2_arrayint rows,
			  M2_arrayint cols,
			  RingElement_array *values);

  virtual DMatrix<CoeffRing> * submatrix(const M2_arrayint rows, 
					 const M2_arrayint cols) const;

  virtual DMatrix<CoeffRing> * submatrix(const M2_arrayint cols) const;

  virtual MutableMatrixOrNull * add(const MutableMatrix *B) const; 
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * subtract(const MutableMatrix *B) const; 
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * mult(const MutableMatrix *B,
				     M2_bool opposite_mult) const; 
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.

  virtual MutableMatrixOrNull * mult(const RingElement *f,
				     M2_bool opposite_mult) const; 
  // return f*this.  return NULL of sizes or types do not match.

  virtual MutableMatrix * negate() const;

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
