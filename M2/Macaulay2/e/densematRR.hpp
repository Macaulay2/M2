// Copyright 2004  Michael E. Stillman

#ifndef _densematRR_hpp_
#define _densematRR_hpp_

#include "mutablemat.hpp"

class DenseMutableMatrixRR : public DenseMutableMatrix
{
  double *array_; // array has length nrows*ncols
                     // columns stored one after another

  void initialize(int nrows, int ncols, double *array);

  DenseMutableMatrixRR();

  virtual ~DenseMutableMatrixRR() {}
public:
  static DenseMutableMatrixRR *zero_matrix(int nrows, int ncols);

  virtual DenseMutableMatrixRR * cast_to_DenseMutableMatrixRR() { return this; }
  virtual const DenseMutableMatrixRR * cast_to_DenseMutableMatrixRR() const { return this; }

public:
  virtual Matrix *to_matrix() const;

  virtual MutableMatrix *copy(bool prefer_dense) const;

public:
  virtual int lead_row(int col) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */

  virtual int lead_row(int col, ring_elem &result) const;
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0 */

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

  ///////////////////////////////
  // Matrix operations //////////
  ///////////////////////////////

  virtual bool is_zero() const;

  virtual bool is_equal(const MutableMatrix *B) const;

  virtual bool set_values(M2_arrayint rows,
			  M2_arrayint cols,
			  RingElement_array *values);

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

  virtual MutableMatrix * submatrix(const M2_arrayint rows, const M2_arrayint cols) const;

  virtual MutableMatrix * submatrix(const M2_arrayint cols) const;
};

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
