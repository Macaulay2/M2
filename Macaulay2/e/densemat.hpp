// Copyright 2004  Michael E. Stillman

#ifndef _densemat_hpp_
#define _densemat_hpp_

#include "mutablemat.hpp"

class DenseMutableMatrixRing : public DenseMutableMatrix
{
  ring_elem *array_; // array has length nrows*ncols
                     // columns stored one after another

  void initialize(int nrows, int ncols, ring_elem *array);

  DenseMutableMatrixRing(const Ring *R, int nrows, int ncols);

  virtual ~DenseMutableMatrixRing() {}
public:
  static DenseMutableMatrixRing *zero_matrix(const Ring *R, int nrows, int ncols);

  virtual DenseMutableMatrixRing * cast_to_DenseMutableMatrixRing() { return this; }

public:
  virtual Matrix *to_matrix() const;

  virtual MutableMatrix *copy(bool prefer_dense) const;

public:
  ///////////////////////////////
  // Row and column operations //
  ///////////////////////////////
  // The following routines return false if one of the row or columns given
  // is out of range.

  virtual bool get_entry(int r, int c, ring_elem &result) const; 
  // Returns false if (r,c) is out of range.

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

  virtual bool row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording=true); // DONE
  /* row(i) <- row(i) + r * row(j) */

  virtual bool column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording=true); // DONE
  /* column(i) <- column(i) + r * column(j) */

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
