// Copyright 1996 by Michael E. Stillman.

#ifndef _det_hh_
#define _det_hh_

#include "comp.hpp"
#include "matrix.hpp"
#include "comb.hpp"

class DetComputation : public computation
{
  const Ring *R;
  Matrix M;
  Matrix result;  // Either:One row matrix collecting non-zero 
		  // determinants, or the resulting
		  // exterior power; depending on 'do_exterior'

  //hashtable<ring_elem> table;	// Only up through size p-1 by p-1.
  bool done;
  int p;

  bool do_exterior;		// true = construct exterior
				// power of matrix, false =
				// collect non-zero minors
  int * row_set;
  int * col_set;
  int this_row;
  int this_col;

  bool do_trivial_case();

  ring_elem calc_det(int *r, int *c, int p);
     // Compute the determinant of the minor with rows r[0]..r[p-1]
     // and columns c[0]..c[p-1].
public:
  DetComputation(const Matrix &M, int p, bool do_exterior);
  ~DetComputation();

  class_identifier class_id() const { return CLASS_DetComputation; }

  // serialize
  virtual void write_object(object_writer &o) const;
  static DetComputation *read_object(object_reader &i);

  int step();
  int calc(int nsteps);

  // The following two routines are only valid for 'do_exterior=0'
  void clear();
  void discard() { clear(); }
  void set_next_minor(const int* rows, const int* cols);
  
  Matrix determinants() { return result; }

  int length_of() const { return result.n_cols(); }
  object index_of(int) { return result; }
};

#endif
