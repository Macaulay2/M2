// Copyright 1996 Michael E. Stillman.

#ifndef _pfaff_hh_
#define _pfaff_hh_

#include "comp.hpp"
#include "matrix.hpp"
#include "comb.hpp"

class PfaffianComputation : public computation
{
  const Ring *R;
  Matrix M;
  Matrix pfaffs;			// One row matrix collecting non-zero 
				// pfaffians.
  //hashtable<ring_elem> table;	// Only up through size p-1 by p-1.
  int p;

  int I;			// I (resp. J) is row_set (col_set) encoded
  intarray row_set;
  int endI;

  ring_elem calc_pfaff(int *r, int p);
     // Compute the pfaffian of the minor with rows r[0]..r[p-1]
     // and columns c[0]..c[p-1].
public:
  PfaffianComputation(const Matrix &M, int p);
  ~PfaffianComputation();

  int step();	
	// Compute one more pfaffian of size p.
	// increments I and/or J and updates 'pfaffs', 'table'.
  int calc(int nsteps);
  
  Matrix pfaffians() { return pfaffs; }

  int length_of() const { return pfaffs.n_cols(); }
  object index_of(int) { return pfaffs; }

  class_identifier class_id() const { return CLASS_PfaffianComputation; }

  const Ring * get_ring () const { return R; }
};

#endif
