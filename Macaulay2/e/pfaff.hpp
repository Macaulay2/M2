// Copyright 1996 Michael E. Stillman.

#ifndef _pfaff_hh_
#define _pfaff_hh_

#include "matrix.hpp"
#include "comb.hpp"
#include "matrix-con.hpp"

class MatrixConstructor;

class PfaffianComputation : public our_new_delete
{
  const Ring *R;
  const Matrix *M;
  MatrixConstructor pfaffs;
  //  Matrix *pfaffs;		// One row matrix collecting non-zero 
				// pfaffians.
  int p;

  int I;			// row_set encoded
  intarray row_set;
  int endI;

  ring_elem calc_pfaff(int *r, int p);
     // Compute the pfaffian of the minor with rows r[0]..r[p-1]
     // and columns c[0]..c[p-1].
public:
  PfaffianComputation(const Matrix *M, int p);
  ~PfaffianComputation();

  int step();	
	// Compute one more pfaffian of size p.
	// increments I and/or J and updates 'pfaffs', 'table'.
  int calc(int nsteps);
  
  Matrix *pfaffians() { return pfaffs.to_matrix(); }

  const Ring * get_ring () const { return R; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
