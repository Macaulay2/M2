// Copyright 2005  Michael E. Stillman

#ifndef _dmat_LU_hpp_
#define _dmat_LU_hpp_

#include "engine.h"
#include "dmat.hpp"

template <typename CoeffRing>
class DMatLU
{
  typedef typename CoeffRing::elem elem;
public:
  static void LU_step(DMat<CoeffRing> *A, // col-th column is modified
		      M2_arrayint perm, // modified, should be of length >= A->n_rows()
		      M2_arrayint pivotcols, // modified, of length >= A->n_rows()
		      int &n_pivot_rows, // how many have already been set
		      int col
		      );
  
  static M2_arrayint LU(DMat<CoeffRing> *A); // modifies A

  static bool solve(DMat<CoeffRing> *A, // in LU format
		    M2_arrayint perm, // modified, should be of length >= A->n_rows()
		    M2_arrayint pivotcols, // modified, of length >= A->n_rows()
		    int &n_pivot_rows, // how many have already been set
		    DMat<CoeffRing> *b); // replaces b with the solution
  // returns true iff every column of b has a solution

  static void kernel(DMat<CoeffRing> *A, // in LU format
		    M2_arrayint perm, // modified, should be of length >= A->n_rows()
		    M2_arrayint pivotcols, // modified, of length >= A->n_rows()
		    int &n_pivot_rows, // how many have already been set
		    DMat<CoeffRing> *result); // replaces this with a matrix whose
                                              // columns form the kernel of the original A.

  // What about: rank, determinant, obtaining P, L, U as matrices?
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
