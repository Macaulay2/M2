// Copyright 2005  Michael E. Stillman

#ifndef _dmat_LU_hpp_
#define _dmat_LU_hpp_

#include "engine.h"
#include "dmat.hpp"

template <typename CoeffRing>
class DMatLU
{
  typedef typename CoeffRing::elem elem;

  static void set_pivot_info(DMat<CoeffRing> *A,
		      int ncols, // columns 0..ncols-1 are considered
		      M2_arrayint &pivotcols,
		      int &n_pivots);

  static M2_arrayint encode_permutation(M2_arrayint permutation);

  static M2_arrayint decode_permutation(M2_arrayint perm);

  static void LU1(DMat<CoeffRing> *A, // col-th column is modified
		      M2_arrayint perm, // modified, should be of length >= A->n_rows()
		      M2_arrayint pivotcols, // modified, of length >= A->n_rows()
		      int &n_pivot_rows, // how many have already been set
		      int col
		      );

  static bool solve1(DMat<CoeffRing> *A, // in LU format
		    M2_arrayint perm, // modified, should be of length >= A->n_rows()
		    M2_arrayint pivotcols, // modified, of length >= A->n_rows()
		    int &n_pivot_rows, // how many have already been set
		    DMat<CoeffRing> *b); // replaces b with the solution
  // returns true iff every column of b has a solution

  static void kernel1(DMat<CoeffRing> *A, // in LU format
		    M2_arrayint perm, // modified, should be of length >= A->n_rows()
		    M2_arrayint pivotcols, // modified, of length >= A->n_rows()
		    int &n_pivot_rows, // how many have already been set
		    DMat<CoeffRing> *result); // replaces this with a matrix whose
                                              // columns form the kernel of the original A.
public:
  static M2_arrayint LU(DMat<CoeffRing> *A); // modifies A

  static M2_arrayint LU_one_step(DMat<CoeffRing> *LU, // modifies last col of LU,
				                      // might also swap rows
				 M2_arrayint permutation,
				 int this_col);

  static bool solve(DMat<CoeffRing> *LU, // in LU format
		    M2_arrayint permutation, // should be of length >= A->n_rows()
		    DMat<CoeffRing> *b); // replaces b with the solution
  // returns true iff every column of b has a solution

  static void kernel(DMat<CoeffRing> *LU, // in LU format
		    M2_arrayint permutation, // should be of length >= A->n_rows()
		    DMat<CoeffRing> *result); // replaces this with a matrix whose
                                              // columns form the kernel of the original A.

  static int rank(DMat<CoeffRing> *LU);

  static elem determinant(DMat<CoeffRing> *LU);
  // What about: rank, determinant, obtaining P, L, U as matrices?
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
