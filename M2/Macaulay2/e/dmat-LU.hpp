// Copyright 2005  Michael E. Stillman

#ifndef _dmat_LU_hpp_
#define _dmat_LU_hpp_

#include "engine.h"
#include "dmat.hpp"

template <typename CoeffRing>
class DMatLU
{
  typedef typename CoeffRing::elem elem;

  static void set_pivot_info(const DMat<CoeffRing> *A,
		      int ncols, // columns 0..ncols-1 are considered
		      M2_arrayint &pivotcols,
		      int &n_pivots);

  static void solveF(const DMat<CoeffRing> *L, // m by m
	      M2_arrayint perm, // 0..m-1
	      const elem *b, // length m
	      elem *y); // length m

  static void solveB(const DMat<CoeffRing> *U, // m by n
		     M2_arrayint pivotcols,
		     int npivots,
		     const elem *y, // length m
		     elem *x); // length n

  static bool LU1(const DMat<CoeffRing> *A, // col-th column is modified
	   DMat<CoeffRing> *L,
	   DMat<CoeffRing> *U,
	   M2_arrayint perm, // modified, should be of length >= A->n_rows()
	   int &n_pivot_rows, // how many have already been set
	   int col
	   );

  static bool solve(const DMat<CoeffRing> *L, 
		    const DMat<CoeffRing> *U,
		    M2_arrayint permutation, // should be of length  L->n_rows()
		    const DMat<CoeffRing> *b,
		    DMat<CoeffRing> *x); // x is replaced with a matrix with same #cols as b, same
                                         // number of columns as U
  // returns true iff every column of b has a solution

  static bool kernel(const DMat<CoeffRing> *L, 
		     const DMat<CoeffRing> *U,
		     M2_arrayint permutation, // should be of length  L->n_rows()
		     DMat<CoeffRing> *x); // x is replaced with a matrix with the same
                                         // number of columns as U

  static int rank(const DMat<CoeffRing> *U); // don't need L or permutation

  static elem determinant(const DMat<CoeffRing> *L, // not needed 
			  const DMat<CoeffRing> *U,
			  M2_arrayint permutation);
  // What about: rank, determinant, obtaining P, L, U as matrices?

  static int rank(DMat<CoeffRing> *U);

public:
  static M2_arrayint LU(const DMat<CoeffRing> *A,
		 DMat<CoeffRing> *&L,
		 DMat<CoeffRing> *&U
		 );

  static bool solve(const DMat<CoeffRing> *A,
		    const DMat<CoeffRing> *b,
		    DMat<CoeffRing> *x); // resulting solution set
  // returns true iff every column of b has a solution

  static void nullspaceU(const DMat<CoeffRing> *U,
			 DMat<CoeffRing> *x); // resulting kernel




#if 0
// public:
//   // START CODE TO BE REMOVED //
// 
//   static M2_arrayint encode_permutation(M2_arrayint permutation);
// 
//   static M2_arrayint decode_permutation(M2_arrayint perm);
// 
//   static void LU1(DMat<CoeffRing> *A, // col-th column is modified
// 		      M2_arrayint perm, // modified, should be of length >= A->n_rows()
// 		      M2_arrayint pivotcols, // modified, of length >= A->n_rows()
// 		      int &n_pivot_rows, // how many have already been set
// 		      int col
// 		      );
// 
//   static bool solve1(DMat<CoeffRing> *A, // in LU format
// 		    M2_arrayint perm, // modified, should be of length >= A->n_rows()
// 		    M2_arrayint pivotcols, // modified, of length >= A->n_rows()
// 		    int &n_pivot_rows, // how many have already been set
// 		    DMat<CoeffRing> *b); // replaces b with the solution
//   // returns true iff every column of b has a solution
// 
//   static void kernel1(DMat<CoeffRing> *A, // in LU format
// 		    M2_arrayint perm, // modified, should be of length >= A->n_rows()
// 		    M2_arrayint pivotcols, // modified, of length >= A->n_rows()
// 		    int &n_pivot_rows, // how many have already been set
// 		    DMat<CoeffRing> *result); // replaces this with a matrix whose
//                                               // columns form the kernel of the original A.
// public:
//   static M2_arrayint LU(DMat<CoeffRing> *A); // modifies A
// 
//   static M2_arrayint LU_one_step(DMat<CoeffRing> *LU, // modifies last col of LU,
// 				                      // might also swap rows
// 				 M2_arrayint permutation,
// 				 int this_col);
// 
//   static bool solve(DMat<CoeffRing> *LU, // in LU format
// 		    M2_arrayint permutation, // should be of length >= A->n_rows()
// 		    DMat<CoeffRing> *b); // replaces b with the solution
//   // returns true iff every column of b has a solution
// 
//   static void kernel(DMat<CoeffRing> *LU, // in LU format
// 		    M2_arrayint permutation, // should be of length >= A->n_rows()
// 		    DMat<CoeffRing> *result); // replaces this with a matrix whose
//                                               // columns form the kernel of the original A.
// 
//   static elem determinant(DMat<CoeffRing> *LU);
//   // What about: rank, determinant, obtaining P, L, U as matrices?
#endif
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
