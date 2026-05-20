// Copyright 1997  Michael E. Stillman

#ifndef _fractionfreeLU_hpp_
#define _fractionfreeLU_hpp_

#include "mat.hpp"

class FF_LUComputation
{
  // This is a class encapsulating the LU decomposition
  // over a domain, using fraction free Gaussian elimination.

  const Ring *R;  // R should be a domain.
  MutableMatrix *M;
  int *col_perm;
  bool *need_div;
  int pivot_col;
  ring_elem lastpivot;
  ring_elem pivot;

 private:
  FF_LUComputation(MutableMatrix *M);
  ~FF_LUComputation();

  bool choose_pivot_column(int lo, int hi, int &result);
  // Chooses a pivot in the column range lo..hi, among those with
  // the highest index row.  Returns true if there is a non-zero
  // column in the range lo..hi, and sets 'result' in this case.
  // If all such columns are zero, returns false.

  void do_pivots(int lo, int hi, int pivot_col);
  // Use the lead element (pivot, in row r, in pivot_col to clear all
  // elements in row r in columns lo..hi.  This uses fraction-free
  // methods, and uses 'need_div' to determine whether division
  // by the previous pivot should be done.  It also sets 'need_div'
  // for the next time.

  bool calc();
  // Returns true if the computation completed.  False if it was
  // user interrupted.

  M2_arrayint get_column_permutation();

 public:
  static M2_arrayintOrNull DO(MutableMatrix *M);
  // Replace M with its column echelon form.  If M has
  // column recording going on, then one obtains the whole
  // LU decomposition.
  // If the computation was interrupted, or M is in a ring which is found to not
  // be a domain (e.g. a non-commutative ring), then NULL is returned, and M is
  // left in an intermediate state.
  // Otherwise, M is modified, and the column permutation needed
  // to obtain the resulting M is returned.
};

#endif
