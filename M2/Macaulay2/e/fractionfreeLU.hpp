// Copyright 1997  Michael E. Stillman

#ifndef _fractionfreeLU_hpp_
#define _fractionfreeLU_hpp_

/**
 * @file fractionfreeLU.hpp
 * @brief `FF_LUComputation` --- Bareiss-style fraction-free LU over an integral domain.
 *
 * Declares `FF_LUComputation`, a private-constructor class that
 * runs Bareiss's algorithm on a `MutableMatrix` whose base ring
 * `R` is asserted to be an integral domain. The Bareiss update
 * `M[i, j] := (M[i, j] M[k, k] - M[i, k] M[k, j]) / lastpivot`
 * is guaranteed to produce an exact ring element (with no
 * remainder) whenever `R` is a domain, so every intermediate
 * value stays in `R` without a detour through the fraction field.
 * For matrices with polynomial coefficients this can be orders of
 * magnitude faster than ordinary LU plus fraction simplification.
 *
 * The class tracks the column permutation it applied
 * (`col_perm`), the previous pivot (`lastpivot`, used as the
 * Bareiss denominator), the current pivot, and a per-step
 * `need_div` flag indicating whether the division should run (the
 * first step does not). After completion `M` carries the LU
 * factors packed in the standard compact form (strictly-below for
 * `L`, on-and-above for `U`) and the determinant is the final
 * pivot up to the sign of the column permutation. M2 reaches this
 * path via `LUdecomposition` over polynomial coefficient rings
 * and via the `DET_BAREISS` strategy in `det.hpp`.
 *
 * @see det.hpp
 * @see mat.hpp
 */

#include "mat.hpp"

/**
 * @brief LU decomposition over a domain using fraction-free Gaussian
 * elimination.
 *
 * @details The classical Bareiss-style algorithm: at each step, the new
 * entry is `(pivot * a - factor * b) / lastpivot`, where the
 * division is exact because the ring is a domain. Tracks
 * `col_perm` for the column permutation and `need_div[i]` for
 * which columns still need the trailing division. Used by the
 * engine wherever an integer-coefficient LU is needed without
 * introducing fractions.
 */
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
