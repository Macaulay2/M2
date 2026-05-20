// Copyright 1996 Michael E. Stillman.

#ifndef _pfaff_hh_
#define _pfaff_hh_

/**
 * @file pfaff.hpp
 * @brief `PfaffianComputation` --- Pfaffians of skew-symmetric matrices via row expansion.
 *
 * Declares `PfaffianComputation`, the engine routine that
 * returns `Pf(A)` for a square skew-symmetric `A`. The Pfaffian
 * satisfies `Pf(A)^2 = det(A)` but has half the polynomial
 * degree in the entries, so for skew-symmetric inputs it is the
 * natural and cheaper invariant. The class stores the ambient
 * ring `R`, the input matrix `M` (skew-symmetry is asserted by
 * the caller --- not verified), a `size_t* row_set` scratch
 * for the current surviving index set, and an output
 * `MatrixConstructor pfaffs` that collects the non-zero
 * Pfaffians as one-row entries; `calc(nsteps)` drives the
 * iteration via `step()`, and `pfaffians()` materialises the
 * accumulated row matrix.
 *
 * Computation runs the recursive Laplace-style expansion
 * `Pf(A) = sum_j (-1)^{j+1} A[1, j] Pf(A_{1, j})` (with
 * `A_{1, j}` the `(2n-2) x (2n-2)` matrix obtained by deleting
 * rows / columns 1 and `j`), bottoming out at the `2 x 2` base
 * case `Pf([[0, a], [-a, 0]]) = a`. No memoisation cache ---
 * unlike `det.hpp`'s `DET_DYNAMIC` strategy, `calc_pfaff`
 * recomputes each submatrix Pfaffian afresh.
 *
 * @see matrix.hpp
 * @see matrix-con.hpp
 * @see det.hpp
 */

#include "matrix.hpp"
#include "matrix-con.hpp"

class MatrixConstructor;

/**
    @ingroup comp

    @brief Computation of pfaffians of a skew symmetric matrix.
*/

class PfaffianComputation : public our_new_delete
{
  const Ring *R;
  const Matrix *M;
  MatrixConstructor pfaffs;
  // One row matrix collecting non-zero pfaffians

  int p;
  bool done;
  size_t *row_set;

  ring_elem calc_pfaff(size_t *r, int p);
  // Compute the pfaffian of the minor with rows
  // and columns r[0]..r[p-1]

  int step();
  // Compute one more pfaffian of size p.

 public:
  PfaffianComputation(const Matrix *M, int p);
  ~PfaffianComputation();

  int calc(int nsteps = -1);
  // Compute nsteps pfaffians.  Only non-negative ones are appended
  // -1 means compute all of them.

  Matrix *pfaffians() { return pfaffs.to_matrix(); }
  const Ring *get_ring() const { return R; }
  ring_elem calc_pfaff(void);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
