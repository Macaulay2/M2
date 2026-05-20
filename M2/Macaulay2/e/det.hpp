// Copyright 1996 by Michael E. Stillman.

#ifndef _det_hh_
#  define _det_hh_

/**
 * @file det.hpp
 * @brief Determinants and minors of `Matrix` values via Bareiss, cofactor, or dynamic-programming strategies.
 *
 * Declares `DetComputation` and the three integer strategy
 * constants the caller selects between: `DET_BAREISS`,
 * `DET_COFACTOR`, and `DET_DYNAMIC`. Bareiss is the fraction-free
 * Gaussian variant --- requires the coefficient ring to be a
 * domain (the in-source comment annotates it "fraction free,
 * DOMAINS only") and keeps the intermediate pivots integral by
 * carrying the previous pivot through the divisions. Cofactor
 * is a straight recursive Laplace expansion along the last row
 * of each submatrix, skipping zero entries; it is the natural
 * fallback over rings where Bareiss division does not apply
 * (inexact fields, quotient rings). Dynamic is the memoised
 * variant: it caches submatrix minors in nested `std::map`s
 * keyed by row/column index sets so that minor-list queries of
 * many sizes share work. The user-facing `det` / `minors`
 * functions defined in `m2/multilin.m2` map their `Strategy =>`
 * option onto these three constants.
 *
 * Entry points operate on a `Matrix` and return either a
 * `RingElement` (the determinant of a square submatrix) or a
 * `Matrix` collecting minors (or an exterior power when the
 * `do_exterior` flag is set).
 *
 * @see matrix.hpp
 * @see matrix-con.hpp
 */

#  include "matrix.hpp"
#  include "matrix-con.hpp"
#  include <utility>
#  include <vector>
#  include <map>
#  include <algorithm>

const int DET_BAREISS = 0;
const int DET_COFACTOR = 1;
const int DET_DYNAMIC = 2;

/**
    @ingroup comp

    @brief Computation of minors of a matrix
*/
class DetComputation : public our_new_delete
{
  const Ring *R;
  const Matrix *M;
  const FreeModule *F;  // target free module of the result
  //  Matrix *result;  // Either:One row matrix collecting non-zero
  // determinants, or the resulting
  // exterior power; depending on 'do_exterior'
  MatrixConstructor result;  // Either:One row matrix collecting non-zero
                             // determinants, or the resulting
                             // exterior power; depending on 'do_exterior'

  bool done;
  int p;

  bool do_exterior;  // true = construct exterior
                     // power of matrix, false =
                     // collect non-zero minors
  int strategy;      // 0: use Bareiss (fraction free, DOMAINS only)
                     // 1: use cofactor method.
                     // 2: use dynamic method (cache subcomputations)
  size_t *row_set;
  size_t *col_set;
  int this_row;
  int this_col;

  ring_elem **D;  // size p by p, dense representation.

  // Dynamic method, vector of maps
  using ColRowIndices = std::pair<std::vector<int>, std::vector<int>>;
  using Subdeterminant =
      std::map<ColRowIndices,
               ring_elem,
               std::less<ColRowIndices>,
               gc_allocator<std::pair<const ColRowIndices, ring_elem>>>;
  using MinorsSubCache =
      std::map<int,
               Subdeterminant,
               std::less<int>,
               gc_allocator<std::pair<const int, Subdeterminant>>>;
  using MinorsCache = std::vector<MinorsSubCache, gc_allocator<MinorsSubCache>>;
  // The entry dynamic_cache[i][j][{r, c}] is the determinant of the submatrix
  // corresponding to rows r and columns c, given as vectors of ints.
  // The sizes of r and c are i, and the first entry of r is the jth nonzero
  // row. The jth nonzero row is also the row given by row_lookup[j].
  MinorsCache dynamic_cache;
  std::map<int, int> row_lookup;

  void get_minor(size_t *r, size_t *c, int p, ring_elem **D);
  // Sets D[0..p-1,0..p-1] with the given minor of M.

  // Used in Dynamic:
  int make_dynamic_cache();

  // Used in Bareiss:
  bool get_pivot(ring_elem **D, size_t p, ring_elem &pivot, size_t &pivot_col);
  ring_elem detmult(ring_elem f1,
                    ring_elem g1,
                    ring_elem f2,
                    ring_elem g2,
                    ring_elem d);
  void gauss(ring_elem **D,
             size_t i,
             size_t r,
             size_t pivot_col,
             ring_elem lastpivot);

  ring_elem calc_det(size_t *r, size_t *c, int p);
  // Compute the determinant of the minor with rows r[0]..r[p-1]
  // and columns c[0]..c[p-1].

  ring_elem bareiss_det();
  // Compute the determinant of the minor with rows r[0]..r[p-1]
  // and columns c[0]..c[p-1].

  // Subroutines for use in Bareiss algorithm:

 public:
  DetComputation(const Matrix *M, int p, bool do_exterior, int strategy);
  ~DetComputation();

  int step();
  int calc(int nsteps);

  // The following two routines are only valid for 'do_exterior=0'
  void clear();
  void discard() { clear(); }
  void set_next_minor(const int *rows, const int *cols);

  Matrix *determinants() { return result.to_matrix(); }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
