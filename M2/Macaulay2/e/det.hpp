// Copyright 1996 by Michael E. Stillman.

#ifndef _det_hh_
#define _det_hh_

#include "matrix.hpp"
#include "matrix-con.hpp"
#include <utility>
#include <vector>
#include <map>
#include <algorithm>

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
  using Subdeterminant = std::map<ColRowIndices, ring_elem, std::less<ColRowIndices>, gc_allocator<std::pair<const ColRowIndices, ring_elem>>>;
  using MinorsSubCache = std::map<int, Subdeterminant, std::less<int>, gc_allocator<std::pair<const int, Subdeterminant>>>;
  using MinorsCache = std::vector<MinorsSubCache, gc_allocator<MinorsSubCache>>;
  // The entry dynamic_cache[i][j][{r, c}] is the determinant of the submatrix
  // corresponding to rows r and columns c, given as vectors of ints.
  // The sizes of r and c are i, and the first entry of r is the jth nonzero row.
  // The jth nonzero row is also the row given by row_lookup[j].
  MinorsCache dynamic_cache;
  std::map<int, int> row_lookup;


  void get_minor(size_t *r, size_t *c, int p, ring_elem **D);
  // Sets D[0..p-1,0..p-1] with the given minor of M.

  // Used in Dynamic:
  void make_dynamic_cache();

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
