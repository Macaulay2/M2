// Copyright 1996 Michael E. Stillman.

#ifndef _pfaff_hh_
#define _pfaff_hh_

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
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
