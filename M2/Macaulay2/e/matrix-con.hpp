#ifndef matrixcon_hpp_
#define matrixcon_hpp_

#include <vector>
#include "ring.hpp"
#include <utility>

class MatrixGenerator;

/**
 * \ingroup matrices
 */
class MatrixConstructor
{
  const Ring *R;
  VECTOR(vec) entries;
  const FreeModule *rows;
  const FreeModule *cols;  // If cols is given at the beginning, this is used.
  // If this is immutable, no changes are allowed, other than to replace the
  // entire thing.

  bool cols_frozen;  // Once this is set, no more modifications to the 'cols'
                     // are allowed.  In particular, if the 'source' is set at
  // the beginning via the constructor, and that free module is
  // immutable, then no more changes are allowed.

  const int *deg;

  void compute_column_degree(int i);

 public:
  MatrixConstructor();
  MatrixConstructor(const FreeModule *target, int ncols);
  MatrixConstructor(const FreeModule *target,
                    const FreeModule *source,
                    const int *deg = 0);

  // The copy constructor just does the default thing: copy over all items.

  void append(vec v);
  void append(vec v, const int *deg);

  void set_entry(int r, int c, ring_elem a);
  void set_column(int c, vec v);

  void compute_column_degrees(); /* Takes into acount the matrix degree */

  void set_column_degree(int i, const int *deg);

  void set_matrix_degree(const int *deg);

  Matrix *to_matrix();

  void debugDisplay() const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
