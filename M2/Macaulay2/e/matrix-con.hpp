#ifndef matrixcon_hpp_
#define matrixcon_hpp_

/**
 * @file matrix-con.hpp
 * @brief `MatrixConstructor` --- the mutable builder that produces an immutable `Matrix`.
 *
 * Declares `MatrixConstructor`, the builder side of the matrix
 * API (one of `Matrix`'s two `friend` declarations, alongside
 * `FreeModule`). It holds the ring, the target and (optionally)
 * source `FreeModule*`s, and a working `VECTOR(vec)` of columns,
 * accumulating state with `set_entry` / `set_column` / `append`
 * before handing back a finished immutable `Matrix*` via
 * `to_matrix()`. The three constructors mirror the two
 * construction modes: `MatrixConstructor(target, ncols)` fixes
 * the column count up front, `MatrixConstructor(target, source,
 * deg)` fixes the source free module too, and the default
 * constructor leaves both unset. The `cols_frozen` flag enforces
 * the distinction --- once `cols` is supplied or the builder
 * commits, no further modifications that would change the
 * column count are allowed.
 *
 * Degree bookkeeping rides along: `set_column_degree`,
 * `set_matrix_degree`, and `compute_column_degrees` set the
 * per-column and overall matrix degrees so the produced
 * `Matrix` carries the same degree data a hand-built one would.
 * `matrix-con.hpp` exists because `Matrix`'s constructors are
 * private and intentionally immutable; this scratch surface is
 * the only sanctioned way to assemble one.
 *
 * @see matrix.hpp
 * @see freemod.hpp
 */

#include <vector>
#include "ring.hpp"
#include <utility>

class MatrixGenerator;

/**
 * @brief Mutable builder used to assemble an immutable `Matrix` one column
 * (or one term) at a time.
 *
 * @details Held while the matrix is in flux: tracks the target / source
 * `FreeModule`s, the in-progress column vectors in `entries`, a
 * shared degree shift `deg`, and the `cols_frozen` flag that
 * locks the source free module once the caller commits to a
 * column count. `to_matrix()` (declared further down) snapshots
 * the current state into a fresh `Matrix*`, after which the
 * `MatrixConstructor` can be discarded or refilled.
 *
 * @ingroup matrices
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

  const_monomial deg;

  void compute_column_degree(int i);

 public:
  MatrixConstructor();
  MatrixConstructor(const FreeModule *target, int ncols);
  MatrixConstructor(const FreeModule *target,
                    const FreeModule *source,
                    const_monomial deg = nullptr);

  // The copy constructor just does the default thing: copy over all items.

  void append(vec v);
  void append(vec v, const_monomial deg);

  void set_entry(int r, int c, ring_elem a);
  void set_column(int c, vec v);

  void compute_column_degrees(); /* Takes into account the matrix degree */

  void set_column_degree(int i, const_monomial deg);

  void set_matrix_degree(const_monomial deg);

  Matrix *to_matrix();

  void debugDisplay() const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
