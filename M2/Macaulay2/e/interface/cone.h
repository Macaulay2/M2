#ifndef M2_INTERFACE_CONE_H_
#define M2_INTERFACE_CONE_H_

#  include "engine-includes.hpp"
#include "m2-types.h"
// TODO: fix this
#  if defined(__cplusplus)
  class Matrix;
#  else
  typedef struct Matrix Matrix;
#  endif

/**
 * \defgroup cones Cones
 * \brief Cone interface routines (most backed by libnormaliz).
 *
 * Macaulay2's sign convention for inequality matrices is uniformly
 * `A * x <= 0` (or `A * x <= b` in the inhomogeneous case).
 * Implementations negate as needed to adapt to the conventions used by
 * libnormaliz (`A * x >= 0`) and the internal `box_enum` helper
 * (`H * x >= rhs`).
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

/**************************************************/
/**** Cone routines (via Normaliz) ****************/
/**************************************************/

/** \brief Extreme rays of a polyhedral cone given by inequalities, via libnormaliz.
 *
 * Computes the extreme rays of the cone `{ x in QQ^c : C * x <= 0 }`
 * defined by linear inequalities. The implementation negates `C` on the
 * way in because libnormaliz uses the opposite convention `A * x >= 0`.
 *
 * \warning The cone is assumed to be pointed (that is, `ker(C) = 0`).
 *          Only extreme rays are returned, so if `C` has a non-trivial
 *          kernel the cone has a non-trivial lineality space and the
 *          positive hull of the returned rays is a *proper* subset of
 *          `{x : C x <= 0}`. Basis vectors for `ker(C)` (and their
 *          negatives) are not emitted.
 *
 * \warning The row/column convention here is the **transpose** of the
 *          pure-M2 `FourierMotzkin` package: that package takes hyperplanes
 *          as **columns** and returns extremal rays as **columns**. This
 *          engine routine takes hyperplanes as **rows** and returns
 *          extremal rays as **rows**. The convention used here may change
 *          in the future to align with the package.
 *
 * \param C An r-by-c matrix over ZZ. Each row is one inequality
 *          `C(i,*) * x <= 0`; columns index the c-dimensional ambient
 *          lattice.
 * \return An n-by-c matrix over ZZ whose **rows** are the extremal rays
 *         of the cone, or `nullptr` on engine error.
 *
 * \par Example (M2 top level)
 * \code{.unparsed}
 *   A = matrix {{1,1,1}, {-1,1,0}, {-1,0,1}, {1,2,3}}
 *   map(ZZ, rawFourierMotzkin raw A)
 *     -- matrix {{-1, -1, -1}, {1, -2, 1}, {1, 1, -2}}
 * \endcode
 * The three rows of the result are the extremal rays of the cone
 * `{x in QQ^3 : A x <= 0}`.
 *
 * \ingroup cones
 */
const Matrix /* or null */ *rawFourierMotzkin(const Matrix *C);

/** \brief Hilbert basis of a polyhedral cone given by its rays, via libnormaliz.
 *
 * Computes the Hilbert basis of the cone `pos(rows of C)` in `QQ^c`,
 * i.e. the unique minimal generating set of the additive monoid
 * `pos(rows of C) intersect ZZ^c`. The cone is presented in
 * V-representation (rays as input rows), the dual to the H-representation
 * accepted by `rawFourierMotzkin`.
 *
 * \warning The cone is assumed to be pointed. For non-pointed cones the
 *          Hilbert basis is not well-defined in the usual minimal-set
 *          sense; libnormaliz will still return generators, but their
 *          interpretation differs.
 *
 * \warning The intended M2 engine convention is **hyperplanes as rows**
 *          and **rays/points as columns**. This routine pre-dates that
 *          convention and uses **rows for both** input rays and output
 *          basis elements. The signature may be transposed in the future
 *          to match the convention; for now the row-based layout is used.
 *
 * \param C An r-by-c matrix over ZZ. Each row is one cone generator
 *          (ray); columns index the c-dimensional ambient lattice.
 * \return An n-by-c matrix over ZZ whose **rows** are the Hilbert basis
 *         elements, or `nullptr` on engine error.
 *
 * \todo Validate that `C` is over ZZ (currently assumed).
 * \todo Lift cones over QQ to ZZ before passing to libnormaliz.
 * \todo Expose libnormaliz's support for cones over algebraic number
 *       fields embedded in RR.
 *
 * \par Example (M2 top level)
 * \code{.unparsed}
 *   debug Core
 *   C = matrix {{1,0}, {1,2}}
 *   map(ZZ, rawHilbertBasis raw C)
 *     -- | 1 0 |
 *     -- | 1 1 |
 *     -- | 1 2 |
 * \endcode
 * The cone in `ZZ^2` spanned by the rays `(1,0)` and `(1,2)` has
 * Hilbert basis `{(1,0), (1,1), (1,2)}`; the middle element `(1,1)`
 * is the non-trivial lattice point in the half-open parallelepiped
 * spanned by the two primitive rays.
 *
 * \sa rawFourierMotzkin (H-representation counterpart)
 * \ingroup cones
 */
const Matrix /* or null */ *rawHilbertBasis(const Matrix *C);

MutableMatrix /* or null */ *rawGVInvariants(M2_arrayint a,
                                            M2_arrayint b,
                                            M2_arrayint c,
                                            M2_arrayint d,
                                            M2_arrayint e,
                                            M2_arrayint f,
                                            M2_arrayint g);

/** \brief Enumerate lattice points in a bounded box of a polyhedron (int-precision).
 *
 * Enumerates every integer vector `x` in `ZZ^d` (where `d = n_cols(A)`)
 * satisfying both
 *   - `A * x <= b`  componentwise, and
 *   - `|x_i| <= B`  for every coordinate `i`.
 *
 * Backed by the cytools `box_enum.h`, written by Nate MacFadden,, which works in C `int`
 * arithmetic. The implementation negates `A` and `b` to convert M2's
 * `A x <= b` into `box_enum`'s `H x >= rhs` convention.
 *
 * \note This routine follows the intended M2 engine convention:
 *       **hyperplanes as rows** of `A`, **lattice points as columns**
 *       of the result.
 *
 * \param A           An m-by-d matrix over ZZ; each row is one
 *                    inequality. All entries must fit in a C `int`.
 * \param b           A column matrix over ZZ with `m` rows and 1 column.
 *                    All entries must fit in a C `int`.
 * \param B           Per-coordinate absolute-value bound on `x`.
 * \param max_N_out   **Soft** cap on the number of returned points: if
 *                    reached, the enumeration stops cleanly and returns
 *                    the partial list. Callers should compare the
 *                    returned column count against this bound to detect
 *                    truncation.
 * \param max_N_nodes **Hard** cap on the number of search-tree nodes
 *                    explored: if exceeded, an error is reported (the
 *                    routine returns `nullptr`) rather than returning
 *                    a partial result, since otherwise the caller cannot
 *                    distinguish a complete enumeration from one cut
 *                    short.
 * \return A dense MutableMatrix over ZZ with `d` rows and one column
 *         per enumerated lattice point, or `nullptr` on error (bad
 *         input shape, ring mismatch, entries that overflow C `int`,
 *         or `max_N_nodes` exceeded).
 *
 * \par Example (M2 top level)
 * \code{.unparsed}
 *   debug Core
 *   A = matrix {{1,1,1}}
 *   b = matrix {{-2}}
 *   map(ZZ, rawLatticePoints(raw A, raw b, 1, 100, 1000))
 *     -- | -1 0  -1 -1 |
 *     -- | -1 -1 0  -1 |
 *     -- | -1 -1 -1 0  |
 * \endcode
 * Enumerates the integer points `x` in `ZZ^3` with `x_1+x_2+x_3 <= -2`
 * and `|x_i| <= 1`. There are exactly four such points:
 * `(-1,-1,-1), (0,-1,-1), (-1,0,-1), (-1,-1,0)`. The output has 3 rows
 * (the ambient dimension) and 4 columns (one per point).
 *
 * \sa rawLatticePointsNormaliz — libnormaliz-backed counterpart with
 *     big-integer support and no box bound, but requires a bounded
 *     polyhedron.
 * \ingroup cones
 */
MutableMatrix /* or null */ *rawLatticePoints(const Matrix *A,
                                              const Matrix *b,
                                              int B,
                                              long max_N_out,
                                              long max_N_nodes);

/** \brief Enumerate all lattice points of a bounded polyhedron, via libnormaliz.
 *
 * Same user-facing convention as `rawLatticePoints` (enumerate every
 * integer `x` in `ZZ^d` with `A * x <= b`), but with no box bound and
 * no caps: every lattice point of the polyhedron is returned. The
 * polyhedron must be bounded; an unbounded input is reported as an
 * error. Big-integer entries in `A` and `b` are fully supported (no
 * fits-in-int restriction).
 *
 * \note This routine follows the intended M2 engine convention:
 *       **hyperplanes as rows** of `A`, **lattice points as columns**
 *       of the result.
 *
 * \param A An m-by-d matrix over ZZ; each row is one inequality.
 * \param b A column matrix over ZZ with `m` rows and 1 column.
 * \return A dense MutableMatrix over ZZ with `d` rows and one column
 *         per lattice point, or `nullptr` on error (ring mismatch,
 *         bad shape, or unbounded polyhedron).
 *
 * \par Example (M2 top level)
 * \code{.unparsed}
 *   debug Core
 *   A = matrix {{1,0},{0,1},{-1,0},{0,-1}}    -- unit square: 0 <= x,y <= 1
 *   b = matrix {{1},{1},{0},{0}}
 *   map(ZZ, rawLatticePointsNormaliz(raw A, raw b))
 *     -- | 0 0 1 1 |
 *     -- | 0 1 0 1 |
 * \endcode
 * The polyhedron is bounded by its inequalities alone (no `B` is
 * needed, unlike `rawLatticePoints`). The four columns are the four
 * lattice points `(0,0), (0,1), (1,0), (1,1)` of the unit square.
 *
 * \sa rawLatticePoints — cytools `box_enum`-backed counterpart that requires
 *     a per-coordinate box bound `B` and uses `int` arithmetic, but
 *     can enumerate inside an unbounded polyhedron clipped by the box.
 * \ingroup cones
 */
MutableMatrix /* or null */ *rawLatticePointsNormaliz(const Matrix *A,
                                                      const Matrix *b);


MutableMatrix /* or null */ *rawConeInteriorPoint(const Matrix *A);
  
#  if defined(__cplusplus)
}
#  endif

#endif /* _cone_h_ */
