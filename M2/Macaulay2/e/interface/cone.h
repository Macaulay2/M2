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
   * Different software has different conventions for how a matrix represents a
   * cone.  Each routine here mentions its own conventions, and also the
   * conventions of the software (e.g. normaliz) it calls.
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
 * \param C An $r \times c$ matrix over ZZ.
 * \return An $n \times c$ matrix over ZZ whose **rows** are the extremal rays
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

/** \brief Hilbert basis of a polyhedral cone given by its rays, via
libnormaliz.
 *
 * Computes the Hilbert basis of the cone `pos(rows of C)` in `QQ^c`,
 * i.e. the unique minimal generating set of the additive monoid
 * `posHull(rows of C) intersect ZZ^c`.
 *
 * \warning The cone is assumed to be pointed. For non-pointed cones the
 *          Hilbert basis is not well-defined in the usual minimal-set
 *          sense; libnormaliz will still return generators, but their
 *          interpretation differs.
 *
 * \param C An $r \times c$ integer matrix$.
 *         The cone is the positive hull of the rows of this matrix, in
 *         $\mathbb{Q}^c$.
 *
 * \return An $n \times c$ integer matrix whose rows are the Hilbert basis
 *         elements, or `nullptr` on engine error.
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
 * The cone in `QQ^2` spanned by the rays `(1,0)` and `(1,2)` has
 * Hilbert basis `{(1,0), (1,1), (1,2)}`.
 *
 * \ingroup cones
 */
const Matrix /* or null */ *rawHilbertBasis(const Matrix *C);

/** \brief Gopakumar-Vafa invariants of a Calabi-Yau threefold.
 *
 * Computes Gopakumar-Vafa (GV) invariants for a Calabi-Yau threefold
 * via the `gvcompute` routine ported from CYTools (see Reference
 * below for the algorithm and the precise meaning of the inputs).
 *
 * Each `M2_arrayint` argument is a flattened encoding produced on
 * the d-language side: nested-list inputs use a length-prefix-per-row
 * format, flat-list inputs are passed through.
 *
 * \param curves        Input curves: the curve classes whose GV
 *                      invariants are to be computed. Each inner
 *                      list has length `h11`.  These should include
 *                      the Hilbert basis elements, not just extremal
 *                      curve classes.
 * \param lightcone     Lightcone curves. Currently non-empty values
 *                      are not yet functional or tested; pass the
 *                      empty list.
 * \param grading       Grading vector.
 * \param Q             GLSM charge (degree) matrix.
 * \param nefPartition  Nef partition data. Currently non-empty values
 *                      are not yet functional or tested; pass the
 *                      empty list.
 * \param intnums       Triple intersection numbers.
 * \param settings      Computation settings: exactly four `int`s, in
 *                      order:
 *                      - `[0] max_deg`: maximum curve degree to
 *                        enumerate. A negative value `-k` is
 *                        reinterpreted as a minimum-points request
 *                        (`min_points = k`, `max_deg` clamped to 0).
 *                      - `[1] digits`: decimal digits of internal
 *                        MPFR precision (valid range 17..2000).
 *                      - `[2] mode`: enumeration strategy and a
 *                        GV/GW switch; see the bit layout and the
 *                        Normal / Hilbert / Verbatim modes documented
 *                        in `Macaulay2/e/computeGV.cpp`. Setting this to
 *                        non-zero value is not yet implemented.
 *                      - `[3] min_mem`: **currently ignored** — the
 *                        memory threshold is hardcoded internally
 *                        (notably on macOS); the slot is kept in the
 *                        signature for forward compatibility.
 *
 * \return A MutableMatrix over ZZ with `h11 + 1` rows, where each column
 *         corresponds to a curve class (first h11 entries in the column, and
 *         the last column contains an integer which is the GV invariant for
 *         that curve class.  Returns `nullptr` if the data is not in the
 *         correct format, or for some reason the function fails.
 *
 * \par Reference
 * M. Demirtas, M. Kim, L. McAllister, J. Moritz, A. Rios-Tascon,
 * *Computational Mirror Symmetry*, arXiv:2303.00757 (2024).
 * https://arxiv.org/abs/2303.00757
 *
 * \par Example (M2 top level)
 * \code{.unparsed}
 *   debug Core
 *
 *   -- Helper: pack a list-of-list-of-int into the flat
 *   -- length-prefix-per-row encoding rawGVInvariants expects.
 *   encodeArrayArrayInt = method()
 *   encodeArrayArrayInt List := List => L -> (
 *       parts := flatten for i from 0 to #L - 1 list prepend(#L#i, L#i);
 *       prepend(#L, parts))
 *
 *   mori       = encodeArrayArrayInt {{-1,3,-1},{0,-2,1},{1,2,-1}}
 *   lightcone  = encodeArrayArrayInt {}
 *   gradingvec = {2, 4, 9}
 *   Q          = encodeArrayArrayInt {{1,0,0,0,1,0,1},{0,1,0,1,0,1,1},
 *                                     {0,0,1,3,1,2,2}}
 *   nefpart    = encodeArrayArrayInt {}
 *   intnums    = encodeArrayArrayInt {{0,0,0,-1},{0,1,1,-2},{0,1,2,1},
 *                                     {1,1,1,8},{1,1,2,-4},{1,2,2,2},
 *                                     {2,2,2,-1}}
 *   settings   = {6, 150, 0, 300000}
 *
 *   transpose map(ZZ, rawGVInvariants(mori, lightcone, gradingvec, Q,
 *                                     nefpart, intnums, settings))
 *     -- | 1  2  -1 1     |
 *     -- | 0  -2 1  -2    |
 *     -- | 1  0  0  126   |
 *     -- | -1 1  0  1     |
 *     -- ...
 *     -- | 1  1  0  20615 |
 *     -- ...                              (19 rows total)
 * \endcode
 * Each row of the (transposed) output is one tuple
 * `[c_1, c_2, c_3, GV(c)]`: the first `h11 = 3` entries are the curve
 * coordinates, the last entry is the GV invariant for that class.
 * Only curve classes with non-zero GV invariants are returned (e.g.
 * the class `(1,1,0)` has `GV = 20615`).
 *
 * \ingroup cones
 */
MutableMatrix /* or null */ *rawGVInvariants(M2_arrayint curves,
                                             M2_arrayint lightcone,
                                             M2_arrayint grading,
                                             M2_arrayint Q,
                                             M2_arrayint nefPartition,
                                             M2_arrayint intnums,
                                             M2_arrayint settings);

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


/** \brief Test full-dimensionality of a cone and produce a witness, via GLPK.
 *
 * For the cone `{ x in QQ^n : A * x <= 0 }`, runs a single linear
 * program (GLPK, double precision) that simultaneously decides
 * full-dimensionality and produces either an interior witness or a
 * Farkas-style dual certificate of non-full-dimensionality. The
 * implementation negates `A` because the helper `coneInteriorPoint()`
 * works with `{x : A x >= 0}`.
 *
 * \par The LP
 * With the cone being `A x <= 0`, the LP is:
 * \code{.unparsed}
 *   maximize     t
 *   subject to   A_i * x  <=  -t        for each row i = 1, ..., m
 *                -1 <= x_j <= 1         for j = 1, ..., n  (normalization)
 *                t >= 0
 * \endcode
 * `t` is the strict-feasibility margin. If the optimum `tStar > 0`
 * (with numerical tolerance `1e-8`), the cone is full-dimensional
 * and the primal `x` is an interior witness (each `A_i x < 0`
 * strictly). If `tStar <= 0` the cone is not full-dimensional, and
 * the row duals supply a non-negative `y >= 0` with `y^T A = 0`.
 * The `[-1, 1]` box on `x` keeps the LP bounded; without it `t`
 * could scale to infinity along any interior ray.
 *
 * \param A An m-by-n matrix over ZZ; each row is one inequality.
 *          Entries must fit in a C `int`.
 * \return A 1-row dense MutableMatrix over RR(53):
 *         - **Full-dimensional** (`tStar > 0`): `2 + n` columns,
 *           `[1, tStar, x_1, ..., x_n]`.
 *         - **Not full-dimensional** (`tStar <= 0`): `2 + m` columns,
 *           `[0, tStar, y_1, ..., y_m]` where `y >= 0` and
 *           `y^T A = 0`.
 *
 *         Returns `nullptr` on error (entry of `A` overflows C `int`,
 *         or engine error).
 *
 * \par Example (M2 top level)
 * \code{.unparsed}
 *   debug Core
 *
 *   -- Full-dimensional: first octant in QQ^3
 *   A = matrix {{-1,0,0},{0,-1,0},{0,0,-1}}
 *   map(RR_53, rawConeInteriorPoint raw A)
 *     -- | 1 1 1 1 1 |        -- flag=1, tStar=1, interior=(1,1,1)
 *
 *   -- Not full-dimensional: the positive y-axis in QQ^2
 *   A = matrix {{1,0},{-1,0},{0,-1}}
 *   map(RR_53, rawConeInteriorPoint raw A)
 *     -- | 0 0 .5 .5 0 |      -- flag=0, tStar=0, dualCert=(.5,.5,0)
 * \endcode
 * In the second example the certificate `(0.5, 0.5, 0)` gives
 * `0.5*(1,0) + 0.5*(-1,0) + 0*(0,-1) = (0,0)`, witnessing that the
 * inequalities `x <= 0` and `-x <= 0` collapse the cone to the line
 * `x = 0`.
 *
 * \ingroup cones
 */
MutableMatrix /* or null */ *rawConeInteriorPoint(const Matrix *A);
  
#  if defined(__cplusplus)
}
#  endif

#endif /* _cone_h_ */
