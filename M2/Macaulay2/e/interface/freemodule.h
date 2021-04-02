#ifndef _freemodule_h_
#  define _freemodule_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class FreeModule;
class Matrix;
class Ring;
#  else
typedef struct FreeModule FreeModule;
typedef struct Matrix Matrix;
typedef struct Ring Ring;
#  endif

/**
   FreeModule interface routines

   A FreeModule in the engine is always over a specific
     ring, and is graded using the degree monoid of the ring.
     Monomials in a free module are ordered, either in a way
     determined by the ordering in the ring, or using an induced
     (Schreyer) monomial ordering (in the case when the ring
     is a polynomial ring of some sort)

   General notes: these are immutable objects, at least once
     they are returned by the engine

   BUGS/TODO: the Schreyer orders produced by sum,tensor,
     symm,exterior, and submodule, ignore the current tie
     breaker values.
     Also: I might keep all freemodules for a ring unique.
     This is not currently done.
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

const Ring *IM2_FreeModule_ring(const FreeModule *F);
/* drg: connected rawRing*/

int IM2_FreeModule_rank(const FreeModule *F);
/* drg: connected rawRank*/

M2_string IM2_FreeModule_to_string(const FreeModule *F);
/* drg: connected */

unsigned int rawFreeModuleHash(const FreeModule *F);
/* not quite connected */

const FreeModule /* or null */ *IM2_FreeModule_make(const Ring *R, int rank);
/* drg: connected rawFreeModule*/

const FreeModule /* or null */ *IM2_FreeModule_make_degs(const Ring *R,
                                                         M2_arrayint degs);
/* drg: connected rawFreeModule*/
/* Make a graded free module over R.  'degs' should be of length
 * divisible by the length 'd' of a degree vector, and the
 * i th degree will be degs[i*d]..degs[i*d+d-1], starting at
 * i = 0.
 */

const FreeModule /* or null */ *IM2_FreeModule_make_schreyer(const Matrix *m);
/* drg: connected rawSchreyerSource */
/* Returns G, (a copy of) the source free module of 'm', modified to
 * use the induced order via m: compare two monomials of G via
 * x^A e_i > x^B e_j iff either
 * leadmonomial((in m)(x^A e_i)) > leadmonomial((in m)(x^B e_j))
 * or these are the same monomial, and i > j.
 * The case where the target of 'm' has a Schreyer order is
 * handled efficiently.
 */

M2_arrayint IM2_FreeModule_get_degrees(const FreeModule *F);
/* drg: connected rawMultiDegree*/

const Matrix *IM2_FreeModule_get_schreyer(const FreeModule *F);
/* drg: connected rawGetSchreyer*/

M2_bool IM2_FreeModule_is_equal(const FreeModule *F, const FreeModule *G);
/* drg: connected === */
/* Determines if F and G are the same graded module.  If one has a
 * Schreyer order and one does not, but their ranks and degrees are the
 * same, then they are considered equal by this routine.
 */

const FreeModule /* or null */ *IM2_FreeModule_sum(const FreeModule *F,
                                                   const FreeModule *G);
/* drg: connected rawDirectSum */
/* The direct sum of two free modules over the same ring, or NULL.
 * If F or G has a Schreyer order, then so does their direct sum
 */

const FreeModule /* or null */ *IM2_FreeModule_tensor(const FreeModule *F,
                                                      const FreeModule *G);
/* drg: connected rawTensor*/
/* The tensor product of two free modules over the same ring, or NULL.
 * If F has (ordered basis {f_1,...,f_r}, and
 * G has (ordered) basis {g_1, ..., g_s}, then
 * the result has (ordered) basis
 *    {f_1 ** g_1, f_1 ** g_2, ..., f_1 ** g_s,
 *     f_2 ** g_1, f_2 ** g_2, ..., f_2 ** g_s,
 *     ...
 *     f_r ** g_1, ...              f_r ** g_s}.
 *  If F or G has a Schreyer order, what about their tensor product?
 *  At the moment, the answer is almost yes...
 */

const FreeModule /* or null */ *IM2_FreeModule_dual(const FreeModule *F);
/* drg: connected rawDual*/
/* Returns the graded dual F^* of F: if F has basis {f_1,...,f_r},
 * with degrees {d_1, ..., d_r}, then F^* has rank r, with
 * degrees {-d_1, ..., -d_r}.  The result does not have a
 * Schreyer order (even if F does).
 */

const FreeModule *IM2_FreeModule_symm(int n, const FreeModule *F);
/* drg: connected rawSymmetricPower*/
/* Returns the n th symmetric power G of F.
 * If F has basis {f_1,...,f_r}, then G has basis
 * the monomials of f_1, ..., f_r of degree exactly n, in
 * descending lexicographic order.
 * If F has a Schreyer order, then G is set to have one as well.
 */

const FreeModule *IM2_FreeModule_exterior(int n, const FreeModule *F);
/* drg: connected rawExteriorPower*/
/* Returns the n th exterior power G of F.
 * If F has basis {f_1,...,f_r}, then G has basis
 * the squarefree monomials of f_1, ..., f_r of degree exactly n, in
 * descending reverse lexicographic order.
 * If F has a Schreyer order, then G is set to have one as well.
 */

const FreeModule /* or null */ *IM2_FreeModule_submodule(const FreeModule *F,
                                                         M2_arrayint selection);
/* drg: connected rawSubmodule*/
/* Returns a free module obtained by choosing basis elements of F:
 * if F has basis {f_0, ..., f_(r-1)} with degrees {d_0, ..,d_(r-1)},
 * and selection = [i_1, ..., i_s], where 0 <= i_j < r for all j,
 * then return a free module of rank s, having degrees
 * d_(i_1), ..., d_(i_s).  'selection' may include duplicate values.
 * If F has a Schreyer order, the result has one as well.
 */

M2_arrayintOrNull rawFreeModuleSelectByDegrees(const FreeModule *F,
                                               M2_arrayint lo,
                                               M2_arrayint hi);
/* If F_i has multi-degree >= lo, AND <= hi, then add i to the result
   IF: lo has length 0, then treat that as -infinity in each component.
   Same with hi.
*/

#  if defined(__cplusplus)
}
#  endif

#endif /* _freemodule_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
