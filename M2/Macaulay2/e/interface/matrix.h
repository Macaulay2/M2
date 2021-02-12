#ifndef _matrix_h_
#  define _matrix_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class FreeModule;
class Matrix;
class Ring;
class RingElement;
#  else
typedef struct FreeModule FreeModule;
typedef struct Matrix Matrix;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
#  endif

/**
   Matrix interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

const Matrix /* or null */ *IM2_Matrix_promote(const FreeModule *newTarget,
                                               const Matrix *f);
/* connected to rawPromote*/

const Matrix /* or null */ *IM2_Matrix_lift(int *success_return,
                                            const FreeModule *newTarget,
                                            const Matrix *f);
/* connected to rawLift */
// returns null if lifting not possible

/**************************************************/
/**** Matrix routines *****************************/
/**************************************************/

const FreeModule *IM2_Matrix_get_target(
    const Matrix *M); /* drg: connected rawTarget*/

const FreeModule *IM2_Matrix_get_source(
    const Matrix *M); /* drg: connected rawSource, used in rawMatrixColumns*/

int IM2_Matrix_n_rows(const Matrix *M); /* drg: connected rawNumberOfRows*/

int IM2_Matrix_n_cols(const Matrix *M); /* drg: connected rawNumberOfColumns*/

M2_arrayint IM2_Matrix_get_degree(
    const Matrix *M); /* drg: connected rawMultiDegree*/

M2_string IM2_Matrix_to_string(const Matrix *M); /* drg: connected */

unsigned int rawMatrixHash(const Matrix *M); /* drg: connected to "hash"  */

const RingElement /* or null */ *IM2_Matrix_get_entry(
    const Matrix *M,
    int r,
    int c); /* drg: connected rawMatrixEntry, OK*/

/*******************************************************************************/
const Matrix *IM2_Matrix_identity(
    const FreeModule *F,
    int preference); /* drg: connected rawIdentity, OK*/

const Matrix /* or null */ *IM2_Matrix_zero(
    const FreeModule *F,
    const FreeModule *G,
    int preference); /* drg: connected rawZero, OK */

const Matrix /* or null */ *IM2_Matrix_make1(
    const FreeModule *target,
    int ncols,
    const engine_RawRingElementArray M,
    int preference); /* drg: connected rawMatrix1, OK */

const Matrix /* or null */ *IM2_Matrix_make2(
    const FreeModule *target,
    const FreeModule *source,
    M2_arrayint deg,
    const engine_RawRingElementArray M,
    int preference); /* drg: connected rawMatrix2, OK */

const Matrix /* or null */ *IM2_Matrix_make_sparse1(
    const FreeModule *target,
    int ncols,
    M2_arrayint rows,
    M2_arrayint cols,
    const engine_RawRingElementArray entries,
    int preference); /* drg: connected rawSparseMatrix1, OK */

const Matrix /* or null */ *IM2_Matrix_make_sparse2(
    const FreeModule *target,
    const FreeModule *source,
    M2_arrayint deg,
    M2_arrayint rows,
    M2_arrayint cols,
    const engine_RawRingElementArray entries,
    int preference); /* drg: connected rawSparseMatrix2, OK */

M2_bool IM2_Matrix_is_implemented_as_dense(
    const Matrix *M); /* connected to rawIsDense */
/* Is the matrix M implemented in the engine as a dense matrix? */

const Matrix /* or null */ *IM2_Matrix_remake1(
    const FreeModule *target,
    const Matrix *M,
    int preference); /* drg: connected rawMatrixRemake1, OK  */
/* Create a new matrix (mutable or immutable), from M, with new target,
   and/or mutable-ness. The target free module must have the expected rank.
   The source free module is computed heuristically from the the target and the
   columns of the matrix.
*/

const Matrix /* or null */ *IM2_Matrix_remake2(
    const FreeModule *target,
    const FreeModule *source,
    M2_arrayint deg,
    const Matrix *M,
    int preference); /* drg: connected rawMatrixRemake2, OK */
/* Create a new matrix (mutable or immutable), from M, with new target,
   source, deg and/or mutable-ness. The new free modules must have
   the expected rank.
*/

const Matrix /* or null */ *IM2_Matrix_random(
    const Ring *R,
    int r,
    int c,
    double fraction_non_zero,
    int special_type, /* 0: general, 1:upper triangular, others? */
    int preference);  /* connected to rawRandomConstantMatrix, OK */

/**********************************************************************************/

M2_bool IM2_Matrix_is_zero(const Matrix *M); /* drg: connected rawIsZero*/

int IM2_Matrix_is_equal(
    const Matrix *M,
    const Matrix *N); /* drg: connected === and to rawIsEqual for use with == */
                      // 1 = true, 0 = false, -1 = error
/* This checks that the entries of M,N are the same, as well as
   that the source and target are the same (as graded free modules).
   Therefore, it can happen that M-N == 0, but M != N.
*/

M2_bool IM2_Matrix_is_graded(
    const Matrix *M); /* drg: connected rawIsHomogeneous*/

const Matrix /* or null */ *IM2_Matrix_concat(
    const engine_RawMatrixArray Ms); /* drg: connected rawConcat*/

const Matrix /* or null */ *IM2_Matrix_direct_sum(
    const engine_RawMatrixArray Ms); /* drg: connected rawDirectSum*/

const Matrix /* or null */ *IM2_Matrix_tensor(
    const Matrix *M,
    const Matrix *N); /* drg: connected rawTensor*/

const Matrix /* or null */ *IM2_Matrix_transpose(
    const Matrix *M); /* drg: connected rawDual*/

const Matrix /* or null */ *IM2_Matrix_reshape(
    const Matrix *M,
    const FreeModule *F,
    const FreeModule *G); /* drg: connected rawReshape*/

const Matrix /* or null */ *IM2_Matrix_flip(
    const FreeModule *F,
    const FreeModule *G); /* drg: connected rawFlip*/

const Matrix /* or null */ *rawWedgeProduct(
    int p,
    int q,
    const FreeModule *F); /* drg: connected rawWedgeProduct */
/* Constructs the map
   exterior(p,F) ** exterior(q,F) --> exterior(p+q,F)
*/

const Matrix /* or null */ *IM2_Matrix_submatrix(
    const Matrix *M,
    M2_arrayint rows,
    M2_arrayint cols); /* drg: connected rawSubmatrix*/

const Matrix /* or null */ *IM2_Matrix_submatrix1(
    const Matrix *M,
    M2_arrayint cols); /* drg: connected rawSubmatrix*/

const Matrix /* or null */ *IM2_Matrix_koszul(
    int p,
    const Matrix *M); /* drg: connected rawKoszul*/

const Matrix /* or null */ *rawKoszulMonomials(
    int nskew,
    const Matrix *M,
    const Matrix *N); /* drg: connected rawKoszulMonomials */
/* M and N should each have one row, and the base ring should be a
   polynomial ring.  The (i,j) th entry of the resulting matrix is
   1 or -1 times N_j/M_i (if M_i divides N_j). The sign is determined only from
   the first nskew variables.  The sign is the sign of M_i * (N_j/M_i) in
   exterior algebra (on this set of variables).  The actual commutativity of the
   common ring of M and N is ignored. */

const Matrix /* or null */ *IM2_Matrix_symm(
    int p,
    const Matrix *M); /* drg: connected rawSymmetricPower*/

const Matrix /* or null */ *IM2_Matrix_exterior(
    int p,
    const Matrix *M,
    int strategy); /* drg: connected rawExteriorPower*/

M2_arrayint IM2_Matrix_sort_columns(
    const Matrix *M,
    int deg_order,
    int mon_order); /* drg: connected rawSortColumns*/

const Matrix /* or null */ *
IM2_Matrix_minors(int p, const Matrix *M, int strategy); /* drg: unconnected*/

const Matrix /* or null */ *rawMinors(
    int p,
    const Matrix *M,
    int strategy,
    int n_minors_to_compute, /* -1 means all */
    M2_arrayintOrNull first_row_set,
    M2_arrayintOrNull first_col_set); /* connected to rawMinors */
/* If first_row_set or first_col_set is not NULL, they should both be non-NULL,
   and both have length p.  If not, NULL is returned.
   Compute n_minors_to_compute minors, starting at (first_row_set,first_col_set)
   if given, otherwise starting at the first (0..p-1,0..p-1).
*/

const Matrix /* or null */ *IM2_Matrix_pfaffians(
    int p,
    const Matrix *M); /* drg: connected rawPfaffians*/

const Matrix *rawMatrixCompress(
    const Matrix *M); /* connected rawMatrixCompress */

const Matrix /* or null */ *IM2_Matrix_uniquify(const Matrix *M); /* TODO */
/* if any two columns are the same up to a scalar multiple, then keep only
   one of the columns.  Remove any zero columns too.
   The definition of "same up to a scalar" is this:
   if K is the base field or ring (i.e. QQ in QQ(x,y,z)[s,t]),
   and if c and d are the lead scalar coeff's of vecs v,w, resp, then
   v and w are scalar multiplies iff d*v == c*w.
   Warning: Over non-domains, this might not be the intended effect.
*/

const Matrix *rawRemoveScalarMultiples(const Matrix *m); /* connected */

const Matrix *rawRemoveMonomialFactors(const Matrix *m,
                                       M2_bool make_squarefree_only);
/* connected to rawRemoveMonomialFactors */

////  Content ////////////////////////////////////////
// The content of a ring element, polynomial or vector is defined to be an
// element of the base (either a finite field, ZZ, QQ, or a fraction)
// If base is ZZ: the result is the gcd of all of the terms appearing in the
// polynomial or vector
//   The sign is chosen so that after division by the content the resulting
//   polynomial is monic
// If base is QQ or frac(A): the numerator is the gcd of all of the numerators,
//                the denominator is the lcm of all of the denominators
// If base is kk: the result is the lead coefficient.  For vectors, what should
// this mean?
//////////////////////////////////////////////////////

const Matrix /* or null */ *rawMatrixContent(
    const Matrix *M); /* connect to rawContent */
// returns the matrix of the content of each column of M

const Matrix /* or null */ *rawMatrixRemoveContent(
    const Matrix *M); /* connect to rawRemoveContent */
// returns the matrix with the content (as defined above) removed

const Matrix /* or null */ *rawMatrixSplitContent(
    const Matrix *M,
    const Matrix /* or null */ **result); /* connect to rawSplitContent */
// returns the matrix of the content of each column of M,
// and result is set to the result of rawMatrixRemoveContent.

const Matrix /* or null */ *IM2_Matrix_remove_content(
    const Matrix *M); /* connected rawRemoveContent*/

/* Routines for use when the base ring is a polynomial ring of some sort */

const Matrix /* or null */ *IM2_Matrix_diff(
    const Matrix *M,
    const Matrix *N); /* drg: connected rawMatrixDiff*/

const Matrix /* or null */ *IM2_Matrix_contract(
    const Matrix *M,
    const Matrix *N); /* drg: connected rawMatrixContract*/

const Matrix /* or null */ *IM2_Matrix_homogenize(
    const Matrix *M,
    int var,
    M2_arrayint wts); /* drg: connected rawHomogenize*/

const Matrix /* or null */ *rawCoefficients(
    M2_arrayint vars,
    const Matrix *monoms,
    const Matrix *M); /* drg: connected as rawCoefficients*/
/* Given:
 *  vars : a list of variable indices in the (common) ring R of monoms and M
 *  monoms : a map R^b --> R^a such that each column has exactly one monomial
 *      which is only in the variables in 'vars'.
 *  M : a map R^c --> R^a such that every (module) monomial of each column of M
 *      matches one of the columns of 'monoms', in the variables 'vars'.
 *
 * Returns: a matrix C : R^c --> R^b such that
 *      (i) the entries of C do not involve the variables in 'vars', and
 *      (ii) monoms * C == M
 *
 * Assumptions on rings: if R is non-commutative, then the variables in 'vars'
 *      should commute with the variables outside of 'vars'.
 *
 * If each column of monoms has more than one monomial, or if variables other
 * than those in 'vars' occur, then only the first monomial is used, and the
 * other variables are ignored. If a monomial occurs twice, then one of them
 * will be used (which one is left undefined)
 */

const Matrix /* or null */ *IM2_Matrix_monomials(
    M2_arrayint vars,
    const Matrix *M); /* drg: connected rawMonomials*/

const Matrix *IM2_Matrix_initial(
    int nparts,
    const Matrix *M); /* drg: connected rawInitial*/

M2_arrayint IM2_Matrix_elim_vars(
    int nparts,
    const Matrix *M); /* drg: connected rawEliminateVariables*/

M2_arrayint IM2_Matrix_keep_vars(
    int nparts,
    const Matrix *M); /* drg: connected rawKeepVariables*/

engine_RawMatrixAndInt IM2_Matrix_divide_by_var(
    const Matrix *M,
    int var,
    int maxdegree); /* drg: connected rawDivideByVariable*/
/* If M = [v1, ..., vn], and x = 'var'th variable in the ring,
   return the matrix [w1,...,wn], where wi * x^(ai) = vi,
   and wi is not divisible by x, or ai = maxdegree,
   and the integer which is the maximum of the ai's.
   QUESTION: what rings should this work over?
*/

engine_RawMatrixPairOrNull rawTopCoefficients(
    const Matrix *M); /* connected to rawTopCoefficients */
/* Returns a pair of matrices: the first is a list of monomials (of form
   var^exp), and the second has the same row space as M.  For each column, find
   the smallest index variable, var,  which occurs, and exp, the largest degree
   to which it occurs in that column.  Place var^exp in the first matrix. Place
   the coeff of var^exp (a vector) into the second matrix. If the ring is not a
   polynomial ring, an error is given, and Null is returned.
*/

M2_arrayintOrNull rawMatrixIndices(
    const Matrix *f); /* connected to rawIndices */

M2_arrayint IM2_Matrix_min_leadterms(const Matrix *M,
                                     M2_arrayint vars); /* TODO */

const Matrix /* or null */ *IM2_Matrix_auto_reduce(const Matrix *M); /* TODO */

const Matrix /* or null */ *IM2_Matrix_reduce(const Matrix *M,
                                              const Matrix *N); /* TODO */

const Matrix /* or null */ *IM2_Matrix_reduce_by_ideal(
    const Matrix *M,
    const Matrix *N); /* TODO */

/* Routines when considering matrices as modules of some sort */

const Matrix /* or null */ *rawModuleTensor(
    const Matrix *M,
    const Matrix *N); /* connected rawModuleTensor */

const Matrix /* or null */ *rawBasis(
    const Matrix *M,
    M2_arrayint lo_degree, /* possibly length 0 */
    M2_arrayint hi_degree,
    M2_arrayint wt,
    M2_arrayint vars,
    M2_bool do_truncation,
    int limit); /* connected to rawBasis */
/* Yields a monomial basis of part of the graded R-module cokernel(M).
 * Returns a matrix of monomials which maps to the target of M, such that
 *  (i) The image spans the sum of M_i, for lo_degree <= i <= hi_degree
 *       where M_i is the degree i piece of M.
 * Notes:
 *  -- 'vars' is a list of variables.  The entries of the result will only
 * involve these variables.
 *  -- 'wt' should be a list of integers of length <= number of degrees,
 *     with the property that each variable in 'vars' has (its degree) dot wt >
 * 0.
 *  -- if lo_degree has length 0, then it is assumed to be -infinity
 *  -- if hi_degree has length 0, then it is assumed to be infinity
 *  -- in either of these cases, or if lo_degree is not equal to hi_degree, then
 *     the degree ring of R must have one variable.
 *  -- if limit >= 0, then only the first 'limit' monomials are placed into the
 * result.
 *  -- if do_truncation is set, then monomials of degree higher than hi_degree
 * will be placed into .
 *
 * If R is a quotient ring, then the monomial order had better be a product
 * order such that the first block (or blocks) consists of the variables in
 * 'vars'.
 *
 */

int IM2_Matrix_dimension(const Matrix *M); /* TODO */

const RingElement /* or null */ *IM2_Matrix_Hilbert(
    const Matrix *M); /* drg: connected rawHilbert*/
/* This routine computes the numerator of the Hilbert series
   for coker leadterms(M), using the degrees of the rows of M.
   NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */

const Matrix *IM2_kernel_of_GB(const Matrix *G); /* connected rawKernelOfGB */
/* Assuming that the columns of G form a GB, this computes
   a Groebner basis of the kernel of these elements, using an appropriate
   Schreyer order on the source of G. */

#  if defined(__cplusplus)
}
#  endif

#endif /* _matrix_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
