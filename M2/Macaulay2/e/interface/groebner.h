// FIXME: this header is based on what is defined in groebner.cpp,
// but the declarations in engine.h don't seem to match

#ifndef _groebner_h_
#  define _groebner_h_

#  include "engine-includes.hpp"
#  include "interface/computation.h"

// TODO: fix this
#  if defined(__cplusplus)
class Computation;
class FreeModule;
class Matrix;
class Ring;
class RingElement;
class MonomialOrdering;
class MutableMatrix;
class RingMap;
#  else
typedef struct Computation Computation;
typedef struct FreeModule FreeModule;
typedef struct Matrix Matrix;
typedef struct MonomialOrdering MonomialOrdering;
typedef struct MutableMatrix MutableMatrix;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
typedef struct RingMap RingMap;
#  endif

/**
   Groebner computations interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

void test_over_RR_or_CC(const Ring *R);

/////////////////
// F4 routines //
/////////////////
M2_arrayint rawMinimalBetti(Computation *G,
                            M2_arrayint slanted_degree_limit,
                            M2_arrayint length_limit);
/* connected: rawMinimalBetti */

const RingElement /* or null */ *IM2_Matrix_Hilbert(const Matrix *M);
/* This routine computes the numerator of the Hilbert series
   for coker leadterms(M), using the degrees of the rows of M.
   NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */

///////////////////////////////////////////////////////////////////////////////
/////// The following will be removed once the new code is functional  ///////
///////////////////////////////////////////////////////////////////////////////
Computation /* or null */ *IM2_GB_make(
    const Matrix *m,
    M2_bool collect_syz,
    int n_rows_to_keep,
    M2_arrayint gb_weights,
    M2_bool use_max_degree,
    int max_degree,
    int algorithm,
    int strategy,
    int max_reduction_count); /* drg: connected rawGB */

Computation /* or null */ *IM2_res_make(const Matrix *m,
                                        M2_bool resolve_cokernel,
                                        int max_level,
                                        M2_bool use_max_slanted_degree,
                                        int max_slanted_degree,
                                        int algorithm,
                                        int strategy);

Computation /* or null */ *IM2_GB_set_hilbert_function(Computation *C,
                                                       const RingElement *h);

Computation /* or null */ *IM2_GB_force(
    const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
    const Matrix *gb,
    const Matrix *change, /* same number of columns as 'gb', if not 0 */
    const Matrix *syz);   /* possibly 0 too, otherwise same rows as change */

Computation /* or null */ *rawMarkedGB(
    const Matrix *leadterms,
    const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
    const Matrix *gb,
    const Matrix *change, /* same number of columns as 'gb', if not 0 */
    const Matrix *syz);   /* possibly 0 too, otherwise same rows as change */

Computation /* or null */ *rawGroebnerWalk(const Matrix *gb,
                                           const MonomialOrdering *order1);

Computation /* or null */ *IM2_Computation_set_stop(
    Computation *G,
    M2_bool always_stop,
    M2_arrayint degree_limit,
    int basis_element_limit,
    int syzygy_limit,
    int pair_limit,
    int codim_limit,
    int subring_limit,
    M2_bool just_min_gens,
    M2_arrayint length_limit); /* TODO */
/* LongPolynomial, Sort, Primary, Inhomogeneous, Homogeneous */
/* Res: SortStrategy, 0, 1, 2, 3 ?? */

Computation /* or null */ *rawStartComputation(Computation *C);
/* start or continue the computation */

enum ComputationStatusCode rawStatus1(Computation *C);

int rawStatus2(Computation *C);
void rawShowComputation(const Computation *C);
const Matrix /* or null */ *rawGBGetMatrix(Computation *C);
/* Get the minimal, auto-reduced GB of a GB computation.
   Each call to this will produce a different raw matrix */

const Matrix /* or null */ *rawGBMinimalGenerators(Computation *C);
/* Yields a matrix whose columns form a minimal generating set
   for the ideal or submodule, as computed so far.  In the
   inhomogeneous case, this yields a generating set which is
   sometimes smaller than the entire Groebner basis. */

const Matrix /* or null */ *rawGBChangeOfBasis(Computation *C);
/* Yields the change of basis matrix from the Groebner basis to
   the original generators, at least if n_rows_to_keep was set
   when creating the GB computation.  This matrix, after the
   computation has run to completion, should satisfy:
   (original matrix) = (GB matrix) * (change of basis matrix). */

const Matrix /* or null */ *rawGBGetLeadTerms(Computation *C, int nparts);

const Matrix /* or null */ *rawGBGetParallelLeadTerms(Computation *C,
                                                      M2_arrayint w);

const Matrix /* or null */ *rawGBSyzygies(Computation *C);
/* Yields a matrix containing the syzygies computed so far
   via the GB computation C, assuming that 'collect_syz' was
   set when the computation was created.  If 'n_rows_to_keep' was
   set to a non-negative integer, then only that many rows of each
   syzygy are kept. */

const Matrix /* or null */ *rawGBMatrixRemainder(Computation *C,
                                                 const Matrix *m);

M2_bool IM2_GB_matrix_lift(Computation *C,
                           const Matrix *m,
                           const Matrix /* or null */ **result_remainder,
                           const Matrix /* or null */ **result_quotient);

int IM2_GB_contains(Computation *C, const Matrix *m);

/*******************************************
 * Noncommutative Groebner bases ***********
 *******************************************/

/* Returns a 2-sided GB of the 2-sided ideal from the one-row matrix 'input'
   computed up to and including degree 'maxdeg'.  This 'maxdeg' is the heft
   degree (in the case of multigradings). 'strategy': is an integer whose
   various bits encode stratgy options Assumptions:
     1. input is a one row matrix, whose entries are the generators of a 2-sided
   ideal.
     2. If the computation is interrupted, we return the elements we have
   constructed so far.
     3. use gbTrace as usual to get verbose messages during computation.
   Not done yet:
     writing GB elements in terms of original generators.
     We will need a new function
   Strategy bits:
     bits 0..3: choice of reduction heap strategy.
*/
const Matrix *rawNCGroebnerBasisTwoSided(const Matrix *input,
                                         int maxdeg,
                                         int strategy);

const Matrix *rawNCReductionTwoSided(const Matrix *toBeReduced,
                                     const Matrix *reducers);

const Matrix *rawNCBasis(const Matrix *gb2SidedIdeal,
                         M2_arrayint lo_degree,
                         M2_arrayint hi_degree,
                         int limit);

/*******************************************
 *******************************************
 *******************************************/

const Matrix /* or null */ *rawResolutionGetMatrix(Computation *C, int level);

MutableMatrix /* or null */ *rawResolutionGetMatrix2(Computation *C,
                                                     int level,
                                                     int degree);

MutableMatrix /* or null */ *rawResolutionGetMutableMatrixB(Computation *C,
                                                            const Ring *R,
                                                            int level);
// Perhaps a HACK that might change.
// First: C must be a nonminimal res computation, over QQ M.
// Second: R must be a polynomial ring with the same monoid M as C's,
//  and the coefficient ring must be either RR, or ZZ/p, where p is the (a)
//  prime being used in the computation.

MutableMatrix /* or null */ *rawResolutionGetMutableMatrix2B(
    Computation *C,
    const Ring *KK,  // should be RR, or a finite field used in C.
    int level,
    int degree);

const FreeModule /* or null */ *rawResolutionGetFree(Computation *C, int level);

/* TODO */
int IM2_Resolution_status(Computation *C,
                          int *complete_up_through_this_degree,
                          int *complete_up_through_this_level);

/* TODO */
enum ComputationStatusCode IM2_Resolution_status_level(
    Computation *C,
    int level,
    M2_bool minimize,
    int *complete_up_through_this_degree);

M2_arrayintOrNull rawResolutionBetti(Computation *C, int type);
/* see engine.h for description of what 'type' should be */

M2_string IM2_GB_to_string(Computation *C);
/* TODO */

unsigned int rawComputationHash(const Computation *C);

Matrix /* or null */ *rawSubduction(int numparts,
                                    const Matrix *M,
                                    const RingMap *F,
                                    Computation *C);
   
Matrix /* or null */ *rawSubduction1(int numparts,
                                     const Ring *rawT,
                                     const Ring *rawS,
                                     const Matrix *m,
                                     const RingMap *inclusionAmbient,
                                     const RingMap *fullSubstitution,
                                     const RingMap *substitutionInclusion,
                                     Computation *rawGBI,
                                     Computation *rawGBReductionIdeal); 

void rawDisplayMatrixStream(const Matrix *inputMatrix);

const Matrix *rawMGB(
    const Matrix *inputMatrix,
    int reducer,
    int spairGroupSize,  // a value of 0 means let the algorithm choose
    int nthreads,
    M2_string logging);

#  if defined(__cplusplus)
}
#  endif

#endif /* _groebner_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
