/* Copyright 2002 by Michael E. Stillman */

#ifndef _engine_h_
#define _engine_h_

/**
   \mainpage Hi, this is my main documentation page.
 */

#include "engine-includes.hpp"

#if defined(__cplusplus)
class FreeModule;
class Matrix;
class MutableMatrix;
class RingMap;
class Computation;
class MutableComplex;
#else
/* Define the externally visible types here */
typedef struct FreeModule FreeModule;
typedef struct Matrix Matrix;
typedef struct MutableMatrix MutableMatrix;
typedef struct RingMap RingMap;
typedef struct Computation Computation;
typedef struct MutableComplex MutableComplex;
#endif

#include "interface/aring.h"
#include "interface/computation.h"
#include "interface/cra.h"
#include "interface/factory.h"
#include "interface/flint.h"
#include "interface/freemodule.h"
#include "interface/groebner.h"
#include "interface/matrix.h"
#include "interface/monoid.h"
#include "interface/monomial-ideal.h"
#include "interface/monomial-ordering.h"
#include "interface/mutable-matrix.h"
#include "interface/random.h"
#include "interface/ring.h"
#include "interface/ringelement.h"
#include "interface/ringmap.h"

#include "interface/NAG.h"

#if defined(__cplusplus)
extern "C" {
#endif
  void IM2_initialize(void); /* drg: connected */
  M2_string IM2_last_error_message(void); /* drg: connected */

  M2_string engineMemory(); /* connected MES to engineMemory */
  
  /**************************************************/
  /**** Monomial routines ***************************/
  /**************************************************/
  /* Monomials in the engine: are not associated with a monoid,
   * and may have negative exponents. Monomials are immutable objects.
   */

  /**************************************************/
  /**** Mutable Complex routines ********************/
  /**************************************************/

  M2_string rawMutableComplexToString(const MutableComplex *M);

  unsigned int  rawMutableComplexHash(const MutableComplex *M);

  MutableComplex* rawMutableComplex(const engine_RawMutableMatrixArray M);

  M2_arrayint rawPruneBetti(MutableComplex* C, int n, int f);

  MutableComplex* rawPruneComplex(MutableComplex* C, int n, int f);

  engine_RawMutableMatrixArray rawPruningMorphism(MutableComplex* C, int n, int f);

  /**************************************************/
  /**** Local Ring routines *************************/
  /**************************************************/

  Matrix * rawLiftLocalMatrix(const Ring * R, const Matrix *m);
  M2_bool  rawIsLocalUnit(const RingElement *f);

  /**************************************************/
  /**** Groebner basis and resolution routines ******/
  /**************************************************/

  Computation /* or null */* IM2_Computation_set_stop(Computation *G,
                                     M2_bool always_stop,       /* 1 */
                                     M2_arrayint degree_limit,  /* 2*/
                                     int basis_element_limit,   /* 3 */
                                     int syzygy_limit,          /* 4 */
                                     int pair_limit,            /* 5 */
                                     int codim_limit,           /* 6 */
                                     int subring_limit,         /* 7 */
                                     M2_bool just_min_gens,     /* 8 */
                                     M2_arrayint length_limit   /* 9 */  /* not for GB */
                                     ); /* drg: connected rawGBSetStop */

  /* Each of these routines can return NULL, because of errors */

  Computation /* or null */ *rawStartComputation(Computation *G);
  /* start or continue the computation */

  enum ComputationStatusCode rawStatus1(Computation *C);

  int rawStatus2(Computation *C);
  /* The computation is complete up to and including this degree.
     The exact meaning of 'degree' is computation specific */

  M2_string IM2_GB_to_string(Computation *C); /* drg: connected, in actors4.d */

  unsigned int rawComputationHash(const Computation *C); /* drg: connected, in basic.d */

  void rawShowComputation(const Computation *C); /* Dan: connected to rawShowComputation */

  /*******************************************
   * Computation routines for Groebner bases *
   *******************************************/

  /* 
     routine to compute a Groebner basis of an ideal in a polynomial ring
     over a finite prime field.  Interfaces to mathicgb.
     reducer: 0 is ClassicReducer, 1 is MatrixReducer
   */
  const Matrix* /* or null */ rawMGB(const Matrix* input, 
                                     int reducer,
                                     int spairGroupSize,
                                     int nthreads,
                                     const M2_string logging
                                     ); /* connected: rawMGB */

  Computation /* or null */ *IM2_GB_make(const Matrix *m,
                                 M2_bool collect_syz,
                                 int n_rows_to_keep,
                                 M2_arrayint gb_weights,
                                 M2_bool use_max_degree,
                                 int max_degree,
                                 int algorithm,
                                 int strategy,
                                 int max_reduction_count); /* drg: connected rawGB */

  Computation /* or null */ *IM2_GB_force(const Matrix *m,
                                  const Matrix *gb,
                                  const Matrix *change,
                                  const Matrix *syz); /* drg: connected rawGBForce */

  Computation /* or null */ *rawMarkedGB(const Matrix *leadterms,
                                 const Matrix *m,
                                 const Matrix *gb,
                                 const Matrix *change,
                                 const Matrix *syz); /* mes: connected rawMarkedGB */

  Computation /* or null */ *rawGroebnerWalk(const Matrix *gb,
                                     const MonomialOrdering *order1);
  /* Create a GB algorithm which will compute using the generic Groebner walk algorithm
     Input: gb: a matrix which, under order1, would be a Groebner basis, except that
                'gb' is a matrix over a polynomial ring whose order is 'order2'.
            order1: a monomial ordering
     Output: a Groebner basis computation object which will compute a GB of gb wrt
            order2, using the Geneeric Groebner Walk algorithm of ...
     Assumptions: the base ring is a polynomial ring over a field, with NO quotient elements
  */

  Computation /* or null */ *IM2_GB_set_hilbert_function(Computation *G,
                                                 const RingElement *h); /* drg: connected rawGBSetHilbertFunction */


  const Matrix /* or null */ *rawGBGetMatrix(Computation *C);
  /* Get the minimal, auto-reduced GB of a GB computation.
     Each call to this may produce a different raw matrix */

  const Matrix /* or null */ *rawGBGetLeadTerms(Computation *G, int nparts);

  const Matrix /* or null */ *rawGBGetParallelLeadTerms(Computation *C, M2_arrayint w);

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

  const Matrix /* or null */ *rawGBSyzygies(Computation *C);
  /* Yields a matrix containing the syzygies computed so far
     via the GB computation C, assuming that 'collect_syz' was
     set when the computation was created.  If 'n_rows_to_keep' was
     set to a non-negative integer, then only that many rows of each
     syzygy are kept. */

  const Matrix /* or null */ *rawGBMatrixRemainder(Computation *G,
                                           const Matrix *m); /* drg: connected rawGBMatrixRemainder */

  M2_bool IM2_GB_matrix_lift(Computation *G,
                          const Matrix *m,
                          const Matrix /* or null */ **result_remainder,
                          const Matrix /* or null */ **result_quotient
                          ); /* drg: connected rawGBMatrixLift */
  /* false is returned if there is an error or if the remainder is NON-zero */

  int IM2_GB_contains(Computation *G,
                      const Matrix *m); /* drg: connected rawGBContains */


  /*******************************************
   * Computation routines for Resolutions ****
   *******************************************/

  /* LongPolynomial, Sort, Primary, Inhomogeneous, Homogeneous */
  /* Res: SortStrategy, 0, 1, 2, 3 ?? */

  Computation /* or null */ *IM2_res_make(const Matrix *m,
                                  M2_bool resolve_cokernel,
                                  int max_level,
                                  M2_bool use_max_slanted_degree,
                                  int max_slanted_degree,
                                  int algorithm,
                                  int strategy /* drg: connected rawResolution */
                                  );

  const Matrix /* or null */ *rawResolutionGetMatrix(Computation *G,int level);
  /* rawResolutionGetMatrix */

  MutableMatrix /* or null */ *rawResolutionGetMatrix2(Computation *G,int level,int degree);
  /* rawResolutionGetMatrix2 */

  // This might be temporary!
  MutableMatrix /* or null */ *
  rawResolutionGetMutableMatrixB(Computation *C,
                                 const Ring* R, // A polynomial ring with coeffs = RR, or a finite field used in C, same monoid as C's ring.
                                 int level);

  // This might be temporary!
  MutableMatrix /* or null */ *
  rawResolutionGetMutableMatrix2B(Computation *C,
                           const Ring* KK, // should be RR, or a finite field used in C.
                           int level,
                           int degree);

  const FreeModule /* or null */ *rawResolutionGetFree(Computation *G, int level);
    /*drg: connected rawResolutionGetFree*/

  M2_arrayint rawResolutionBetti(Computation *G,
                                 int type); /* drg: connected rawGBBetti */
  /* type:
         0: minimal betti numbers, (for FastNonminimal=>true, the ACTUAL betti numbers)
         1: non-minimal betti numbers (skeleton size, or size of GB's).
           (for FastNonminimal=>true, same as "0" case)
         2: number of S-pairs remaining to consider
         3: number of monomials in polynomials at this slot
         4: for FastNonminimal=>true resolutions, the minimal betti numbers
            other cases, this is an error.
     Not all of these may be accessible with all algorithms.  If not available,
     A betti diagram with all -1's is displayed.
  */

  /* I don't know what this is supposed to do (mike) */
  int IM2_Resolution_status(Computation *G,
                    int * complete_up_through_this_degree,
                    int * complete_up_through_this_level); /* drg: TODO */
  /* -1: error condition, and the error message is set.
     0: not made, and in fact it won't ever be done...
     1: not started,
     2: started,
     3: stopped because of a stopping condition
     4: finished the computation completely
  */

  enum ComputationStatusCode IM2_Resolution_status_level(Computation *G,
                                                         int level,
                                                         M2_bool minimize,
                                                         int * complete_up_through_this_degree);
  /* WARNING: 'minimize' is completely ignored, and should be removed from the interface */
  /* drg: connected rawResolutionStatusLevel */

  /**************************************************/
  /**** Specialized operations **********************/
  /**************************************************/

  Matrix /* or null */ * rawSubduction(int numparts,
                                       const Matrix *M,
                               const RingMap *F,
                               Computation *C);
  /*
    Perform a subalgebra reduction of the entries of the one row matrix M.
    C should be a GB computed in high enough degree to handle the elements of M,
      of an ideal of the form y_i - m_i (m_i is the lead monomial of f_i).
    F should be a ring map R --> R, sending y_i to f_i.
    M should be a matrix over the ring R, usually only involving the variables in the f_i.
    R should be a ring containing the variables of the f_i, and the variables y_i,
      with a monomial order eliminating the first set of variables (which is 'numparts' parts of the
      monomial ordering).
    numparts: number of parts in the monomial order of the original ring.
   The resulting matrix will have no monomials which are in the subalgebra
   generated by the monomials m_i, and each entry of M and the corresponding entry of the
   result differ by an element of the subalgebra generated by the f_i.
   */

  M2_bool rawIdealOfPoints(const Ring *R,
                      const MutableMatrix *Pts,
                      Matrix /* or null */ ** result_GB,
                      Matrix /* or null */ ** result_std_monoms);
  /* Returns false if an error occured.
     Input: R: a polynomial ring of the form K[x1,...,xn]
            Pts: an n by d matrix over K.
     Action: Compute the ideal of the points in n-space
             given by the columns of 'Pts'
     Output: result_GB: the GB of this ideal
             result_std_monoms: the standard monomials (1 by d matrix)
     Question: should this return the separators as well?
  */

  const Matrix /* or null */ *rawGbBoolean(const Matrix *m);
  const Matrix /* or null */ *rawBIBasis(const Matrix* m, int toGroebner);

#if defined(__cplusplus)
}
#endif

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
