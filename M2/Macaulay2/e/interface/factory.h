#ifndef _factory_h_
#  define _factory_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Matrix;
class RingElement;
#  else
typedef struct Matrix Matrix;
typedef struct RingElement RingElement;
#  endif

/**
   Polynomial GCD and factorization interface routines via Factory and MPSolve
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

const RingElement *rawGCDRingElement(const RingElement *f,
                                     const RingElement *g,
                                     const RingElement *mipo,
                                     M2_bool inExtension);

const RingElement *rawExtendedGCDRingElement(const RingElement *f,
                                             const RingElement *g,
                                             const RingElement **A,
                                             const RingElement **B);

const RingElement *rawPseudoRemainder(const RingElement *f,
                                      const RingElement *g);

void rawFactor(const RingElement *f,
               engine_RawRingElementArrayOrNull *result_factors,
               M2_arrayintOrNull *result_powers);

void rawFactor2(const RingElement *f,
                const RingElement *minpoly,
                engine_RawRingElementArrayOrNull *result_factors,
                M2_arrayintOrNull *result_powers);

M2_arrayintOrNull rawIdealReorder(const Matrix *M);

engine_RawMatrixArrayOrNull rawCharSeries(const Matrix *M);

/* uses MPSolve */
engine_RawRingElementArrayOrNull rawRoots(const RingElement *g,
                                          long prec,
                                          int unique);

#  if defined(__cplusplus)
}
#  endif

#endif /* _factory_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
