#ifndef _factory_h_
#  define _factory_h_

/**
 * @file interface/factory.h
 * @brief Engine-boundary C API for polynomial GCD, factorisation, and root finding.
 *
 * Declares the `extern "C"` entry points the M2 interpreter
 * routes to Singular's Factory library (and MPSolve for
 * numerical complex roots). GCDs:
 * `rawGCDRingElement(f, g, mipo, inExtension)` computes the
 * GCD, with the optional minimal polynomial `mipo` and
 * `inExtension` flag selecting computation over an algebraic
 * extension; `rawExtendedGCDRingElement(f, g, **A, **B)`
 * returns the GCD via the regular return path and fills the
 * Bezout coefficients `A`, `B` (with `A*f + B*g = gcd`)
 * through output pointers --- no extension support on this
 * variant. `rawPseudoRemainder(f, g)` performs pseudo-division
 * (leading-coefficient scaling so the result stays in the
 * integral domain).
 *
 * Factorisation: `rawFactor` and `rawFactor2` decompose a
 * `RingElement` into `(factors, powers)` output arrays,
 * with `rawFactor2` additionally accepting a minimal polynomial
 * so the factorisation runs over an algebraic extension.
 * `rawIdealReorder` / `rawCharSeries` do characteristic-set
 * preprocessing of polynomial ideals. `rawRoots(g, prec,
 * unique)` returns arbitrary-precision numerical roots of a
 * univariate polynomial via MPSolve.
 *
 * If Factory or MPSolve is absent at build time the
 * corresponding entries return null and the M2 layer reports
 * "feature not available."
 *
 * @see factory.cpp
 * @see flint.h
 * @see engine-includes.hpp
 */

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
