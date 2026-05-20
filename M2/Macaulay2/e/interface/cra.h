#ifndef _cra_h_
#  define _cra_h_

/**
 * @file interface/cra.h
 * @brief Engine-boundary C API for Chinese-remainder lifting and rational reconstruction.
 *
 * Declares the `extern "C"` entry points the M2 interpreter
 * calls to combine modular results from independent prime-field
 * computations and lift them back to characteristic 0:
 * `rawRingElementCRA` / `rawMatrixCRA` merge two CRT residues
 * with moduli `m` and `n` into a single residue mod `m*n`, and
 * `rawRingElementRatConversion` / `rawMatrixRatConversion` run
 * rational reconstruction --- recovering the unique `a/b` with
 * `|a|, |b| <= sqrt(m)/2` --- against a target `RQ` polynomial
 * ring whose coefficients are `QQ`. The moduli arrive as GMP
 * `mpz_srcptr` so the front end can pass arbitrarily large
 * primes (or products of primes) without an extra conversion.
 *
 * Heavily used by the modular-F4 path and by rational
 * resolutions / Hilbert-function computations that run the same
 * algorithm modulo many primes in parallel and reconstruct
 * afterwards.
 *
 * @see cra.cpp
 * @see engine-includes.hpp
 */

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Matrix;
class Ring;
class RingElement;
#  else
typedef struct Matrix Matrix;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
#  endif

/**
   Chinese remainder and rational reconstruction interface routines 
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

const RingElement *rawRingElementCRA(const RingElement *f,
                                     const RingElement *g,
                                     mpz_srcptr m,
                                     mpz_srcptr n);

const Matrix *rawMatrixCRA(const Matrix *f,
                           const Matrix *g,
                           mpz_srcptr m,
                           mpz_srcptr n);

// f should be an element in the polynomial ring R (over ZZ).
// RQ should be the same ring as R, but with rational coefficients
const RingElement *rawRingElementRatConversion(const RingElement *f,
                                               mpz_srcptr m,
                                               const Ring *RQ);

// f should be a matrix in the polynomial ring R (over ZZ).
// RQ should be the same ring as R, but with rational coefficients
const Matrix *rawMatrixRatConversion(const Matrix *f,
                                     mpz_srcptr m,
                                     const Ring *RQ);

#  if defined(__cplusplus)
}
#  endif

#endif /* _cra_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
