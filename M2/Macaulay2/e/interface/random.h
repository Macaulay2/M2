// Copyright 2008 by Michael Stillman

#ifndef _rand_h_
#  define _rand_h_

/**
 * @file interface/random.h
 * @brief Engine-boundary C API for the engine's PRNG and rational / real / complex random draws.
 *
 * Declares the `extern "C"` entry points that seed and consult
 * the engine's pseudo-random number generator:
 * `rawRandomInitialize` resets it, `rawSetRandomSeed(gmp_ZZ)`
 * plants a reproducible seed, `rawSetRandomMax(gmp_ZZ)` installs
 * the default upper bound for integer draws, and the typed
 * draws cover every coefficient flavour the engine supports ---
 * `rawRandomULong` / `rawRandomInt` for sub-word integers,
 * `rawRandomInteger` / `rawSetRandomInteger` for big integers
 * (the latter writing through an `mpz_ptr` to avoid the
 * garbage-collected allocation),
 * `rawRandomQQ` / `rawSetRandomQQ` for rationals with bounded
 * numerator and denominator,
 * `rawRandomRRUniform` / `rawRandomRRNormal` / `rawRandomCC`
 * for arbitrary-precision real and complex variates,
 * `rawRandomRRi` / `rawSetRandomRRi` / `rawRandomCCi` for
 * interval variants, the MPFR-direct `randomMpfr`, and
 * `randomDouble` / `system_randomint` for the C fast paths.
 * Farey-approximation helpers (`rawFareyApproximation`,
 * `rawSetFareyApproximation`) sit here since they reuse the
 * same big-integer height parameter.
 *
 * The PRNG state is a triple of file-scope statics in
 * `random.cpp` (the GMP `gmp_randstate_t state`, the
 * `int32_t RandomSeed` Schrage-style seed for
 * `rawRandomInt`, and the `mpz_t maxHeight` default upper
 * bound) --- there is no `thread_local` or mutex protection,
 * so single-threaded runs are reproducible from a given seed
 * but concurrent threads share state and a parallel run is
 * neither race-free nor reproducible. The `aring-*` coefficient
 * backends pick this up via `randomDouble` / `rawRandomInteger`.
 *
 * @see random.cpp
 * @see engine-includes.hpp
 */

#  include "engine-includes.hpp"

/**
   Randomization interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

void rawRandomInitialize();

void rawSetRandomSeed(gmp_ZZ newseed);

void rawSetRandomMax(gmp_ZZ);

unsigned long rawRandomULong(unsigned long max);
/* generate a random number in the range 0..max-1 */

int32_t rawRandomInt(int32_t max);
/* generate a random number in the range 0..max-1 */

void rawSetRandomInteger(mpz_ptr result, gmp_ZZ maxN);
/* if height is the null pointer, use the default height */
/* doesn't deal w/ garbage collection */

gmp_ZZ rawRandomInteger(gmp_ZZ maxN);
/* if height is the null pointer, use the default height */
/* returns garbage-collected memory */

void rawSetFareyApproximation(mpq_ptr result, gmp_RR x, gmp_ZZ height);
/* sets result = the nearest rational to x w/ denominator <= height */

gmp_QQ rawFareyApproximation(gmp_RR x, gmp_ZZ height);
/* returns the nearest rational to x w/ denominator <= height */

gmp_QQ rawRandomQQ(gmp_ZZ height);
/* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */

void rawSetRandomQQ(mpq_ptr result, gmp_ZZ height);
/* sets result = random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */

gmp_RR rawRandomRRUniform(unsigned long prec);
/* returns a uniformly distributed random real with the given precision, in
 * range [0.0,1.0] */

gmp_RR rawRandomRRNormal(unsigned long prec);
/* returns a normally distributed random real with the given precision */

gmp_CC rawRandomCC(unsigned long prec);

void randomMpfr(mpfr_t result);

gmp_RRi rawRandomRRi(unsigned long prec);
void rawSetRandomRRi(mpfi_ptr result);
/* returns an interval with the given precision containing two random
 * variates from the uniform distribution on [0, 1] */

gmp_CCi rawRandomCCi(unsigned long prec);
/* returns a complex interval with the given precision whose real and imaginary
 * parts each contain two random variates from the uniform distribution on
 * [0, 1] */

double randomDouble();

int system_randomint();

#  if defined(__cplusplus)
}
#  endif

#endif

// Local Variables:
// indent-tabs-mode: nil
// End:
