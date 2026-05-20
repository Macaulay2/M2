#ifndef _flint_h_
#  define _flint_h_

/**
 * @file interface/flint.h
 * @brief Engine-boundary C API exposing FLINT's integer primality and factorisation services.
 *
 * Declares the three `extern "C"` entry points the M2
 * interpreter routes to FLINT for big-integer number theory:
 * `rawZZisPrime` (exact primality via `fmpz_is_prime` ---
 * errors out via `ERROR` if FLINT returns a negative status),
 * `rawZZisProbablePrime` (probable-primality via FLINT's
 * `fmpz_is_probabprime`, which combines several tests internally),
 * and `rawZZfactor` (full integer factorisation through
 * `fmpz_factor`). Inputs arrive as `gmp_ZZ`; the
 * implementation handles the GMP-to-FLINT
 * `fmpz_set_mpz` / `fmpz_clear` round-trip internally so the
 * interpreter never sees `fmpz_t`. `rawZZfactor` returns a
 * `gmp_arrayZZ` of total length `2*num_factors + 1` shaped
 * `[sign, prime_1, mult_1, prime_2, mult_2, ...]` --- the
 * leading element is the input's sign, not a prime.
 *
 * FLINT is consumed pervasively *inside* the engine (see the
 * `aring-*-flint` and `dmat-*-flint` families), but this
 * header is the only place where FLINT-backed services are
 * exposed directly to the interpreter; everything else stays
 * private to its owning class.
 *
 * @see flint.cpp
 * @see factory.h
 * @see engine-includes.hpp
 */

#  include "engine-includes.hpp"

/**
   Integer primality and factorization interface routines via Flint
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

M2_bool rawZZisPrime(gmp_ZZ a);

M2_bool rawZZisProbablePrime(gmp_ZZ a);

gmp_arrayZZ rawZZfactor(gmp_ZZ a);

#  if defined(__cplusplus)
}
#  endif

#endif /* _flint_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
