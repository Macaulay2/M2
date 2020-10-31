// Copyright 2008 by Michael Stillman

#ifndef _rand_h_
#  define _rand_h_

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

gmp_ZZ rawRandomInteger(gmp_ZZ maxN);
/* if height is the null pointer, use the default height */

gmp_QQ rawRandomQQ(gmp_ZZ height);
/* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */

void rawSetRandomQQ(mpq_ptr result, gmp_ZZ height);
/* sets result = random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */

gmp_RR rawRandomRR(unsigned long prec);
/* returns a uniformly distributed random real with the given precision, in
 * range [0.0,1.0] */

gmp_CC rawRandomCC(unsigned long prec);

void randomMpfr(mpfr_t result);

double randomDouble();

int system_randomint();

#  if defined(__cplusplus)
}
#  endif

#endif

// Local Variables:
// indent-tabs-mode: nil
// End:
