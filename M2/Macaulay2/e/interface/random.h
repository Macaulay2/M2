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

void rawSetFareyApproximation(mpq_ptr result, gmp_RR x, gmp_ZZ height);
/* sets result = the nearest rational to x w/ denominator <= height */

gmp_QQ rawFareyApproximation(gmp_RR x, gmp_ZZ height);
/* returns the nearest rational to x w/ denominator <= height */

gmp_QQ rawRandomQQ(gmp_ZZ height);
/* returns a  sample from the uniform distribution on [0, height], */
/* rounded to the nearest rational number with denominator bounded by height */

void rawSetRandomQQ(mpq_ptr result, gmp_ZZ height);
/* sets result = a sample from the uniform distribution on [0, height], */
/* rounded to the nearest rational number with denominator bounded by height */

gmp_RR rawRandomRRUniform(unsigned long prec);
/* returns a uniformly distributed random real with the given precision, in
 * range [0.0,1.0] */

gmp_RR rawRandomRRNormal(unsigned long prec);
/* returns a normally distributed random real with the given precision */

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
