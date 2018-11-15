// Copyright 2008 by Michael Stillman

#include "rand.h"
#include "../d/M2mem.h"

#define INITIALMAXINT 10

#define IA 16807
#define IM 2147483647
#define IQ 127773
#define IR 2836
#define MASK 123459876

static mpz_t maxHeight;
static gmp_randstate_t state;
static int32_t RandomSeed = MASK;

void rawRandomInitialize()
{
  RandomSeed = MASK;
  mpz_init_set_si(maxHeight, INITIALMAXINT);
  gmp_randinit_default(state);
}

void rawSetRandomSeed(gmp_ZZ newseed)
{
  gmp_randseed(state, newseed);

  int32_t s = mpz_get_si(newseed);
  s = s & 0x7fffffff;
  if (s == MASK) s = 0;
  RandomSeed = s ^ MASK;
}

void rawSetRandomMax(gmp_ZZ newHeight) { mpz_set(maxHeight, newHeight); }
unsigned long rawRandomULong(unsigned long max)
{
  return gmp_urandomm_ui(state, max);
}

int32_t rawRandomInt(int32_t max)
/* generate a random number in the range 0..max-1 */
{
  if (max <= 0) return 0;
  int32_t k = RandomSeed / IQ;
  RandomSeed =
      IA * (RandomSeed - k * IQ) - IR * k; /* Schrage algorithm to compute
                                  idum = (IA*idum) mod IM */
  if (RandomSeed < 0) RandomSeed += IM;
  return RandomSeed % max;
}

gmp_ZZ rawRandomInteger(gmp_ZZ maxN)
/* if height is the null pointer, use the default height */
{
  gmp_ZZ result = getmemstructtype(gmp_ZZ);
  mpz_init(result);
  if (maxN == 0)
    mpz_urandomm(result, state, maxHeight);
  else if (1 != mpz_sgn(maxN))
    {
      mpz_set_si(result, 0);
    }
  else
    mpz_urandomm(result, state, maxN);
  return result;
}

gmp_QQ rawRandomQQ(gmp_ZZ height)
/* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */
{
  gmp_QQ result = getmemstructtype(gmp_QQ);
  mpq_init(result);
  if (height == 0) height = maxHeight;
  mpz_urandomm(mpq_numref(result), state, height);
  mpz_urandomm(mpq_denref(result), state, height);
  mpz_add_ui(mpq_numref(result), mpq_numref(result), 1);
  mpz_add_ui(mpq_denref(result), mpq_denref(result), 1);
  mpq_canonicalize(result);
  return result;
}

void rawSetRandomQQ(mpq_ptr result, gmp_ZZ height)
/* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */
{
  if (height == 0) height = maxHeight;
  mpz_urandomm(mpq_numref(result), state, height);
  mpz_urandomm(mpq_denref(result), state, height);
  mpz_add_ui(mpq_numref(result), mpq_numref(result), 1);
  mpz_add_ui(mpq_denref(result), mpq_denref(result), 1);
  mpq_canonicalize(result);
}

gmp_RR rawRandomRR(unsigned long precision)
/* returns a uniformly distributed random real with the given precision, in
 * range [0.0,1.0] */
{
  gmp_RR result = getmemstructtype(gmp_RR);
  mpfr_init2(result, precision);
  mpfr_urandomb(result, state);
  return result;
}

void rawRandomMpfr(mpfr_t result, unsigned long precision)
/* returns a uniformly distributed random real with the given precision, in
 * range [0.0,1.0] */
{
  mpfr_init2(result, precision);
  mpfr_urandomb(result, state);
}

gmp_CC rawRandomCC(unsigned long precision)
/* returns a uniformly distributed random complex in the box [0.0,0.0],
 * [1.0,1.0] */
{
  gmp_CC result = getmemstructtype(gmp_CC);
  result->re = rawRandomRR(precision);
  result->im = rawRandomRR(precision);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
