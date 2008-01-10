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
  mpz_init_set_si(maxHeight, INITIALMAXINT);
  gmp_randinit_default(state);
}

void rawSetRandomSeed(M2_Integer newseed)
{
  gmp_randseed(state, newseed);


  int32_t s = mpz_get_si(newseed);
  s = s & 0x7fffffff;
  if (s == MASK) s = 0;
  RandomSeed = s ^ MASK;
}

void rawSetRandomMax(M2_Integer newHeight)
{
  mpz_set(maxHeight, newHeight);
}

int32_t rawRandomInt(int32_t max)
/* generate a random number in the range 0..max-1 */
{
  if (max <= 0) return 0;
  int32_t k = RandomSeed/IQ;
  RandomSeed = IA * (RandomSeed - k*IQ) - IR*k; /* Schrage algorithm to compute 
				       idum = (IA*idum) mod IM */
  if (RandomSeed < 0) RandomSeed += IM;
  return RandomSeed % max;
}

M2_Integer rawRandomInteger(M2_Integer maxN)
/* if height is the null pointer, use the default height */
{
  M2_Integer result = (M2_Integer)getmem(sizeof(__mpz_struct));
  mpz_init(result);
  if (maxN == 0)
    mpz_urandomm(result, state, maxHeight);
  else
    mpz_urandomm(result, state, maxN);
  return result;
}

M2_Rational rawRandomQQ(M2_Integer height)
  /* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */
{
  M2_Rational result = (M2_Rational)getmem(sizeof(__mpq_struct));
  mpq_init(result);
  if (height == 0) height = maxHeight;
  mpz_urandomm(mpq_numref(result), state, height);
  mpz_urandomm(mpq_denref(result), state, height);
  mpz_add_ui(mpq_numref(result), mpq_numref(result), 1);
  mpz_add_ui(mpq_denref(result), mpq_denref(result), 1);
  mpq_canonicalize(result);
  return result;
}

M2_RRR rawRandomRR(unsigned long precision)
  /* returns a uniformly distributed random real with the given precision, in range [0.0,1.0] */
{
  M2_RRR result = (M2_RRR)getmem(sizeof(__mpfr_struct));
  mpfr_init2(result,precision);
  mpfr_urandomb(result, state);
  return result;
}

M2_CCC rawRandomCC(unsigned long precision)
  /* returns a uniformly distributed random complex in the box [0.0,0.0], [1.0,1.0] */
{
  M2_CCC result = (M2_CCC)getmem(sizeof(struct M2_CCC_struct));
  result->re = rawRandomRR(precision);
  result->im = rawRandomRR(precision);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
