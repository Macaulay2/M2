// Copyright 1997 by Michael Stillman

#define IA 16807
#define IM 2147483647
#define IQ 127773
#define IR 2836
#define MASK 123459876

#include "random.hpp"
#include "ZZ.hpp"
#include "relem.hpp"
#include <gmp.h>
#include <mpfr.h>

extern RingZZ *globalZZ;
int32_t RandomSeed = MASK;

#define INITIALMAXINT 10
int32_t Random::maxNint = INITIALMAXINT;
bool Random::maxNisSmall = true;

RingElement *Random::maxint;

mpz_t Random::maxN;
gmp_randstate_t Random::state;

void Random::i_random()
{
  maxint = RingElement::make_raw(globalZZ, globalZZ->from_int(INITIALMAXINT));
  mpz_init_set_si(maxN, INITIALMAXINT);
  gmp_randinit_default(state);
}

int32_t Random::set_seed(M2_Integer newseed)
{
  int32_t old = RandomSeed;
  int32_t s = mpz_get_si(newseed);
  s = s & 0x7fffffff; // Used to be dome in cmd_random_seed
  if (s == MASK) s = 0;
  RandomSeed = s ^ MASK;

  gmp_randseed(state, newseed);
  return old;
}

int32_t Random::random0()
{
  int32_t k = RandomSeed/IQ;
  RandomSeed = IA * (RandomSeed - k*IQ) - IR*k; /* Schrage algorithm to compute 
				       idum = (IA*idum) mod IM */
  if (RandomSeed < 0) RandomSeed += IM;

  return RandomSeed;
}

int32_t Random::random0(int32_t r)
{
     // this routine returns a number in the range 0 .. r-1
  if (r <= 0) return 0;
  return random0() % r;
}

void Random::set_max_int(M2_Integer N)
{
  mpz_set(maxN, N);
  if (mpz_fits_sint_p(maxN)) {
       maxNisSmall = true;
       maxNint = mpz_get_si(maxN);
  }
  else {
       maxNisSmall = false;
       maxNint = 0;
  }
}

M2_Integer Random::get_random_integer(M2_Integer mxN) // this one is used for random integers internally in random matrices and polynomials
{
  M2_Integer result = newitem(__mpz_struct);
  if (mpz_fits_sint_p(mxN)) {
       // return a number in the range -mxN .. mxN-1
       int32_t n = mpz_get_si(mxN);
       mpz_init_set_si(result,Random::random0(2*n)-n);
  }
  else {
       // return a number in the range 0 .. mxN-1
       mpz_init(result);
       mpz_urandomm(result, state, mxN);
  }
  return result;
}

M2_Integer Random::get_random_integer_0(M2_Integer mxN)	// this one is used for random integers at top level
{
  M2_Integer result = newitem(__mpz_struct);
  if (mpz_fits_sint_p(mxN)) {
       // return a number in the range 0 .. mxN-1
       int32_t n = mpz_get_si(mxN);
       mpz_init_set_si(result,Random::random0(n));
  }
  else {
       // return a number in the range 0 .. mxN-1
       mpz_init(result);
       mpz_urandomm(result, state, mxN);
  }
  return result;
}

void Random::random_integer(M2_Integer result)
  // this is a destructive version of get_random_integer
  // a should be an mpz_t which has been initialized
{
  if (maxNisSmall)
    {
      // return a number in the range -maxNint .. maxNint-1
      int32_t r = Random::random0(2*maxNint)-maxNint;
      mpz_init_set_si(result,r);
    }
  else
    {
      // return a number in the range 0 .. maxN-1
      mpz_init(result);
      mpz_urandomm(result, state, maxN);
    }
}

extern "C"
int32_t random00() {
     return Random::random0();
}

RingElement *Random::random()
{
     // this routine returns a number in the range -maxNint .. maxNint-1
  int result = random0(2*maxNint);
  result -= maxNint;
  return RingElement::make_raw(globalZZ, globalZZ->from_int(result));
}

RingElement *Random::get_max_int()
{
  return maxint;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
