// Copyright 1997 by Michael Stillman

#define IA 16807
#define IM 2147483647
#define IQ 127773
#define IR 2836
#define MASK 123459876

#include "random.hpp"
#include "ZZ.hpp"
#include "relem.hpp"
#include "gmp.h"

extern ZZ *globalZZ;
int32 RandomSeed = MASK;

#define INITIALMAXINT 10
int32 Random::maxint32 = INITIALMAXINT;
int32 RandomFoo::maxNint = INITIALMAXINT;
bool RandomFoo::maxNisSmall = true;

RingElement *Random::maxint;

mpz_t Random::maxN;
gmp_randstate_t Random::state;

void Random::i_random()
{
  maxint = RingElement::make_raw(globalZZ, globalZZ->from_int(maxint32));
  mpz_init_set_si(maxN, maxint32);
  gmp_randinit_default(state);
}

int32 Random::set_seed(M2_Integer newseed)
{
  int32 old = RandomSeed;
  int32 s = mpz_get_si(newseed);
  s = s & 0x7fffffff; // Used to be dome in cmd_random_seed
  if (s == MASK) s = 0;
  RandomSeed = s ^ MASK;

  gmp_randseed(state, newseed);
  return old;
}

int32 Random::random0()
{
  int32 k = RandomSeed/IQ;
  RandomSeed = IA * (RandomSeed - k*IQ) - IR*k; /* Schrage algorithm to compute 
				       idum = (IA*idum) mod IM */
  if (RandomSeed < 0) RandomSeed += IM;

  return RandomSeed;
}

int32 Random::random0(int32 r)
{
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

M2_Integer Random::get_random_integer(M2_Integer mxN)
{
  M2_Integer result = newitem(__mpz_struct);
  if (mpz_fits_sint_p(mxN)) {
       int32 n = mpz_get_si(mxN);
       mpz_init_set_si(result,Random::random0(n));
  }
  else {
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
      mpz_init_set_si(result,Random::random0(maxNint));
    }
  else
    {
      mpz_init(result);
      mpz_urandomm(result, state, maxN);
    }
}

extern "C"
int32 random00() {
     return Random::random0();
}

RingElement *Random::random()
{
  int result = random0(2*maxint32);
  result -= maxint32;
  return RingElement::make_raw(globalZZ, globalZZ->from_int(result));
}
#if 0
void Random::set_max_int(RingElement *a)
{
  maxint = a;
  // If maxint is larger than 2^29 (somewhat arbitrary bound, but
  // less than the 2^31-1 by a factor of several...
  int cmp = mask_mpz_cmp_si(MPZ_VAL(maxint->get_value()), IM/2);
  if (cmp <= 0)
    maxint32 = mpz_get_ui(MPZ_VAL(maxint->get_value()));
  else
    ERROR("max random integer is %d",IM/2);
}
#endif
RingElement *Random::get_max_int()
{
  return maxint;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
