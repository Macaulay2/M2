// Copyright 1997 by Michael Stillman

#define IA 16807
#define IM 2147483647
#define IQ 127773
#define IR 2836
#define MASK 123459876

#include "random.hpp"
#include "Z.hpp"
#include "relem.hpp"
#include "gmp.h"

extern Z *ZZ;
int32 Random::seed;
int32 Random::maxint32;
RingElement *Random::maxint;

mpz_t Random::_maxN;
gmp_randstate_t Random::_st;

void Random::i_random()
{
  seed = MASK;
  maxint = RingElement::make_raw(ZZ, ZZ->from_int(10));
  maxint32 = 10;

  mpz_init_set_si(_maxN, 10);
  gmp_randinit_default(_st);
}

void Random::set_seed(int32 s)
{
  s = s & 0x7fffffff; // Used to be dome in cmd_random_seed
  if (s == MASK) s = 0;
  seed = s ^ MASK;

  gmp_randseed_ui(_st, (unsigned long int) seed);
}

void Random::set_max_int(RingElement *a)
{
  maxint = a;
  // If maxint is larger than 2^29 (somewhat arbitrary bound, but
  // less than the 2^31-1 by a factor of several...
  int cmp = mpz_cmp_si(MPZ_VAL(maxint->get_value()), IM/2);
  if (cmp <= 0)
    maxint32 = mpz_get_ui(MPZ_VAL(maxint->get_value()));
  else
    ERROR("max random integer is %d",IM/2);
}

RingElement *Random::get_max_int()
{
  return maxint;
}

int32 Random::random0()
{
  int32 k = seed/IQ;
  seed = IA * (seed - k*IQ) - IR*k; /* Schrage algorithm to compute 
				       idum = (IA*idum) mod IM */
  if (seed < 0) seed += IM;

  return seed;
}

extern "C"
int32 random00() {
     return Random::random0();
}

int32 Random::random0(int32 r)
{
  if (r <= 0) return 0;
  return random0() % r;
}

RingElement *Random::random()
{
  int result = random0(2*maxint32);
  result -= maxint32;
  return RingElement::make_raw(ZZ, ZZ->from_int(result));
}

void Random::set_max_int(M2_Integer N)
{
  mpz_set(_maxN, N);
}

void Random::random_integer(M2_Integer a)
  // a should be an mpz_t which has been initialized
{
  mpz_urandomm(a, _st, _maxN);
}
