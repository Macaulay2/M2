// Copyright 1997 by Michael Stillman

#define IA 16807
#define IM 2147483647
#define IQ 127773
#define IR 2836
#define MASK 123459876

#include "random.hpp"
#include "Z.hpp"

extern Z *ZZ;
long Random::seed;
long Random::maxlong;
RingElement Random::maxint;

void Random::i_random()
{
  seed = MASK;
  maxint = RingElement(ZZ,10);
  maxlong = 10;
}

void Random::set_seed(long s)
{
  if (s == MASK) s = 0;
  seed = s ^ MASK;
}

void Random::set_max_int(RingElement a)
{
  maxint = a;
  // If maxint is larger than 2^29 (somewhat arbitrary bound, but
  // less than the 2^31-1 by a factor of several...
  int cmp = mpz_cmp_si(MPZ_VAL(maxint.get_value()), IM/2);
  if (cmp <= 0)
    maxlong = mpz_get_ui(MPZ_VAL(maxint.get_value()));
  else
    *gError << "max random integer is " << IM/2;
}

RingElement Random::get_max_int()
{
  return maxint;
}

long Random::random0()
{
  long k = seed/IQ;
  seed = IA * (seed - k*IQ) - IR*k; /* Schrage algorithm to compute 
				       idum = (IA*idum) mod IM */
  if (seed < 0) seed += IM;

  return seed;
}

extern "C"
long random00() {
     return Random::random0();
}

long Random::random0(long r)
{
  if (r <= 0) return 0;
  return random0() % r;
}

RingElement Random::random()
{
  int result = random0(2*maxlong);
  result -= maxlong;
  return RingElement(ZZ,result);
}
