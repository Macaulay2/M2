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
int32 Random::seed;
int32 Random::maxint32;
RingElement *Random::maxint;

mpz_t Random::_maxN;
gmp_randstate_t Random::_st;

void Random::i_random()
{
  seed = MASK;
  maxint = RingElement::make_raw(globalZZ, globalZZ->from_int(10));
  maxint32 = 100;

  mpz_init_set_si(_maxN, 100);
  gmp_randinit_default(_st);
}

void Random::set_seed(M2_Integer newseed)
{
  int32 s = mpz_get_si(newseed);
  s = s & 0x7fffffff; // Used to be dome in cmd_random_seed
  if (s == MASK) s = 0;
  seed = s ^ MASK;

  gmp_randseed(_st, newseed);
}

int32 Random::random0()
{
  int32 k = seed/IQ;
  seed = IA * (seed - k*IQ) - IR*k; /* Schrage algorithm to compute 
				       idum = (IA*idum) mod IM */
  if (seed < 0) seed += IM;

  return seed;
}

int32 Random::random0(int32 r)
{
  if (r <= 0) return 0;
  return random0() % r;
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

M2_Integer Random::get_random_integer(M2_Integer maxN)
{
  M2_Integer result = newitem(__mpz_struct);
  if (mpz_fits_sint_p(maxN))
    {
      int32 maxval = mpz_get_si(maxN);
      int32 r = Random::random0(maxval);
      mpz_init_set_si(result,r);
    }
  else
    {
      mpz_init(result);
      mpz_urandomm(result, _st, maxN);
    }
  return result;
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
