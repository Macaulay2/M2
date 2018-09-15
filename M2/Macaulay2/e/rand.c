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

void mpfr_reallocate_limbs (mpfr_ptr _z)
{
  __mpfr_struct tmp;
  tmp = *_z;
  mp_limb_t *p = (mp_limb_t*) GC_MALLOC(tmp._mpfr_prec * sizeof(mp_limb_t));
  memcpy(p, _z->_mpfr_d, tmp._mpfr_prec * sizeof(mp_limb_t));
  mpfr_clear(_z);
  _z->_mpfr_prec = tmp._mpfr_prec;
  _z->_mpfr_sign = tmp._mpfr_sign;
  _z->_mpfr_exp = tmp._mpfr_exp;
  _z->_mpfr_d = p;
}

inline void mpz_reallocate_limbs (mpz_ptr _z)
{ 
  int _s = _z->_mp_size;
  int _as = (_s>0)?_s:-_s;
  mp_limb_t *_p = (mp_limb_t*) GC_MALLOC(_as*sizeof(mp_limb_t));
  memcpy(_p,_z->_mp_d,_as*sizeof(mp_limb_t));
  mpz_clear(_z);
  _z->_mp_d = _p;
  _z->_mp_size = _s;
  _z->_mp_alloc = _as;
}

inline gmp_QQ moveTo_gmpQQ (mpq_ptr z)
{
  mpz_reallocate_limbs(mpq_numref(z));
  mpz_reallocate_limbs(mpq_denref(z));
  return z;
}

mpfr_srcptr moveTo_gmpRR (mpfr_ptr _z)
{
  mpfr_reallocate_limbs(_z);
  return _z;
}

inline gmp_CC moveTo_gmpCC (cc_ptr _z)
{
  CCmutable_struct* a = (CCmutable_struct*) _z;
  mpfr_reallocate_limbs(a->re);
  mpfr_reallocate_limbs(a->im);
  return (gmp_CC) a;
}

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
  mpz_ptr result = getmemstructtype(mpz_ptr);
  mpz_init(result);
  if (maxN == 0)
    mpz_urandomm(result, state, maxHeight);
  else if (1 != mpz_sgn(maxN))
    {
      mpz_set_si(result, 0);
    }
  else
    mpz_urandomm(result, state, maxN);
  mpz_reallocate_limbs(result);
  return result;
}

gmp_QQ rawRandomQQ(gmp_ZZ height)
/* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */
{
  mpq_ptr result = getmemstructtype(mpq_ptr);
  mpq_init(result);
  if (height == 0) height = maxHeight;
  mpz_urandomm(mpq_numref(result), state, height);
  mpz_urandomm(mpq_denref(result), state, height);
  mpz_add_ui(mpq_numref(result), mpq_numref(result), 1);
  mpz_add_ui(mpq_denref(result), mpq_denref(result), 1);
  mpq_canonicalize(result);
  return moveTo_gmpQQ(result);
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
  mpfr_ptr result = getmemstructtype(mpfr_ptr);
  mpfr_init2(result, precision);
  mpfr_urandomb(result, state);
  return moveTo_gmpRR(result);
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
  cc_ptr result = getmemstructtype(cc_ptr);
  mpfr_init2(& result->re, precision);
  mpfr_init2(& result->im, precision);
  rawRandomMpfr(& result->re, precision);
  rawRandomMpfr(& result->im, precision);
  return moveTo_gmpCC(result);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
