// Copyright 2008 by Michael Stillman

#include "interface/random.h"
#include "interface/gmp-util.h"

#include "exceptions.hpp"
#include "error.h"

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
  mpz_ptr result = getmemstructtype(mpz_ptr);
  mpz_init(result);
  if (maxN == nullptr)
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

void rawSetFareyApproximation(mpq_ptr result, gmp_RR x, gmp_ZZ height)
/* sets result = the nearest rational to x w/ denominator <= height */
/* see https://en.wikipedia.org/wiki/Farey_sequence                 */
{
  int sgn;
  mpfr_t fracpart, tmp1, tmp2;
  mpz_t intpart, a, b, c, d, q, p;
  mpq_t tmp3;

  // get integer and fractional parts of |x|
  sgn = mpfr_sgn(x);
  mpfr_init2(fracpart, mpfr_get_prec(x));
  mpfr_abs(fracpart, x, MPFR_RNDN);
  mpz_init(intpart);
  mpfr_get_z(intpart, fracpart, MPFR_RNDD);
  mpfr_frac(fracpart, fracpart, MPFR_RNDN);

  // goal: find nearest rational number to fracpart
  // start w/ a/b = 0, p/q = 1/2, c/d = 1
  mpfr_init2(tmp1, mpfr_get_prec(x));
  mpfr_init2(tmp2, mpfr_get_prec(x));
  mpz_init_set_ui(a, 0);
  mpz_init_set_ui(b, 1);
  mpz_init_set_ui(c, 1);
  mpz_init_set_ui(d, 1);
  mpz_init_set_ui(p, 1);
  mpz_init_set_ui(q, 2);
  mpq_init(tmp3);

  // compute mediant p/q = (a+c)/(b+d) until q > height
  while (mpz_cmp(q, height) <= 0) {
    mpfr_mul_z(tmp1, fracpart, q, MPFR_RNDN);
    // check if fracpart is to the left or right of p/q
    // and update a/b or c/d accordingly
    if (mpfr_cmp_z(tmp1, p) <= 0) {
      mpz_set(c, p);
      mpz_set(d, q);
    } else {
      mpz_set(a, p);
      mpz_set(b, q);
    }
    mpz_add(p, a, c);
    mpz_add(q, b, d);
  }

  // now check which endpoint is closest
  // tmp1 = fracpart - a/b
  mpfr_set_z(tmp1, a, MPFR_RNDN);
  mpfr_neg(tmp1, tmp1, MPFR_RNDN);
  mpfr_div_z(tmp1, tmp1, b, MPFR_RNDN);
  mpfr_add(tmp1, tmp1, fracpart, MPFR_RNDN);

  // tmp2 = c/d - fracpart
  mpfr_set_z(tmp2, c, MPFR_RNDN);
  mpfr_div_z(tmp2, tmp2, d, MPFR_RNDN);
  mpfr_sub(tmp2, tmp2, fracpart, MPFR_RNDN);

  if (mpfr_cmp(tmp1, tmp2) <= 0) {
    mpq_set_z(result, a);
    mpq_set_z(tmp3, b);
  } else {
    mpq_set_z(result, c);
    mpq_set_z(tmp3, d);
  }

  // finally, add back intpart and negate if x < 0
  mpq_div(result, result, tmp3);
  mpq_set_z(tmp3, intpart);
  mpq_add(result, result, tmp3);
  mpq_set_si(tmp3, sgn, 1);
  mpq_mul(result, result, tmp3);
  mpq_canonicalize(result);

  mpz_clears(intpart, a, b, c, d, p, q, nullptr);
  mpfr_clears(fracpart, tmp1, tmp2, nullptr);
  mpq_clear(tmp3);
}

gmp_QQ rawFareyApproximation(gmp_RR x, gmp_ZZ height)
/* returns the nearest rational to x w/ denominator <= height */
{
  mpq_ptr result = getmemstructtype(mpq_ptr);
  mpq_init(result);
  rawSetFareyApproximation(result, x, height);
  return moveTo_gmpQQ(result);
}

void rawSetRandomQQ(mpq_ptr result, gmp_ZZ height)
/* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */
{
  mpz_t d;

  mpz_init(d);
  if (height == nullptr) height = maxHeight;
  if (mpz_cmp_si(height, 0) <= 0)
    throw exc::engine_error("expected a positive height");

  while (true) {
    mpz_urandomm(mpq_numref(result), state, height);
    mpz_urandomm(mpq_denref(result), state, height);
    mpz_add_ui(mpq_numref(result), mpq_numref(result), 1);
    mpz_add_ui(mpq_denref(result), mpq_denref(result), 1);
    mpz_gcd(d, mpq_numref(result), mpq_denref(result));
    if (mpz_cmp_ui(d, 1) == 0)
      break;
  }

  mpz_clear(d);
}

gmp_QQ rawRandomQQ(gmp_ZZ height)
/* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
/* if height is the null pointer, use the default height */
{
  mpq_ptr result = getmemstructtype(mpq_ptr);
  mpq_init(result);

  try {
    rawSetRandomQQ(result, height);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }

  return moveTo_gmpQQ(result);
}

gmp_RR rawRandomRRUniform(unsigned long precision)
/* returns a uniformly distributed random real with the given precision, in
 * range [0.0,1.0] */
{
  mpfr_ptr result = getmemstructtype(mpfr_ptr);
  mpfr_init2(result, precision);
  mpfr_urandomb(result, state);
  return moveTo_gmpRR(result);
}

gmp_RR rawRandomRRNormal(unsigned long precision)
/* returns a normally distributed random real with the given precision */
{
  mpfr_ptr result = getmemstructtype(mpfr_ptr);
  mpfr_init2(result, precision);
  mpfr_nrandom(result, state, MPFR_RNDN);
  return moveTo_gmpRR(result);
}

gmp_CC rawRandomCC(unsigned long precision)
/* returns a uniformly distributed random complex in the box [0.0,0.0],
 * [1.0,1.0] */
{
  gmp_CCmutable result = getmemstructtype(gmp_CCmutable);
  result->re = const_cast<gmp_RRmutable>(rawRandomRRUniform(precision));
  result->im = const_cast<gmp_RRmutable>(rawRandomRRUniform(precision));
  return reinterpret_cast<gmp_CC>(result);
}

void randomMpfr(mpfr_t result)
/* returns a uniformly distributed random real with the given precision, in
 * range [0.0,1.0] 
 * (result is assumed to be initialized) */
{
  mpfr_urandomb(result, state);
}

double randomDouble()
{
  mpfr_t val;
  mpfr_init2(val, 53);
  randomMpfr(val);
  double result = mpfr_get_d(val, MPFR_RNDN);
  mpfr_clear(val);
  return result;
}

int system_randomint()
{
#if 0
  extern long random();
  return random();
#elif 0
  extern long random00();
  return random00();
#else
  return rawRandomInt((int32_t)~0 >> 1);
#endif
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
