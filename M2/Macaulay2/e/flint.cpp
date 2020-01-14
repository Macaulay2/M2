#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/arith.h>
#include <flint/fmpz.h>
#pragma GCC diagnostic pop

bool flintIsPrime(gmp_ZZ a)
{
}

bool flintIsPseudoprime(gmp_ZZ a)
{
}

util_arrayZZ flintToFrontend(std::vector<fmpz_t>); // this function copies data to front end type.

std::vector<mpz_srcptr> flintFactorInteger(mpz_srcptr x)
{
  fmpz_t n;
  fmpz_set_mpz(n, x);
  fmpz_factor_t factor;
  fmpz_factor_init(factor);
  fmpz_factor(factor,n);
  int len = factor->num;
  gmp_arrayZZ result = getmemarraytype(gmp_arrayZZ,2*len+1);
  result->len = 2*len+1;
  __mpz_struct *tmp;
  tmp = (void *)getmem(sizeof(__mpz_struct));
  mpz_init(tmp);
  mpz_set_si(tmp, factor->sign);
  result->array[0] = tmp;
  for (i=0; i<len; i++) {
    tmp = (void *)getmem(sizeof(__mpz_struct));
    mpz_init(tmp);
    fmpz_get_mpz(tmp,factor->p + i);
    result->array[2*i+1] = tmp;
    tmp = (void *)getmem(sizeof(__mpz_struct));
    mpz_init(tmp);
    fmpz_get_mpz(tmp,(fmpz *)(factor->exp + i));
    result->array[2*i+2] = tmp;
  }
  fmpz_factor_clear(factor);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
