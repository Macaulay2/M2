#include "interface/flint.h"
#include "interface/gmp-util.h"

#include <iostream>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/arith.h>
#include <flint/fmpz.h>
#pragma GCC diagnostic pop

#include "error.h"

M2_bool rawZZisPrime(gmp_ZZ a)
{
  fmpz_t n;
  fmpz_init(n);
  fmpz_set_mpz(n, a);
  auto ret = fmpz_is_prime(n);
  fmpz_clear(n);
  if(ret<0) {
    ERROR("flint's is_prime failed");
    std::cout << "fmpz_is_prime returned " << ret << std::endl;
  }
  return ret;
}

M2_bool rawZZisProbablePrime(gmp_ZZ a)
{
  fmpz_t n;
  fmpz_init(n);
  fmpz_set_mpz(n, a);
  int ret = fmpz_is_probabprime(n);
  fmpz_clear(n);
  return ret;
}

// TODO: not yet implemented or connected to top level
// This function is intended to copy data to front end type.
gmp_arrayZZ flintToFrontend(std::vector<fmpz_t>);

gmp_arrayZZ rawZZfactor(gmp_ZZ x)
{
  fmpz_t n;
  fmpz_init(n);
  fmpz_set_mpz(n, x);
  //std::cout << "factoring fmpz " << static_cast<void*>(n) << std::endl;
  fmpz_factor_t factor;
  fmpz_factor_init(factor);
  fmpz_factor(factor,n);
  int len = factor->num;
  gmp_arrayZZ result = getmemarraytype(gmp_arrayZZ,2*len+1);
  result->len = 2*len+1;
  __mpz_struct *tmp;
  // The sign is the first element of the result.
  tmp = newitem(__mpz_struct);
  mpz_init(tmp);
  mpz_set_si(tmp, factor->sign);
  mpz_reallocate_limbs(tmp);
  result->array[0] = tmp;
  for (int i=0; i<len; i++) {
    // Get the i-th factor
    tmp = newitem(__mpz_struct);
    mpz_init(tmp);
    fmpz_get_mpz(tmp,factor->p + i);
    mpz_reallocate_limbs(tmp);
    result->array[2*i+1] = tmp;
    // Get the i-th factor and its exponent
    tmp = newitem(__mpz_struct);
    mpz_init(tmp);
    fmpz_get_mpz(tmp,(fmpz *)(factor->exp + i));
    mpz_reallocate_limbs(tmp);
    result->array[2*i+2] = tmp;
  }
  fmpz_factor_clear(factor);
  fmpz_clear(n);
  return result;
}

// Local Variables:
// indent-tabs-mode: nil
// End:
