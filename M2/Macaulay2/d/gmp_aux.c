/* some routines to augment the gmp library */
#include <M2/config.h>
#include "M2/math-include.h"
#include <stdlib.h>
#include <string.h>

uint64_t mpz_hash(mpz_srcptr x) {
  size_t n_bytes, count, i;
  uint64_t h;
  unsigned char *buf;

  n_bytes = (mpz_sizeinbase(x, 2) + 7) / 8;
  buf = malloc(n_bytes + 1);
  mpz_export(buf, &count, 1, 1, 1, 0, x);
  h = 0;

  for (i = 0; i < count; i++)
    h = h * 3737 + buf[i];

  free(buf);

  return h + 6701 * mpz_sgn(x);
}

uint64_t mpfr_hash(mpfr_srcptr x) {
  mpfr_exp_t exp;
  mpz_t sig;
  uint64_t h;

  h = 3737 * mpfr_get_prec(x);

  if (mpfr_zero_p(x))
    return h + 6599 * mpfr_signbit(x) + 569;
  if (mpfr_nan_p(x))
    return h + 3581;
  if (mpfr_inf_p(x))
    return h +  5039 * mpfr_sgn(x) + 9733;

  mpz_init(sig);
  exp = mpfr_get_z_2exp(sig, x);

  h += 3449 * mpz_hash(sig) + 2143 * exp;
  mpz_clear(sig);
  return h;
}

uint64_t mpfi_hash(mpfi_srcptr x) {
  return mpfr_hash(&x->left) + 3737 * mpfr_hash(&x->right);
}

void mp_free_str(char *str){
    void (*free_function) (void *ptr, size_t size);
    mp_get_memory_functions(NULL,NULL,&free_function);
    free_function(str,strlen(str)+1);

}

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
