/* some routines to augment the gmp library */

#include <gmp.h>
#include <mpfr.h>

int mpz_hash(mpz_t x) {
  int h = 0;
  int n = x->_mp_size;
  int i;
  if (n < 0) n = -n;
  for (i = 0; i<n; i++, h*=3737) h += x->_mp_d[i];
  if (x->_mp_size < 0) h = -h;
  return h;
}

int mpfr_hash(mpfr_t x) {
  int h = 0;
  int n = (x->_mpfr_prec+mp_bits_per_limb-1)/mp_bits_per_limb;
  int i;
  for (i = 0; i<n; i++, h*=3737) h += x->_mpfr_d[i];
  return h * 3737 + x->_mpfr_exp + 11 * x->_mpfr_sign;
}
