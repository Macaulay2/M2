/* some routines to augment the gmp library */

#include <gmp.h>

int mpz_hash(mpz_t x) {
  int h = 0;
  int n = x->_mp_size;
  int i;
  if (n < 0) n = -n;
  for (i = 0; i<n; i++) h = 3737 * h + x->_mp_d[i];
  if (x->_mp_size < 0) h = -h;
  return h;
}

int mpf_hash(mpf_t x) {
  int h = 0;
  int n = x->_mp_size;
  int i;
  if (n < 0) n = -n;
  for (i = x->_mp_exp; i<n; i++) h = 3737 * h + x->_mp_d[i];
  if (x->_mp_size < 0) h = -h;
  return h;
}
