/* some routines to augment the gmp library */
#include <M2/config.h>
#include "M2/math-include.h"

int mpz_hash(mpz_srcptr x) {
  int h = 0;
  int n = x->_mp_size;
  int i;
  if (n < 0) n = -n;
  for (i = 0; i<n; i++, h*=3737) h += x->_mp_d[i];
  if (x->_mp_size < 0) h = -h;
  return h;
}

int mpfr_hash(mpfr_srcptr x) {
  int h = 0;
  int n = (x->_mpfr_prec+mp_bits_per_limb-1)/mp_bits_per_limb;
  int i;
  if (0 != mpfr_sgn(x))
    for (i = 0; i<n; i++, h*=3737) h += x->_mpfr_d[i];
  return 777 + h * 3737 + x->_mpfr_exp + 11 * x->_mpfr_sign;
}

int mpfi_hash(mpfr_srcptr x) { // Added for MPFI
    int h = 0;
    int n = (x->_mpfr_prec+mp_bits_per_limb-1)/mp_bits_per_limb;
    int i;
    if (0 != mpfr_sgn(x))
    for (i = 0; i<n; i++, h*=3737) h += x->_mpfr_d[i];
    return 777 + h * 3737 + x->_mpfr_exp + 11 * x->_mpfr_sign;
} // End added for MPFI

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
