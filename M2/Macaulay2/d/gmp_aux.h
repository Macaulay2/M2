/* some routines to augment the gmp library */

#include <stdint.h>

extern uint64_t mpz_hash(mpz_srcptr x);
extern uint64_t mpfr_hash(mpfr_srcptr x);
extern uint64_t mpfi_hash(mpfi_srcptr x);
extern void mp_free_str(char* str);
