/* some routines to augment the gmp library */

extern int mpz_hash(mpz_srcptr x);
extern int mpfr_hash(mpfr_srcptr x);
extern int mpfi_hash(mpfi_srcptr x);
extern void mp_free_str(char* str);
