// (c) 1994 Michael E. Stillman
#ifndef _text_io_hh_
#define _text_io_hh_

#include <iostream.h>
#include <gmp.h>

extern int p_one;
extern int p_plus;
extern int p_parens;
extern int i_text_io();

void bignum_text_out(ostream &o, mpz_t a);

#endif
