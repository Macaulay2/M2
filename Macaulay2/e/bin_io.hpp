// (c) 1994 Michael E. Stillman
#ifndef _bin_io_hh_
#define _bin_io_hh_

#include "style.hpp"

void bin_mpz_out(buffer &o, mpz_t n);
void bin_mpz_in(mpz_t result, char *&s, int &len);

void bin_int_out(buffer &o, int n);
int bin_int_in(char *&s, int &len);
#endif
