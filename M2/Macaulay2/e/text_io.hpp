// (c) 1994 Michael E. Stillman
#ifndef _text_io_hh_
#define _text_io_hh_

#include <gmp.h>
#include "buffer.hpp"

extern int p_one;
extern int p_plus;
extern int p_parens;
extern int i_text_io();

extern "C" char newline[];

void bignum_text_out(buffer &o, mpz_t a);

void clear_emit_size();
void emit_wrapped(char *s);

void emit(char *s); // print onto stderr, or cerr.
void emit_line(char *s); // print onto stderr, or cerr.
#endif
