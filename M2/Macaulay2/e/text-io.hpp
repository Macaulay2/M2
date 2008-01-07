// (c) 1994 Michael E. Stillman
#ifndef _text_io_hh_
#define _text_io_hh_

#include <gmp.h>
#include <mpfr.h>
#include "buffer.hpp"
#include "debug.hpp"

extern int gbTrace;
extern int p_one;
extern int p_plus;
extern int p_parens;
extern int i_text_io();

extern "C" char newline[];

#define wrapping_prefix "   -- "

void bignum_text_out(buffer &o, mpz_t a);

void clear_emit_size();
void emit_wrapped(char *s);
inline void emit_wrapped(int prlevel, char *s) {
  if (gbTrace >= prlevel) emit_wrapped(s);
}

void emit(char *s); // print onto stderr, or cerr.
void emit_line(char *s); // print onto stderr, or cerr.
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
