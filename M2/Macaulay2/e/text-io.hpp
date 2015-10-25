// (c) 1994 Michael E. Stillman
#ifndef _text_io_hh_
#define _text_io_hh_

//#include <mpfr.h>
//#include "debug.hpp"
#include "buffer.hpp"
#include <stddef.h>
#include <gmp.h>

extern int i_text_io();

#define wrapping_prefix "   -- "

void bignum_text_out(buffer &o, mpz_t a);

void clear_emit_size();
void emit_wrapped(const char *s);
inline void emit_wrapped(int prlevel, const char *s) {
  if (M2_gbTrace >= prlevel) emit_wrapped(s);
}

void emit(const char *s); // print onto stderr, or cerr.
void emit_line(const char *s); // print onto stderr, or cerr.
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
