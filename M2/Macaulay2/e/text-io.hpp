// (c) 1994 Michael E. Stillman
#ifndef _text_io_hh_
#define _text_io_hh_

/**
 * @file text-io.hpp
 * @brief Text-formatting helpers layered on `buffer`: bignum print, line wrapping, `M2_gbTrace`-gated emit.
 *
 * Provides the engine-wide text utilities every output path
 * reaches for. `bignum_text_out(buffer&, mpz_srcptr)` renders
 * an `mpz_t` integer with M2's display conventions (no
 * thousands separator, leading minus for negatives, no leading
 * plus). `emit_wrapped(s)` prints to **stdout** with
 * continuation lines prefixed by the `wrapping_prefix` macro
 * `"   -- "` so long output stays visually unambiguous; the
 * `(prlevel, s)` overload checks `M2_gbTrace >= prlevel` first
 * to gate tracing output behind the global verbosity dial.
 * `emit(s)` and `emit_line(s)` are the simpler variants for
 * unconditional diagnostic messages; despite the in-source
 * header comment "print onto stderr, or cerr", the
 * implementation in `text-io.cpp` actually writes to **stdout**
 * via `fputs` / `putchar` + `fflush(stdout)`, so all three
 * `emit*` functions share the same stream.
 *
 * Pair with `buffer.hpp` for the underlying append-only text
 * accumulator. The `i_text_io()` initialiser hooks up the
 * column / wrap-width bookkeeping at startup, and
 * `clear_emit_size()` resets the current column so the next
 * `emit_wrapped` call starts wrapping from a known position.
 *
 * @see buffer.hpp
 */

#include "buffer.hpp"
#include "engine-includes.hpp"

extern int i_text_io();

#define wrapping_prefix "   -- "

void bignum_text_out(buffer &o, mpz_srcptr a);

void clear_emit_size();
void emit_wrapped(const char *s);
inline void emit_wrapped(int prlevel, const char *s)
{
  if (M2_gbTrace >= prlevel) emit_wrapped(s);
}

void emit(const char *s);       // print onto stderr, or cerr.
void emit_line(const char *s);  // print onto stderr, or cerr.
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
