// (c) 1997 Michael E. Stillman

#ifndef _error_h_
#define _error_h_

/**
 * @file error.h
 * @brief Engine error-reporting primitives: `ERROR`, `INTERNAL_ERROR`, `error`, `error_message`.
 *
 * Declares the C-linkage error API used throughout the engine.
 * `ERROR(fmt, ...)` is the printf-style entry point that
 * `vsprintf`s its formatted string into a fixed `MAXERROR`-byte
 * `errmsg` buffer (200 bytes) and sets an `iserror` flag; if an
 * unread error is already pending the previous message is
 * dumped to `stderr` with a `"--error message bumped:"` tag
 * before being overwritten. `error()` reads `iserror`;
 * `error_message()` returns the buffer **and clears `iserror`
 * as a side effect** so the next caller sees a clean slate.
 * `INTERNAL_ERROR(...)` is the unrecoverable variant: it
 * formats the message to its own scratch buffer, prints
 * `"--internal error: ..."` to stderr, and `abort()`s the
 * process. The header `#undef`s a stray `ERROR` macro that some
 * MinGW system headers define as `0`.
 *
 * The engine avoids C++ exceptions across the boundary in
 * favour of this protocol: a `.cpp` routine calls `ERROR(...)`
 * on a detected problem and returns; every caller checks the
 * flag and propagates an early exit; the interpreter loop
 * ultimately surfaces the message to the M2 user. C linkage is
 * deliberate so the routines remain callable from the
 * `.d`-generated glue, the C++ engine code, and external
 * library callbacks alike. **Caveat:** `iserror` and `errmsg`
 * are file-static in `error.c` --- not thread-local --- so
 * concurrent workers share the same error slot; the higher-
 * level engine is responsible for serialising access.
 *
 * @see buffer.hpp
 * @see overflow.hpp
 */

#if defined(__cplusplus)
extern "C" {
#endif

  // under mingw32 an include file defines ERROR as 0
  #undef ERROR

  void ERROR(const char *s,...);
  void INTERNAL_ERROR(const char *s, ...); /* Exits the program with an error code */
  int error(); /* returns 0 for false, 1 for true */
  const char *error_message();

#if defined(__cplusplus)
}
#endif

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
