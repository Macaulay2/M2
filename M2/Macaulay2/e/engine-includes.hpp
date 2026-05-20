// Copyright 2012  Michael E. Stillman
// file to be included by all cpp files in the engine
// what about c files?

#ifndef __engine_includes_hpp__
#define __engine_includes_hpp__

/**
 * @file engine-includes.hpp
 * @brief Engine-wide include prelude --- a single point of truth for portability shims.
 *
 * The header wraps four pieces of glue: `M2/config.h` (autotools
 * / CMake `HAVE_*` macros), `interface/m2-types.h` (the
 * cross-language `M2_arrayint`, `M2_string`, ... types shared with
 * the interpreter --- skipped when `SAFEC_EXPORTS` is set, signalling
 * that scc1 is generating the C ABI), the right fixed-width-int
 * header (`<stdint.h>` if `HAVE_STDINT_H`, else `<inttypes.h>`, with
 * `__STDC_LIMIT_MACROS` defined first so C++ sees `UINT64_MAX` and
 * friends), and `M2/gc-include.h` --- but only when compiled as C
 * (C++ defers GC integration to `newdelete.hpp` and its overloaded
 * `operator new`).
 *
 * The whole body sits inside an
 * `IWYU pragma: begin_exports`/`end_exports` pair so
 * include-what-you-use treats the bundled headers as re-exported:
 * files including `engine-includes.hpp` count as including the
 * inner declarations and IWYU will not suggest pulling them in
 * directly. Avoid adding new entries here unless they are genuinely
 * needed by ~90% of the engine; touching this file recompiles
 * everything downstream.
 *
 * @see newdelete.hpp
 * @see interface/m2-types.h
 */

// IWYU pragma: begin_exports

#include <M2/config.h>

#if !defined(SAFEC_EXPORTS)
//#include <engine-exports.h>
#include "interface/m2-types.h"
#endif

#if HAVE_STDINT_H
#if !defined(__STDC_LIMIT_MACROS)
#define __STDC_LIMIT_MACROS
#endif
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#else
#error integer type definitions not available
#endif

#ifndef __cplusplus /* These are coming from newdelete.hpp, in C++ */
#include <M2/gc-include.h>
//#include "../d/M2mem.h"
//#include "../d/debug.h"
#endif

// IWYU pragma: end_exports

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
