// Copyright 2012  Michael E. Stillman
// file to be included by all cpp files in the engine
// what about c files?

#ifndef __engine_includes_hpp__
#define __engine_includes_hpp__

// IWYU pragma: begin_exports

#include <M2/config.h>

#if !defined(SAFEC_EXPORTS)
#include <engine-exports.h>
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
#include "../d/M2mem.h"
#include "../d/debug.h"
#endif

// IWYU pragma: end_exports

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
