// Copyright 2012  Michael E. Stillman
// file to be included by all cpp files in the engine
// what about c files?

#ifndef __engine_includes_hpp__
#define __engine_includes_hpp__

#include <M2/config.h>
#include <stdio.h>
#include <stddef.h>
#include <gmp.h>
#include <mpfr.h>

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

#ifndef __cplusplus  /* These are coming from newdelete.hpp, in C++ */
  #include <M2/gc-include.h>
  #include "../d/M2mem.h"
  #include "../d/debug.h"
#endif

#ifndef BASECLASS
#ifdef __cplusplus
  #define BASECLASS : public our_new_delete
  #include "newdelete.hpp"
#else
  #define BASECLASS
#endif
#endif

#ifndef NDEBUG
#ifdef _cplusplus
#include <cassert>
#else
#endif
#include <assert.h>
#define ASSERT(X) assert(X);
#define IF_DEBUG(X) X
#else
#define ASSERT(X)
#define IF_DEBUG(X)
#endif

typedef int32_t deg_t;  // this is the integer type to use for degrees and weights

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
