/* include math.h and the appropriate bignumber package */

#ifndef M2_MATH_INCLUDE
#define M2_MATH_INCLUDE

  // IWYU pragma: begin_exports

  #ifndef PACKAGE_NAME
  #include <M2/config.h>
  #endif

  #include <math.h>

  #ifdef HAVE_STDINT_H
    #define __STDC_LIMIT_MACROS
    #include <stdint.h>
  #endif
  #ifdef HAVE_STDDEF_H
    /* this prevents a problem in Mac OS X, where 'cstddef' is loaded before 'stddef.h', and it causes a problem */
    #include <stddef.h>
  #endif

  #include <gmp.h>

  #define MPFR_USE_NO_MACRO
  #include <mpfr.h>
  #include <mpfi.h>

  // IWYU pragma: end_exports

#endif
