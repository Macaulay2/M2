/* include math.h and the appropriate bignumber package */

#ifndef M2_MATH_INCLUDE
#define M2_MATH_INCLUDE

  #ifndef PACKAGE_NAME
  #include <M2/config.h>
  #endif

  #include <math.h>

  #ifdef HAVE_STDINT_H
    /* This prevents a problem with mpir.h and mpirxx.h, that arises when stdint.h is loaded
       after mpir.h is but before mpirxx.h is.  Solution: load it first.  We load it
       here, just before loading gmp.h, which, for us, used to be just a link to mpir.h. */
    #define __STDC_LIMIT_MACROS
    #include <stdint.h>
  #endif

  #ifdef HAVE_STDDEF_H
    /* this prevents a problem in Mac OS X, where 'cstddef' is loaded before 'stddef.h', and it causes a problem */
    #include <stddef.h>
  #endif

  #if USING_MPIR 
  #include <mpir.h>
  #else
  #include <gmp.h>
  #endif

  #include <mpfr.h>
  #include <mpfi.h>

#endif
