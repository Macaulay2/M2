// Copyright 2008 by Michael Stillman

#ifndef _rand_h_
#define _rand_h

#include "../d/M2types.h"

#include "config.h"
#include <stdio.h>
#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#else
#error integer type definitions not available
#endif

#if defined(__cplusplus)
extern "C" {
#endif

  void rawRandomInitialize();
  
  void rawSetRandomSeed(M2_Integer newseed);
  
  void rawSetRandomMax(M2_Integer newHeight);
  
  int32_t rawRandomInt(int32_t max);
  /* generate a random number in the range 0..max-1 */
  
  M2_Integer rawRandomInteger(M2_Integer maxN);
  /* if height is the null pointer, use the default height */
  
  M2_Rational rawRandomQQ(M2_Integer height);
  /* returns random a/b, where 1 <= b <= height, 1 <= a <= height */
  /* if height is the null pointer, use the default height */
  
  M2_RRR rawRandomRR(unsigned long precision);
  /* returns a uniformly distributed random real with the given precision, in range [0.0,1.0] */
  
  M2_CCC rawRandomCC(unsigned long precision);

#if defined(__cplusplus)
}
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
