#ifndef _flint_h_
#  define _flint_h_

#  include "engine-includes.hpp"

/**
   Integer primality and factorization interface routines via Flint
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

M2_bool rawZZisPrime(gmp_ZZ a);

M2_bool rawZZisProbablePrime(gmp_ZZ a);

gmp_arrayZZ rawZZfactor(gmp_ZZ a);

#  if defined(__cplusplus)
}
#  endif

#endif /* _flint_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
