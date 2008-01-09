// Copyright 2007  Michael E. Stillman

#ifndef _complex_hpp_
#define _complex_hpp_
#include "../d/M2types.h"

/* The interface is similar to mpfr:
   Every M2_CCC_struct needs to be initialized with init or init_set.
   All rounding is GMP_RNDN.
   Resulting values are the first argument
*/

#if defined(__cplusplus)
extern "C" {
#endif

  void mpfc_init(M2_CCC result, long precision);
  void mpfc_clear(M2_CCC result);
  void mpfc_init_set(M2_CCC result, M2_CCC a);
  void mpfc_set_si(M2_CCC result, long re);
  int mpfc_is_zero(M2_CCC a);
  void mpfc_add(M2_CCC result, M2_CCC a, M2_CCC b);
  void mpfc_sub(M2_CCC result, M2_CCC a, M2_CCC b);
  void mpfc_mul(M2_CCC result, M2_CCC a, M2_CCC b);
  void mpfc_invert(M2_CCC result, M2_CCC v);

  void mpfc_sub_mult(M2_CCC result, M2_CCC a, M2_CCC b);
  /* result -= a*b */

  void mpfc_div(M2_CCC result, M2_CCC a, M2_CCC b);
  void mpfc_abs(M2_CCC result, M2_CCC a);
  void mpfc_sqrt(M2_CCC result, M2_CCC a);
  void mpfc_conj(M2_CCC result, M2_CCC a);
  
#if defined(__cplusplus)
}
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

