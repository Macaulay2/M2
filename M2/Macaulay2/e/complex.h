// Copyright 2008  Michael E. Stillman

#ifndef _complex_h_
#define _complex_h_

/**
 * @file complex.h
 * @brief `gmp_CC` C primitives: arithmetic on arbitrary-precision complex values (pair of MPFR reals).
 *
 * Declares the engine's C-style API for `gmp_CC` --- an
 * arbitrary-precision complex number stored as a pair of MPFR
 * floats. The function naming follows MPFR/GMP conventions:
 * lifecycle helpers `mpfc_init` / `mpfc_init_set` /
 * `mpfc_clear` / `mpfc_set` / `mpfc_set_si`, predicates
 * `mpfc_is_zero` / `mpfc_is_equal`, arithmetic `mpfc_add` /
 * `_neg` / `_sub` / `_mul` / `_invert` / `_sub_mult` / `_div`,
 * plus the single non-elementary entry `mpfc_sqrt` and the
 * helpers `mpfc_abs` (returns the real-valued modulus into a
 * `gmp_RRmutable`) and `mpfc_conj`. There is **no**
 * `mpfc_exp` or `mpfc_log` --- transcendentals are not on this
 * surface. The result is always the first argument, every
 * rounding step uses `MPFR_RNDN`, and every `gmp_CC` must be
 * initialised with `init` (or `init_set`) before use and freed
 * with `clear`.
 *
 * Caveat: a grep of `*.cpp` / `*.hpp` finds **no consumers** of
 * the `mpfc_*` functions outside `complex.c` itself, and the
 * production complex-arithmetic path `M2::ARingCCC`
 * (`aring-CCC.hpp`) implements its own arithmetic by calling
 * `mpfr_*` directly on the `(re, im)` fields rather than
 * routing through this header. The surface persists as a
 * C-side FFI for non-C++ callers (the interpreter's
 * `.dd`-generated glue and other C consumers) but is not the
 * active in-engine implementation.
 *
 * @see aring-CCC.hpp
 * @see aring-CC.hpp
 */

/* The interface is similar to mpfr:
   Every gmp_CC struct needs to be initialized with init or init_set.
   All rounding is MPFR_RNDN.
   Resulting values are the first argument
*/

#if !defined(SAFEC_EXPORTS)
//#include <engine-exports.h>
#include "interface/m2-types.h"
#endif

#if defined(__cplusplus)
extern "C" {
#endif

  void mpfc_init(gmp_CCmutable result, long precision);
  void mpfc_clear(gmp_CCmutable result);
  void mpfc_init_set(gmp_CCmutable result, gmp_CCmutable a);
  void mpfc_set_si(gmp_CCmutable result, long re);
  void mpfc_set(gmp_CCmutable result, gmp_CCmutable a);
  int mpfc_is_zero(gmp_CCmutable a);
  int mpfc_is_equal(gmp_CCmutable a, gmp_CCmutable b);
  void mpfc_add(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b);
  void mpfc_neg(gmp_CCmutable result, gmp_CCmutable a);
  void mpfc_sub(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b);
  void mpfc_mul(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b);
  void mpfc_invert(gmp_CCmutable result, gmp_CCmutable v);

  void mpfc_sub_mult(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b);
  /* result -= a*b */

  void mpfc_div(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b);
  void mpfc_abs(gmp_RRmutable result, gmp_CCmutable a);
  void mpfc_sqrt(gmp_CCmutable result, gmp_CC a);
  void mpfc_conj(gmp_CCmutable result, gmp_CCmutable a);

#if defined(__cplusplus)
}
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
