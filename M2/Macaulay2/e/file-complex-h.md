# `complex.h` — `gmp_CC` complex-number primitives

`complex.h` declares the engine's **`gmp_CC` complex number
primitives** — initialisation, arithmetic, and special functions
on arbitrary-precision complex values built from two MPFR reals.

Part of the [engine](README.md) — coefficient rings.

[← engine overview](README.md) · [coefficient rings](coefficient-rings.md)

## Header

```c
// Copyright 2008  Michael E. Stillman

#ifndef _complex_h_
#define _complex_h_

/* The interface is similar to mpfr:
   Every gmp_CC struct needs to be initialized with init or init_set.
   All rounding is MPFR_RNDN.
   Resulting values are the first argument
*/

#if !defined(SAFEC_EXPORTS)
//#include <engine-exports.h>
#include "interface/m2-types.h"
#endif
```

The header's docstring captures the design:

- **MPFR-style API** — `init`, `init_set`, `clear`, `set`,
  `add`, `mul`, `div`, etc. Same function-naming convention as
  GMP/MPFR.
- **First argument is the result** — common for C math libraries
  that don't have constructors.
- **MPFR_RNDN throughout** — round-to-nearest-even, the typical
  IEEE-754 default.

## Why a C-style API for complex

The engine already has C++ classes (`M2::ARingCCC` etc.) that
internally use this API. But:

- The C API is what crosses the engine/interpreter boundary.
- Some legacy paths use `gmp_CC` directly (predate the templated
  `aring` framework).
- The MPFR-style API is easier to read for users familiar with
  MPFR.

## What's declared

```c
void gmp_CC_init(gmp_CC z, mpfr_prec_t prec);
void gmp_CC_set(gmp_CC dst, const gmp_CC src);
void gmp_CC_add(gmp_CC dst, const gmp_CC a, const gmp_CC b);
void gmp_CC_mul(gmp_CC dst, const gmp_CC a, const gmp_CC b);
void gmp_CC_div(gmp_CC dst, const gmp_CC a, const gmp_CC b);
...
void gmp_CC_exp(gmp_CC dst, const gmp_CC a);
void gmp_CC_log(gmp_CC dst, const gmp_CC a);
void gmp_CC_sqrt(gmp_CC dst, const gmp_CC a);
```

(Approximate — the full list covers arithmetic, special functions,
and conversion.)

## Used by

- `M2::ARingCCC` ([`file-aring-CCC.md`](file-aring-CCC.md)) —
  internally.
- The interpreter's `CC` coefficient type, when crossing the
  engine boundary.
- Some numerical-algebraic-geometry paths.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-aring-CCC.md`](file-aring-CCC.md) — modern templated
  wrapper.
- [`file-aring-CC.md`](file-aring-CC.md) — `complex<double>`
  version.
- [`coefficient-rings.md`](coefficient-rings.md) — area.
- MPFR / MPC — external linked libraries.
