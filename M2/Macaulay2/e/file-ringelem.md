# `ringelem.hpp` — `ring_elem` (the engine's universal value type)

`ringelem.hpp` is the **single most heavily-included header in the
engine**. It declares `ring_elem` — the universal value type that
every ring's elements share at the engine boundary — plus the
underlying numeric type aliases.

Part of the [Ring elements & maps](ring-elements-and-maps.md) area.

[← per-area: ring-elements-and-maps](ring-elements-and-maps.md) · [← engine overview](README.md)

## Numeric typedefs

```cpp
#include "M2/math-include.h"
#include "monoid.hpp"
#include "newdelete.hpp"

using ZZ          = mpz_srcptr;     // read-only GMP integer
using ZZmutable   = mpz_ptr;        // mutable GMP integer
using QQ          = mpq_srcptr;
using QQmutable   = mpq_ptr;
using RRRelement  = mpfr_srcptr;
using RRRmutable  = mpfr_ptr;
using RRielement  = mpfi_srcptr;
using RRimutable  = mpfi_ptr;
```

The `…element` / `…mutable` split is a const-ness distinction: the
read-only form takes the immutable variant of the GMP/MPFR pointer,
the mutable form takes the in-place writable variant.

Engine code uses these aliases throughout to make const-ness
explicit. Compare:

```cpp
void foo(ZZ a, ZZmutable result);   // clearer
void foo(mpz_srcptr a, mpz_ptr result);  // same thing, denser
```

## Complex-number struct

```cpp
struct cc_struct {
    __mpfr_struct re;
    __mpfr_struct im;
};
```

The `cc_struct` carries two MPFR floats inline for the complex
arithmetic in [`file-aring-CCC.md`](file-aring-CCC.md). The header
comment "Perhaps we should have it be …" indicates this layout is
under review for future refactoring — pointer-based storage would
reduce by-value copy cost.

## `ring_elem` itself

Lower in the header (not shown above) is the `ring_elem` type — a
union/struct that can hold:

- An `int` (for small Z/p, GF, etc.).
- An `mpz_t` / `mpq_t` / `mpfr_t` / `mpfi_t` pointer.
- A pointer to a `Nterm` (the polynomial type).
- A pointer to a `gbvector` (the GB value type).
- Various other engine value pointers.

The discriminator is implicit: the owning `Ring*` knows what `ring_elem`
means in its context. This makes `ring_elem` cheap (machine-word
sized) but unsafe to interpret without context — every operation must
go through the ring.

## Why `ring_elem` matters

Every engine function that works with values takes `Ring*` plus
`ring_elem`. The pattern `R->method(ring_elem)` is the engine-wide
calling convention. Without `ring_elem`, the engine would need
templated arithmetic everywhere (the path the `aring` framework is
moving toward).

## Where `ring_elem` is *not* used

The newer code paths use templated arithmetic over concrete `aring`
classes ([`file-aring.md`](file-aring.md)) and avoid `ring_elem`
entirely in hot loops. `ring_elem` remains the lingua franca at the
engine boundary; templated paths use it only when receiving or
returning values to/from M2.

## Related

- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — area
  overview.
- [`file-relem.md`](file-relem.md) — `RingElement` (the M2-facing
  wrapper over `Ring*` plus `ring_elem`).
- [`file-aring.md`](file-aring.md) — the templated arithmetic path
  that bypasses `ring_elem`.
- [`file-monoid.md`](file-monoid.md) — included for `monomial` type.
