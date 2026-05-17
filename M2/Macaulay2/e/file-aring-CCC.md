# `aring-CCC.{cpp,hpp}` — `M2::ARingCCC` (arbitrary-precision complex)

`aring-CCC.cpp` implements **arbitrary-precision complex numbers** as a
pair of MPFR floats (mantissa-precision flexible, IEEE 754 rounding).
It is the complex sibling of [`file-aring-RRR.md`](file-aring-RRR.md).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "aring-RRR.hpp"

namespace M2 {

class ARingCCC : public SimpleARing<ARingCCC> {
    // complex numbers represented as pairs of MPFRs.
public:
    static const RingID ringID = ring_CCC;
    // typedef <pair of mpfr_t> elem;
    // arithmetic, conjugation, modulus, RNG, precision
};

}
```

The class pulls in [`aring-RRR`](file-aring-RRR.md) for the real-part
arithmetic; the complex layer is a thin overlay that performs the
standard component-wise formulas.

## Arithmetic

The standard complex formulas, evaluated component-wise via MPFR:

- `(a + bi) + (c + di) = (a + c) + (b + d) i`
- `(a + bi) · (c + di) = (a·c − b·d) + (a·d + b·c) i`
- `1 / (a + bi) = (a − b·i) / (a² + b²)`
- `|a + bi| = sqrt(a² + b²)`

Each `mpfr_add`, `mpfr_mul`, `mpfr_sqrt` call uses MPFR_RNDN
(round-to-nearest-ties-even).

## Header preamble

```cpp
#include "interface/gmp-util.h"  // for mpfr_reallocate_limbs, moveTo_gmpCC
#include "interface/random.h"    // for randomMpfr
```

Two helpers:

- **`mpfr_reallocate_limbs`** — sister of
  [`mpz_reallocate_limbs`](interface/file-gmp-util-interface.md);
  moves an MPFR float's mantissa from MPFR's malloc heap into the GC
  heap.
- **`moveTo_gmpCC`** — bundles two MPFRs into the GC-managed
  complex-number type the interpreter consumes.

## Used by

- High-precision NAG ([`file-NAG.md`](file-NAG.md)) — homotopy
  continuation at arbitrary precision.
- High-precision root-finding via [`interface/factory.h`](interface/file-factory-interface.md)
  routes that select MPSolve.
- Numerical linear algebra over `CC_n` for `n != 53`.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-RRR.md`](file-aring-RRR.md) — real sibling.
- [`file-aring-CC.md`](file-aring-CC.md) — hardware-precision sibling.
- `aring-CCi.{cpp,hpp}` — complex-interval sibling (Arb).
- MPFR (linked, not vendored).
