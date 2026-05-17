# `aring-RRR.{cpp,hpp}` — `M2::ARingRRR` (arbitrary-precision real)

`aring-RRR.cpp` implements **arbitrary-precision real numbers** using
MPFR's `mpfr_t` value type. It is the precision-flexible sibling of
[`file-aring-RR.md`](file-aring-RR.md), used whenever an M2 user
specifies a precision other than `53` for `RR`.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "interface/gmp-util.h"  // for moveTo_gmpRR
#include "interface/random.h"    // for randomMpfr

namespace M2 {

class ARingRRR : public SimpleARing<ARingRRR> {
public:
    static const RingID ringID = ring_RRR;
    // typedef mpfr_t elem;
    // arithmetic, conversion, RNG, precision tracking
};

}
```

Each `ARingRRR` instance carries a **precision** (the number of
mantissa bits). Values are `mpfr_t`s allocated with that precision.
Different precisions produce different `ARingRRR` instances — the
engine treats them as distinct rings, so you can't accidentally mix
53-bit and 100-bit values.

## Arithmetic via MPFR

Every operation routes to MPFR:

- `mpfr_add(out, a, b, MPFR_RNDN)`
- `mpfr_mul(out, a, b, MPFR_RNDN)`
- `mpfr_sin`, `mpfr_log`, etc. for the transcendentals.

The rounding mode is **`MPFR_RNDN`** (round to nearest, ties to even)
throughout — the IEEE 754 default. Other modes would be implementable
but aren't exposed.

## Allocation

`mpfr_t` is a small struct with a pointer to a heap-allocated mantissa
buffer. The engine handles GC via `moveTo_gmpRR`
([`interface/file-gmp-util-interface.md`](interface/file-gmp-util-interface.md))
when returning values to the interpreter, similar to how
[`file-aring-qq-flint.md`](file-aring-qq-flint.md) routes through
`mpz_reallocate_limbs`.

## Random numbers

```cpp
#include "interface/random.h"  // for randomMpfr
```

The header pulls in `randomMpfr`, the MPFR-aware RNG that produces a
uniformly distributed `mpfr_t` value at the ring's precision.

## When to choose `ARingRRR` over `ARingRR`

| Use | Pick |
|---|---|
| Hardware-speed double precision | `ARingRR` ([file-aring-RR.md](file-aring-RR.md)) |
| Configurable precision (interactive numerical work) | `ARingRRR` (this file) |
| Interval arithmetic | `ARingRRi` (Arb-based) |

The dispatcher in [`file-aring.md`](file-aring.md) picks based on the
M2-side precision option.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-RR.md`](file-aring-RR.md) — hardware-precision sibling.
- `aring-RRi.{cpp,hpp}` — interval-arithmetic sibling.
- `aring-CCC.{cpp,hpp}` — complex MPFR sibling.
- MPFR (linked, not vendored as a submodule).
