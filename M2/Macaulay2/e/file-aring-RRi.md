# `aring-RRi.{cpp,hpp}` — `M2::ARingRRi` (real intervals)

`aring-RRi.cpp` implements **real intervals** — an interval `[a, b]`
where `a, b` are MPFR-precision reals and the represented value is
"some real in this interval." It uses MPFI (Multiple Precision Floating-
point Interval library) for the arithmetic.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <mpfi.h>
#include "interface/random.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"
#include "aring-RRR.hpp"
```

Two notable dependencies:

- **`<mpfi.h>`** — the external MPFI library. MPFI is linked, not
  vendored.
- **[`file-aring-RRR.md`](file-aring-RRR.md)** — `ARingRRR`. Interval
  endpoints are MPFR floats, so the interval class layers on top of
  the existing `ARingRRR` arithmetic.

## Class shape

```cpp
namespace M2 {

class ARingRRi : public SimpleARing<ARingRRi> {
    // Higher precision real intervals
public:
    static const RingID ringID = ring_RRi;
    // typedef mpfi_t elem;
    // arithmetic that produces strictly-enclosing intervals
};

}
```

Each `ARingRRi` instance carries a **precision** (mantissa bits per
endpoint). Distinct precisions yield distinct rings.

## Why interval arithmetic

Interval arithmetic is the **certified** counterpart to floating-point:
every operation produces an enclosure of the mathematical result rather
than a single approximate value. The enclosure is the *correct* answer —
the true result is guaranteed to lie within.

Use cases:

- Numerical algebraic geometry where you need provable bounds.
- Root-isolation algorithms.
- Validation of approximate solutions.

The trade-off is a wider error bound than ordinary MPFR.

## How arithmetic works

For `[a, b] + [c, d]`:

```
result = [a + c, b + d]   (rounded outward to widest enclosure)
```

For `[a, b] · [c, d]` the formula depends on signs but uses the same
outward-rounding principle. MPFI's `mpfi_add`, `mpfi_mul`, etc. handle
the rounding direction automatically.

## When to choose

| Need | Pick |
|---|---|
| Fast point arithmetic, no certification | `ARingRR` ([file-aring-RR.md](file-aring-RR.md)) |
| Configurable-precision point arithmetic | `ARingRRR` ([file-aring-RRR.md](file-aring-RRR.md)) |
| Certified enclosures | `ARingRRi` (this file) |

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-RRR.md`](file-aring-RRR.md) — endpoint arithmetic.
- `aring-CCi.{cpp,hpp}` — complex-interval sibling.
- MPFI — external dependency (linked).
