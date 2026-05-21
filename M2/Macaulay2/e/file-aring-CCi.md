# `aring-CCi.{cpp,hpp}` — `M2::ARingCCi` (complex intervals)

`aring-CCi.cpp` implements **complex intervals** — typically represented
as a pair of real intervals (one for real part, one for imaginary), or
as a rectangle in the complex plane. It is the complex sibling of
[`file-aring-RRi.md`](file-aring-RRi.md).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <mpfi.h>
#include "interface/random.h"
#include "interface/gmp-util.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"
#include "aring-RRR.hpp"
#include "aring-RRi.hpp"
#include "aring-CCC.hpp"
```

`ARingCCi` builds on **three** other arings:

- [`ARingRRR`](file-aring-RRR.md) — endpoint precision.
- [`ARingRRi`](file-aring-RRi.md) — real-interval arithmetic for parts.
- [`ARingCCC`](file-aring-CCC.md) — exact complex MPFR arithmetic as
  reference / fallback.

`<mpfi.h>` is the underlying interval library.

## Representation choice

A complex interval can be:

1. A **rectangle** `[a, b] + [c, d]i` (Cartesian product of two real
   intervals). Simple but produces overly wide enclosures under
   multiplication and rotation.
2. A **disk** `{z : |z − center| ≤ radius}` (centre + radius). Tighter
   under multiplication but requires more arithmetic per operation.

`ARingCCi` uses the **rectangle** representation: each value is
`(real_interval, imag_interval)`. The implementation uses MPFI under
the hood for the underlying interval arithmetic.

## Arithmetic

Standard formulas with outward-rounded interval arithmetic:

- `[a + bi] + [c + di] = (a + c) + (b + d) i` — straightforward.
- `[a + bi] · [c + di] = (ac − bd) + (ad + bc) i` — each pair of
  multiplications produces an interval; we sum / subtract those.

Each interval operation calls into MPFI to maintain certified
enclosures.

## When to choose

| Need | Pick |
|---|---|
| Fast point CC arithmetic | `ARingCC` ([file-aring-CC.md](file-aring-CC.md)) |
| MPFR-precision CC arithmetic | `ARingCCC` ([file-aring-CCC.md](file-aring-CCC.md)) |
| Certified complex enclosures | `ARingCCi` (this file) |

The dispatcher in [`file-aring.md`](file-aring.md) picks based on the
M2-level precision option plus the "is interval?" flag.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-RRi.md`](file-aring-RRi.md) — real-interval sibling.
- [`file-aring-CCC.md`](file-aring-CCC.md) — point-precision sibling.
- MPFI — external dependency (linked).
