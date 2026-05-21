# `aring-RR.{cpp,hpp}` — `M2::ARingRR` (machine `double`)

`aring-RR.cpp` implements the engine's **machine-precision real
numbers** — `RR` backed by hardware `double`. It is the simplest of the
real-number rings, used when speed dominates and precision doesn't need
to exceed IEEE 754 double precision.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
namespace M2 {

class ARingRR : public SimpleARing<ARingRR> {
public:
    static const RingID ringID = ring_RR;
    typedef double elem;   // approximate real numbers, double precision
    // ...
};

}
```

`elem` is just `double`. Addition, multiplication, and comparison are
hardware operations — no library calls, no allocations, no exceptions.

## Where `ARingRR` is selected

The interpreter chooses `ARingRR` when M2 code says `RR_53` or just `RR`
(53-bit mantissa is the default precision). For arbitrary precision the
user gets [`ARingRRR`](coefficient-rings.md) (`R` for arbitrary; uses
MPFR underneath) instead.

## Trade-offs vs. `ARingRRR`

| Aspect | `ARingRR` (this file) | `ARingRRR` (MPFR) |
|---|---|---|
| Precision | 53 bits (IEEE 754 double) | Arbitrary |
| Speed | Hardware; ~1 cycle per op | Library call per op |
| Allocation | None | MPFR arena |
| Use case | NAG, numerical linear algebra | Symbolic-numerical bridges |

`ARingRR` is the right call for:

- Numerical algebraic geometry ([`file-NAG.md`](file-NAG.md)).
- Numerical linear algebra over `double`-valued matrices via
  [`dmat`](file-dmat.md).
- Anywhere FFLAS / BLAS dispatch is available.

## Exception handling

```cpp
#include "exceptions.hpp"
```

`ARingRR` raises engine exceptions on `divide_by_zero` and similar — even
though the hardware semantics for `1.0 / 0.0` are well-defined (returns
`Inf`), the engine prefers the explicit-error path so M2 users see a
clear message rather than `Inf` propagating silently.

## Random numbers

```cpp
#include "interface/random.h"
```

The header pulls in the engine's RNG interface for generating random
`double` values in `[0, 1)`.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md) — `aring` framework.
- `aring-RRR.{cpp,hpp}` — MPFR arbitrary-precision sibling.
- `aring-RRi.{cpp,hpp}` — interval-arithmetic sibling.
- `aring-CC.{cpp,hpp}` — complex sibling (pair of `double`).
- [`file-NAG.md`](file-NAG.md), [`file-SLP.md`](file-SLP.md) — primary
  numerical consumers.
