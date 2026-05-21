# `ZZp.{cpp,hpp}` — legacy `Z_mod` (`Ring`-based Z/p)

`ZZp.hpp` declares the **legacy `Z_mod`** — the engine's original Z/p
class before the [`aring`](file-aring.md) framework. It is still in
some M2 paths but new code prefers
[`file-aring-zzp.md`](file-aring-zzp.md),
[`file-aring-zzp-flint.md`](file-aring-zzp-flint.md), or
[`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "ring.hpp"
#include "coeffrings.hpp"

namespace M2 {
class ARingZZp;
}

class Z_mod : public Ring {
    // int P; // this is defined in class Ring
    // ... log / exp / Zech tables ...
};
```

`Z_mod` inherits from `Ring` and stores Z/p elements via
**log/exp/Zech tables** (the same approach as
[`file-coeffrings.md`](file-coeffrings.md)'s `CoefficientRingZZp` and
[`file-aring-zzp.md`](file-aring-zzp.md)'s `ARingZZp`). The
representation:

- `0` represents the field's zero.
- `1 ≤ n ≤ p − 1` represents `α^n mod p` where `α` is a primitive
  root.

Arithmetic uses table lookups:

- Multiplication is `(a + b) mod (p - 1)` on the log-index integers.
- Addition uses a Zech-log table.

## Coexistence

Four Z/p implementations exist in the engine today:

| Class | File | Notes |
|---|---|---|
| `Z_mod` (legacy) | this file | Direct `Ring` subclass; log tables |
| `CoefficientRingZZp` | [`file-coeffrings.md`](file-coeffrings.md) | `SimpleARing` CRTP example |
| `ARingZZp` | [`file-aring-zzp.md`](file-aring-zzp.md) | aring; portable (no FLINT) |
| `ARingZZpFlint` | [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md) | aring; FLINT-backed |
| `ARingZZpFFPACK` | [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) | aring; FFLAS-backed |

The dispatcher picks among these by prime size, hardware capability,
and user preference.

## Why kept around

`Z_mod` is the **reference implementation** that the others are
validated against in regression tests. It is also constructed by
some legacy M2 paths that pre-date the aring refactor.

A future cleanup pass may remove it once those legacy paths migrate.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-zzp.md`](file-aring-zzp.md), [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md),
  [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) — modern alternatives.
- [`file-coeffrings.md`](file-coeffrings.md) — `CoefficientRingZZp`
  (a non-aring, table-based path).
- [`file-aring.md`](file-aring.md), [`file-aring-glue.md`](file-aring-glue.md)
  — framework.
