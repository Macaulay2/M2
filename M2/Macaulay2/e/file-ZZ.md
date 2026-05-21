# `ZZ.{cpp,hpp}` — legacy `ZZ` (`Ring`-based integers)

`ZZ.hpp` declares the **legacy `Ring`-based ZZ** — the engine's
original integer ring class, before the
[`aring`](file-aring.md) framework. It is still constructed by some
M2 paths but new code prefers [`file-aring-zz-flint.md`](file-aring-zz-flint.md).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "error.h"
#include "ring.hpp"

namespace M2 {
class ARingZZGMP;
}

// The following lines are here only to remove complaints about old style casts
// from gmp
```

The header (uniquely among ring headers) carries a note about GMP's
old-style C casts triggering compiler warnings — a relic from when
M2 supported very old GMP versions.

The legacy `ZZ` class inherits directly from `Ring` and uses GMP's
`mpz_t` value type. It coexists with:

- `M2::ARingZZGMP` ([`file-aring-zz-gmp.md`](file-aring-zz-gmp.md))
  — GMP via the `aring` framework.
- `M2::ARingZZ` ([`file-aring-zz-flint.md`](file-aring-zz-flint.md))
  — FLINT via the `aring` framework.

`ZZ` is essentially the same as `ARingZZGMP` minus the `SimpleARing`
CRTP scaffolding. The new aring path is preferred everywhere.

## What's still used

The legacy `ZZ` is still constructed by:

- The interpreter when it asks for "the integer ring" via the
  oldest entry points (some `rawZZ()` paths).
- Code that explicitly opts into the legacy path for compatibility
  testing.

Most engine code today gets `ZZ` via [`interface/aring.h`](interface/file-aring-interface.md)'s
`rawARingZZ()`-style constructor, which returns the FLINT-backed
aring wrapper.

## Coexistence

Three ZZ types in the engine:

| Class | File | Backed by |
|---|---|---|
| `ZZ` (legacy) | this file | GMP |
| `ARingZZGMP` | [`file-aring-zz-gmp.md`](file-aring-zz-gmp.md) | GMP via aring |
| `ARingZZ` | [`file-aring-zz-flint.md`](file-aring-zz-flint.md) | FLINT via aring |

The unification effort is ongoing; today all three remain.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-zz-flint.md`](file-aring-zz-flint.md) — modern default.
- [`file-aring-zz-gmp.md`](file-aring-zz-gmp.md) — modern GMP variant.
- [`file-aring.md`](file-aring.md) — `aring` framework.
- [`file-aring-glue.md`](file-aring-glue.md) — bridge between aring
  and the legacy `Ring` interface.
