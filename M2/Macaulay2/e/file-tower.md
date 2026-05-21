# `tower.{cpp,hpp}` — `Tower` (legacy tower-of-extensions ring)

`tower.cpp` implements **`Tower`** — the engine's **legacy** ring class
for iterated finite extensions of `Z/p`. It pre-dates the
[`aring`](file-aring.md)-based [`file-aring-tower.md`](file-aring-tower.md)
and is still used as a fallback path and by code that hasn't migrated.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class structure (paraphrased)

```cpp
#include "relem.hpp"

class RingMap;
class DRing;

class Tower : public Ring {
    friend class TowerEvaluator;
    int  level;
    int  nvars;
    // ... per-level polynomial ring data ...
};
```

A `Tower` carries:

- **`level`** — depth in the extension tower (`level == 0` is the base
  `Z/p`).
- **`nvars`** — the number of variables in the level's polynomial
  representation.
- A pointer to the level below — recursively, forming the tower chain.
- A primitive polynomial defining the extension at this level.

Elements are stored as polynomials in the level's variable, with
coefficients from the level below.

## Relation to `aring-tower`

| File | Era | Style |
|---|---|---|
| `Tower` (this file) | Older | Inherits directly from `Ring`; virtual dispatch |
| [`ARingTower`](file-aring-tower.md) | Newer | `SimpleARing<ARingTower>` CRTP; inline arithmetic |

Both implement the same mathematical object. The newer aring path is
faster in tight loops; the legacy `Tower` is kept for compatibility
with existing tests and any code paths that still construct it.

## `TowerEvaluator`

A friend class declared but not defined in this header — it lives in
the `.cpp`. `TowerEvaluator` walks a `Tower` element and produces a
numerical evaluation at a sequence of points, used in NAG-style code.

## `DRing`

A forward-declared class that represents the *base* finite field
(`Z/p`) used to build the tower. Each level extends `DRing`'s field
by one variable; the recursive structure ultimately bottoms out there.

## Used by

- Older M2 code paths that explicitly construct `Tower(...)`.
- Some unit tests in [`unit-tests/`](unit-tests/README.md) — particularly
  `RingTowerTest.cpp`.
- Engine code that hasn't migrated to `aring`.

New code should target [`file-aring-tower.md`](file-aring-tower.md) /
[`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md) instead.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-tower.md`](file-aring-tower.md) — modern aring counterpart.
- [`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md) — large-GF
  alternative.
- [`unit-tests/`](unit-tests/README.md) — `RingTowerTest.cpp` covers this.
