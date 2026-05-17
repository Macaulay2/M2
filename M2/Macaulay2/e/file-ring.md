# `ring.hpp`, `ring.cpp` — the legacy `Ring` base class

`ring.hpp` and `ring.cpp` define **`Ring`** — the abstract base
class for every coefficient and polynomial ring in M2's older
(legacy) ring interface. The whole `ring_elem`-based API is
methods on this class.

Part of the [engine](README.md) — foundation.

[← engine overview](README.md) · [coefficient rings](coefficient-rings.md) · [polynomial rings](polynomial-rings.md)

## What `Ring` is

```cpp
class ARing;
class CCC;
class CoefficientRingR;
class FractionField;
class FreeModule;
class GF;
class LocalRing;
class Monoid;
class MutableMatrix;
class PolyQQ;
```

Pre-declarations of every ring kind, followed by the `Ring` class
itself. The class is **enormous** — every operation a ring
supports is a virtual method here.

## Why a giant abstract base

The legacy design is straightforward inheritance:

```
Ring (abstract)
├── RingZZ
├── Z_mod
├── RingQQ
├── RingRRR, RingCCC (now via ConcreteRing<...>)
├── PolyRing
│   ├── PolyQuotient
│   ├── ...
├── FractionField
├── LocalRing
├── ConcreteRing<ARingXXX>   ← bridge from the modern aring framework
└── ...
```

Each subclass implements the virtual methods (`add`, `mult`,
`is_zero`, `is_unit`, `from_long`, `eval`, `text_out`, etc.) for
its specific ring type. Callers dispatch through `Ring*`.

## Why "legacy"

The 2012-onward `aring` framework
([`file-aring-hpp.md`](file-aring-hpp.md) if added) uses templates
instead of virtual dispatch — faster inner loops, more
type-safety. New rings target `aring`.

But:

- Polynomial rings (most rings users see) are still `Ring`-based.
- The C ABI uses `ring_elem` (a `Ring`-flavored type).
- The interpreter boundary uses `ring_elem`.

So `Ring` isn't going away. It's the bridge between the C ABI and
the modern `aring` rings (via `ConcreteRing<ARingXXX>`).

## A representative method group

```cpp
const Monoid *Ring::degree_monoid() const { return degree_ring->getMonoid(); }
RingZZ *makeIntegerRing() { return new RingZZ; }
```

Every ring has:

- A **degree monoid** — for multigraded rings.
- A **coefficient ring** — for rings built over another ring.
- Standard arithmetic — `add`, `subtract`, `mult`, `divide`,
  `power`, `gcd`, `negate`.
- I/O — `text_out`, `elem_text_out`, `eval` (for ring maps).
- Factorisation — `is_unit`, `is_zero`, `is_equal`,
  `is_homogeneous`.

## The two-headed integer-ring layout

```cpp
#if 1
RingZZ *makeIntegerRing() { return new RingZZ; }
#endif
#if 0
ARingZZ* makeIntegerRing()
{
  return new M2::ConcreteRing<M2::ARingZZ>;
}
#endif
```

The commented-out `#if 0` block shows the migration plan: replace
the legacy `RingZZ` with `ConcreteRing<ARingZZ>`. As of writing,
the legacy path is still active for the C ABI to stay stable.

## Used by

- Every ring-aware engine file.
- [`file-mat.md`](file-mat.md) — matrices know their `Ring*`.
- [`file-poly.md`](file-poly.md) — polynomial rings inherit.
- The interpreter via [`engine.h`](file-engine-h.md).

## Related

- [`README.md`](README.md) — engine overview.
- [`coefficient-rings.md`](coefficient-rings.md) — per-coefficient
  ring details.
- [`polynomial-rings.md`](polynomial-rings.md) — polynomial ring
  details.
- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) —
  `ring_elem`.
