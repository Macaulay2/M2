# `ring.{h,cpp}` (in `interface/`) — public C entry points for the legacy `Ring`

`interface/ring.h` declares the **public C functions** the interpreter uses
to construct and query rings through the engine's legacy
`Ring` interface. The newer
[`aring`-based path](file-aring-interface.md) coexists with this one;
together they cover every coefficient and polynomial ring M2 understands.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Computation;
class Matrix;
class Monoid;
class Ring;
class RingElement;
#else
typedef struct Computation  Computation;
typedef struct Matrix       Matrix;
typedef struct Monoid       Monoid;
typedef struct Ring         Ring;
typedef struct RingElement  RingElement;
#endif
```

The dual-mode block is identical to every other `interface/*.h` file. The
forward declarations include `Computation` because the quotient-ring
construction below takes a `GBComputation*`.

## Entry points

Ring construction is split by ring family:

- **Coefficient rings (legacy)** — `rawZZ()`, `rawQQ()`, `rawZZp(p)`,
  `rawGaloisField(...)`, `rawRR(prec)`, `rawCC(prec)`, …  Most have an
  `aring`-backed counterpart in [`file-aring-interface.md`](file-aring-interface.md).
- **Polynomial rings** — `rawPolynomialRing(K, M)` from a base ring `K`
  and monoid `M`. Plus flavoured variants: `rawSkewPolynomialRing`,
  `rawWeylAlgebra`, `rawSolvableAlgebra`.
- **Composite rings** — `rawFractionRing(R)`, `rawQuotientRing(R, gb)`,
  `rawLocalRing(R, P)`.
- **Inspection** — `rawIsField`, `rawIsCommutative`, `rawCharacteristic`,
  `rawRingType`.
- **Conversion** — `rawCoefficientRing` (extract base), `rawAmbientRing`
  (strip quotient).

The `raw…` naming and `Ring*`-return conventions are the same as
[`file-aring-interface.md`](file-aring-interface.md).

## Coexistence with `aring`

There are **two parallel construction paths** for some rings:

- `rawZZp(p)` builds a legacy `Z/p` (table-based).
- `rawARingZZp(p)` builds an `aring`-backed `Z/p`.

Both end up wrapped in a `Ring*` the interpreter holds. The newer code
prefers `aring` because the templated arithmetic is faster, but both
options remain wired up.

## Related

- [`README.md`](README.md) — interface overview.
- [`file-aring-interface.md`](file-aring-interface.md) — sibling for
  aring-backed rings.
- [`../coefficient-rings.md`](../coefficient-rings.md) — coefficient-ring
  area.
- [`../polynomial-rings.md`](../polynomial-rings.md) — polynomial-ring area.
- [`../file-polyring.md`](../file-polyring.md), [`../file-aring.md`](../file-aring.md)
  — implementations.
