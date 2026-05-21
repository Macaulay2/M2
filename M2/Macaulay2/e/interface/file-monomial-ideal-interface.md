# `monomial-ideal.{h,cpp}` (in `interface/`) — public C entry points for `MonomialIdeal`

`interface/monomial-ideal.h` declares the **public C functions** the
interpreter uses to construct and operate on
[`MonomialIdeal`](../file-monideal.md) values.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Matrix;
class EngineMonomial;
class MonomialIdeal;
class RingElement;
#else
typedef struct Matrix          Matrix;
typedef struct EngineMonomial  EngineMonomial;
typedef struct MonomialIdeal   MonomialIdeal;
typedef struct RingElement     RingElement;
#endif
```

`EngineMonomial` is the opaque single-monomial type the interpreter uses
to pass individual monomials across the boundary. A `MonomialIdeal` is a
set of these.

## Entry points

Operations exposed here:

- **Construction** — `rawMonomialIdeal(M)` from a matrix whose columns
  are individual monomials.
- **Set operations** — `rawIntersect`, `rawUnion` (a.k.a. sum), `rawProduct`.
- **Ideal operations** — `rawQuotient`, `rawSaturate`, `rawRadical`,
  `rawBorel`, `rawIsBorel`.
- **Decomposition** — `rawAssociatedPrimes`, `rawCodimension`,
  `rawColon`.
- **Conversion** — `rawGenerators` (matrix of minimal generators),
  `rawMonomialIdealToMatrix`.

All operations dispatch into [`monideal.cpp`](../file-monideal.md), which
implements them as exponent-vector operations.

## Why monomial ideals get a dedicated interface

Monomial ideals appear constantly inside the engine — the initial ideal
of any ideal w.r.t. a GB, the leading term of any matrix, every Hilbert
function call. Having a dedicated C API for them lets the interpreter
manipulate them without paying for the polynomial-ring machinery.

The corresponding M2-level entry points are in
[`m2/monideal.m2`](../../m2/README.md).

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-monideal.md`](../file-monideal.md) — `MonomialIdeal` class.
- [`../computations.md`](../computations.md) — area overview.
- [`../file-assprime.md`](../file-assprime.md) — primary backer of the
  `rawAssociatedPrimes` entry point.
- [`../README-monideals.md`](../README-monideals.md) — implementation
  notes.
