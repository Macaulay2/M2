# `newring.m2` — `flattenRing`, `tensor`, ring-construction helpers

`newring.m2` provides the **derived ring constructors** that take an
existing ring or ideal and produce a new one: `flattenRing`,
`tensor` of rings, weight modifications, ring extensions.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Copyright 1996 Michael E. Stillman

needs "galois.m2"
needs "monoids.m2"
needs "quotring.m2"
needs "ringmap.m2"
needs "rings.m2"
needs "matrix2.m2"     -- for lift
```

Wide dependency list — `newring.m2` builds on essentially all the
ring-type files because it works with all of them as inputs.

## Key operations

### `flattenRing R`

Given a tower like `R = (S/I)/J`, return a single ring `R'` over the
base polynomial ring `S` (or the inner polynomial ring) plus the
isomorphism `R -> R'`. The output ring has the same generators but
combined ideal.

This is the workhorse of M2 ring manipulation. Most algorithms that
take a "ring" silently flatten first because the engine's GB / Hilbert
machinery only works on flat rings.

### `tensor(R, S)`

Tensor product of two rings. The variables of `S` are appended to
those of `R`; the relations are unioned.

```m2
tensor(QQ[x, y], QQ[z, w]) == QQ[x, y, z, w]
```

### `R ** S` and `R[x_1, ..., x_n]`

Sugar over `tensor`. The polynomial-ring extension syntax
`R[x_1, ..., x_n]` desugars to `tensor(R, K[x_1, ..., x_n])` for
appropriate `K`.

### Other constructors

- `ambient R` — strip quotient relations.
- `coefficientRing R` — drill down through ring towers.
- `lift(f, R)` — represent `f` over `R` if possible.
- `promote(f, R)` — coerce `f` to `R`.

## Used by

- Every M2 user constructing a quotient or extension ring.
- Algebraic-geometry packages that work with morphisms (need
  consistent flat rings).
- The engine boundary — which prefers flat rings.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-rings.md`](file-rings.md), [`file-polyrings.md`](file-polyrings.md),
  [`file-quotring.md`](file-quotring.md) — ring types.
- [`file-ringmap.md`](file-ringmap.md) — `RingMap` returned by
  `flattenRing`.
- [`file-minPres.md`](file-minPres.md) — similar simplification
  philosophy.
