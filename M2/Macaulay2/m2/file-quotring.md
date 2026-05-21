# `quotring.m2` — `QuotientRing`

`quotring.m2` defines **`QuotientRing`** — the M2-side type for
`R/I` rings, paired with the engine's
[`PolyQuotient`](../e/file-polyquotient.md) class. The user-facing
syntax `R/I` is implemented here.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "enginering.m2"
needs "polyrings.m2"
needs "matrix1.m2"

-----------------------------------------------------------------------------
-- QuotientRing type declaration and basic methods
-----------------------------------------------------------------------------
```

`QuotientRing` inherits from
[`EngineRing`](file-enginering.md) and adds:

- A pointer to the **ambient ring** `R` (typically a
  [`PolynomialRing`](file-polyrings.md)).
- The **defining ideal** `I`, with its precomputed GB.
- M2-side methods overridden for quotients (`isField`,
  `coefficientRing`, `flattenRing`, etc.).

## `R/I` syntax

```m2
R = QQ[x, y]
I = ideal(x*y - 1)
Q = R/I
```

The `/` operator dispatches to the `Ring / Ideal` method declared in
this file:

1. Validate that `I` is an ideal of `R`.
2. Compute a GB of `I` if not already cached.
3. Call into the engine's quotient constructor
   ([`../e/file-qring.md`](../e/file-qring.md)).
4. Wrap the engine-side `PolyQuotient*` in an M2 `QuotientRing`.

The result `Q` is a `Ring` like any other — every M2 operation works
on it.

## Multiplication in a quotient

When two `RingElement`s in `Q` are multiplied:

1. Multiply as elements of the ambient `R`.
2. Reduce the result modulo the stored GB (the engine handles this
   in [`../e/file-polyquotient.md`](../e/file-polyquotient.md)).

The user sees the result as a normal-form element. Multiplication
preserves the quotient ring's value semantics.

## `coefficientRing` and friends

The file overrides standard reflection methods:

- **`coefficientRing Q`** — returns `R`'s coefficient ring.
- **`isField Q`** — checks via the GB whether the quotient is a
  field.
- **`flattenRing Q`** — collapses a tower of quotients down to a
  single one over a polynomial ring.

The latter is non-trivial: `(R/I)/J` is conceptually a quotient of
`R` by the larger ideal, and `flattenRing` reduces the tower.

## Used by

- Every M2 user writing `R/I`.
- Algebraic-geometry packages that manipulate quotient rings.
- The engine's quotient code paths in
  [`../e/file-polyquotient.md`](../e/file-polyquotient.md).

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-polyrings.md`](file-polyrings.md) — ambient ring type.
- [`file-rings.md`](file-rings.md), [`file-enginering.md`](file-enginering.md)
  — parent type chain.
- [`../e/file-qring.md`](../e/file-qring.md), [`../e/file-polyquotient.md`](../e/file-polyquotient.md)
  — engine peers.
