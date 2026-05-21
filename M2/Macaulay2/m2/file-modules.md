# `modules.m2` — the `Module` type

`modules.m2` defines **`Module`** — the M2-side type for finitely
generated graded modules over a ring. Together with `modules2.m2` it
covers the M2 module API.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "monoids.m2"   -- for degreesMonoid
needs "reals.m2"     -- for inexact number
needs "gateway.m2"   -- for id

-----------------------------------------------------------------------------
-- Module
```

The `Module` type is declared in this file and supplemented in
`modules2.m2`. Together they cover:

- Free modules (`R^n`, `R^{d_1, …, d_n}` with degree shifts).
- Subquotient modules (`subquotient(M, N)` for `M / N`).
- Module operations: `+`, `*`, `⊕`, `⊗`, `Hom`, `dual`, ...
- Map construction (between modules).

`Module` is a `HashTable` subclass — like `Matrix`, with engine-side
backing via its `RawFreeModule` slot when the module is free.

## The Subquotient model

Every M2 `Module` is internally represented as a **subquotient**:

```
M = (image of generators)  /  (image of relations)
```

For a free module the relations are empty; for `R/I` modules the
generators are `e_1` and the relations are the generators of `I`.

Two key methods:

- **`generators M`** — a matrix whose columns generate `M` as a
  subspace of an ambient free module.
- **`relations M`** — a matrix whose columns are the relations.

Both are matrices into a common ambient free module — typically
`R^n` for some `n`.

## Operations

The full module API includes:

- **Construction** — `R^n`, `R^{...}`, `subquotient(...)`, `image f`,
  `kernel f`, `coker f`.
- **Arithmetic** — `M ++ N`, `M ** N`, `Hom(M, N)`, `dual M`,
  `prune M`.
- **Predicates** — `isHomogeneous M`, `isFreeModule M`.
- **Information** — `rank M`, `degrees M`, `numgens M`.

## Used by

- Every M2 user constructing or manipulating modules.
- [`file-gb.md`](file-gb.md), [`file-complexes.md`](file-complexes.md)
  — operate on `Module`.
- All algebraic-geometry packages.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-freemod.md`](../e/file-freemod.md) — engine
  `FreeModule`.
- [`../e/interface/file-freemodule-interface.md`](../e/interface/file-freemodule-interface.md)
  — public C entry points.
- [`file-matrix.md`](file-matrix.md) — `Matrix` as map between
  modules.
- `Hom.m2`, `multilin.m2` — companion module operations.
