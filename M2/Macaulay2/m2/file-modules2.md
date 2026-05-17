# `modules2.m2` — second-tier `Module` operations

`modules2.m2` is the **second of two** files implementing the M2
`Module` API. It adds operations beyond the basics: `Ideal * Vector`,
`modulo`, `super`, more `Hom`-like operations, and module-quotient
machinery.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

needs "matrix1.m2"      -- for Ideal
needs "matrix2.m2"      -- for modulo
needs "quotring.m2"     -- for QuotientRing

Ideal * Vector := (I, v) -> (
    image((generators I) ** v#0)
    )
```

The single shown method `Ideal * Vector` is a good representative:
multiply an ideal by a vector, return the resulting module image.
The file is full of similar concise method definitions for
operator-style usage.

## What lives here

Operations beyond `modules.m2`'s basics:

- **`Ideal * Module`**, **`Ideal * Vector`** — multiplication.
- **`modulo`** — `modulo(M, N)` for the quotient module `M / N`.
- **`super M`** — the ambient free module of `M`'s presentation.
- **`isSubmodule(M, N)`, `isSubset(M, N)`** — predicates.
- Cached versions of `generators M`, `relations M`, etc.

## Two-file split

| File | Approximate scope |
|---|---|
| [`file-modules.md`](file-modules.md) | `Module` type + constructors + basic ops |
| `modules2.m2` (this file) | Operator overloads + module-arithmetic |

The two-file split mirrors `matrix.m2` / `matrix1.m2` / `matrix2.m2`
on the matrix side — historical, reflects load order, and keeps
each file at a reasonable size.

## Used by

- Every M2 user doing module arithmetic.
- [`file-Hom.md`](file-Hom.md) — many operations build on what's
  defined here.
- [`file-complexes.md`](file-complexes.md) — for chain-complex
  operations.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-modules.md`](file-modules.md) — companion file.
- [`file-matrix1.md`](file-matrix1.md), [`file-matrix2.md`](file-matrix2.md)
  — parallel matrix-side split.
- [`file-Hom.md`](file-Hom.md), [`file-multilin.md`](file-multilin.md)
  — module-operation siblings.
