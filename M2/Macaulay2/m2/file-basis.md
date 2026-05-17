# `basis.m2` — `basis(d, M)` and module-basis computations

`basis.m2` defines **`basis(d, M)`** and family — the operation that
computes the k-vector-space basis of a graded module in a given set
of degrees. It is the M2-side wrapper for the engine's
[k-basis path](../e/file-matrix-kbasis.md).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman
-- Updated 2021 by Mahrud Sayrafi
-* TODO:
 0. hookify, cache
 1. what are (basis, ZZ, List, *) methods for? why only Ring and Ideal?
 2. why isn't (basis, Matrix) implemented?
*-

needs "gb.m2"
needs "max.m2"        -- for InfiniteNumber
```

The TODOs flag open design questions about the basis API:

- Should `basis` be hook-able / caching?
- The 4-argument variants are inconsistently implemented across types.
- `basis(Matrix)` is missing.

These are improvements to be made, not blockers — the existing API
works.

## What `basis` produces

For a graded module `M` and a degree `d`:

```m2
basis(d, M)
```

Returns a `Matrix` whose columns are basis elements of `M_d` as a
free `k`-module (where `k` is the coefficient ring). Specifically:

- The matrix's target is `M`.
- The matrix's source is `k^n` where `n = dim_k M_d`.
- Each column is a degree-`d` element of `M`.

For a polynomial ring `R = k[x_1, …, x_n]`, `basis(d, R)` is the
matrix of all degree-`d` monomials.

## Variants

- **`basis(d, R)`** — `d`-th graded piece of a ring.
- **`basis(d, I)`** — `d`-th graded piece of an ideal.
- **`basis(d, M)`** — `d`-th graded piece of a module.
- **`basis(d_lo, d_hi, ...)`** — basis in a range of degrees.
- **`basis(M)`** — full basis (only if `M` is finite-dimensional).

## Engine backing

The heavy lifting happens in [`../e/file-matrix-kbasis.md`](../e/file-matrix-kbasis.md).
This file is the M2-side wrapper that prepares inputs, calls the
engine, and post-processes outputs.

## Used by

- Every M2 user computing finite-dimensional bases.
- [`file-Hom.md`](file-Hom.md) — `Hom` over free modules uses
  `basis`.
- [`file-pushforward.md`](file-pushforward.md) — explicitly uses
  `basis` for restricting scalars.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-matrix-kbasis.md`](../e/file-matrix-kbasis.md) —
  engine k-basis.
- [`file-modules.md`](file-modules.md) — input type.
- [`file-gb.md`](file-gb.md) — GB used internally.
- [`file-hilbert.md`](file-hilbert.md) — Hilbert function is the
  count of `basis` sizes.
