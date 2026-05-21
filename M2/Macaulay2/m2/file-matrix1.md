# `matrix1.m2` — second-tier `Matrix` operations (`Ideal`, kernel, image)

`matrix1.m2` is the **second of three** files implementing the M2
`Matrix` API. It adds `Ideal`, kernel, image, submatrix, presentation,
and basis-related operations.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
--		Copyright 1993-2002 by Daniel R. Grayson

needs "matrix.m2"
needs "modules.m2"
needs "quotient.m2"

module RingFamily :=
module Ring := Module => (cacheValue symbol module)(R -> R^1)
```

`module Ring := R -> R^1` makes `R` and `R^1` interchangeable in
contexts asking for a module — when the user writes `f : M -> R` for
some matrix `f` and ring `R`, this method resolves `R` to its rank-1
free module so the type checking works.

## What lives here

| Type / op | Description |
|---|---|
| `Ideal` | Type declaration; an `Ideal` is just a `Module` viewed as `R^1` quotient |
| `ideal(...)` | Construct from a list of generators |
| `kernel f` | Kernel of a matrix `f` |
| `image f` | Image (column module) |
| `coker f` | Cokernel |
| `submatrix(f, rows, cols)` | Submatrix selection |
| `presentation M` | Presentation matrix of `M` |
| `entries f` | Convert a `Matrix` to a list-of-lists |
| `transpose f` | Transpose (returns a new matrix) |

These are the workhorses of M2 module manipulation. Most algorithms
that touch modules call into `kernel`, `image`, or `coker` at some
point.

## Three-file split

The `Matrix` API split:

| File | Approximate scope |
|---|---|
| [`file-matrix.md`](file-matrix.md) | Construction, basic arithmetic, source/target |
| `matrix1.m2` (this file) | `Ideal`, kernel, image, submatrix, presentation |
| `matrix2.m2` ([`file-matrix2.md`](file-matrix2.md)) | LU, determinants, solve, advanced LA |

The split is historical and reflects how much the original authors
needed in each file. Combined size is what motivated splitting.

## Used by

- Every M2 user computing kernels, images, submatrices.
- Algebraic-geometry packages (very heavily).
- Engine-side fall-back paths.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-matrix.md`](file-matrix.md), [`file-matrix2.md`](file-matrix2.md)
  — sibling files in the three-file split.
- [`file-modules.md`](file-modules.md) — `Module` type these
  operations produce.
- [`file-gb.md`](file-gb.md) — `kernel` calls into GB.
