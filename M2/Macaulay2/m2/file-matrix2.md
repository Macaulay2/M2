# `matrix2.m2` — `Matrix` linear-algebra operations

`matrix2.m2` is the **third of three** files implementing the M2
`Matrix` API. It carries the linear-algebra operations: LU
decomposition, solving, rank, determinant, inverse, and similar.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What lives here

| Operation | What it does |
|---|---|
| `LUdecomposition M` | Returns `(L, U, P)` — lower / upper / permutation |
| `solve(A, b)` | Solve `A · x = b` for `x` |
| `rank M` | Matrix rank |
| `det M` | Determinant |
| `inverse M` | Matrix inverse |
| `nullSpace M` | Basis of the null space |
| `minors(p, M)` | The ideal of `p × p` minors (also see `multilin.m2`) |
| `transpose M` | Transpose |

For each, the file:

1. Validates inputs (square / non-square, ring compatibility).
2. Picks an engine back end based on the entry ring (FFLAS-FFPACK,
   FLINT, LAPACK, MPFR).
3. Calls into the engine via
   [`../e/interface/file-mutable-matrix-interface.md`](../e/interface/file-mutable-matrix-interface.md)
   and [`../e/file-mat-linalg.md`](../e/file-mat-linalg.md).
4. Marshals the result back to an M2 `Matrix` or sequence.

## Strategy selection

Operations like `rank` and `det` have multiple available algorithms.
The dispatch happens here:

- **`Strategy => Default`** — pick automatically.
- **`Strategy => Bareiss`** — fraction-free elimination
  ([`../e/file-fractionfreeLU.md`](../e/file-fractionfreeLU.md)).
- **`Strategy => Cofactor`** — Laplace expansion.
- **`Strategy => Dynamic`** — memoised minors.

## Three-file split

| File | Approximate scope |
|---|---|
| [`file-matrix.md`](file-matrix.md) | Construction, basic arithmetic |
| [`file-matrix1.md`](file-matrix1.md) | Ideal, kernel, image, submatrix |
| `matrix2.m2` (this file) | LU, det, solve, rank, etc. |

The file gets the most attention when adding linear-algebra
back ends — adding FFLAS-FFPACK support for a new operation requires
plumbing through this file.

## Used by

- M2 users doing linear algebra.
- Numerical packages that need fast LU / solve.
- Algebraic-geometry packages computing degrees via det.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-matrix.md`](file-matrix.md), [`file-matrix1.md`](file-matrix1.md)
  — sibling files.
- [`../e/file-mat-linalg.md`](../e/file-mat-linalg.md) — engine
  templated linear algebra.
- [`../e/file-dmat-lu.md`](../e/file-dmat-lu.md) — engine LU
  specialisations.
- [`../e/file-det.md`](../e/file-det.md) — engine determinant /
  minors.
- [`../e/file-lapack.md`](../e/file-lapack.md), [`../e/file-eigen.md`](../e/file-eigen.md)
  — engine numerical back ends.
