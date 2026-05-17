# `mutablemat.m2` — M2-side `MutableMatrix`

`mutablemat.m2` defines the M2-side **`MutableMatrix`** type — the
in-place-modifiable matrix type wrapping the engine's
[`MutableMatrix`](../e/file-mutablemat.md).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "matrix.m2"

MutableMatrix = new Type of HashTable
MutableMatrix.synonym = "mutable matrix"
raw MutableMatrix := m -> m.RawMutableMatrix
ring MutableMatrix := m -> m.Ring
MutableMatrix == ZZ := (m, i) -> raw m == i
ZZ == MutableMatrix := (i, m) -> raw m == i
```

A `MutableMatrix` is a `HashTable` with:

- A `RawMutableMatrix` engine pointer.
- A `Ring` reference.
- Plus engine-internal caching.

## Why a separate type from `Matrix`

`Matrix` is immutable — operations like `M + N` return a new matrix.
`MutableMatrix` is mutable — `setEntry(M, i, j, v)` modifies `M` in
place. The two have different use cases:

- **`Matrix`** — for algebraic operations where source/target
  matter, where the matrix is a homomorphism, where degrees track.
- **`MutableMatrix`** — for numerical linear algebra, in-place
  scratch space, LLL, GB internals.

## API

- **`mutableMatrix M`** — coerce a `Matrix` to mutable.
- **`matrix M`** — coerce back to immutable.
- **`mutableIdentity(R, n)`** — `n × n` identity.
- **`mutableMatrix(R, m, n)`** — zero matrix.
- **`setEntry(M, i, j, val)`**, **`getEntry(M, i, j)`** — direct
  access.
- **In-place ops** — `rowSwap`, `columnSwap`, `rowAdd`,
  `columnAdd`, `rowMult`, `columnMult`.
- **Linear algebra** — `LUdecomposition`, `solve`, `rank`, etc.

## Engine boundary

Every operation goes through
[`../e/interface/file-mutable-matrix-interface.md`](../e/interface/file-mutable-matrix-interface.md).
The engine handles per-ring back-end dispatch via
[`../e/file-mutablemat.md`](../e/file-mutablemat.md).

## Used by

- [`file-matrix2.md`](file-matrix2.md) — many linear-algebra
  operations route through here.
- [`file-LLL.md`](../e/file-LLL.md) (engine) — LLL operates on
  `MutableMatrix`.
- Numerical packages.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-mutablemat.md`](../e/file-mutablemat.md) — engine class.
- [`file-matrix.md`](file-matrix.md), [`file-matrix1.md`](file-matrix1.md),
  [`file-matrix2.md`](file-matrix2.md) — immutable counterpart.
- [`../e/interface/file-mutable-matrix-interface.md`](../e/interface/file-mutable-matrix-interface.md)
  — C entry points.
