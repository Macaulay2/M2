# `smat.hpp` — sparse matrix template

`SMat<ACoeffRing>` is the engine's **sparse matrix template** —
the sister of `DMat<R>` for matrices where most entries are
zero.

Part of the [engine](README.md) — matrices.

[← engine overview](README.md) · [matrices](matrices.md)

## Header

```cpp
// Copyright 2005  Michael E. Stillman

union ring_elem;
#include "ZZp.hpp"

class MutableMatrix;

template <typename MT>
class MatElementaryOps;

template <typename ACoeffRing>
class SMat : public our_new_delete
```

A C++ template parameterised on a coefficient ring. The `MT`
type-traits template (`MatElementaryOps`) provides the
specialisation point for elementary row/column operations.

## Internal representation

`SMat` typically stores:

- An **array of column heads** (one pointer per column).
- Each column is a **linked list** of `(row_index, value)` nodes
  sorted by row index.

This is the canonical "column-oriented sparse" layout — fast
column scans, slow row scans. Most M2 matrix algorithms (column
echelon form, Gauss reduction by column) are column-oriented, so
this fits.

## `DMat` vs `SMat`

| Aspect | `DMat<R>` | `SMat<R>` |
|---|---|---|
| Storage | Dense `vector<ElementType>` | Linked-list per column |
| Memory | `O(rows * cols)` | `O(nonzeros)` |
| Fast on | Dense matrices, FFPACK / LAPACK lifts | Very sparse matrices |
| Linear algebra | Fast (BLAS-style libs) | Slower per-op but fewer ops |

Both are wrapped by `MutableMat<MatT>`
([`file-mat.md`](file-mat.md)) into the polymorphic
`MutableMatrix` interface. The user / heuristic chooses dense or
sparse based on density.

## When to pick which

The interpreter's `mutableMatrix(M, Dense => ...)` lets users
force either kind. The default heuristic:

- Field coefficients + small dimensions → `DMat`.
- Polynomial coefficients → typically `SMat` (each entry is
  potentially large).
- User-provided dense input → `DMat`.

## Used by

- `MutableMat<SMat<R>>` — the polymorphic wrapper.
- M2 user code constructing sparse matrices.
- Some GB / resolution paths that need sparse intermediate
  storage.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-mat.md`](file-mat.md) — `MutableMatrix` abstract base.
- [`file-dmat.md`](file-dmat.md) — dense counterpart.
- [`file-mutablemat-imp.md`](file-mutablemat-imp.md) — `MutableMat`
  wrapper.
- [`matrices.md`](matrices.md) — area.
