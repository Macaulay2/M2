# `det.{cpp,hpp}` — matrix determinants and minors

`det.cpp` implements **determinant** and **minor** computations over
the engine's matrix types. It supports three algorithm strategies
selectable by the caller.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Strategy constants

```cpp
const int DET_BAREISS   = 0;
const int DET_COFACTOR  = 1;
const int DET_DYNAMIC   = 2;
```

The caller picks an algorithm by passing one of these constants. They
control the trade-off between speed and memory:

### `DET_BAREISS`

**Fraction-free Bareiss algorithm**. Like Gaussian elimination but
preserves integer-valued pivots through cancellation. Optimal when the
matrix lives over `ZZ` or a polynomial ring with integer coefficients —
no `frac` field needed, no coefficient blowup beyond what's necessary.

### `DET_COFACTOR`

**Laplace cofactor expansion**. Expands along the row or column with
the most zeros. Optimal for very sparse matrices (e.g. many incidence
matrices) where most cofactors are zero. The choice of row / column to
expand along is made dynamically per-submatrix.

### `DET_DYNAMIC`

**Dynamic-programming variant**. Caches minors of fixed size to avoid
recomputation when many minors share submatrices. Best for matrices
where the user requests many minors of various sizes.

## Inputs

The header pulls in [`Matrix`](file-matrix.md) and `MatrixConstructor`:

```cpp
#include "matrix.hpp"
#include "matrix-con.hpp"
#include <utility>
#include <vector>
#include <map>
#include <algorithm>
```

The entry points operate on a `Matrix` and produce either:

- A `RingElement` (the determinant of a square submatrix).
- A `Matrix` of minors (e.g. all `p × p` minors for a given `p`).

## Algorithm choice

The user-facing M2 functions `det` / `minors` pass through a strategy
argument that maps to one of the three constants. The interpreter side
is in [`m2/matrix2.m2`](../m2/README.md).

## When you'd care which strategy

For most users `DET_DYNAMIC` is the default and works well. Switch
to:

- `DET_BAREISS` for large integer matrices where coefficient growth
  matters.
- `DET_COFACTOR` for highly sparse small matrices.

The engine's gtest suite ([`unit-tests/`](unit-tests/README.md))
includes correctness comparisons between the three strategies.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix.md`](file-matrix.md), `matrix-con.{cpp,hpp}` — matrix
  types this consumes.
- [`m2/matrix2.m2`](../m2/README.md) — M2-side wrappers.
- [`file-LLL.md`](file-LLL.md) — neighbouring integer-matrix algorithm.
