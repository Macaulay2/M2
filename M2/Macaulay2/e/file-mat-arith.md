# `mat-arith.hpp` — templated arithmetic for `DMat<R>` and `SMat<R>`

`mat-arith.hpp` declares the **arithmetic operations** for the engine's
dense and sparse matrix templates: addition, subtraction,
multiplication, scaling, negation. It is the basic-arithmetic
counterpart of [`mat-linalg.hpp`](file-mat-linalg.md).

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Class structure

```cpp
template <typename MT>
class MatElementaryOps;

#include "dmat.hpp"
#include "smat.hpp"

struct MatrixWindow {
    long begin_row;
    long begin_column;
    long end_row;
    long end_column;
    MatrixWindow(long x, long y, long nrows, long ncols)
        : begin_row(x), begin_column(y),
          end_row(x + nrows), end_column(y + ncols) {}
};
```

Two pieces:

1. **`MatElementaryOps<MT>`** — a templated class declared here,
   fully defined in [`mat-elem-ops.hpp`](file-mat-elem-ops.md). It
   parameterises elementary row / column operations over any matrix
   type `MT` (either `DMat<RT>` or `SMat<RT>`).
2. **`MatrixWindow`** — a half-open rectangular window into a matrix.
   Used by the arithmetic routines to operate on submatrices without
   physical extraction.

## `MatrixWindow` usage

```cpp
MatrixWindow w(/*first_row=*/ 0,
               /*first_col=*/ 0,
               /*nrows=*/    3,
               /*ncols=*/    3);
```

A window doesn't own data — it's a logical view over an existing
matrix's first 3 rows × 3 cols. Arithmetic operations that take a
window restrict their work to that region. Used pervasively in:

- LU decomposition (operates on the trailing sub-matrix).
- Block matrix arithmetic.
- Strassen-style fast multiplication (when enabled).

## Arithmetic operations (informal sketch)

The file declares (with full definitions in companion headers):

- **`add(out, A, B)`** — `out = A + B`.
- **`subtract(out, A, B)`** — `out = A - B`.
- **`multiply(out, A, B)`** — `out = A · B`. Specialised back ends
  dispatch to FLINT / FFLAS / BLAS where possible.
- **`mult_by_element(M, c)`** — `M *= c` for a scalar `c`.
- **`negateInPlace(M)`**, **`transposeInPlace(M)`** — when both
  shapes allow.

All take `Mat &` parameters (where `Mat` is `DMat<RT>` or `SMat<RT>`)
and route to the most specific back end via the templated dispatch in
[`mat-linalg.hpp`](file-mat-linalg.md)'s sibling specialisations.

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — its arithmetic
  forwarding goes through this header.
- [`file-matrix.md`](file-matrix.md) — immutable-matrix arithmetic
  builds a `DMat` window internally for some operations.
- [`file-LLL.md`](file-LLL.md), [`file-det.md`](file-det.md), and
  similar consumers.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-mat-linalg.md`](file-mat-linalg.md) — linear-algebra sibling.
- [`file-mat-elem-ops.md`](file-mat-elem-ops.md) — elementary
  operations.
- [`file-dmat.md`](file-dmat.md), `smat.hpp` — matrix templates this
  operates on.
