# `mat-elem-ops.hpp` — `MatElementaryOps<MT>` (elementary row/column ops)

`mat-elem-ops.hpp` defines **elementary matrix operations** — row swap,
column swap, row scaling, row reduction by another row — templated on
the matrix storage type `MT`. It supplies the building blocks every
linear-algebra algorithm composes.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Specialised on dense vs. sparse

```cpp
#include <memory>

template <typename MT>
class MatElementaryOps;
template <typename RT>
class DMat;
template <typename RT>
class SMat;

template <typename RT>
class MatElementaryOps<DMat<RT>> {
public:
    typedef DMat<RT> Mat;
    typedef typename Mat::ElementType ElementType;
    // ... DMat-specific row / column operations ...
};

// Separate specialization for SMat<RT>
template <typename RT>
class MatElementaryOps<SMat<RT>> {
public:
    // ... SMat-specific row / column operations ...
};
```

The template gets **separate specialisations** for `DMat<RT>` and
`SMat<RT>` because the underlying operations look different:

- For `DMat<RT>` (dense), a row swap is `std::swap` of pointer arrays.
- For `SMat<RT>` (sparse), it's a swap of linked-list heads.

The templated declaration lets generic code in `mat-linalg.hpp`
([`file-mat-linalg.md`](file-mat-linalg.md)) and `mat-jordan.hpp`
work over both flavours.

## Operations exposed

For each `Mat`-specific specialisation:

- **`swap_rows(M, i, j)`**, **`swap_columns(M, i, j)`** — swap two
  rows / columns.
- **`scale_row(M, i, c)`**, **`scale_column(M, i, c)`** — multiply a
  row / column by a scalar `c`.
- **`row_op(M, i, c, j)`** — `row_i += c * row_j`. The elementary
  reduction operation.
- **`column_op(M, i, c, j)`** — analogous column op.
- **`negate_row(M, i)`**, **`zero_row(M, i)`** — convenience.

## Why elementary ops matter

Every Gaussian-elimination-style algorithm in the engine ultimately
calls into `MatElementaryOps`:

- LU decomposition is a sequence of `row_op`s.
- Reduced row echelon form is `scale_row` + `row_op` until done.
- Hermite normal form ([`file-hermite.md`](file-hermite.md)) uses
  `row_op` with Bezout coefficients.

Centralising these operations means coefficient-ring specialisations
(FLINT, FFLAS, MPFR) only need to specialise the few elementary ops,
and every algorithm above them inherits the speed.

## Used by

- [`file-mat-linalg.md`](file-mat-linalg.md) — every linear algebra
  routine.
- [`file-LLL.md`](file-LLL.md), [`file-hermite.md`](file-hermite.md) —
  algorithm-specific consumers.
- [`file-mutablemat.md`](file-mutablemat.md) — public M2 ops route
  through here.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md), `smat.hpp` — matrix storage.
- [`file-mat-arith.md`](file-mat-arith.md), [`file-mat-linalg.md`](file-mat-linalg.md)
  — sibling operations.
