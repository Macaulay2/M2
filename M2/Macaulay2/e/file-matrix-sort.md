# `matrix-sort.cpp` — `MatrixSorter` (sort the columns of a matrix)

`matrix-sort.cpp` implements **`MatrixSorter`** — a helper class that
sorts the columns of a [`Matrix`](file-matrix.md) by their leading
term, leading degree, or another configurable key. It is the engine
code behind M2's `sort(M)` and `mingens(M)`.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "matrix.hpp"

class MatrixSorter {
    const Ring  *R;
    int          deg_ascending;
    int          ringorder_ascending;
    int         *sort_vals;
    vec         *sort_vecs;
    int         *sort_degs;
    M2_arrayint  result;

    int sort_compare(int i, int j) {
        if (i == j) return 0;
        // ...
    }
};
```

The sorter carries:

- The ring `R`, for monomial-order context.
- Two **ascending flags** controlling degree and ring-order sort
  direction.
- Parallel arrays `sort_vals`, `sort_vecs`, `sort_degs` indexed by
  column.
- An **output permutation** `result` — the user gets back the new
  column order without the columns themselves being moved.

## Why a permutation output

`MatrixSorter` doesn't actually move the columns. It computes the
permutation that *would* sort them, and the user (typically the
M2-level `sort` wrapper) applies the permutation to whatever
matrices need re-ordering — input, output, change-of-basis, etc.

This is useful when sorting one matrix means the same permutation
should apply to several related matrices (e.g. a Gröbner basis plus
its change-of-basis matrix plus its syzygies).

## Comparator

`sort_compare(i, j)` returns `LT` / `EQ` / `GT` for columns `i` and
`j` — see [`file-style.md`](file-style.md) for the comparison-code
convention. It compares:

1. **Degrees** (using `deg_ascending` to flip direction).
2. **Leading monomials** by the ring's monomial order (using
   `ringorder_ascending`).
3. **Index tiebreak** as a last resort.

## Used by

- M2's `sort(M)` built-in.
- `gens(I)` after a GB computation — sorts the GB into a canonical
  order.
- Output paths that want deterministic column ordering across runs.

## Companion files

- `matrix-stream.cpp` ([`file-matrix-stream.md`](file-matrix-stream.md))
  — has its own simpler internal sort for streamed inputs.
- `monsort.hpp` ([`file-monsort.md`](file-monsort.md)) — generic
  templated sorter at the monomial level.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix.md`](file-matrix.md) — the input matrix type.
- [`file-style.md`](file-style.md) — `LT` / `EQ` / `GT`.
- [`file-monsort.md`](file-monsort.md) — sibling monomial sorter.
