# `MacaulayMatrix.{cpp,hpp}` — the Macaulay matrix

`MacaulayMatrix` is the **matrix reduced at each F4 step**. It is the heart
of the F4 algorithm: instead of reducing one S-polynomial at a time, F4
builds this matrix from all S-pairs and tail-reducers of the current
degree and reduces them as a batch.

Part of the [`gb-f4/`](README.md) subdirectory; the file is at an early
stage in this refactored F4 codebase.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## Current state

The header at the time of writing is intentionally minimal:

```cpp
namespace newf4 {

class MacaulayMatrix {
private:
    MonomialHashTable mMonomials;
};

struct Column { };
struct Row    { };

}
```

The `Column` and `Row` types are placeholders that will be filled in as the
refactor proceeds; the only state today is the `mMonomials` table that maps
column indices to monomials.

## How a Macaulay matrix is used

Conceptually:

- **Rows** = polynomials (S-polynomials, basis elements selected as
  reducers, generators).
- **Columns** = monomials that appear in any row, sorted by the monomial
  order.
- **Entries** = coefficients; mostly zero (the matrix is sparse).

After row-reducing to echelon form, the **nonzero echelon rows whose
leading column is new** become new basis elements. The matrix is discarded;
F4 advances to the next degree.

## Memory layout

Sparse storage. Each row carries its non-zero entries; columns are
indexed via the monomial hash table. The header lazily allocates rows on
demand from a [`MemoryBlock`](../utilities.md).

## Status

This file is part of a long-running refactor (see `TODO-refactor-f4` in
[`gb-f4/`](README.md)). The interface is intentionally minimal today; the
plan is to grow it with explicit row-add, column-lookup, and reduce
operations as the rest of the new F4 path lands.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-GBF4Computation.md`](file-GBF4Computation.md) — top-level driver
  that builds and reduces this matrix.
- [`file-Basis.md`](file-Basis.md) — where new basis elements go after
  reduction.
- [`MonomialHashTable.{cpp,hpp}`](README.md) — column-index table.
- [`SPairs.{cpp,hpp}`](README.md) — produces the row population.
- [`../f4/README.md`](../f4/README.md) — the older F4 with a more complete
  matrix implementation.
