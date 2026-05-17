# `NCF4.{cpp,hpp}` — `NCF4`

`NCF4` is the **non-commutative F4** Gröbner basis algorithm. Rather than
reducing one S-polynomial / overlap at a time (as
[`NCGroebner`](file-NCGroebner.md) does), it batches all overlaps in a given
degree and reduces them simultaneously as one giant matrix — the
**Macaulay matrix** — over the coefficient ring.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class NCF4 : public our_new_delete {
    // Inherits from FreeAlgebra; uses VectorArithmetic for the matrix work.
    // Tracks:
    //   - OverlapTable mOverlapTable;
    //   - WordTable    mWordTable;
    //   - MemoryBlock  mMonomialPool;
    //   - Macaulay matrix in (dense) row-major layout
    //   - row→polynomial map
    //   - column→monomial map
    // ...
};
```

The matrix back end uses [`VectorArithmetic`](../matrices.md) (declared at
the engine top level) — the same templated arithmetic infrastructure used by
the commutative F4 path. This is why the file also pulls in `m2tbb.hpp`
(TBB-based parallel primitives).

## Algorithm shape

```text
for d = 1, 2, …:
    collect all overlaps of degree d
    build the Macaulay matrix M:
        rows  = collected overlap-polynomials + tail reducers
        cols  = monomials seen in any row, sorted by NC monomial order
    reduce M to row echelon form
    new basis elements = nonzero echelon rows whose leading column was not
                         already a leading column of an existing basis element
    add new overlaps, prune subsumed overlaps
```

The matrix can be huge — billions of nonzeros in extreme cases. The
implementation streams rows in/out as needed and exploits parallelism via
TBB.

## TBB integration

`#include "m2tbb.hpp"` brings in the engine's Intel TBB wrapper.
Matrix-row arithmetic and column-index lookups parallelise cleanly across
threads. The TBB submodule lives in [`submodules/`](../../../submodules/README.md);
detection happens at configure time via
[`FindTBB.cmake`](../../../cmake/README.md).

## When `NCF4` wins

For dense non-commutative inputs — many overlaps per degree, many shared
suffixes — F4-style batch reduction is dramatically faster than the
one-overlap-at-a-time Buchberger path in
[`NCGroebner`](file-NCGroebner.md). For sparse inputs the per-row overhead
dominates and Buchberger wins.

The dispatcher in [`NCGroebner`](file-NCGroebner.md) picks between the two
based on density heuristics; the user can override via `Strategy =>`.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-NCGroebner.md`](file-NCGroebner.md) — Buchberger-style alternative.
- [`file-FreeAlgebra.md`](file-FreeAlgebra.md) — host ring.
- [`../f4/README.md`](../f4/README.md), [`../gb-f4/README.md`](../gb-f4/README.md)
  — commutative F4 cousins.
- TBB submodule under [`submodules/`](../../../submodules/README.md).
