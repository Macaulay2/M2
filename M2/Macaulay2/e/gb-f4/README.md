# `M2/Macaulay2/e/gb-f4/` — refactored F4 Gröbner basis engine

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (the `newf4::` namespace, typed-integer family, struct-of-arrays polynomial representation, monomial hashing, comparison with original `f4/`).

A newer implementation of F4 that splits the algorithm along cleaner lines:
basis, S-pairs, monomial tables, and the Macaulay matrix each get their own
header pair, making the code easier to test and reuse.

The older implementation in [`../f4/`](../f4/README.md) is still in the tree
and built alongside this one. See `TODO-refactor-f4` here for the long-running
refactoring notes.

## Per-file deep dives

| File doc | Class |
|---|---|
| [`file-GBF4Computation.md`](file-GBF4Computation.md) | `GBF4Computation` (top-level) |
| [`file-MacaulayMatrix.md`](file-MacaulayMatrix.md) | `MacaulayMatrix` |
| [`file-Basis.md`](file-Basis.md) | `Basis` (evolving GB) |
| [`file-SPairs.md`](file-SPairs.md) | `SPairs` (refactored F4 S-pair queue) |
| [`file-MonomialHashTable.md`](file-MonomialHashTable.md) | `MonomialHashFunction` + table |
| [`file-MonomialLookupTable.md`](file-MonomialLookupTable.md) | `MonomialLookupTable` (divisibility index) |
| [`file-PolynomialList.md`](file-PolynomialList.md) | `PolynomialList` (typed polynomial container) |
| [`file-MonomialView.md`](file-MonomialView.md) | `MonomialView` (non-owning encoded-monomial view) |
| [`file-MonomialTypes.md`](file-MonomialTypes.md) | Typed integers (`Index`, `MonomialIndex`, `HashInt`, …) |
| [`file-GBF4Interface.md`](file-GBF4Interface.md) | `createGBF4Interface` — engine-boundary entry to the new F4 |

**Coverage:** every source file in this directory has a dedicated deep-dive doc.

## Files

| File | Role |
|---|---|
| `GBF4Computation.{cpp,hpp}` | Top-level Computation conforming to the engine framework |
| `GBF4Interface.{cpp,hpp}` | Bridge between engine matrices and internal types |
| `Basis.{cpp,hpp}` | The evolving Gröbner basis |
| `SPairs.{cpp,hpp}` | S-pair selection |
| `MacaulayMatrix.{cpp,hpp}` | The matrix built and reduced each F4 step |
| `MonomialHashTable.{cpp,hpp}` | Hash table for monomials (parallels `../f4/monhashtable`) |
| `MonomialLookupTable.{cpp,hpp}` | Monomial → row-index lookup |
| `MonomialView.{cpp,hpp}`, `MonomialTypes.hpp` | Lightweight monomial views |
| `PolynomialList.{cpp,hpp}` | Container of polynomials |
| `mathicGBExample.m2`, `testing.m2` | M2-level test inputs |
| `TODO-refactor-f4` | Design notes / outstanding work |

## Related

- [`../f4/`](../f4/README.md) — the original F4 implementation.
- mathicgb submodule — another GB option used in some paths.

[← back to engine overview](../README.md)
