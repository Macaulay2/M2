# `M2/Macaulay2/e/gb-f4/` — refactored F4 Gröbner basis engine

A newer implementation of F4 that splits the algorithm along cleaner lines:
basis, S-pairs, monomial tables, and the Macaulay matrix each get their own
header pair, making the code easier to test and reuse.

The older implementation in [`../f4/`](../f4/README.md) is still in the tree
and built alongside this one. See `TODO-refactor-f4` here for the long-running
refactoring notes.

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
