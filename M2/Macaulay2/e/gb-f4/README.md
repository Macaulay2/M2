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

## What triggers this engine

This refactored F4 engine is selected when:

| M2 user code | What happens | Why this engine |
|---|---|---|
| `gb(I, Algorithm => LinearAlgebra, Strategy => NewF4)` | Engine dispatcher → `GBF4Computation` here | Explicit user request for the newer F4 variant |
| `gb(I, …)` with input shapes the dispatcher decides are better served here | Auto-routed when heuristics favour the cleaner code paths | Heuristic in `comp-gb.cpp`; depends on `I` shape and ring |
| Calls to `createGBF4Interface(...)` from new engine code | Direct API for engine-internal callers (no M2 user path) | The C-level entry point in `GBF4Interface` |

For the full M2-spec → engine mapping see the parent [`groebner-bases.md`](../groebner-bases.md) "M2 strategy → engine algorithm" table.

## Where in the engine pipeline this fits

```
M2:  gb(I, Algorithm => LinearAlgebra, Strategy => NewF4)
   ↓ comp-gb.cpp dispatcher
   ↓
GBF4Computation (file-GBF4Computation.md)
   ↓ exposed to engine boundary via
GBF4Interface (file-GBF4Interface.md)
   ↓ owns and drives
Basis (file-Basis.md)                          — the evolving GB
SPairs (file-SPairs.md)                        — S-pair queue
MacaulayMatrix (file-MacaulayMatrix.md)        — the matrix built and reduced each step
MonomialHashTable (file-MonomialHashTable.md)  — monomial → hash → index
MonomialLookupTable (file-MonomialLookupTable.md) — divisibility lookups
PolynomialList (file-PolynomialList.md)        — typed polynomial container
MonomialView, MonomialTypes (file-MonomialView.md, file-MonomialTypes.md)
                                                 — lightweight monomial views
                                                   (Index, MonomialIndex, HashInt typed-integer family)
   ↓ produces
A sequence of (degree, basis-element) data returned as a normal GBComputation
   back to the interpreter
```

The **clean separation** of concerns is the architectural improvement over `../f4/`: each phase of an F4 step (S-pair selection, symbolic preprocessing, matrix construction, matrix reduction, basis update) lives in its own type with its own header. The `newf4::` namespace contains everything.

## Comparison with `../f4/`

| Aspect | `../f4/` (original) | `gb-f4/` (refactored) |
|---|---|---|
| Polynomial representation | Mix of `Nterm*` and templated monomial | Struct-of-arrays via `PolynomialList` |
| Monomial encoding | Templated over two choices (`varpower`/`ntuple`) | Single typed `MonomialView` + `MonomialHashFunction` |
| S-pair selection | `F4SPairSet` | `SPairs` (cleaner queue interface) |
| Macaulay matrix | Inlined in `F4GB::do_spairs` | Separate `MacaulayMatrix` class |
| Testability | Hard — tightly coupled | Easier — each piece has its own tests |
| Strategy name | default `LinearAlgebra` | `LinearAlgebra` with `Strategy => NewF4` |

Both implementations remain in the tree and produce identical results; the choice is performance/maintainability trade-off per input. The `TODO-refactor-f4` design notes track outstanding work.

## Related

- [`../f4/`](../f4/README.md) — the original F4 implementation.
- [`../groebner-bases.md`](../groebner-bases.md) — the parent area doc with the full M2-spec → engine GB algorithm mapping.
- mathicgb submodule — another GB option used in some paths.

[← back to engine overview](../README.md)
