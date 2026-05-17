# `M2/Macaulay2/e/f4/` — original F4 Gröbner basis engine

A native implementation of the **F4 algorithm** for computing Gröbner bases.
F4 reduces Gröbner basis computation to linear algebra on the so-called
*Macaulay matrix* — at each step it picks a set of S-pairs, builds the matrix
of all relevant polynomial multiples, reduces it, and reads off new basis
elements.

This is the older of the two F4 implementations in the engine; the newer one
lives in [`../gb-f4/`](../gb-f4/README.md) and was added when this one became
hard to refactor. Both are still built.

## Per-file deep dives

| File doc | Class |
|---|---|
| [`file-f4-computation.md`](file-f4-computation.md) | `F4Computation` (top-level glue) |
| [`file-f4-spairs.md`](file-f4-spairs.md) | `F4SPairSet` |
| [`file-f4-m2-interface.md`](file-f4-m2-interface.md) | `F4toM2Interface` |
| [`file-monhashtable.md`](file-monhashtable.md) | Monomial hash-table traits |

## Files

| File | Role |
|---|---|
| `f4.{cpp,hpp}` | Main F4 driver |
| `f4-computation.{cpp,hpp}` | Glue conforming to the engine's `Computation` framework |
| `f4-m2-interface.{cpp,hpp}` | Bridge between M2 polynomial types and the F4 internal types |
| `f4-types.hpp` | Plain-data types used throughout F4 |
| `f4-spairs.{cpp,hpp}` | S-pair selection |
| `f4-monlookup.{cpp,hpp}` | Monomial → row-index lookup table for the Macaulay matrix |
| `monhashtable.{cpp,hpp}` | Hash table specialised for monomials |
| `moninfo.{cpp,hpp}` | Compact monomial encoding info |
| `varpower-monomial.hpp`, `ntuple-monomial.hpp` | Two alternative monomial representations |
| `memblock.hpp` | Bump-pointer allocator |
| `hilb-fcn.{cpp,hpp}` | Hilbert function shortcut used to prune computation |

## Related

- [`../gb-f4/`](../gb-f4/README.md) — newer F4 implementation.
- [`../comp-gb.{cpp,hpp}`](../README.md) — generic Gröbner computation
  framework that drives this code.
- mathicgb (vendored as a [submodule](../../../submodules/README.md)) — a
  signature-based GB engine; an alternative path.

[← back to engine overview](../README.md)
