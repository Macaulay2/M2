# `M2/Macaulay2/e/f4/` — original F4 Gröbner basis engine

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (pipeline, three-layer design, Macaulay-matrix sweep details, memory model, comparison with `gb-f4/`).

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
| [`file-varpower-monomial.md`](file-varpower-monomial.md) | Sparse `(variable, exponent)` encoding |
| [`file-ntuple-monomial.md`](file-ntuple-monomial.md) | Dense `int64_t[nvars]` encoding |
| [`file-moninfo.md`](file-moninfo.md) | `MonomialInfo` (F4 monomial layout) |
| [`file-f4.md`](file-f4.md) | `F4GB` (the F4 algorithm) |
| [`file-hilb-fcn.md`](file-hilb-fcn.md) | `HilbertController` (Hilbert-driven early exit) |
| [`file-memblock.md`](file-memblock.md) | `F4MemoryBlock<T>` (slab allocator) |
| [`file-f4-monlookup.md`](file-f4-monlookup.md) | `F4MonomialLookupTableT<Key>` (tree index) |
| [`file-f4-types.md`](file-f4-types.md) | F4 type vocabulary (`spair`, `gbelem_type`, …) |

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

## What triggers this engine

This F4 engine is selected when:

| M2 user code | What happens | Why this engine |
|---|---|---|
| `gb(I, Algorithm => LinearAlgebra)` | Engine dispatcher → `F4Computation` here | Explicit user request for the linear-algebra-based GB |
| `gb(I, Algorithm => LinearAlgebra, Strategy => …)` with a strategy this engine recognises | Selected over the `gb-f4/` newer variant | The default `LinearAlgebra` strategy still routes here for stability — see [`../gb-f4/`](../gb-f4/README.md) for the cases where the newer one is picked |
| Auto-selection inside `comp-gb.cpp` for very dense `Z/p` inputs | Routed when not overridden | Heuristic; depends on input shape |

For the full M2-spec → engine mapping see the parent [`groebner-bases.md`](../groebner-bases.md) "M2 strategy → engine algorithm" table.

## Where in the engine pipeline this fits

```
M2:  gb(I, Algorithm => LinearAlgebra)
   ↓ comp-gb.cpp dispatcher
   ↓
F4Computation (this dir, file-f4-computation.md)
   ↓ owns
F4GB (this dir, file-f4.md)        — the algorithm driver
   ↓ uses
F4SPairSet (file-f4-spairs.md)            — S-pair selection
F4MonomialLookupTableT (file-f4-monlookup.md) — monomial → row-index table
F4MemoryBlock<T> (file-memblock.md)        — bump allocator
monhashtable.{cpp,hpp}                     — monomial hash table
MonomialInfo (file-moninfo.md)             — compact monomial info
varpower-monomial / ntuple-monomial         — the two monomial encodings
HilbertController (file-hilb-fcn.md)        — Hilbert-function early-exit
   ↓ produces
A sequence of (degree, basis-element) data the parent dispatcher returns
   as a normal GBComputation back to the interpreter
```

The two monomial encodings (`varpower` sparse, `ntuple` dense) are templated parameters. Inner loops are written templated so the compiler picks the encoding's arithmetic at compile time.

## Related

- [`../gb-f4/`](../gb-f4/README.md) — newer F4 implementation.
- [`../comp-gb.{cpp,hpp}`](../README.md) — generic Gröbner computation
  framework that drives this code.
- [`../groebner-bases.md`](../groebner-bases.md) — the parent area doc with the full M2-spec → engine GB algorithm mapping.
- mathicgb (vendored as a [submodule](../../../submodules/README.md)) — a
  signature-based GB engine; an alternative path.

[← back to engine overview](../README.md)
