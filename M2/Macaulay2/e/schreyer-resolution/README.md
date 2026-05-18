# `M2/Macaulay2/e/schreyer-resolution/` — F4-style Schreyer resolutions

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (Schreyer-frame trick, three-layer design, TBB task-graph parallelism, dense vs sparse monoid choice, memory model).

This directory implements a **Schreyer-frame–based free resolution** built on
F4-style matrix reduction. It is the modern resolution engine that replaces
the older `res-a*` series at the top level of `e/`.

Schreyer's algorithm computes a resolution by augmenting the basis with a
total order ("Schreyer order") that makes leading-term arithmetic local to
each homological degree. This dramatically reduces the work compared to a
naïve approach. The F4 layer here computes the necessary reductions in batches
of Macaulay-matrix form.

## Per-file deep dives

| File doc | Class |
|---|---|
| [`file-res-f4-computation.md`](file-res-f4-computation.md) | `F4ResComputation` (top-level) |
| [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) | `SchreyerFrame` |
| [`file-res-poly-ring.md`](file-res-poly-ring.md) | `ResPolyRing` / `ResPolynomial` |
| [`file-res-monomial-sorter.md`](file-res-monomial-sorter.md) | `MonomialSorterObject` |
| [`file-res-dep-graph.md`](file-res-dep-graph.md) | TBB dependency graph |
| [`file-res-moninfo.md`](file-res-moninfo.md) | `ResMonoid` (dense/sparse dispatcher) |
| [`file-res-schreyer-order.md`](file-res-schreyer-order.md) | `ResSchreyerOrder` |
| [`file-res-f4.md`](file-res-f4.md) | `F4Res` (F4 reduction loop for resolutions) |
| [`file-res-monomial-types.md`](file-res-monomial-types.md) | Type vocabulary + `ResMonoid` encoding typedefs |
| [`file-res-f4-monlookup.md`](file-res-f4-monlookup.md) | `ResF4MonomialLookupTableT<Key>` |
| [`file-res-f4-m2-interface.md`](file-res-f4-m2-interface.md) | `ResF4toM2Interface` (translation layer) |
| [`file-res-memblock.md`](file-res-memblock.md) | `ResMemoryBlock<T>` (slab allocator) |
| [`file-res-moninfo-impls.md`](file-res-moninfo-impls.md) | `ResMonoidDense` / `ResMonoidSparse` (the two concrete monoid implementations) |
| [`file-res-tasking-example.md`](file-res-tasking-example.md) | `res-tasking-example.cpp` — TBB task-graph sandbox |

**Coverage:** every source file in this directory has a dedicated deep-dive doc.

## Files

### Driver

| File | Role |
|---|---|
| `res-f4-computation.{cpp,hpp}` | Top-level Computation object |
| `res-f4.{cpp,hpp}` | The F4-style reduction loop specialised for resolution |
| `res-f4-m2-interface.{cpp,hpp}` | Bridge between M2 matrices/modules and internal types |

### Schreyer frame and ordering

| File | Role |
|---|---|
| `res-schreyer-frame.{cpp,hpp}` | The frame data structure (basis + Schreyer order metadata) |
| `res-schreyer-order.hpp` | The Schreyer order itself |
| `res-monomial-sorter.{cpp,hpp}` | Sorting under the Schreyer order |
| `res-dep-graph.{cpp,hpp}` | Dependency graph used for parallel scheduling |

### Polynomial / monomial layer

| File | Role |
|---|---|
| `res-poly-ring.{cpp,hpp}` | Polynomial ring view specialised to resolution work |
| `res-moninfo.{cpp,hpp}`, `res-moninfo-dense.{cpp,hpp}`, `res-moninfo-sparse.{cpp,hpp}` | Monomial encoding, dense and sparse forms |
| `res-monomial-types.hpp` | Common typedefs |
| `res-f4-monlookup.{cpp,hpp}` | Monomial → row-index lookup |
| `res-memblock.hpp` | Bump allocator used by the inner loop |

### Reference / notes

| File | Role |
|---|---|
| `README-RES-F4` | Original developer notes |
| `TODO-branch-res-2018` | Outstanding work captured during a 2018 refactor |
| `res-tasking-example.cpp` | Example of parallel scheduling for the resolution |

## What triggers this engine

This Schreyer-frame F4 resolution engine is selected when:

| M2 user code | What happens | Why this engine |
|---|---|---|
| `freeResolution M` (no flags) | Engine dispatcher → `F4ResComputation` here | The **default** resolution engine; auto-selected |
| `resolution(M, Strategy => 4)` | Explicitly route here | Strategy 4 is this engine's strategy code |
| `freeResolution(M, Strategy => Nonminimal)` | Routed here, but skips final minimisation | Faster when minimality isn't required |
| `freeResolution(M, Strategy => NonminimalWithGB)` | Routed here, with GB pre-computed | Some special inputs |
| `freeResolution(M, LengthLimit => N)` | Routed here with a length bound | Same engine, bounded |
| `Ext^i(M, N)`, `Tor_i(M, N)` from [`Complexes`](../../packages/file-Complexes.md) | Internally calls `freeResolution` of `M`, so routes here | Indirect — through the resolution it computes |
| Resolutions inside [`Varieties`'s `HH^i F`](../../packages/file-Varieties.md) | Indirect via `Complexes.Ext` → `freeResolution` | Indirect |

For the full M2-spec → engine mapping see the parent [`resolutions.md`](../resolutions.md) "Strategy selection" section.

## Where in the engine pipeline this fits

```
M2:  freeResolution M
   ↓ comp-res.cpp dispatcher
   ↓
F4ResComputation (file-res-f4-computation.md)   — the Computation object
   ↓ owns and drives
SchreyerFrame (file-res-schreyer-frame.md)      — basis + Schreyer-order metadata
   ↓ uses
ResSchreyerOrder (file-res-schreyer-order.md)   — the order itself
MonomialSorterObject (file-res-monomial-sorter.md) — sorting under Schreyer order
ResPolyRing / ResPolynomial (file-res-poly-ring.md) — polynomial-ring view specialised for resolution
ResMonoid (file-res-moninfo.md)                  — monomial-encoding dispatcher
   ↳ ResMonoidDense or ResMonoidSparse           — picked per ring shape
                                                   (file-res-moninfo-impls.md)
ResF4MonomialLookupTableT (file-res-f4-monlookup.md) — monomial → row-index
F4Res (file-res-f4.md)                           — F4 reduction loop adapted for resolution
   ↳ ResMemoryBlock<T> (file-res-memblock.md)    — slab allocator
   ↳ TBB dependency graph (file-res-dep-graph.md) — schedules (level, degree) cells in parallel
ResF4toM2Interface (file-res-f4-m2-interface.md) — translates back to M2 modules/matrices
   ↓ produces
A free resolution as a Complex, returned via Complexes.freeResolution
```

The **Schreyer-frame trick** makes leading-term arithmetic local to each homological degree, which means the F4 reduction at level `i` only sees data from levels `i-1` and `i`. This both reduces work and enables the TBB parallelisation across `(level, degree)` cells.

## Dense vs sparse monoid choice

`ResMonoid` is a dispatcher; the actual encoding is either `ResMonoidDense` or `ResMonoidSparse` ([`file-res-moninfo-impls.md`](file-res-moninfo-impls.md)). The choice depends on:

| Ring shape | Pick | Why |
|---|---|---|
| Few variables, low expected degrees | `ResMonoidDense` | Fixed-size exponent vector; cache-friendly |
| Many variables, sparse exponents | `ResMonoidSparse` | Pack non-zero exponents only; saves memory at the cost of comparison overhead |
| Standard graded polynomial ring | Dense usually wins | Heuristic in [`file-res-poly-ring.md`](file-res-poly-ring.md) |
| Multi-graded ring with many variables | Sparse usually wins | Same heuristic |

The choice is made at `F4ResComputation` construction time and is stable for the resolution's lifetime.

## Related

- [`../comp-res.{cpp,hpp}`](../README.md) — generic Computation framework that
  this Computation conforms to.
- [`../resolutions.md`](../resolutions.md) — parent area doc with the Strategy → engine table and the full older-engines catalogue.
- Older resolution implementations: `res-a0`, `res-a1`, `res-a2` at the top
  level of `e/`. These predate this directory and are being phased out.
- [`../NCResolutions/`](../NCResolutions/README.md) — non-commutative analogue.

[← back to engine overview](../README.md)
