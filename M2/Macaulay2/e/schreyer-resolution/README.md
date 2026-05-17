# `M2/Macaulay2/e/schreyer-resolution/` — F4-style Schreyer resolutions

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

## Related

- [`../comp-res.{cpp,hpp}`](../README.md) — generic Computation framework that
  this Computation conforms to.
- Older resolution implementations: `res-a0`, `res-a1`, `res-a2` at the top
  level of `e/`. These predate this directory and are being phased out.
- [`../NCResolutions/`](../NCResolutions/README.md) — non-commutative analogue.

[← back to engine overview](../README.md)
