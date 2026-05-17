# Schreyer-frame resolution engine architecture

This document is the **architectural reference** for the modern
F4-style free-resolution engine in `M2/Macaulay2/e/schreyer-resolution/`.
A separate engine, parallel-friendly, replaces the older `res-a0`/
`res-a1`/`res-a2` engines for most workloads.

[← schreyer-resolution/ overview](README.md) · [← engine architecture](../architecture.md)

## What this engine computes

Given a finitely-presented module `M = R^a / im(f₀)`, compute a
**free resolution**:

```
… → R^{a_3} → R^{a_2} → R^{a_1} → R^{a_0} → M → 0
       d_3       d_2       d_1       d_0
```

Each `d_i` is a matrix whose columns are syzygies of `d_{i-1}`'s
columns.

## The Schreyer-frame trick

Without preparation, computing each `d_i` requires a fresh Gröbner
basis. That's expensive.

**Schreyer's theorem**: if you carry along the right
**monomial-ordering data** as you build the resolution, the syzygies
at each level have *trivial* leading terms — no GB needed.

The "Schreyer frame" is exactly this carried-along data: at level
`i`, each generator `e_j` records the **leading monomial of d_i(e_j)**.
The Schreyer order on `R^{a_i}` uses these as tiebreakers.

Result: each new differential `d_{i+1}` is computed by a single
F4-style Macaulay-matrix sweep, no GB recomputation.

## Three-layer architecture

```
┌──────────────────────────────────────────────────────┐
│   Interface + driver                                   │
│   res-f4-computation, res-f4-m2-interface              │
├──────────────────────────────────────────────────────┤
│   Algorithm                                            │
│   res-schreyer-frame (frame + level navigation)        │
│   res-schreyer-order (Schreyer-order data)             │
│   res-f4 (per-level F4 reduction)                      │
│   res-dep-graph (TBB-parallel level / degree DAG)      │
├──────────────────────────────────────────────────────┤
│   Primitives                                           │
│   res-monomial-types (type vocabulary)                 │
│   res-poly-ring (specialised polynomial ring)          │
│   res-moninfo + dense/sparse implementations           │
│   res-f4-monlookup (divisibility lookup)               │
│   res-memblock (slab allocator)                        │
│   res-monomial-sorter (sorting utility)                │
└──────────────────────────────────────────────────────┘
```

## The pipeline

```
input matrix (presentation of M)
   │
   ▼ res-f4-m2-interface.cpp
internal Schreyer-resolution representation
   │
   ▼ res-f4-computation.cpp
F4ResComputation (resumable, follows e/comp.{cpp,hpp})
   │
   ▼ res-schreyer-frame.cpp
SchreyerFrame: build level 0, level 1, ...
   │
   ▼ per level:
   ┌─────────────────────────────────────────────┐
   │ for each homological degree d:               │
   │   collect_syzygy_candidates(level, d)        │
   │   build_macaulay_matrix(level, d)            │
   │     ↓  res-monomial-types, res-poly-ring     │
   │   reduce_matrix()                            │
   │     ↓  res-f4 (F4 reduction loop)            │
   │   extract_new_basis_elements()               │
   │     ↓                                        │
   │   record_schreyer_order_for_new_module()     │
   └─────────────────────────────────────────────┘
   │
   ▼ res-f4-m2-interface.cpp
output: ChainComplex object
```

## Parallelism: TBB task graphs

The resolution can compute several `(level, degree)` cells **in
parallel** — different parts of the DAG don't interfere.

[`res-dep-graph.{cpp,hpp}`](file-res-dep-graph.md) builds an
Intel TBB `flow::graph` where each node is one `(level, degree)`
cell. Dependencies between cells (level `i` depends on level
`i-1`, lower-degree cells, etc.) form the edges.

TBB schedules the cells across worker threads. The
[`res-tasking-example.cpp`](file-res-tasking-example.md) sandbox
explores this pattern in isolation.

## Two monomial representations

Schreyer-resolution monomials need to encode both the
**exponent vector** *and* the **component index** (which generator
of the free module the term belongs to). Two implementations:

- **`ResMonoidDense`** ([`file-res-moninfo-impls.md`](file-res-moninfo-impls.md))
  — one slot per variable, plus a component slot. Memory: `O(nvars)`
  per monomial.
- **`ResMonoidSparse`** — sorted multiset of variable indices, plus
  length/hash/component prefix. Memory: `O(total_degree)` per
  monomial.

Choice depends on input shape (sparse exponents prefer sparse;
many-variable dense exponents prefer dense). The dispatcher
[`res-moninfo`](file-res-moninfo.md) picks at runtime.

## Memory model

```
Input polynomials  ←  GC-managed (Boehm)
Schreyer frame     ←  GC-managed
Per-cell scratch   ←  res-memblock (slab, released per cell)
Inner F4 state     ←  res-memblock
```

The frame and per-cell scratch separation is what lets the TBB
task graph release per-cell memory as soon as a cell's neighbours
are done with it. Without that, peak memory would dominate.

## Why a separate subdir

Three reasons:

1. **Templated heavily on monomial type** — would explode `e/`'s
   already-large file count.
2. **Parallel-aware** — TBB / `std::atomic` use is concentrated
   here.
3. **Newer code, cleaner separation** — kept apart so its
   conventions (typed integers, namespaced types, modern C++)
   don't have to retrofit the older engine.

## Used by

- M2's `resolution` user function when the modern strategy is
  selected (the default for most rings).
- The `Complexes` package indirectly.

## Comparing the resolution engines

| Engine | Implementation | Status |
|---|---|---|
| `schreyer-resolution/` (this) | F4 sweeps + Schreyer frame | Modern default |
| [`e/res-a2.{cpp,hpp}`](../file-res-old.md) | Older Schreyer-style | Backup strategy |
| [`e/res-a1.{cpp,hpp}`](../file-res-old.md) | Original resolution code | Backup strategy |
| [`e/res-a0.{cpp,hpp}`](../file-res-old.md) | Earliest resolution code | Backup strategy |
| [`NCResolutions/`](../NCResolutions/README.md) | Non-commutative case | Different domain |

The older `res-a*` engines live at top-level of `e/` and remain
selectable via the `Strategy =>` option to `resolution` for
regression testing and edge cases.

## Related

- [`README.md`](README.md) — schreyer-resolution/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine
  architectural reference.
- [`../resolutions.md`](../resolutions.md) — top-level resolution
  area.
- [`../f4/architecture.md`](../f4/architecture.md) — sister F4
  engine for GB (same algorithmic ideas, different application).
- [`../free-modules.md`](../free-modules.md) — Schreyer orders.
- [`../groebner-bases.md`](../groebner-bases.md) — GB engines used
  during frame construction.
