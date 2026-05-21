# F4 GB engine architecture

This document is the **architectural reference** for the original
F4 Gröbner-basis engine in `M2/Macaulay2/e/f4/`. The newer
refactored variant in [`gb-f4/`](../gb-f4/README.md) has its own
companion architecture page.

[← f4/ overview](README.md) · [← engine architecture](../architecture.md) · [← engine overview](../README.md)

## What F4 is

F4 (Faugère, 1999) replaces the **S-pair-then-reduce** pattern
of Buchberger-style GB with **Macaulay-matrix sweeps**:

1. Collect every S-polynomial *and* every monomial they could
   reduce against, into one big matrix.
2. Row-reduce in finite-field linear algebra — fast.
3. Read new basis elements off the reduced rows.

The win: dense linear algebra (BLAS, FFPACK, FLINT) is orders of
magnitude faster than the symbolic S-pair-by-S-pair reduction the
classical Buchberger algorithm would do.

## Pipeline

```
input matrix of generators
   │
   ▼ f4-m2-interface.cpp
internal F4 polynomial representation
   │
   ▼ f4-computation.cpp
F4Computation (resumable, follows e/comp.{cpp,hpp})
   │
   ▼ f4.cpp
F4GB algorithm:
   ┌─────────────────────────────────────────────┐
   │ for each degree d:                           │
   │   collect_spairs_at_degree(d)                │
   │     ↓  f4-spairs.{cpp,hpp}                   │
   │   build_macaulay_matrix(d)                   │
   │     ↓  monhashtable, moninfo, monomial encs  │
   │   reduce_matrix()                            │
   │     ↓  finite-field linear algebra           │
   │   extract_new_basis_elements()               │
   │     ↓                                        │
   │   update_basis_and_pairs()                   │
   └─────────────────────────────────────────────┘
   │
   ▼ f4-m2-interface.cpp
output matrix (M2-shaped result)
```

Each "phase" lives in its own file pair; the main loop in
`f4.cpp` is the orchestration.

## Three-layer architecture

```
┌──────────────────────────────────────────────────────┐
│   Interface: f4-computation, f4-m2-interface          │
│   (wraps F4GB as a Computation; converts M2 ↔ F4)     │
├──────────────────────────────────────────────────────┤
│   Algorithm: f4 (main loop), f4-spairs, hilb-fcn      │
│   (the F4 algorithm proper + Hilbert termination)     │
├──────────────────────────────────────────────────────┤
│   Primitives: monomial encodings, hash tables, mem    │
│   (varpower, ntuple, moninfo, monhashtable, memblock) │
└──────────────────────────────────────────────────────┘
```

### Why this engine duplicates work the rest of `e/` does

F4's inner loop is **hot enough** that the engine's general-purpose
abstractions are too slow. `f4/` has its own:

- **Monomial encodings** ([`varpower-monomial`](file-varpower-monomial.md),
  [`ntuple-monomial`](file-ntuple-monomial.md)) — specialised dense
  and sparse forms.
- **Monomial hash table** ([`monhashtable`](file-monhashtable.md)) —
  not the general engine `montable`.
- **Memory block** ([`memblock`](file-memblock.md)) — bump allocator
  bypassing the GC entirely for short-lived F4 nodes.
- **Monomial-info struct** ([`moninfo`](file-moninfo.md)) — layout
  description tuned for F4's access pattern.

This is the **engine subdirectory pattern**: when a sub-system gets
hot enough to need its own internal primitives, it gets its own
subdirectory with parallel-but-specialised versions of engine-level
concepts.

## The Macaulay-matrix sweep

A single F4 step at fixed total degree `d`:

```
        ┌──────────────────────────────────────────┐
        │ monomials in degree d (columns)          │
        │ ──────────────────────────────────────── │
basis-  │ row k_1: a_11  a_12  ...                 │
multipliers (rows) ────────────────────────────────│
        │ row k_2: a_21  a_22  ...                 │
        │   ...                                    │
        │ S-pair- │ s_11  s_12  ...                │
        │ rows   ────────────────────────────────  │
        │   ...                                    │
        └──────────────────────────────────────────┘
```

The matrix is **sparse** — most cells are zero. F4's hash table
maps monomials to column indices; for each polynomial (row), only
the nonzero columns are touched.

Row-reduction over `Z/p` uses **FFPACK** or **FLINT** for dense
sub-blocks; for `ZZ`-coefficient inputs, fraction-free elimination
or modular runs with CRT lift.

## Termination

F4 needs to know when to stop. Two strategies:

1. **Degree exhaustion** — process all degrees up to a user-given
   limit.
2. **Hilbert-function-driven** — if the Hilbert function predicts
   no further reductions, stop early.

The Hilbert path is faster when Hilbert info is available; the
strategy is selectable via the `Hilbert =>` option to `gb`. See
[`hilb-fcn`](file-hilb-fcn.md).

## Memory model

F4 allocates **many small objects** (monomial nodes, polynomial
terms, S-pair records). Three approaches:

- **Hash-table interning** — monomials get unique integer IDs via
  [`monhashtable`](file-monhashtable.md). No duplicate
  representations.
- **Memory blocks** — F4-internal nodes use
  [`memblock`](file-memblock.md), a bump allocator the F4 step
  releases en masse at end.
- **GC-managed boundary** — input / output polynomials use the
  engine's standard GC machinery.

## Used by

- M2's `gb I` when strategy `F4` is selected.
- M2's `gb` over `Z/p` by default (because F4 is fastest there).
- The `MSolve` and `BoijSoederberg` packages indirectly.

## Comparing the two F4 engines

| `f4/` (this) | [`gb-f4/`](../gb-f4/README.md) |
|---|---|
| Original (Stillman/Roune, ~2012) | Refactored (Stillman, ~2020) |
| Heavily monomial-template-specialised | Cleaner type separation |
| Used by default for `Z/p` gb | Selectable via strategy option |
| In production | Newer, being graduated to default |

Both exist in parallel. `gb-f4/` is the future; `f4/` is the
current default for backward compatibility.

## Related

- [`README.md`](README.md) — f4/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine
  architectural reference (this is one of its subdirectories).
- [`../gb-f4/README.md`](../gb-f4/README.md) — newer refactored
  variant.
- [`../groebner-bases.md`](../groebner-bases.md) — top-level GB
  engine area.
- [`../schreyer-resolution/architecture.md`](../schreyer-resolution/architecture.md)
  — sister subdir that uses the same F4 ideas for resolutions.
