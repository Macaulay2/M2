# `mutablecomplex.{cpp,hpp}` — `MutableComplex` (in-place chain complex)

`MutableComplex` is the engine's representation of a **mutable chain
complex** — a sequence of [`MutableMatrix`](file-mutablemat.md)
differentials with in-place editing. It is the cousin of
[`Matrix`](file-matrix.md)'s mutable variant, generalised to a
multi-step complex.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Why a separate type

The engine already has [`MutableMatrix`](file-mutablemat.md). A complex
is "just" a sequence of mutable matrices with the standard
composition-equals-zero invariant. Why a dedicated type?

- **Atomic editing** — when you do a row operation on one matrix, the
  invariant requires a corresponding column operation on its
  predecessor. `MutableComplex` enforces this; manual sync is
  error-prone.
- **Linked-degree tracking** — degrees of generators must propagate
  correctly through composition. `MutableComplex` knows the chain
  structure and updates degrees consistently.
- **Memory locality** — operations that span multiple levels (e.g.
  Smith normal form on the whole complex) benefit from having all
  matrices in one container.

## State

```cpp
class MutableComplex : public EngineObject {
    // vector of MutableMatrix*, one per non-zero differential
    // chained source/target free modules
    // working degree map
};
```

A complex is typically constructed from an existing resolution
([`file-comp-res.md`](file-comp-res.md)) and then edited in place to
produce minimisations, lifts, splittings, or other transformations.

## Operations

- **Row / column operations** with automatic propagation.
- **Minimise** — remove zero or constant entries by sequential row /
  column reductions.
- **Smith normal form** — across the whole complex.
- **Split** — extract a sub-complex (a chain map's image / cokernel).

Each operation calls into the underlying `MutableMatrix` operations
plus extra cross-matrix bookkeeping.

## Used by

- **Minimal resolutions** — converting a non-minimal resolution
  (e.g. from `Strategy => 1` paths in [`file-comp-res.md`](file-comp-res.md))
  into a minimal one.
- **Complex arithmetic** at the M2 level — `Hom(C, D)`,
  `C ⊗ D`, `extend(...)`.
- **Algebraic-geometry packages** that manipulate sheaf cohomology
  complexes.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-mutablemat.md`](file-mutablemat.md) — building-block type.
- [`file-comp-res.md`](file-comp-res.md) — source of input complexes.
- [`file-betti.md`](file-betti.md) — Betti tables read off the
  underlying complex shape.
- [`m2/complexes.m2`](../m2/README.md) — M2-side `Complex` type.
