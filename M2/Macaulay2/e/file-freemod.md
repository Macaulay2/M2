# `freemod.{cpp,hpp}` — the `FreeModule` class

`FreeModule` is the engine's representation of a free module `R^n` over a
polynomial (or other) ring `R`. It is the source and target type of every
`Matrix` and the domain/codomain of every map in the resolution code.

Part of the [Free modules](free-modules.md) area.

[← per-area: free-modules](free-modules.md) · [← engine overview](README.md)

## State

A `FreeModule` carries:

- A pointer to its **base ring** (`Ring*`).
- The **rank** `n` — number of generators.
- A **degree vector** for each generator. These live in the ring's degree
  monoid (see [`file-monoid.md`](file-monoid.md)).
- Optionally a **Schreyer order** (see [`schorder.cpp`](schorder.cpp)) — when
  the free module is the target of a syzygy module in a resolution.

## Operations

The most heavily used operations:

- `rank()` — number of generators.
- `degree(i)` — degree vector of the *i*-th generator.
- `sub_space(...)`, `direct_sum(...)` — categorical constructors.
- `compare(a_i, b_j)` — order between basis elements (uses the Schreyer order
  if one is installed).

A free module is **immutable** once constructed; transforming it produces a
new instance.

## Schreyer orders

If a Schreyer order is installed, comparison between two basis elements
`e_i · m` and `e_j · m'` proceeds via:

```
compare(m_i · m, m_j · m')      // m_i, m_j: stored leading monomials
   then tiebreak by index
```

This makes a free module's order "remember" how it arose as a syzygy module
— essential for keeping leading-term arithmetic localised to a homological
degree. The Schreyer order data lives in [`schorder.cpp`](schorder.cpp).

## How `Matrix` uses it

A `Matrix` `f : F → G` stores pointers to `F` (source) and `G` (target).
Validity invariants — degree compatibility, base-ring agreement — are checked
through these pointers. Free modules thus serve as the type-level guarantees
for matrix-shaped objects.

## Related

- [`free-modules.md`](free-modules.md) — area overview.
- [`schorder.cpp`](schorder.cpp) — Schreyer order storage.
- [`matrices.md`](matrices.md) — matrices between free modules.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — biggest consumer
  of Schreyer orders.
- [`interface/freemodule.{h,cpp}`](interface/README.md) — public C interface.
