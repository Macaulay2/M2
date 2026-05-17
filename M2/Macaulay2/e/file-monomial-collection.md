# `monomial-collection.{cpp,hpp}` — `IntsSet` / `ModuleMonomSet`

`monomial-collection.{cpp,hpp}` is the engine's **set-of-monomials**
helper class, in transition between two names. The header's TODOs
(quoted below) sketch the planned cleanup.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Status (from the header)

```text
TODO Feb 2018:
  rename IntsSet (to e.g. ModuleMonomSet, making it not a template)
  sort() function: should match Matrix sort.
  try to have a function return a ModuleMonomSet.
  Monom, ModuleMonom:
    try to remove operator[], etc, so the type is pretty much opaque.
    add in another type: VarPowerMonom (or call it SparseMonom, …)
  use these other types from M2FreeAlgebra, commutative version.
  improve the hash function.
  remove dead code, e.g. starting at #if 0 below.
  use this code for coefficients, monomials, even in the commutative
  variant.
  get M2FreeAlgebra.m2 so 'make check' works in a reasonable amount of
  time.
  add in leadCoeff, leadMonomial, leadTerm. What other poly routines
  need to be added for M2FreeAlgebra?
```

The TODOs make clear this file is **mid-refactor**:

- The class is currently called `IntsSet` (a template); the plan is
  to rename it to `ModuleMonomSet` and make it concrete.
- A `VarPowerMonom` type is planned alongside the existing `Monom` /
  `ModuleMonom`.
- The hash function is flagged for improvement.

## What's there today

A templated `IntsSet<MonomType>` that stores a set of `MonomType`
values with:

- Insertion (returns existing index if monomial already present).
- Lookup by content.
- Iteration in insertion order.

It is used by [`M2FreeAlgebra`](file-M2FreeAlgebra.md) and the
non-commutative side to track which monomials have been encountered
across a computation, and by the commutative `gb-f4/` code (where
[`gb-f4/file-MonomialHashTable.md`](gb-f4/file-MonomialHashTable.md)
is a more polished version of this same idea).

## Used by

- [`file-M2FreeAlgebra.md`](file-M2FreeAlgebra.md) — set of words seen
  during a multiplication.
- [`NCAlgebras/`](NCAlgebras/README.md) inner code — through
  `M2FreeAlgebra`'s wrapper.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-monomial-sets.md`](file-monomial-sets.md) — the more polished
  set-of-monomials implementation (intended successor).
- [`gb-f4/file-MonomialHashTable.md`](gb-f4/file-MonomialHashTable.md)
  — the F4-side counterpart.
- [`file-M2FreeAlgebra.md`](file-M2FreeAlgebra.md) — primary consumer.
