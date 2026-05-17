# `Hom.m2` — `Hom` and `Ext` functors

`Hom.m2` defines the **`Hom`** functor and related operations
(`Ext`, dual, lift, image) for modules. It is the module-theoretic
counterpart of `Tensor` on `modules.m2`.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-----------------------------------------------------------------------------
-- Hom functor and related methods
-----------------------------------------------------------------------------
-- most of this file used to be in modules2.m2

needs "modules.m2"
needs "matrix1.m2"

Hom = method(Options => {
    DegreeLimit       => null,
    ...
})
```

"Most of this file used to be in modules2.m2" — the contents were
hoisted out into their own file when `Hom` became one of M2's
most-used operations and deserved its own home.

## What `Hom` does

`Hom(M, N)` for two modules `M`, `N` over the same ring:

- If both modules are free, returns a free module of rank
  `rank M · rank N` whose generators are the matrix entries
  `e_i → f_j`.
- If either is not free, computes via a presentation: `M = ker
  (R^a → R^b)`, so `Hom(M, N) = ker (Hom(R^b, N) → Hom(R^a, N))`.

Internally the operation is a ring-theoretic computation that
typically requires a GB.

## Related operations

- **`Hom(f, N)`** for a map `f` — induced map.
- **`Ext^i(M, N)`** — the *i*-th `Ext` group (homology of `Hom` of a
  resolution).
- **`Tor_i(M, N)`** — the *i*-th `Tor` group (homology of tensor of
  a resolution).
- **`dual M = Hom(M, R)`** — algebraic dual.
- **`adjoint(f, X, Y)`** — for a map `f: X × M → N`, produces
  `g: X → Hom(M, N)`.

Each is defined either in this file or in
[`file-complexes.md`](file-complexes.md) (for `Ext` and `Tor`).

## Options

The `Options => {DegreeLimit => null, ...}` block lets the user cap
computation: `Hom(M, N, DegreeLimit => 5)` computes only the part
of `Hom` up to degree 5. Useful for infinite-dimensional `Hom` cases
where the user only wants a finite piece.

## Used by

- M2 users computing module `Hom`s, `Ext`s, `Tor`s.
- Algebraic-geometry packages computing sheaf cohomology.
- Commutative-algebra packages computing module invariants.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-modules.md`](file-modules.md) — `Module` type.
- [`file-complexes.md`](file-complexes.md) — `Ext` / `Tor` are
  defined there.
- [`file-gb.md`](file-gb.md) — GB used in non-free `Hom`.
- `multilin.m2` — tensor / symmetric / exterior products.
