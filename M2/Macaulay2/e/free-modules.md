# Free modules

A **free module** `R^n` over a ring `R` is the simplest non-trivial module.
Most of the engine's machinery — matrices, Gröbner bases, resolutions —
ultimately works on free modules: a matrix is a homomorphism `F → G` between
two free modules, a GB lives in a free module, a resolution is a complex of
free modules.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Files

| File pair | Purpose |
|---|---|
| `freemod.{cpp,hpp}` | The `FreeModule` class — base ring, rank, degree vector per generator |
| `schorder.{cpp,hpp}` | Schreyer orderings on free modules — used to make leading-term computations local to a homological degree in resolutions |

## Anatomy of a `FreeModule`

A `FreeModule` carries:

- a pointer to its **base ring**
- the **rank** (number of generators)
- a **degree vector** for each generator (in the ring's degree monoid)
- optionally a **Schreyer order** (when used as the target of a syzygy module
  in a resolution)

Matrices ([`matrices.md`](matrices.md)) carry pointers to source and target
free modules; their entries respect the degrees.

## Schreyer orders

A Schreyer order on `F = R^r` is induced from a list of *leading monomials*
`(m_1, …, m_r)` (one per generator) plus the ring's ambient monomial order.
The order on `F` is:

```
e_i m  <  e_j m'   iff   m_i m  <  m_j m'   (ambient order)
                          or equal-and-then i > j (tiebreak)
```

This makes the leading term of a polynomial in `F` factor cleanly across
homological degrees, which is the trick that makes Schreyer-style resolutions
efficient. See [`schreyer-resolution/`](schreyer-resolution/README.md) for the
F4-style implementation.

## Related

- [`matrices.md`](matrices.md) — matrices between free modules.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — primary consumer
  of Schreyer orders.
- [`interface/freemodule.{h,cpp}`](interface/README.md) — public API.
