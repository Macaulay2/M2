# Other heavy computations (`hilb`, `LLL`, `NAG`, `SLP`, `assprime`, …)

This page covers the long-running mathematical computations that have their
own files at the top level of `e/` but don't fit cleanly under
[Gröbner bases](groebner-bases.md) or [resolutions](resolutions.md).

Each follows the **Computation** pattern (resumable, with stop conditions —
see `comp.{cpp,hpp}` in [`groebner-bases.md`](groebner-bases.md)).

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Hilbert function & series

| File pair | Purpose |
|---|---|
| `hilb.{cpp,hpp}` | Hilbert function, Hilbert series, Hilbert polynomial. **Deep dive:** [`file-hilb.md`](file-hilb.md) |

## LLL lattice reduction

| File pair | Purpose |
|---|---|
| `LLL.{cpp,hpp}` | Lenstra-Lenstra-Lovász basis reduction over `ZZ`. **Deep dive:** [`file-LLL.md`](file-LLL.md) |

## Numerical algebraic geometry

| File pair | Purpose |
|---|---|
| `NAG.{cpp,hpp}` | Numerical AG entry points: continuation, witness sets, sample. **Deep dive:** [`file-NAG.md`](file-NAG.md) |
| `SLP.{cpp,hpp}` | Straight-line programs — the evaluation model used in NAG. **Deep dive:** [`file-SLP.md`](file-SLP.md) |
| `SLP-defs.hpp`, `SLP-imp.hpp` | Templates and concrete implementations of SLP node types — see [`file-SLP.md`](file-SLP.md) |

SLPs let NAG cheaply evaluate the same polynomial system at many points
without re-parsing — essential for homotopy continuation. The TODOs file
`TODO-SLPs` in this directory tracks outstanding work in this area.

## Primary decomposition support

| File pair | Purpose |
|---|---|
| `assprime.{cpp,hpp}` | Associated primes / minimal primes. **Deep dive:** [`file-assprime.md`](file-assprime.md) |

## Monomial ideals

| File pair | Purpose |
|---|---|
| `monideal.{cpp,hpp}` | Monomial ideal operations: intersection, radical, primary decomposition (specialised, much faster than the polynomial path). **Deep dive:** [`file-monideal.md`](file-monideal.md) |

A separate page of notes lives at
[`README-monideals.md`](README-monideals.md).

## Combinatorial helpers

| File pair | Purpose |
|---|---|
| `comb.{cpp,hpp}` | Binomial coefficients, partitions, combinatorial iterators used internally. **Deep dive:** [`file-comb.md`](file-comb.md) |

## Related

- [`interface/cra.{h,cpp}`](interface/README.md) — Chinese Remainder for
  reconstructing rational answers from modular computations.
- [`groebner-bases.md`](groebner-bases.md) — Computation framework these
  conform to.
- [`matrices.md`](matrices.md) — `LLL` operates on integer matrices.
