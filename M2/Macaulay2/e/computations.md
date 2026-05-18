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

## M2 operation → engine entry

The miscellaneous M2 operations that route into this area:

| M2 operation | Engine entry | Source file | Notes |
|---|---|---|---|
| `hilbertSeries I`, `hilbertPolynomial I`, `hilbertFunction(d, I)` | `Hilbert::compute` (Bigatti algorithm) | `hilb.{cpp,hpp}` | The default; very fast for monomial ideals |
| `LLL M` | `rawLLL` → `LLLoperations::run` | `LLL.{cpp,hpp}` | Lattice basis reduction; see also [`file-LLLBases.md`](../packages/file-LLLBases.md) for the M2 package |
| `solveSystem F` (numerical) | `rawHomotopy*` + the NAG family | `NAG.{cpp,hpp}` | Homotopy continuation engine entry; user-facing path is via the [`NumericalAlgebraicGeometry` package](../packages/file-NumericalAlgebraicGeometry.md) |
| `evaluate(M, SLP, point)` | `rawSLP*` → `StraightLineProgram::evaluate` | `SLP.{cpp,hpp}` | Cheap evaluation of pre-compiled polynomial systems |
| `associatedPrimes I` (engine path) | `AssociatedPrimes::compute` | `assprime.{cpp,hpp}` | Engine implementation; M2-level wrapper in [`PrimaryDecomposition`](../packages/file-PrimaryDecomposition.md) routes through here for some strategies |
| `intersect(I, J)` on monomial ideals | `MonomialIdeal::intersect` | `monideal.{cpp,hpp}` | Specialised much faster than generic ideal intersection |
| `radical I` on monomial ideals | `MonomialIdeal::radical` | `monideal.{cpp,hpp}` | Specialised path; user-level [`MinimalPrimes`](../packages/file-MinimalPrimes.md) detects monomial input and routes here |
| `rawColon` (engine-level ideal quotient) | `MonomialIdeal::quotient` | `monideal.{cpp,hpp}` | Backs [`Saturation`](../packages/file-Saturation.md)'s `Monomial` strategy |
| Various combinatorial helpers (subsets, partitions enumeration) | `Subsets::*` | `comb.{cpp,hpp}` | Used internally by `det`, `minors`, and parts of `hilb` |

The full M2-level catalogue is in [`COMPUTATIONS.md`](../../../COMPUTATIONS.md). This page just covers the engine-internal entry points for the operations that don't fit under GB or resolution.

## Choosing a strategy

For the M2-level operations that have multiple engine paths:

| Want | Pick |
|---|---|
| `hilbertSeries I` for a monomial ideal | Default Bigatti is best; no flag needed |
| `hilbertSeries I` for a non-monomial ideal | The default routes through a GB first; consider `gb I` once and reuse |
| `LLL M` over a large integer matrix | Default is fine for engine-level; for M2-level use [`LLLBases`](../packages/file-LLLBases.md) which offers NTL / fpLLL backends |
| Numerical solving | Use the [`NumericalAlgebraicGeometry` package](../packages/file-NumericalAlgebraicGeometry.md) at the M2 level — `solveSystem F`; the engine path is opaque to most users |
| Associated-primes / minimal-primes / primary decomposition | Stay at the M2 level: [`MinimalPrimes`](../packages/file-MinimalPrimes.md), [`PrimaryDecomposition`](../packages/file-PrimaryDecomposition.md), [`Saturation`](../packages/file-Saturation.md); the engine paths here are *one* of several strategies they consult |
| Monomial-ideal ops (intersect / radical / quotient) | Ensure the M2 type is `MonomialIdeal` (not `Ideal`); then user-level calls auto-route here |

## Related

- [`interface/cra.{h,cpp}`](interface/README.md) — Chinese Remainder for
  reconstructing rational answers from modular computations.
- [`groebner-bases.md`](groebner-bases.md) — Computation framework these
  conform to.
- [`matrices.md`](matrices.md) — `LLL` operates on integer matrices.
