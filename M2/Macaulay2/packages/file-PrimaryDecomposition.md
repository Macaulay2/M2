# `PrimaryDecomposition.m2` — primary decomposition & associated primes

The `PrimaryDecomposition` package implements `primaryDecomposition`,
`associatedPrimes`, `localize`, `primaryComponent`, `isPrimary`, and
`irreducibleDecomposition`. **Auto-loaded** — available in every M2
session without `needsPackage`. Sibling to
[`MinimalPrimes`](file-MinimalPrimes.md); together they cover the
algebraic-decomposition operations on ideals.

- Main file: `PrimaryDecomposition.m2` (639 lines)
- Auxiliary directory: `PrimaryDecomposition/` (13 files, 5323 lines)
- Authors: Mike Stillman, Carolyn Yackel, Justin Chen, Mahrud Sayrafi
- Re-exports: [`Saturation`](Saturation.m2), [`MinimalPrimes`](file-MinimalPrimes.md)
- Imports: HomologicalAlgebraPackage, [`Elimination`](Elimination.m2)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

```m2
primaryDecomposition I    -- list of primary ideals whose intersection is I
associatedPrimes I        -- the associated primes Ass(R/I)
ass I                     -- alias for associatedPrimes
isPrimary I               -- is I primary?
isPrimary(I, P)           -- is I P-primary?
localize(I, P)            -- ideal of I in R_P (a P-primary ideal)
primaryComponent(I, P)    -- the P-primary component of I
irreducibleDecomposition I  -- intersection of irreducible ideals (monomial case)
topComponents I           -- equidimensional components of top dimension
kernelOfLocalization     -- internal helper, exported
regSeqInIdeal            -- regular sequence search, exported for advanced use
```

Plus the strategy symbols:

- `ShimoyamaYokoyama` (SY)
- `EisenbudHunekeVasconcelos` (EHV)
- `GTZ` (Gianni–Trager–Zacharias)
- `Increment` (option for SY)

## Architecture

```
PrimaryDecomposition.m2                           ← entry / dispatch
   │
   ├─── Eisenbud-Huneke-Vasconcelos.m2   ← EHV strategy for associatedPrimes
   │    (299 lines)
   │
   ├─── Shimoyama-Yokoyama.m2            ← SY strategy for full primary decomposition
   │    (495 lines)                       — primdecComputation, localize, …
   │
   ├─── GTZ.m2                            ← original GTZ algorithm
   │    (623 lines)                       — primary decomposition over towers
   │
   ├─── newGTZ.m2                         ← refactored GTZ
   │    (755 lines)                       — refined version with flatteners
   │
   ├─── newGTZGenPos.m2                   ← newGTZ in general position
   │    (376 lines)
   │
   ├─── doc.m2                            ← 749-line documentation block
   ├─── examples.m2                       ← 1191-line example library
   ├─── PDexamples.m2                     ← 161 lines of harder examples
   └─── tests.m2 + *-test.m2 files        ← test suites (~669 lines)
```

## Three primary algorithms

| Strategy | Symbol | Sources | Best for |
|---|---|---|---|
| **Shimoyama–Yokoyama** | `ShimoyamaYokoyama` | `Shimoyama-Yokoyama.m2` | Default; reasonable performance on most inputs |
| **Eisenbud–Huneke–Vasconcelos** | `EisenbudHunekeVasconcelos` | `Eisenbud-Huneke-Vasconcelos.m2` | Used primarily for `associatedPrimes`; computes via Ext modules |
| **Gianni–Trager–Zacharias** | `GTZ` | `GTZ.m2`, `newGTZ.m2`, `newGTZGenPos.m2` | Inputs where SY is slow; uses generic position changes and elimination |

The dispatcher in `PrimaryDecomposition.m2` consults
`opts.Strategy`; if `null`, picks a default per problem shape.
Specialised paths exist for **monomial ideals** (handed off to the
engine's [`monideal`](../e/file-monideal.md)) and for
**squarefree polynomials**.

## EHV: associated primes via Ext

The Eisenbud–Huneke–Vasconcelos approach computes:

```
Ass(R/I) = ⋃ supp(Ext^i_R(R/I, R))
```

The implementation in `Eisenbud-Huneke-Vasconcelos.m2` walks the Ext
modules computed by the engine's resolution machinery
([`resolutions.md`](../e/resolutions.md)). For each non-zero Ext, it
extracts the supports — these are the associated primes.

This is **always slower than SY for primary decomposition**, but
direct for associated primes alone (which is its main use). It
exports `EHVprimaryDecomposition` and `HprimaryDecomposition`
(homogeneous variant).

## SY: Shimoyama–Yokoyama

The default. Sketch:

1. Find a regular sequence inside `I` of length `dim I`.
2. **Localize** away from each minimal prime, getting a sequence of
   primary components.
3. Saturate / intersect to reconstruct `I` as an intersection of
   primary ideals.

Key internal functions (defined in `Shimoyama-Yokoyama.m2`):
`primdecComputation`, `minSat`, `minSatPPD`, `quotMin`, `flattener`,
`sortByDegree`. The `Increment` option controls how aggressively the
localisation step expands at each iteration.

## GTZ: Gianni–Trager–Zacharias

Three increasingly-refined versions:

- `GTZ.m2` — original. Generic position changes + iterated
  elimination + factorisation.
- `newGTZ.m2` — refactored with cleaner flattening of the ring
  tower.
- `newGTZGenPos.m2` — adds explicit generic-position handling.

Useful when SY stalls on inputs with many embedded primes or large
nilpotent radicals.

## Boundary with `MinimalPrimes`

This package **re-exports** `MinimalPrimes`. The minimal primes of
`I` are a subset of `Ass(R/I)` (the minimal elements), so the two
operations share infrastructure:

- `minimalPrimes I` → `MinimalPrimes` (faster, dedicated)
- `associatedPrimes I` → here (also computes embedded primes)
- `primaryDecomposition I` → here (full structure)

The `installMinprimes()` function from `MinimalPrimes` affects
which implementation backs `minimalPrimes` calls inside this
package's algorithms too.

## When this is slow

| Symptom | Try |
|---|---|
| SY hangs on a small ideal | `Strategy => GTZ` or `Strategy => EisenbudHunekeVasconcelos` |
| Just associated primes | `Strategy => EisenbudHunekeVasconcelos` (often skips primary-extraction cost) |
| Monomial ideal | Already specialised — check ring is recognised as `MonomialIdeal` |
| Many embedded primes | newGTZ variants handle these better |
| Coefficient ring not `QQ` / `ZZ/p` | Some strategies don't generalise; check error message |

## Auxiliary file roles

| File | Role |
|---|---|
| `Eisenbud-Huneke-Vasconcelos.m2` | EHV strategy implementation (associated primes via Ext) |
| `Shimoyama-Yokoyama.m2` | SY strategy (default primaryDecomposition) |
| `GTZ.m2` | Original GTZ implementation |
| `newGTZ.m2` | Refactored GTZ |
| `newGTZGenPos.m2` | newGTZ + generic-position handling |
| `doc.m2` | 749-line M2 doc DSL documenting the exported API |
| `examples.m2` | Example library, 1191 lines |
| `PDexamples.m2` | Harder examples kept separate |
| `gbexample1.m2` | One specific GB-heavy example file |
| `tests.m2` | Main test suite for `check "PrimaryDecomposition"` |
| `associatedPrimes-test.m2`, `associatedPrimes2-test.m2`, `primaryDecomposition-test.m2` | Topic-specific test suites |

## See also

- [`file-MinimalPrimes.md`](file-MinimalPrimes.md) — sibling auto-loaded package
- [`Saturation.m2`](Saturation.m2) — re-exported by this package; both share the `MinimalPrimes` infrastructure
- [`Elimination.m2`](Elimination.m2) — imported by this package
- [`file-package-conventions.md`](file-package-conventions.md) — conventions
- [Engine-side associated-primes computation: `e/file-assprime.md`](../e/file-assprime.md)
- [Engine-side monomial-ideal ops: `e/file-monideal.md`](../e/file-monideal.md)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — full computation-engine catalogue
