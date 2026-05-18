# `M2/Macaulay2/packages/` — distributed packages

This directory holds the ~400 user-contributed packages that ship with
Macaulay2. Each one is either a single `Foo.m2` file or a `Foo.m2` plus a
sibling `Foo/` directory with auxiliary files (tests, examples, docs nodes,
data).

Authoritative submission and licensing guidance is in the plain-text
[`README`](README) file. This file is the navigation overview.

## How packages register

```
Foo.m2                          ← package source
Foo/                            ← optional auxiliary files
=distributed-packages           ← controls what ships (whitespace-sensitive)
```

A package is included in the distribution **only if its name appears in
`=distributed-packages`**. The file is whitespace-sensitive: one name per
line, no trailing blank lines.

(296 packages are listed there today; the `packages/` directory itself
contains other things — `EngineTests` lives in `PACKAGES_DEVEL` and is checked
but not installed.)

## CMake-side dependency rules

Some packages need extra system libraries to install. Those special
dependencies are wired up in [`CMakeLists.txt`](CMakeLists.txt). Examples:

| Package | Needs |
|---|---|
| `FourTiTwo` | `4ti2` |
| `Polyhedra` | `lrslib` |
| `Normaliz` | `normaliz` |
| `Bertini` | `bertini` |
| `gfanInterface` | `gfan` |

The matching `Find*.cmake` modules in [`M2/cmake/`](../../cmake/README.md)
exist primarily to support these packages.

## Working on a single package

Inside a running M2:

```m2
loadPackage("Foo", Reload => true)   -- code-only iteration
check "Foo"                           -- run its tests
installPackage "Foo"                  -- regenerate HTML / info / examples (slow!)
```

From the shell:

```sh
ctest -R "check-Foo"                  # CMake build
make check-Foo                        # autotools build
```

`installPackage` is intentionally slow: it runs every example to capture
output and rebuilds the info database. For pure code changes, prefer
`loadPackage("Foo", Reload => true)`.

## Adding a new package

The full procedure lives in [`README`](README), but the short version:

1. Drop `Foo.m2` (and optional `Foo/`) here.
2. **Append `Foo` to [`=distributed-packages`](=distributed-packages)**.
3. If the package needs an external library, add an entry to
   [`CMakeLists.txt`](CMakeLists.txt) and ensure
   [`M2/cmake/Find<Lib>.cmake`](../../cmake/README.md) exists.
4. Add a "Copyright and license" entry as described in [`README`](README).
5. Verify `installPackage "Foo"` and `check "Foo"` both succeed in a fresh
   build, then open a PR.

## Key-package and conventions deep dives

| Topic | Deep dive |
|---|---|
| `Macaulay2Doc.m2` — main user documentation package | [`file-Macaulay2Doc.md`](file-Macaulay2Doc.md) |
| `Style.m2` — doc styling + `generateGrammar` export | [`file-Style.md`](file-Style.md) |
| `EngineTests.m2` — M2-level engine test suite | [`file-EngineTests.md`](file-EngineTests.md) |
| Package conventions — layout, dependencies, doc DSL, tests | [`file-package-conventions.md`](file-package-conventions.md) |
| `MinimalPrimes.m2` — auto-loaded `minimalPrimes` / `radical` / `isPrime` implementation | [`file-MinimalPrimes.md`](file-MinimalPrimes.md) |
| `PrimaryDecomposition.m2` — auto-loaded `primaryDecomposition` / `associatedPrimes` / `localize` (SY, EHV, GTZ strategies) | [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) |
| `Saturation.m2` — auto-loaded `saturate` / `quotient` / `annihilator` (with the `addHook` strategy table architecture) | [`file-Saturation.md`](file-Saturation.md) |
| `Elimination.m2` — auto-loaded `eliminate` / `resultant` / `discriminant` / `sylvesterMatrix` (imported by the three above) | [`file-Elimination.md`](file-Elimination.md) |
| `Complexes.m2` — auto-loaded `HomologicalAlgebraPackage`: `Complex` / `ComplexMap` / `freeResolution` / `Ext` / `Tor` / Yoneda / pruning (12-file aux dir, ~17 500 lines) | [`file-Complexes.md`](file-Complexes.md) |
| `SimpleDoc.m2` — auto-loaded indentation-aware doc DSL (`doc ///…///` syntax used by virtually every package) | [`file-SimpleDoc.md`](file-SimpleDoc.md) |
| `Varieties.m2` — auto-loaded `Variety` / `AffineVariety` / `ProjectiveVariety` / `CoherentSheaf` / `SheafMap`; `Spec`, `Proj`, `sheaf`, `tangentSheaf`, `canonicalBundle`, `OO`, `HH^i`, `hh` | [`file-Varieties.md`](file-Varieties.md) |
| `IntegralClosure.m2` — auto-loaded `integralClosure` (ring/ideal), `conductor`, `icMap`, `icFractions`, `icFracP` (char-p variant), 6 strategies | [`file-IntegralClosure.md`](file-IntegralClosure.md) |
| `ReesAlgebra.m2` — auto-loaded JSAG-certified Rees algebras: `reesIdeal`, `associatedGradedRing`, `specialFiber`, `analyticSpread`, `multiplicity`, `minimalReduction`, `distinguished` | [`file-ReesAlgebra.md`](file-ReesAlgebra.md) |
| `LLLBases.m2` — auto-loaded `LLL`, `kernelLLL`, `hermite`, `gcdLLL`, `gramm`; dispatches across NTL / fpLLL / Cohen backends with bit-encoded strategy flags | [`file-LLLBases.md`](file-LLLBases.md) |
| `InverseSystems.m2` — auto-loaded Macaulay inverse systems: `inverseSystem` / `toDividedPowers` / `fromDividedPowers` / `fromDual` / `toDual`; the standard tool for constructing artinian Gorenstein ideals | [`file-InverseSystems.md`](file-InverseSystems.md) |
| `Isomorphism.m2` — auto-loaded probabilistic `isIsomorphic(N,M)` / `isomorphism(N,M)` / `checkDegrees` for modules; random-map approach + degree-shift detection + result caching | [`file-Isomorphism.md`](file-Isomorphism.md) |
| The five small auto-loaded utility packages: `Classic` (classic-Macaulay polynomial parser), `ConwayPolynomials` (`GF(q)` database), `OnlineLookup` (`oeis` / `isc`), `PackageCitations` (`cite`), `TangentCone` (`tangentCone`) | [`file-utility-packages.md`](file-utility-packages.md) |
| `Truncations.m2` — re-exported-by-`Complexes` `truncate` for modules/ideals/matrices + `effCone` / `nefCone` for the effective and nef cones | [`file-Truncations.md`](file-Truncations.md) |
| `Polyhedra.m2` — JSAG-certified convex polyhedra, cones, fans, polyhedral complexes; the largest single package (~44 000 lines across 60+ files); imported by `Truncations` so effectively auto-loaded | [`file-Polyhedra.md`](file-Polyhedra.md) |
| `NormalToricVarieties.m2` — normal toric varieties: `NormalToricVariety`, `ToricDivisor`, `ToricMap`; the five-divisor-group diagram; toric Chow rings; database of all smooth toric Fano varieties through dim 6 (~11 500 lines) | [`file-NormalToricVarieties.md`](file-NormalToricVarieties.md) |
| `LocalRings.m2` — operations over localised rings `R_P`: `localRing`, `liftUp`, `hilbertSamuelFunction`, `localResolution`, `localsyz`, the lift-and-descend pattern; foundational for singularity analysis | [`file-LocalRings.md`](file-LocalRings.md) |
| `BernsteinSato.m2` — Bernstein-Sato polynomials `b_f(s)`, multiplier ideals `J(f^c)`, D-module operations (restriction, localisation, de Rham / local / intersection cohomology, Weyl closure); ~6 600 lines across 15 algorithmic modules | [`file-BernsteinSato.md`](file-BernsteinSato.md) |
| `Graphs.m2` — graph theory: `Graph`/`Digraph` types, ~40+ named families (Petersen, Kneser, Johnson, …), chromatic/clique/independence numbers, edge ideals; single-file 5 542-line package | [`file-Graphs.md`](file-Graphs.md) |
| `EdgeIdeals.m2` — JSAG-certified `Graph` + `HyperGraph` tuned for edge-ideal analysis: `edgeIdeal`, `coverIdeal`, `cliqueComplex`, `independenceComplex`, good-leaf splitting, `isCM`/`isSCM` Cohen-Macaulay tests | [`file-EdgeIdeals.md`](file-EdgeIdeals.md) |
| `FrobeniusThresholds.m2` — JSAG-certified char-p commutative algebra: `fpt f` (F-pure threshold), `isFJumpingExponent`, `frobeniusNu`, Frobenius powers / roots; the char-p counterpart of `BernsteinSato` | [`file-FrobeniusThresholds.md`](file-FrobeniusThresholds.md) |
| `Posets.m2` — JSAG-certified partially ordered sets: ~15 named families (booleanLattice, divisorPoset, dominanceLattice, …), Hibi ideal & ring, Hasse diagram, order complex, Möbius function, lattice-property predicates | [`file-Posets.md`](file-Posets.md) |
| `GraphicalModels.m2` — algebraic statistics: `discreteVanishingIdeal`, `gaussianVanishingIdeal`, `conditionalIndependenceIdeal`, `markovRing` / `gaussianRing`, `trekIdeal`/`trekSeparation`, global/local/pair Markov properties | [`file-GraphicalModels.md`](file-GraphicalModels.md) |
| `WeylAlgebras.m2` — D-module infrastructure: `makeWeylAlgebra`, `gbw`, `Ddim`, `isHolonomic`, Fourier transform, Stafford-Smith `makeCyclic`, `factorWA`; foundation that `BernsteinSato` and `HolonomicSystems` sit on | [`file-WeylAlgebras.md`](file-WeylAlgebras.md) |
| `HolonomicSystems.m2` — holonomic D-module algorithms: GKZ hypergeometric systems (`gkz`, `AppellF1`), canonical-series solutions (`cssExpts`, `indicialIdeal`, `solveFrobeniusIdeal`), differential-operator rings (`diffOps`); middle layer between `WeylAlgebras` and `BernsteinSato` | [`file-HolonomicSystems.md`](file-HolonomicSystems.md) |
| `Bertini.m2` — M2 interface to the Bertini numerical solver; `bertiniZeroDimSolve`, `bertiniPosDimSolve`, `bertiniParameterHomotopy`, multi-precision (`MPType`) controls; backend of `NumericalAlgebraicGeometry`'s `Software => BERTINI` | [`file-Bertini.md`](file-Bertini.md) |
| `PHCpack.m2` — JSAG-certified M2 interface to the PHCpack polyhedral-homotopy solver; `solveSystem`, `mixedVolume`, `cascade`, `numericalIrreducibleDecomposition`, multi-threading, multi-precision; backend of `Software => PHCPACK` | [`file-PHCpack.md`](file-PHCpack.md) |
| `FourTiTwo.m2` — interface to the external 4ti2 library: `toricMarkov`, `toricGroebner`, `toricCircuits`, `toricGraver`, `hilbertBasis` (overrides Polyhedra's); foundational for toric ideals, Markov bases, Graver bases | [`file-FourTiTwo.md`](file-FourTiTwo.md) |
| `Normaliz.m2` — JSAG-certified interface to Normaliz: `normalToricRing`, `intclToricRing`, `intclMonIdeal`, `ehrhartRing`, `torusInvariants`, OpenMP-threaded Hilbert-basis / integral-closure / Ehrhart algorithms | [`file-Normaliz.md`](file-Normaliz.md) |
| `gfanInterface.m2` — interface to Anders Jensen's Gfan: ~60 `gfan*` operations for Gröbner fans, tropical varieties (`gfanTropicalVariety`, `gfanTropicalBasis`, `gfanTropicalTraverse`), fan refinements, `MarkedPolynomialList` type | [`file-gfanInterface.md`](file-gfanInterface.md) |
| `Tropical.m2` — research-level tropical geometry: `TropicalCycle` type, `tropicalVariety`, `tropicalPrevariety`, `isTropicalBasis`, `stableIntersection`, `BergmanFan`, `isBalanced`, `multiplicities`, min/max convention switch | [`file-Tropical.md`](file-Tropical.md) |
| `Matroids.m2` — JSAG-certified matroid theory: `Matroid` type from matrices/graphs/ideals/axioms, ~97 operations (deletion/contraction/duality/minors/Tutte polynomial), `chowRing M` for Adiprasito-Huh-Katz; named-matroid database (Fano, Vámos, …) | [`file-Matroids.md`](file-Matroids.md) |
| `Schubert2.m2` — intersection theory for "abstract varieties": `AbstractVariety`, `AbstractSheaf`, Chern/Todd classes, `schubertCycle`, `blowup`, `flagBundle`; M2 successor to the classical Maple `Schubert` package | [`file-Schubert2.md`](file-Schubert2.md) |
| `NumericalAlgebraicGeometry.m2` — JSAG-certified numerical AG: homotopy continuation, `solveSystem`, witness sets, irreducible decomposition; multiple software backends (M2engine, BERTINI, PHCPACK, HOM4PS2) | [`file-NumericalAlgebraicGeometry.md`](file-NumericalAlgebraicGeometry.md) |
| `SimplicialComplexes.m2` — JSAG-certified abstract simplicial complexes; Stanley-Reisner interface; named topological examples (Klein bottle, Poincaré sphere, …); resolutions of monomial ideals (Taylor, Scarf, Buchberger, Lyubeznik) | [`file-SimplicialComplexes.md`](file-SimplicialComplexes.md) |

The full ~400 per-package coverage would be repetitive — these deep-dives cover the structural patterns every package follows plus the foundational and frequently-used packages that ship.

## Auto-loaded packages

The following 17 packages are **preloaded** at every M2 startup (listed in `Core.m2`'s `Core#"preloaded packages"` table plus `HomologicalAlgebraPackage`, which resolves to `Complexes`). Users get these without any `needsPackage` call. **All 17 now have deep-dive coverage**:

| Package | Deep dive |
|---|---|
| `Classic` | [`file-utility-packages.md`](file-utility-packages.md) (batched) |
| `Complexes` (HomologicalAlgebraPackage) | [`file-Complexes.md`](file-Complexes.md) |
| `ConwayPolynomials` | [`file-utility-packages.md`](file-utility-packages.md) (batched) |
| `Elimination` | [`file-Elimination.md`](file-Elimination.md) |
| `IntegralClosure` | [`file-IntegralClosure.md`](file-IntegralClosure.md) |
| `InverseSystems` | [`file-InverseSystems.md`](file-InverseSystems.md) |
| `Isomorphism` | [`file-Isomorphism.md`](file-Isomorphism.md) |
| `LLLBases` | [`file-LLLBases.md`](file-LLLBases.md) |
| `MinimalPrimes` | [`file-MinimalPrimes.md`](file-MinimalPrimes.md) |
| `OnlineLookup` | [`file-utility-packages.md`](file-utility-packages.md) (batched) |
| `PackageCitations` | [`file-utility-packages.md`](file-utility-packages.md) (batched) |
| `PrimaryDecomposition` | [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) |
| `ReesAlgebra` | [`file-ReesAlgebra.md`](file-ReesAlgebra.md) |
| `Saturation` | [`file-Saturation.md`](file-Saturation.md) |
| `SimpleDoc` | [`file-SimpleDoc.md`](file-SimpleDoc.md) |
| `TangentCone` | [`file-utility-packages.md`](file-utility-packages.md) (batched) |
| `Varieties` | [`file-Varieties.md`](file-Varieties.md) |

Adjusting that list (e.g. dropping `IntegralClosure` for a lightweight session) is done by modifying `Core#"preloaded packages"` per the comment in [`m2/system.m2`](../m2/system.m2).

## Related

- [`Macaulay2/m2/installPackage.m2`](../m2/installPackage.m2) — the engine of
  `installPackage`.
- [`Macaulay2/m2/document.m2`](../m2/document.m2) — the documentation DSL
  packages use.
- [`Macaulay2/tests/`](../tests/README.md) — top-level test suites distinct
  from per-package `check`.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
