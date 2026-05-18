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

The full ~400 per-package coverage would be repetitive — these deep-dives cover the structural patterns every package follows plus the foundational and frequently-used packages that ship.

## Auto-loaded packages

The following 16+ packages are **preloaded** at every M2 startup (listed in `Core.m2`'s `Core#"preloaded packages"` table). Users get these without any `needsPackage` call:

```
Classic            HomologicalAlgebraPackage (Complexes)
ConwayPolynomials  OnlineLookup
Elimination        PackageCitations
IntegralClosure    PrimaryDecomposition  ← deep dive
InverseSystems     ReesAlgebra
Isomorphism        Saturation
LLLBases           SimpleDoc
MinimalPrimes      ← deep dive          TangentCone
                    Varieties
```

Adjusting that list (e.g. dropping `IntegralClosure` for a lightweight session) is done by modifying `Core#"preloaded packages"` per the comment in [`m2/system.m2`](../m2/system.m2).

## Related

- [`Macaulay2/m2/installPackage.m2`](../m2/installPackage.m2) — the engine of
  `installPackage`.
- [`Macaulay2/m2/document.m2`](../m2/document.m2) — the documentation DSL
  packages use.
- [`Macaulay2/tests/`](../tests/README.md) — top-level test suites distinct
  from per-package `check`.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
