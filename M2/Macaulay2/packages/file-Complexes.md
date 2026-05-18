# `Complexes.m2` — chain complexes, free resolutions, Ext, Tor

The `Complexes` package implements **chain complexes** (`Complex`),
**chain complex maps** (`ComplexMap`), **free resolutions**
(`freeResolution`), **Ext** and **Tor** modules, plus a battery of
homological-algebra operations (Koszul complexes, Eagon-Northcott,
quasi-isomorphism testing, null-homotopy detection, Yoneda
extension products, …).

This is the modern **HomologicalAlgebraPackage** — the symbol
`HomologicalAlgebraPackage` set in `Core.m2` points here by
default, so this package is auto-loaded into every M2 session via
`Core#"preloaded packages"`. The older `ChainComplexes` machinery
that used to live in Core is still available via legacy hooks.

- Main file: `Complexes.m2` (252 lines — orchestration, exports, load sequence)
- Auxiliary directory: `Complexes/` (12 files, 17 548 lines — the largest aux dir in the auto-loaded set)
- Authors: Gregory G. Smith, Mike Stillman
- Re-exports: [`Truncations`](Truncations.m2)
- Imports: [`LLLBases`](LLLBases.m2)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API (~72 symbols)

### Types

```
Complex             -- a chain complex
ComplexMap          -- a map between chain complexes
GradedModule        ≡ Complex (alias for back-compat)
GradedModuleMap     ≡ ComplexMap
```

### Construction

```m2
complex(modules, differentials)  -- build a Complex from data
freeResolution M                 -- the (minimal) free resolution
res M, resolution M              -- aliases for freeResolution
koszulComplex(L)                 -- Koszul complex on a list/matrix
eagonNorthcottComplex M          -- Eagon-Northcott complex
horseshoeResolution(M, N, ...)   -- pasted resolution of an extension
cylinder f, cone f               -- mapping cylinder/cone of a ComplexMap
```

### Truncation & restriction

```m2
canonicalTruncation(C, lo, hi)   -- the "smart" truncation
naiveTruncation(C, lo, hi)       -- the "dumb" truncation
constantStrand(C, d)             -- the part of internal degree d
concentration C                  -- the range [lo, hi] where C is non-zero
```

### Maps and homotopy

```m2
randomComplexMap(D, C)           -- a random map D → C
nullHomotopy f                   -- a null homotopy of f, if one exists
isNullHomotopic f                -- does one exist?
homotopyMap(f, g, h)             -- canonical homotopy
canonicalMap, augmentationMap, minimizingMap, epicResolutionMap, resolutionMap, connectingMap
liftMapAlongQuasiIsomorphism(f, qis)
isComplexMorphism f, isQuasiIsomorphism f, isShortExactSequence f
```

### Derived functors

```m2
Ext^i(M, N), Hom(M, N), Tor_i(M, N)
yonedaExtension f, yonedaMap, yonedaProduct, yonedaExtension', yonedaMap'
longExactSequence f               -- LES of a SES
connectingExtMap, connectingTorMap
tensorCommutativity, torSymmetry  -- canonical natural isos
```

### Pruning

```m2
pruneComplex C                    -- remove redundant components
toMutableComplex C, toChainComplex C
pruneUnit, pruneDiff, isScalar    -- internal helpers
```

Plus strategy / option symbols: `FreeToExact`, `OverField`, `OverZZ`,
`Homogenization`, `Nonminimal`, `NonminimalWithGB`, `Concentration`,
`Cycle`, `Boundary`, `InternalDegree`, `UseTarget`, `Direction`,
`PruningMap`, `UnitTest`, `LocalRing`.

## Architecture

The 252-line main file is a **load orchestrator** plus the export
list. The actual implementation is split across the auxiliary
directory:

```
Complexes.m2                        ← exports + load order
   │
   ├─→ ChainComplex.m2 (1410 lines)
   │     Complex type + basic ops (sum, shift, tensor)
   │
   ├─→ FreeResolution.m2 (803 lines)
   │     freeResolution method + strategy dispatch
   │
   ├─→ ChainComplexMap.m2 (1466 lines)
   │     ComplexMap type + map algebra (composition, sum,
   │     tensor of maps), homotopy detection
   │
   ├─→ Tor.m2 (245 lines)
   │     Tor functor + symmetry isos + connecting maps
   │
   ├─→ Ext.m2 (108 lines)
   │     Ext functor + Yoneda extension product
   │
   └─→ PruneComplex.m2 (440 lines)
         Pruning (removing identity scalars from differentials)
```

The load order matters: `Tor` and `Ext` build on `ChainComplexMap`
which builds on `ChainComplex`. `PruneComplex` is independent and
loads last.

The remaining aux files are docs and tests:

| File | Role |
|---|---|
| `ChainComplexDoc.m2` (4444 lines) | M2-doc DSL for Complex / ComplexMap construction & basic ops |
| `ChainComplexMapDoc.m2` (4577 lines) | M2-doc DSL for the map algebra |
| `PruneComplexDoc.m2` (424 lines) | M2-doc DSL for pruning |
| `ChainComplexTests.m2` (2413 lines) | Main test suite (`check "Complexes"`) |
| `FreeResolutionTests.m2` (931 lines) | Free-resolution-specific tests |
| `PruneComplexTests.m2` (287 lines) | Pruning tests |

**Documentation and tests together (12 644 lines) outweigh
implementation (4472 lines) by ~3×** — this is one of the
best-documented packages in the distribution.

## `freeResolution` strategy ladder

`freeResolution M` is the most-called entry point. It dispatches by
strategy:

| Strategy | Engine path | When picked |
|---|---|---|
| `Strategy => 4` (default) | Modern F4-style Schreyer in [`e/schreyer-resolution/`](../e/schreyer-resolution/README.md) | Generic case over a field |
| `Strategy => Nonminimal` | Same engine but skips final minimisation | Faster for cases where minimality isn't needed |
| `Strategy => NonminimalWithGB` | Computes a GB first then derives the resolution | Some special inputs |
| `Strategy => 0/1/2` | Older `res-a0`/`a1`/`a2` engines | Regression testing, certain non-generic inputs |
| `Strategy => 3` | The pre-modern `Eschreyer` | Comparison / legacy |
| `OverField` / `OverZZ` | Coefficient-specific specialisation | When the user knows the base ring shape |
| `Homogenization` | Homogenise first, then resolve | Inhomogeneous inputs over graded rings |
| `LocalRing` | Local-ring case | When `R` is a `LocalRing` |

The choice is made by the engine-side dispatcher in
[`e/file-comp-res.md`](../e/file-comp-res.md); see also the
strategy table in [`e/resolutions.md`](../e/resolutions.md) and the
top-level catalogue at [`COMPUTATIONS.md`](../../../COMPUTATIONS.md).

## Boundary with the legacy `ChainComplexes`

M2 used to ship a `ChainComplexes` machinery directly in Core. The
`Complexes` package provides:

1. A **cleaner type hierarchy** (`Complex` vs the older
   `ChainComplex`).
2. **Improved correctness** for non-minimal resolutions and edge
   cases.
3. **Faster paths** for `Ext` / `Tor` via the new engine resolution.

Conversion routines (`toMutableComplex`, `toChainComplex`) bridge
the two worlds. The aliases `GradedModule => Complex`,
`GradedModuleMap => ComplexMap` exist so legacy code keeps
compiling.

The line `load "./OldChainComplexes/conversion.m2"` (lines 156-159)
is gated behind a `isPackageLoaded "OldChainComplexes"` check —
only loaded when both packages are present.

## `pruneComplex` — removing scalar units

After computing a non-minimal resolution, many differentials
contain entries that are units (`1`, `-1`, or invertible scalars).
These represent **redundant generators paired with their
syzygies**. Pruning removes such pairs, shrinking the complex
without changing its homology.

```m2
pruneComplex C => C'   -- C' is quasi-isomorphic and smaller
```

The pruning is delegated to engine routines via the `raw` exports
(`rawMutableComplex`, `rawDeleteColumns`, `rawDeleteRows`,
`rawPruneBetti`, `rawPruneComplex`, `rawPruningMorphism`) which
live in [`e/file-mutablecomplex.md`](../e/file-mutablecomplex.md).

## Ext / Tor / Yoneda products

The `Ext.m2` (108 lines) and `Tor.m2` (245 lines) files implement
the standard derived functors **on top of** the resolution
machinery: `Ext^i(M, N) = H^i(Hom(F•, N))` for a resolution `F•`.

The Yoneda product (`yonedaProduct`, `yonedaExtension`,
`yonedaMap`) provides the ring structure on `Ext^*(M, M)`. These
are central to the deformation-theory and Hochschild-cohomology
workflows.

## When this is slow

| Symptom | Try |
|---|---|
| `freeResolution` hangs | `Strategy => Nonminimal` (skips minimisation) or limit `LengthLimit => N` |
| `freeResolution` of a non-homogeneous module | `Strategy => Homogenization` (homogenise then resolve) |
| `Ext^i` very slow for large `i` | Compute the resolution first with `LengthLimit`, then `Ext` reuses it |
| Pruning runs forever | Pruning is normally fast; if slow, check that the input resolution isn't already minimal (pruning a minimal complex is wasted work) |

## See also

- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- Engine resolution machinery: [`e/resolutions.md`](../e/resolutions.md), [`e/schreyer-resolution/`](../e/schreyer-resolution/README.md)
- Engine mutable complexes: [`e/file-mutablecomplex.md`](../e/file-mutablecomplex.md) — backs `pruneComplex` and friends
- Engine `comp-res` dispatcher: [`e/file-comp-res.md`](../e/file-comp-res.md)
- [`Truncations.m2`](Truncations.m2) — re-exported by this package; provides truncation methods at a deeper level
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — full computation-engine catalogue
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
