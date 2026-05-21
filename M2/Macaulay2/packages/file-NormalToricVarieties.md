# `NormalToricVarieties.m2` — normal toric varieties

The `NormalToricVarieties` package implements **normal toric
varieties** as M2 objects: their construction from fans, their
Picard / class / Cartier-divisor / Weil-divisor groups, the maps
between divisor groups, toric morphisms, toric divisors, sheaves on
toric varieties, **the Chow ring**, and a database of all
~120 smooth toric Fano varieties through dimension 6.

The package's central type `NormalToricVariety` is built on top of
[`Polyhedra`](file-Polyhedra.md)'s `Fan` type and the
[`Varieties`](file-Varieties.md) `Variety` hierarchy — it's the
joining of "convex geometry" with "algebraic geometry of toric
varieties" in M2.

- Main file: `NormalToricVarieties.m2` (175 lines — load orchestration + exports)
- Auxiliary directory: `NormalToricVarieties/` (12 files, **11 460 lines**)
- Author: Gregory G. Smith
- Version: 1.9 (May 2020)
- Re-exports: [`Polyhedra`](file-Polyhedra.md), [`Schubert2`](Schubert2.m2), [`Varieties`](file-Varieties.md), [`Truncations`](file-Truncations.md)
- Imports: `FourierMotzkin`, `Normaliz`, [`LLLBases`](file-LLLBases.md), [`Complexes`](file-Complexes.md)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported types

```
NormalToricVariety       -- a normal toric variety (built from a fan)
ToricDivisor             -- a toric divisor
ToricMap                 -- a morphism of toric varieties
```

## Exported API (38 symbols)

### Construction

```m2
normalToricVariety(rays, cones)        -- general toric variety
affineSpace n                          -- A^n_k
toricProjectiveSpace n                 -- P^n_k
weightedProjectiveSpace {a_0, …, a_n}  -- P(a_0, …, a_n)
hirzebruchSurface n                    -- F_n
kleinschmidt(d, L)                     -- Kleinschmidt's classification
smoothFanoToricVariety(d, i)           -- ith smooth Fano of dim d
smallAmpleToricDivisor(d, i)           -- the natural ample divisor on it
cartesianProduct(X, Y)                 -- X × Y
toricBlowup(X, sigma, v)               -- blow up cone sigma by ray v
makeSimplicial X                       -- refine to a simplicial variety
makeSmooth X                           -- refine to a smooth variety
```

### Divisor groups & maps between them

```m2
weilDivisorGroup X       -- the Weil divisor group (free Z-module of rank #rays)
cartierDivisorGroup X    -- Cartier subgroup
classGroup X             -- divisor class group Cl(X)
picardGroup X            -- Picard group Pic(X)
WeilToClass              -- the canonical projection
fromWDivToCl             -- map from W(X) to Cl(X)
fromCDivToWDiv           -- inclusion C(X) ↪ W(X)
fromCDivToPic            -- C(X) → Pic(X)
fromPicToCl              -- Pic(X) ↪ Cl(X)
toricDivisor             -- construct a ToricDivisor
```

### Morphisms

```m2
diagonalToricMap X       -- the diagonal X → X × X
isDominant ToricMap
isFibration ToricMap
isProper X
isFibration ToricMap
```

### Predicates

```m2
isAmple ToricDivisor
isCartier ToricDivisor
isDegenerate X           -- contains a torus factor?
isEffective ToricDivisor
isFano X                 -- (anticanonical divisor ample)
isNef ToricDivisor
isQQCartier ToricDivisor
```

### Orbit structure

```m2
orbits(X)                -- torus orbits as a hashtable indexed by cone
orbits(X, d)             -- orbits of given dimension
```

## Architecture

The 11 460 lines of the auxiliary directory are organised by topic
into **paired implementation + documentation** files:

```
NormalToricVarieties.m2 (175 lines)              ← orchestration + exports
   │
   ├─→ ToricVarieties.m2 (740 lines)             ← NormalToricVariety type, constructors,
   │   ToricVarietiesDocumentation.m2 (2285)       affineSpace, toricProjectiveSpace, …
   │
   ├─→ Divisors.m2 (538)                          ← ToricDivisor + the 5 divisor groups
   │   DivisorsDocumentation.m2 (2118)              and maps between them
   │
   ├─→ ToricMaps.m2 (433)                         ← ToricMap type + isDominant / isFibration
   │   ToricMapsDocumentation.m2 (1684)
   │
   ├─→ Sheaves.m2 (223)                           ← coherent sheaves on toric varieties
   │   SheavesDocumentation.m2 (449)                 (cohomology, twists)
   │
   ├─→ Chow.m2 (168)                              ← Chow ring of a toric variety
   │   ChowDocumentation.m2 (682)
   │
   ├─→ SmoothFanoToricVarieties.m2 (1459)         ← the database of all smooth Fano
   │                                                  toric varieties through dim 6
   │
   └─→ Tests.m2 (681)                             ← `check "NormalToricVarieties"`
```

**Every implementation file has a matching `*Documentation.m2`**.
Total documentation: 7 218 lines — more than the 4 561 lines of
production code. This is the second-most-thoroughly-documented
package in the distribution after [`Polyhedra`](file-Polyhedra.md).

## The five divisor groups

For a toric variety `X`, the package realises the full diagram:

```
PrincipalDivisors  ⊂  CartierDivisors  ⊂  WeilDivisors
                              │                │
                              ↓ fromCDivToPic  ↓ fromWDivToCl
                          Pic(X)  ⊂  Cl(X)
                              │                │
                              └── fromPicToCl ─┘
```

Each group is a `Module` over `ZZ`, and each map is a `Matrix`. The
implementation lives in `Divisors.m2`; the comprehensive doc in
`DivisorsDocumentation.m2` is the longest doc file in the
distribution (2 118 lines).

The `WeilToClass` map (lower-right corner) is the canonical
projection — it sends a Weil divisor to its class. The full
diagram lets users move between the various group representations
when one is easier to compute with than another.

## The smooth Fano database

`SmoothFanoToricVarieties.m2` (1 459 lines) is a **database** —
literally encoded fans — of all smooth toric Fano varieties through
dimension 6. There are exactly:

| Dim | Count |
|---|---|
| 1 | 1 |
| 2 | 5 |
| 3 | 18 |
| 4 | 124 |
| 5 | 866 |
| 6 | 7 622 |

(The total ~8 600 entries are stored as fan descriptions; only the
fans you actually instantiate are turned into `NormalToricVariety`
objects.)

Access via `smoothFanoToricVariety(d, i)` for `i` in the range for
each dimension. Useful for:

- Surveying examples (e.g. "all smooth Fano 4-folds").
- Testing conjectures across the entire smooth-Fano landscape.
- Looking up `smallAmpleToricDivisor(d, i)` to get the
  corresponding canonical ample divisor.

## Boundary with `Varieties` and `Polyhedra`

| Where | What |
|---|---|
| `NormalToricVariety` ← `Polyhedra.Fan` | Every NTV is built from a fan; methods like `rays`, `cones`, `dim` delegate to the underlying `Fan`. |
| `NormalToricVariety` ⊂ `Variety` | NTV inherits the `Variety` interface (e.g. `Spec X`, `Proj X` analogues). Sheaf operations (`HH^i F`) work through `Varieties` once you `sheaf` a module on `X`. |
| `nefCone X` | The `Varieties` package's hook for `nefCone` is overridden here to compute the actual toric nef cone — the cone in `Pic X ⊗ R` of nef classes. |
| `toric Chow ring` | The Chow ring (in `Chow.m2`) integrates with `Schubert2`'s general Chow-ring machinery. |

## When to use this vs other AG packages

| Want | Use |
|---|---|
| Generic projective variety | [`Varieties`](file-Varieties.md) (Spec / Proj / sheaf) |
| Convex polytope / cone / fan | [`Polyhedra`](file-Polyhedra.md) directly |
| Toric variety + divisor / sheaf / Chow theory | This package |
| Chow ring of any variety | `Schubert2` (re-exported here) |
| Specifically a Hirzebruch surface | `hirzebruchSurface n` (this package) |
| Specifically a weighted projective space | `weightedProjectiveSpace {a_i}` (this package) |
| Sample a Fano example for testing | `smoothFanoToricVariety(d, i)` (this package) |

## When this is slow

| Symptom | Try |
|---|---|
| Constructing `normalToricVariety(rays, cones)` for a complicated fan | Cache the fan once and reuse |
| `picardGroup X` slow | Requires computing the Cartier-Weil-Pic-Cl diagram; cached on `X.cache` after first call |
| `HH^i(F)` of a toric sheaf slow | Delegates to `Varieties`; bound it via `GlobalSectionLimit` (see [Varieties doc](file-Varieties.md)) |
| `isFano` slow | Computes the anticanonical and tests amplitude; for known examples use `smoothFanoToricVariety` lookup instead |

## See also

- [`file-Polyhedra.md`](file-Polyhedra.md) — re-exported by this package; `Fan` underpins every `NormalToricVariety`
- [`file-Varieties.md`](file-Varieties.md) — re-exported; the abstract-variety machinery NTV inherits
- [`file-Truncations.md`](file-Truncations.md) — re-exported; `nefCone` integrates here
- [`Schubert2.m2`](Schubert2.m2) — re-exported; Chow rings
- [`file-Complexes.md`](file-Complexes.md) — imported; sheaf cohomology routes through it
- [`file-LLLBases.md`](file-LLLBases.md) — imported; lattice manipulations of divisor groups
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — Cox ring (the natural homogeneous coordinate ring of a toric variety; provided by NTV but documented under graded polynomial rings)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
