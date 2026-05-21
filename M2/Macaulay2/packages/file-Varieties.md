# `Varieties.m2` — affine / projective varieties & coherent sheaves

The `Varieties` package provides M2's working-level **algebraic
geometry types**: `Variety`, `AffineVariety`, `ProjectiveVariety`,
`CoherentSheaf`, `SheafOfRings`, `SheafMap`, plus the operations
on them: `Proj`, `Spec`, `sheaf`, `tangentSheaf`, `canonicalBundle`,
`idealSheaf`, `cohomology` (via `hh`), `sheafExt`, `sheafHom`, and
the structure-sheaf functor `OO`.

**Auto-loaded** — every M2 session has these types available.
Until 2024 they lived directly in Core (`m2/varieties.m2`); the
move to a package was a deliberate decoupling.

- Main file: `Varieties.m2` (918 lines)
- Auxiliary directory: `Varieties/` (15 files, 4 776 lines)
- Authors: Devlin Mallory, Ritvik Ramkumar, Mahrud Sayrafi, Gregory
  G. Smith, Keller VandeBogert, John Cobb
- Re-exports: HomologicalAlgebraPackage ([`Complexes`](file-Complexes.md)), [`Saturation`](file-Saturation.md)
- Date: 28 Feb 2025

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Types

```
Variety              -- abstract base
  AffineVariety      -- Spec of a ring
  ProjectiveVariety  -- Proj of a graded ring
CoherentSheaf        -- a coherent sheaf on a Variety
SheafOfRings         -- the structure sheaf O_X (special CoherentSheaf)
SheafMap             -- a morphism of coherent sheaves
SheafExpression      -- formal expression used in printing
SumOfTwists          -- formal sum F(>=d) ⊕ F(>=d+1) ⊕ …
LowerBound           -- the >=d / >d part of a twist
```

### Construction

```m2
Spec R                     -- the affine variety Spec R
Proj R                     -- the projective variety Proj R (R graded)
variety I                  -- variety from an ideal / sheaf / module
sheaf M                    -- the sheaf associated to a graded module
idealSheaf I               -- the sheaf I~ for an ideal
```

### Sheaf operations

```m2
sheafHom(F, G)             -- internal Hom sheaf
sheafExt^i(F, G)           -- internal Ext sheaf
tangentSheaf X             -- T_X (the tangent sheaf)
cotangentSheaf X           -- Ω_X (the cotangent sheaf)
canonicalBundle X          -- ω_X (top exterior power of Ω)
hh^(p,q)(X)                -- Hodge numbers
HH^i(F)                    -- sheaf cohomology
OO_X                       -- structure sheaf
OO_X^a (or OO_X^(d, ...))  -- O(d) twists
F(d)                       -- the d-th twist of sheaf F
F**G                       -- tensor of sheaves
F^* (G)                    -- dual sheaves
isProjective X
isLocallyFree F
isHomogeneous F
```

### `SheafMap` operations

`SheafMap`s (defined in `Varieties/SheafMaps.m2`) support
composition, sum, kernel, cokernel, image, coimage, and lifting
between matching twists. The `inverseImage`, `pushforward`, and
related functors are implemented via the underlying `m2`-level
homomorphism machinery.

### Strategy / options

`GlobalSectionLimit`, `SaturationMap`, `TorsionFree` — control
which truncation bound / saturation strategy `HH^i` uses when
computing global sections.

## Architecture

```
Varieties.m2 (918 lines)                ← types + dispatch + most methods
   │
   ├─→ Varieties/SheafMaps.m2 (832)     ← SheafMap type + map algebra
   ├─→ Varieties/SheafComplexes.m2 (278)  ← Complexes of sheaves (disabled by default;
                                            load line is commented out at L813 — work-in-progress)
   │
   ├─→ doc-varieties.m2 (453)           ← doc DSL for Variety / Spec / Proj
   ├─→ doc-sheaves.m2 (982)             ← doc DSL for CoherentSheaf / SheafOfRings
   ├─→ doc-maps.m2 (242)                ← doc DSL for SheafMap
   ├─→ doc-functors.m2 (634)            ← doc DSL for tangent / cotangent / canonical / Ext / Hom
   ├─→ doc-complexes.m2 (41)            ← doc DSL for sheaf complexes (matching the work-in-progress)
   ├─→ euler-doc.m2 (152), genera-doc.m2 (95), genus-doc.m2 (61)
   │     doc DSL for the Euler-characteristic and genus invariants
   │
   └─→ tests-varieties.m2 (297), tests-maps.m2 (500), tests-sheaves.m2 (8),
       tests-functors.m2 (34), tests-complexes.m2 (167)
```

**Notable disabled-by-default features**: lines 813 and 823 in
`Varieties.m2` show `--load "./Varieties/SheafComplexes.m2"` and
`--load "./Varieties/tests-complexes.m2"` commented out. The
**`Complex` of `CoherentSheaf` ⨂ `SheafMap`** infrastructure is
implemented but not yet enabled by default. Enable it locally by
uncommenting those lines after `loadPackage("Varieties", Reload =>
true)`.

## How `sheaf` works

The path from a graded module `M` over a graded ring `R` to a
sheaf `M~` on `Proj R`:

1. **Check ring shape**: `checkRing` (lines 99-103) verifies the
   ring has `degreeLength == 1` and that all variables have the
   same degree — the standard projective hypothesis.
2. **Saturate**: drop torsion at the irrelevant ideal `m =
   (x_0,…,x_n)`. The saturation is cached so re-sheafifying the
   same module is cheap.
3. **Wrap**: the resulting saturated module becomes the
   `CoherentSheaf`'s data; the variety is stored as a back
   reference.

Twisting `F(d)` shifts the underlying module's degrees by `d`.
This makes `F(d)` cheap (no recomputation) and lets `O_X(d)` be
built lazily from `O_X`.

## How `HH^i F` works (sheaf cohomology)

Sheaf cohomology in M2 is computed via the **truncation + module
cohomology** trick:

1. For sufficiently large `d`, `HH^i(F) = (truncation of F at d)
   evaluated by graded-module Ext`.
2. The `GlobalSectionLimit` option controls the truncation bound;
   the package's default uses `regularity F + 1` when known.
3. Underlying `Ext` calculations bottom out at
   [`Complexes.Ext`](file-Complexes.md).

The same machinery powers `hh^(p,q)`, `genus`, `genera`, `euler`
(see the `euler-doc.m2` / `genera-doc.m2` / `genus-doc.m2`
doc-only files).

## Boundary with Core's `m2/varieties.m2`

The Core file `m2/varieties.m2` historically held everything;
since the 2024 extraction it holds only:

- Forward declarations the type system needs early.
- Conversion helpers between the old Core types and the new
  package types.

All operations now live in this package. Code that did
`needsPackage "Varieties"` continues to work (it's auto-loaded
anyway); code that relied on `Variety` being defined in Core
continues to work because the type's symbol still binds.

## What this package depends on

- [`Complexes`](file-Complexes.md) — for `freeResolution`, `Ext`,
  `Tor` underlying every sheaf-cohomology computation.
- [`Saturation`](file-Saturation.md) — for the saturation step in
  `sheaf M`.
- Engine machinery for graded modules and matrices —
  [`e/free-modules.md`](../e/free-modules.md),
  [`e/matrices.md`](../e/matrices.md).

## When this is slow

| Symptom | Try |
|---|---|
| `HH^i F` very slow | Increase `GlobalSectionLimit` (it's bounding `Ext` work too low) or pass `TorsionFree => true` if `F` is known torsion-free |
| `sheaf M` slow | Likely the saturation step; cache it via `M.cache.SaturationMap` and reuse |
| `tangentSheaf X` slow | Computed via cotangent + dual; the cotangent itself is a `kernel` of a Jacobian map and can be expensive — bound it with degree limits if possible |
| `SheafComplexes` features unavailable | Enable manually by uncommenting the load lines (see Architecture above) |

## See also

- [`file-Complexes.md`](file-Complexes.md) — re-exported by this package
- [`file-Saturation.md`](file-Saturation.md) — re-exported by this package
- [`file-Macaulay2Doc.md`](file-Macaulay2Doc.md) — main user doc; the sheaf section now points here
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- M2 Core no longer has a separate `m2/varieties.m2` file deep-dive — since the 2024 extraction, the Core stub is minimal and the substance is here
- Engine: [`e/free-modules.md`](../e/free-modules.md), [`e/matrices.md`](../e/matrices.md)
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — graded rings underlying `Proj`
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — `Ext` / `Tor` backing sheaf cohomology
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
