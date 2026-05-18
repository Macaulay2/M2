# `Schubert2.m2` — characteristic classes for varieties without equations

The `Schubert2` package implements **intersection theory** for
"abstract varieties" — varieties characterised by their
**Chow ring**, **tangent bundle**, **Todd class**, and **point** map
rather than by defining equations. The classical use case: compute
Chern classes, Schubert cycles, Gromov-Witten invariants, and other
intersection-theoretic invariants on Grassmannians, flag bundles,
projective bundles, and abstract Fano varieties.

The name is in homage to the original **Schubert** Maple package
(David Eisenbud, Stein A. Strømme, et al.) from the early 1990s;
`Schubert2` is the M2 successor.

- Main file: `Schubert2.m2` (1 894 lines)
- Auxiliary directory: `Schubert2/` (14 files, **5 384 lines**)
- Authors: Daniel R. Grayson, Mike Stillman, Stein A. Strømme,
  David Eisenbud, Charley Crissman
- Version: 0.7 (April 2013 — long-stable; semantic versioning
  hasn't ticked over because the API hasn't changed)
- Imports: `SchurRings`, `PushForward`, [`Varieties`](file-Varieties.md)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported types

```
AbstractVariety            -- variety described by its Chow ring + tangent + point
AbstractSheaf              -- a (formal) coherent sheaf on an AbstractVariety
AbstractVarietyMap         -- a morphism of AbstractVarieties
Bundle, FlagBundle         -- vector / flag bundles
Correspondence             -- a correspondence (cycle in X × Y)
IncidenceCorrespondence    -- (Σ, π_X, π_Y) on a flag-bundle pair
```

## Construction

```m2
base                       -- a base point (Spec k) as an AbstractVariety
point                      -- the unique class on a Spec k point
abstractVariety(d, IntersectionRing => A, …)
abstractProjectiveSpace n  -- P^n as AbstractVariety
flagBundle(L, V)           -- the flag bundle of L-flags in V
projectiveBundle(V)        -- P(V)
blowup                     -- blow up an inclusion of AbstractVarieties
extensionAlgebra           -- the algebra structure on Ext-style classes
```

## Characteristic classes

```m2
chern V             -- total Chern class c(V)
chern(i, V)         -- i-th Chern class
ch V                -- Chern character
todd V              -- Todd class
ctop V              -- top Chern class (= c_dim(V))
adams(k, V)         -- the k-th Adams operation
sectionClass        -- class of a section
integral cl         -- push forward to a point (read off the degree)
```

## Bundles & maps

```m2
tangentBundle X     -- T_X
cotangentBundle X   -- Ω_X
sectionZeroLocus    -- locus where a section vanishes
degeneracyLocus     -- locus where a map of bundles drops rank
kernelBundle f      -- kernel of f as a bundle
schubertCycle       -- Schubert cycle from partition / sequence
toSchubertBasis     -- decompose in Schubert basis
incidenceCorrespondence
```

## How "without equations" works

A standard variety in M2 (e.g. via `Proj R` from
[`Varieties`](file-Varieties.md)) needs a presentation: a ring,
its quotient, the resulting `Variety`. **Abstract varieties** in
this package skip that — instead a variety is the **data of**:

1. A **dimension** `d`.
2. An **`IntersectionRing`** — a graded quotient ring representing
   the Chow ring `A^*(X)`.
3. A **tangent bundle** as a formal class in that ring.
4. A **point** = the class of a point, used for `integral`.

No equations. No `Proj`. Just enough data to **do intersection
theory**.

This is the right setup for **abstract Fano varieties**, where you
know the cohomology ring abstractly (e.g. for `P^n`,
`A^*(P^n) = ZZ[h]/(h^{n+1})`) and want to compute intersection
numbers without ever materialising the variety as a scheme.

## The classical Schubert calculus on Grassmannians

```m2
G = flagBundle({2, 2})           -- G(2, 4): 2-planes in 4-space
                                  -- dim 4, ChowRing = ZZ[c_1, c_2, c_3, c_4] / relations
(S, Q) = bundles G               -- tautological subbundle S, quotient Q
sigma_{1,1} = schubertCycle({1, 1}, G)  -- a 2-plane meeting a fixed 2-plane in a line
sigma_{2}   = schubertCycle({2}, G)     -- a 2-plane in a fixed 3-plane
integral (sigma_{1,1} * sigma_{2})      -- = 1 (the classical Schubert calculus!)
```

This works in seconds for any Grassmannian. The bound on what's
practical is the **degree of the Chow ring**, not the dimension of
the variety — high-dimensional Grassmannians are still fast as long
as the partition computations involve a few hundred terms or so.

## `blowup` — the largest operation

```m2
blowup(f : Y → X)
  -- where f is an inclusion of AbstractVarieties
  -- returns the blow up of X along Y, plus the exceptional divisor
```

Implements the Hartshorne / Fulton formula for the Chow ring of a
blowup. The implementation is **substantial** — the main file's
~1 894 lines include the full extension-algebra construction in
`extensionAlgebra` and the chain of pushforward/pullback / normal-
bundle / projective-bundle constructions that the blowup formula
requires. `Schubert2/blowup-test.m2` (108 lines) is the
corresponding test suite.

## Auxiliary architecture

```
Schubert2.m2 (1 894 lines)              ← main: types, all standard methods, doc
   │
   ├─→ Schubert2/doc.m2 (3 514)         ← extra documentation (mostly for advanced operations)
   ├─→ Schubert2/demo.m2 (409)          ← demo: standard recipes
   ├─→ Schubert2/demo2.m2, demo3.m2     ← additional demos
   ├─→ Schubert2/demo4.m2 (326)         ← demo: Brill-Noether and other applications
   │
   ├─→ Schubert2/SymmetricProduct.m2 (69) + test (19)
   │   ← compute Chow rings of symmetric products
   │
   ├─→ Schubert2/BrillNoether.m2 (71) + test (15)
   │   ← Brill-Noether divisors on M_g
   │
   ├─→ Schubert2/blowup-test.m2 (108)   ← tests for the blowup formula
   ├─→ Schubert2/schubertCycle.m2 (22)  ← Schubert-cycle helpers
   │
   └─→ Schubert2/test-charley.m2 (454), test-dan.m2 (132), test2-dan.m2 (182)
       ← author-specific test suites
```

The `demo*.m2` files (~800 lines total) are example recipes
**not** loaded by default — they're meant to be `load`ed
interactively to follow along.

## Notable users

| Package | What it uses |
|---|---|
| [`NormalToricVarieties`](file-NormalToricVarieties.md) | Re-exports Schubert2; the toric Chow ring construction integrates with Schubert2's general Chow-ring machinery |
| `GromovWitten` (when available) | Uses Schubert2 to evaluate GW invariants on Grassmannians |
| Various Fano-classification packages | Use abstract-variety setups to do intersection theory on classified examples |

## When this is slow

| Symptom | Try |
|---|---|
| `blowup f` slow | The extension algebra grows; precompute `chern V` ranks first to bound the degrees |
| `schubertCycle(λ, G)` slow for large partitions | The cycle lives in a high-degree piece of the Chow ring; computing in `toSchubertBasis` first can be faster than direct expansion |
| `chern(adams(k, V))` very slow | Adams operations explode in length; use `Strategy => Direct` if the bundle is small |
| `integral` returns symbolic | Means there's no "point" defined on the variety; check `base` is in the construction chain |

## A note on `Strategy` / `EorH`

The package interacts with `SchurRings` and has version-sensitive
code:

```m2
schurVersion = value SchurRings.Options.Version
if  schurVersion < 0.5 then protect EorH
```

Symbols differ between SchurRings ≤ 0.4 and ≥ 0.5 — the package
guards against bugs in older SchurRings versions by protecting the
relevant symbol. If you see odd "private symbol" errors when using
this package, the `SchurRings` version is the first thing to
check.

## See also

- [`file-Varieties.md`](file-Varieties.md) — imported; gives the abstract-variety base classes Schubert2 builds on
- [`file-NormalToricVarieties.md`](file-NormalToricVarieties.md) — re-exports Schubert2 for toric Chow rings
- [`SchurRings.m2`](SchurRings.m2) — imported; provides the Schur-function ring used in Schubert-basis expansions
- [`PushForward.m2`](PushForward.m2) — imported; provides the pushforward primitive used throughout intersection theory
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — Schur rings (the natural home of the partition basis Schubert2 uses)
