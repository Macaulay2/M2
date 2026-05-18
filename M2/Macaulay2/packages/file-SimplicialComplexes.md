# `SimplicialComplexes.m2` — abstract simplicial complexes in M2

The `SimplicialComplexes` package implements **abstract simplicial
complexes** as M2 objects, with operations spanning combinatorial
topology, Stanley-Reisner theory, and free resolutions of monomial
ideals. **Not auto-loaded**, but widely used in combinatorial
commutative algebra workflows.

Major capabilities:
- Construct simplicial complexes from facets, from monomial ideals,
  or from a battery of named examples (Klein bottle, Poincaré
  sphere, dunce hat, Rudin ball, …).
- Compute the **Stanley-Reisner ideal** and its quotient ring.
- Compute the **boundary maps** and **chain complex** for homology.
- Operations: `link`, `star`, `wedge`, `inducedSubcomplex`,
  `barycentricSubdivision`, `elementaryCollapse`, `algebraicShifting`.
- **Monomial-resolution complexes**: `taylorResolution`,
  `scarfSimplicialComplex`, `buchbergerSimplicialComplex`,
  `lyubeznikSimplicialComplex` (and their associated free
  resolutions).
- **`SimplicialMap`** — morphisms of simplicial complexes.

- Main file: `SimplicialComplexes.m2` (124 lines — orchestrator)
- Auxiliary directory: `SimplicialComplexes/` (3 files, **6 534 lines**)
- Authors: Gregory G. Smith, Ben Hersey, Sasha Zotine
- Version: 2.0 (May 2022)
- Certification: published in [JSAG vol. 13, 2023](https://msp.org/jsag/2023/13-1/p05.xhtml)
- Re-exports: [`Polyhedra`](file-Polyhedra.md), [`Complexes`](file-Complexes.md)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Types

```
SimplicialComplex     -- an abstract simplicial complex on a vertex set (= variables in a polynomial ring)
SimplicialMap         -- a simplicial map between complexes
```

### Construction

```m2
simplicialComplex {fct_1, fct_2, …}    -- from a list of facets (monomials)
simplexComplex(d, R)                    -- the standard d-simplex in ring R
simplicialComplex I                     -- from a Stanley-Reisner ideal I (via complement)
```

Named examples (full list of `*Complex` constructors):

| Constructor | What it is |
|---|---|
| `kleinBottleComplex` | The Klein bottle (a triangulation) |
| `realProjectiveSpaceComplex n` | A triangulation of `RP^n` |
| `dunceHatComplex` | The dunce hat — contractible but non-collapsible |
| `bjornerComplex` | Björner's notorious counterexample |
| `bartnetteSphereComplex` | Barnette's small 3-sphere |
| `rudinBallComplex` | Rudin's non-shellable ball |
| `nonPiecewiseLinearSphereComplex` | Edwards's example |
| `poincareSphereComplex` | Brehm-Kühnel triangulation of the Poincaré homology sphere |
| `grunbaumBallComplex` | Grünbaum's non-shellable ball |
| `zieglerBallComplex` | Ziegler's non-extendably-shellable ball |
| `smallManifold(d, n, k)` | Lookup from Lutz's small-manifolds tables |

These examples are why this package is used in **combinatorial
topology research** — many of them are the hardest examples to
construct by hand.

### Stanley-Reisner & monomial-ideal interface

The package automatically constructs `monomialIdeal D` (the
Stanley-Reisner ideal) and `ring D` (its quotient). Both are
cached after first access.

### Combinatorial operations

```m2
link(D, σ)                         -- the link of a face σ
star(D, σ)                         -- the closed star
wedge(D, E, ...)                   -- wedge sum
inducedSubcomplex(D, V)            -- subcomplex on vertex set V
barycentricSubdivision D           -- sd(D)
elementaryCollapse(D, σ)           -- collapse a free face
connectedComponents D              -- list of components
isProper SimplicialMap             -- is it a proper map?
algebraicShifting D                -- compute the algebraically-shifted complex
flagfVector D                      -- the flag f-vector
```

### Homology

```m2
boundaryMap(i, D)                  -- the i-th boundary map
chainComplex D                     -- the augmented chain complex (a Complex)
HH^i D                             -- cohomology
```

The `chainComplex D` returns a `Complex` from
[`Complexes`](file-Complexes.md) (re-exported), so all the homological-
algebra apparatus applies: `HH`, `Ext`, `Tor`, `prune`, etc.

### Resolutions of monomial ideals

Named **simplicial resolutions** — chain complexes whose `i`-th
term is `R^{(faces of dim i of D)}`, where `D` is a simplicial
complex on the generators of `I`:

```m2
taylorResolution I                    -- Taylor complex (always exact, never minimal)
scarfSimplicialComplex I, scarfChainComplex I
                                      -- Bayer-Peeva-Sturmfels Scarf complex
buchbergerSimplicialComplex I, buchbergerResolution I
                                      -- Buchberger graph complex
lyubeznikSimplicialComplex I, lyubeznikResolution I
                                      -- Lyubeznik complex
```

These are alternative **resolutions of `R/I`** for `I` monomial,
each with different size / minimality properties. The Scarf
complex is minimal when `I` is "generic"; Taylor is universal but
huge; Lyubeznik / Buchberger are intermediate.

## Architecture

```
SimplicialComplexes.m2 (124 lines)        ← exports + load three big files
   │
   ├─→ SimplicialComplexes/Code.m2 (1 233 lines)
   │     The entire implementation
   │
   ├─→ SimplicialComplexes/Documentation.m2 (4 551 lines)
   │     M2-doc DSL for the entire user-facing surface
   │     (3.7× the size of Code!)
   │
   └─→ SimplicialComplexes/Tests.m2 (750 lines)
         `check "SimplicialComplexes"` test suite
```

The Documentation file is **the largest auxiliary documentation
file** in the distribution after `NormalToricVarieties`'s
`DivisorsDocumentation.m2` — `SimplicialComplexes` is unusually
thoroughly documented because most of its users are
combinatorialists who need worked examples for every named
construction.

## The two complementary views

`SimplicialComplexes` works at the **intersection of two
perspectives**:

| View | What it gives |
|---|---|
| **Combinatorial** | The complex is a set of subsets (faces) of a vertex set; operations like `link`, `star`, `barycentricSubdivision` live here |
| **Algebraic** (Stanley-Reisner) | The same complex is encoded as a monomial ideal `I_D = ⟨ products of variables of non-faces ⟩` in `R = k[x_1, …, x_n]`; the quotient `R/I_D` is the Stanley-Reisner ring |

The package translates freely between them. You can:

- Build a complex from facets, get `monomialIdeal D` automatically.
- Build a complex from a monomial ideal `I`,
  `simplicialComplex I` returns the dual complex.
- Compute simplicial-topological invariants and have them be
  algebraic invariants of `R/I_D`.

This bidirectionality is what makes `SimplicialComplexes` useful
in commutative algebra — it lets you transport problems between
the combinatorial and algebraic worlds.

## `algebraicShifting` and the `Multigrading` option

```m2
algebraicShifting D                 -- the symmetric or exterior shift
algebraicShifting(D, Multigrading => true)
```

Algebraic shifting (Kalai's construction) produces a **shifted
complex** with the same f-vector and combinatorial topology as `D`
but a simpler structure. Useful in proofs of the upper-bound
theorem and related extremal-combinatorics results. The
`Multigrading` option preserves the natural Z^n-grading.

## Boundary with other packages

| Package | Relationship |
|---|---|
| [`Complexes`](file-Complexes.md) | Re-exported; `chainComplex D` returns a `Complex` |
| [`Polyhedra`](file-Polyhedra.md) | Re-exported; the `Polyhedron` of a complex (when geometric) is computable |
| `GenericInitialIdeal` | Imports `gin` and `Multigraded` for use in `algebraicShifting` |
| [`MinimalPrimes`](file-MinimalPrimes.md) (auto-loaded) | Used for Stanley-Reisner ideal decomposition |

## When this is slow

| Symptom | Try |
|---|---|
| `barycentricSubdivision D` slow on large `D` | Subdivision multiplies vertex count by ~|faces|; consider `inducedSubcomplex` first to focus on a region |
| `algebraicShifting D` slow | Requires `gin` (generic initial ideal) — bottleneck is the GB computation, not this package |
| `taylorResolution I` slow | The Taylor resolution has `2^{#generators}` terms — only practical for small `I` |
| `scarfSimplicialComplex I` returns wrong answer | Scarf is only minimal for **generic** monomial ideals; for non-generic `I` it may not even be a resolution |

## See also

- [`file-Polyhedra.md`](file-Polyhedra.md) — re-exported by this package
- [`file-Complexes.md`](file-Complexes.md) — re-exported; `chainComplex D` lives here
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- Engine monomial-ideal ops: [`e/file-monideal.md`](../e/file-monideal.md)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — chain-complex / resolution catalogue
- [JSAG 2023 article](https://msp.org/jsag/2023/13-1/p05.xhtml) — Hersey-Smith-Zotine: *Simplicial complexes in Macaulay2*
