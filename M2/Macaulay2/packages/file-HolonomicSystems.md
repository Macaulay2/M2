# `HolonomicSystems.m2` — holonomic D-module algorithms

The `HolonomicSystems` package is the **middle layer of the M2 D-module stack**: it sits between [`WeylAlgebras`](file-WeylAlgebras.md) (the Weyl-algebra type + basic algorithms) and [`BernsteinSato`](file-BernsteinSato.md) (b-functions, multiplier ideals, D-module operations). The package focuses on **specific holonomic systems** — GKZ hypergeometric systems, Appell `F_1`, differential operators on toric ideals — and on **canonical-series solutions** to holonomic systems.

The package's headline operations:
- **`gkz(A, β)`** — the GKZ (Gelfand-Kapranov-Zelevinsky) hypergeometric system for a matrix `A` and parameter vector `β`.
- **`AppellF1`** — the Appell `F_1` hypergeometric system.
- **`canonicalSeries`** — series solutions to a holonomic D-module via Frobenius's method.
- **`indicialIdeal`** — the indicial ideal (intersection with the Theta ring `k[x_i ∂_i]`) of a holonomic system.
- **`diffOps`** — the ring of differential operators of a graded ring.

- Main file: `HolonomicSystems.m2` (115 lines — orchestration)
- Auxiliary directory: `HolonomicSystems/` (3 implementation files + `DOC/`, `TST/`, `EXA/` subdirectories)
- Authors: Mahrud Sayrafi, Christine Berkesch, Anton Leykin, Harrison Tsai
- Version: 1.0 (May 2023)
- Re-exports: [`WeylAlgebras`](file-WeylAlgebras.md)
- Imports: `AssociativeAlgebras`, [`PrimaryDecomposition`](file-PrimaryDecomposition.md), [`ReesAlgebra`](file-ReesAlgebra.md), [`Elimination`](file-Elimination.md), `FourTiTwo`, [`Polyhedra`](file-Polyhedra.md)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### GKZ hypergeometric systems (`Dsystems.m2`)

```m2
gkz(A, β)                    -- the GKZ system H_A(β) ⊂ W
                              -- where A is an integer matrix, β a parameter vector
eulerOperators(A, β)         -- the Euler operators for the system
toricIdealPartials A         -- the partial-derivative-substituted toric ideal
AppellF1                     -- the Appell F_1 hypergeometric system
Vars                          -- option: name the variables
```

These are the **specific holonomic systems** that arise across complex analysis, mathematical physics, and number theory. The package's `gkz` constructor takes a matrix `A ∈ Z^{d×n}` and a parameter vector `β ∈ Z^d` and returns the GKZ system as an ideal in a Weyl algebra. The construction uses **toric methods** via the imported `FourTiTwo`.

### Canonical-series solutions (`canonicalSeries.m2`)

```m2
isTorusFixed I               -- is the ideal torus-fixed?
cssExpts I                   -- exponents of the canonical-series solutions
cssExptsMult I               -- exponents with multiplicity
cssLeadTerm                  -- leading term of a canonical series
distraction f                -- the "distraction" of a polynomial (used in indicial analysis)
indicialIdeal I              -- the indicial ideal (intersection with the Theta ring)
solveFrobeniusIdeal I        -- solve a torus-fixed (Frobenius) ideal
nilssonSupport I             -- the Nilsson support of the system
truncatedCanonicalSeries     -- truncated power-series solutions
```

The `cssExpts` / `canonicalSeries` family implements **Frobenius's method** for series solutions to a holonomic system. The starting point is the **indicial ideal** — a 0-dimensional ideal in the Theta ring `k[θ_1, …, θ_n]` (where `θ_i = x_i ∂_i`) whose roots give the exponents of the leading terms of all solutions. From these exponents the package builds the full canonical-series solutions.

### Differential operators (`DiffOps.m2`)

```m2
diffOps(I, k)                -- generators of the k-th order differential operators
                              -- on the quotient ring R/I
putWeylAlgebra W             -- declare a Weyl algebra for differential-operator computation
PolyGens                     -- the polynomial generators
BasisElts                    -- the basis elements
```

The "differential operators of `R/I`" form a (non-commutative) ring related to the Weyl algebra; this part of the package computes generators up to a fixed order.

### Theta-ring infrastructure (re-exported from `WeylAlgebras`)

```m2
createThetaRing W, ThetaRing, WtoT
```

The Theta ring `k[θ_i = x_i ∂_i]` is the central tool for indicial analysis; it's defined in `WeylAlgebras` but **re-exported here** so users don't have to load both packages explicitly.

## Architecture

```
HolonomicSystems.m2 (115 lines)            ← orchestration + exports
   │
   ├─→ Dsystems.m2                          — GKZ, AppellF1, toricIdealPartials
   ├─→ canonicalSeries.m2                   — Frobenius's method, indicialIdeal,
                                              solveFrobeniusIdeal, cssExpts,
                                              nilssonSupport, truncatedCanonicalSeries
   ├─→ DiffOps.m2                            — differential operators on R/I
   │
   ├─→ DOC/Dsystems.m2, canonicalSeries.m2,
   │      DiffOps.m2, main.m2                ← M2-level documentation
   ├─→ TST/tests.m2, canonicalSeries.m2      ← test suite
   └─→ EXA/DiffOps.exa.m2                    ← worked examples (opt-in)
```

The split into three implementation files mirrors the three exposed
subject areas: GKZ systems (`Dsystems`), canonical-series analysis
(`canonicalSeries`), differential-operator rings (`DiffOps`).

## Why the GKZ system matters

The **GKZ system `H_A(β)`** is a uniform family that **specialises
to most named classical hypergeometric systems**: Gauss `_2F_1`,
Appell `F_1` / `F_2` / `F_3` / `F_4`, Lauricella `F_A` / `F_B` /
`F_C` / `F_D`, Horn series, etc. By choosing the matrix `A`, you
recover the corresponding classical system.

For algebraic geometers, GKZ systems also appear in **mirror
symmetry**: the Picard-Fuchs equations of toric Calabi-Yau families
are GKZ systems.

The package's `gkz(A, β)` constructor:
1. Builds the **toric ideal** `I_A` (via `toricMarkov` from `FourTiTwo`).
2. Computes its **partial-derivative substitution** — replacing each variable `x_i` in `I_A` with `∂_i`, yielding an ideal in the Weyl algebra.
3. Adjoins the **Euler operators** `E_j = Σ_i A_{ji} x_i ∂_i - β_j` (one per row of `A`).
4. Returns the resulting ideal.

The ideal is the GKZ system. Operations like `cssExpts`,
`indicialIdeal`, `isHolonomic`, and `Ddim` (from `WeylAlgebras`)
extract its solution-space invariants.

## Canonical-series solutions — what `cssExpts` computes

For a torus-fixed holonomic system, every solution has a
"canonical-series" expansion of the form:

```
f = Σ x^α · log(x)^μ · (Taylor series in x)
```

where `(α, μ)` ranges over the **exponent-multiplicity pairs**
encoded in the indicial ideal. `cssExpts I` returns the list of
`(α, μ)` pairs. From these, `truncatedCanonicalSeries` extracts
the actual series solutions up to a chosen truncation order.

The package's algorithm follows Saito-Sturmfels-Takayama's
*Gröbner Deformations of Hypergeometric Differential Equations*
(Springer 2000).

## When this is slow

| Symptom | Try |
|---|---|
| `gkz(A, β)` slow for large `A` | The bottleneck is the toric-ideal computation; ensure `FourTiTwo`'s external binary is installed |
| `cssExpts I` returns enormous expressions | The exponent count grows with `Ddim`; truncate via `nilssonSupport` first |
| `indicialIdeal I` slow | Routed through GB in the Theta ring; the `WeylAlgebras` weight-order machinery is what does the work |
| `diffOps(I, k)` very slow for large `k` | Differential-operator rings grow polynomially in `k`; bound `k` |

## Hierarchy recap

```
WeylAlgebras           — Weyl-algebra type, gbw, Ddim, isHolonomic, makeCyclic, factorWA
   ↓ inherits
HolonomicSystems       — GKZ systems, AppellF1, canonical-series, differential-operator rings
   ↓ inherits
BernsteinSato          — b-functions, multiplier ideals, Drestriction/Dlocalize/DHom/DeRham/...
   ↓ inherits
User code               — Picard-Fuchs equations, mirror symmetry, log-canonical thresholds, ...
```

This package is the **bridge** between general Weyl-algebra
infrastructure (which is hard to use directly for specific
systems) and the operational D-module algorithms (which need
specific systems as input). For computational mirror symmetry,
hypergeometric function research, and series-solution analysis,
this is the package to load.

## See also

- [`file-WeylAlgebras.md`](file-WeylAlgebras.md) — re-exported by this package; the foundation
- [`file-BernsteinSato.md`](file-BernsteinSato.md) — downstream consumer; b-functions and multiplier ideals
- [`file-Polyhedra.md`](file-Polyhedra.md) — imported; used for fan / cone computations on toric inputs
- [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md), [`file-Elimination.md`](file-Elimination.md), [`file-ReesAlgebra.md`](file-ReesAlgebra.md) — imported (auto-loaded)
- `FourTiTwo` package — imported; provides `toricMarkov` for `gkz`
- `AssociativeAlgebras` package — imported; provides NC algebra infrastructure
- Engine Weyl algebra: [`e/file-weylalg.md`](../e/file-weylalg.md)
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — Weyl-algebra entry
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- Saito-Sturmfels-Takayama, *Gröbner Deformations of Hypergeometric Differential Equations*, Springer 2000
