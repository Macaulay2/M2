# `GraphicalModels.m2` — algebraic statistics of graphical models

The `GraphicalModels` package implements **algebraic statistics**
applied to **graphical models** — joint probability distributions
whose conditional-independence structure is encoded by a graph
(directed, undirected, or mixed). The package's central operations
build the polynomial ideals that vanish on the model:

- **`conditionalIndependenceIdeal`** — the ideal of polynomial
  relations implied by conditional-independence statements.
- **`discreteVanishingIdeal`** / **`gaussianVanishingIdeal`** —
  the model's vanishing ideal (for discrete and Gaussian models
  respectively).
- **`trekIdeal`**, **`trekSeparation`** — the ideal from "trek"
  combinatorics in DAGs.
- **`markovMatrices`**, **`gaussianMatrices`** — the matrices
  whose minors generate the relevant ideals.
- **`globalMarkov`**, **`localMarkov`**, **`pairMarkov`** — three
  standard sets of Markov properties.

Discrete and Gaussian models share most of the API. The bridge
between graph-theoretic structure and commutative-algebra invariants
is the package's headline feature.

- Single-file: `GraphicalModels.m2` (3 992 lines)
- Authors: Carlos Amendola, Luis David Garcia Puente, Roser Homs Pons, Olga Kuznetsova, Harshit J Motwani, Sonja Petrovic, Mike Stillman, Seth Sullivant
- Version: 2.0 (November 2020)
- Re-exports: [`Graphs`](file-Graphs.md), `StatGraphs`
- Imports: [`IntegralClosure`](file-IntegralClosure.md) (auto-loaded), [`Elimination`](file-Elimination.md) (auto-loaded)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API (~50 symbols)

### Probability-ring construction

```m2
markovRing(d_1, …, d_n)                  -- the polynomial ring with variables p_{i_1…i_n}
                                          -- where each i_k ∈ {1, …, d_k} (discrete model)
gaussianRing G                            -- polynomial ring with `s_{ij}` covariance variables
                                          -- (Gaussian model)
gaussianRingData(...), gaussianRing(G, ...)
sVariableName, kVariableName,
lVariableName, pVariableName              -- option keys to control variable naming
sVar, kVar, lVar, pVar                    -- access individual variable families
```

### Ideal construction — discrete case

```m2
discreteVanishingIdeal(G, R)              -- the ideal of the model
markovMatrices(G, R, Stmts)               -- matrices whose minors define
                                          -- the conditional-independence relations
marginMap(R, …)                           -- marginal-distribution maps
hiddenMap(R, …)                           -- hidden-variable elimination
```

### Ideal construction — Gaussian case

```m2
gaussianVanishingIdeal(G, R)
gaussianParametrization(G, R)
gaussianMatrices(G, R, Stmts)
covarianceMatrix R                         -- the symmetric `s_{ij}` matrix
inverseMarginMap                          -- ⊥ counterpart
```

### Graph-theoretic conditional independence

```m2
globalMarkov G                            -- the global Markov property
localMarkov G                              -- the local Markov property
pairMarkov G                               -- the pairwise Markov property
```

Each returns a list of conditional-independence statements
(triples `(A, B | C)`) implied by the graph; `conditionalIndependenceIdeal`
turns each statement into polynomial relations and intersects them.

### Mixed graphs (causal models)

```m2
bidirectedEdgesMatrix G                   -- the matrix for bidirected edges
directedEdgesMatrix G                     -- the matrix for directed edges
undirectedEdgesMatrix G
trekIdeal(R, G)                           -- the ideal from "trek" combinatorics
trekSeparation(G, A, B, …)                -- trek-separation analysis
SimpleTreks                               -- option: restrict to simple treks
identifyParameters(...)                   -- parameter identification
```

### Coefficient / configuration

```m2
Coefficients => …                         -- the base ring for the polynomial ring (default QQ)
graphType                                 -- internal type tag for mixed-graph support
```

## What a graphical model is

A **graphical model** is a multivariate probability distribution
whose **conditional independencies** are determined by a graph `G`
on `n` vertices (one per random variable):

- **DAGs (directed acyclic graphs)**: Bayesian networks. The local
  Markov property says each node is conditionally independent of its
  non-descendants given its parents.
- **Undirected graphs**: Markov random fields. The pairwise Markov
  property says non-adjacent vertices are conditionally independent
  given all others.
- **Mixed graphs** (with bidirected, directed, undirected edges):
  causal models with latent variables.

For each conditional-independence statement `A ⊥ B | C`, there's an
associated set of polynomial constraints on the joint
distribution. The **vanishing ideal of the model** is the ideal
generated by all such constraints over all the model's CI
statements. This is what `discreteVanishingIdeal` / `gaussianVanishingIdeal` returns.

## Discrete vs Gaussian

The package supports two model families:

| Family | Ring construction | CI translation |
|---|---|---|
| **Discrete** | `markovRing(d_1, …, d_n)`: variables `p_{i_1 … i_n}` indexing the joint probability table | A CI statement becomes minors of `markovMatrices` (the conditional probability tables organised as matrices) |
| **Gaussian** | `gaussianRing G`: variables `s_{ij}` for entries of the covariance matrix (plus optional `l_{ij}` for path coefficients, `k_{ij}` for inverse covariance, `p_{ij}` for partial correlations) | A CI statement becomes minors of `gaussianMatrices` (submatrices of the covariance matrix) |

The output ideal in each case is computed by intersecting / saturating the per-statement constraint ideals.

## Three Markov properties

For a DAG `G`:

| Property | Meaning |
|---|---|
| **Pairwise Markov** (`pairMarkov G`) | For every non-adjacent pair `(u, v)` in `G`, `u ⊥ v | <everything except u and v>` |
| **Local Markov** (`localMarkov G`) | Each node is independent of its non-descendants given its parents |
| **Global Markov** (`globalMarkov G`) | All d-separation statements |

For undirected graphs these collapse to the same set (when `G` is decomposable). For DAGs they can be strictly different — see Pearl's *Probabilistic Reasoning in Intelligent Systems*. The package implements all three so users can compute the vanishing ideal under any of them.

## Trek combinatorics

For DAGs with latent variables, the **trek** structure (a path in the underlying mixed graph that has at most one "fork" node) determines the model's conditional independencies and parameter identifiability:

```m2
trekIdeal(R, G)             -- the trek ideal
trekSeparation(G, A, B, …)  -- which pairs in A and B are trek-separated
identifyParameters G        -- recover the original parameters from observed variables
```

This generalises the seminal work of Sullivant et al. on trek separation and parameter identifiability in causal models.

## Architecture

The file is a 3 992-line single file, organised by section comments:

| Section | Topic |
|---|---|
| Header | Package declaration, imports, exports |
| Discrete machinery | `markovRing`, `markovMatrices`, `discreteVanishingIdeal`, `marginMap`, `hiddenMap` |
| Gaussian machinery | `gaussianRing`, `gaussianMatrices`, `gaussianVanishingIdeal`, `covarianceMatrix`, `gaussianParametrization` |
| Markov-property generators | `pairMarkov`, `localMarkov`, `globalMarkov` |
| Mixed-graph / trek code | `directedEdgesMatrix`, `bidirectedEdgesMatrix`, `trekIdeal`, `trekSeparation`, `identifyParameters` |
| Documentation `doc ///…///` | Inline with each implementation block |
| Test suite | Trailing `TEST ///…///` blocks |

## Bridges

| What | Where it comes from |
|---|---|
| `Graph` / `Digraph` / `MixedGraph` types | Re-exported from [`Graphs`](file-Graphs.md) and `StatGraphs` |
| Integral closure of the vanishing ideal | Imported [`IntegralClosure`](file-IntegralClosure.md) |
| Elimination for hidden-variable models | Imported [`Elimination`](file-Elimination.md) |
| GB of the vanishing ideal | Standard `gb` via the engine [GB framework](../e/groebner-bases.md) |
| Primary decomposition for component identification | Auto-loaded [`PrimaryDecomposition`](file-PrimaryDecomposition.md) |

## When this is slow

| Symptom | Try |
|---|---|
| `discreteVanishingIdeal` on a discrete model with many variables / levels | The ring `markovRing` is `∏d_i`-variate; consider only the global Markov property first |
| `gaussianVanishingIdeal` on a large DAG | Sat with the `gaussianParametrization` map can be expensive; use `gaussianMatrices` directly if you only need the minors |
| `trekIdeal` very slow | The trek enumeration grows quickly; pass `SimpleTreks => true` to restrict |
| `identifyParameters` produces a huge output | Bound by `eliminate` over the parameter ring; expect blowup proportional to the GB size of the parametrization ideal |

## When `GraphicalModels` vs related packages

- **`AlgebraicStatistics`** — broader umbrella; uses this package's primitives.
- **`Markov`** (older) — predecessor; the modern code is here.
- **`StatGraphs`** — re-exported; provides `MixedGraph` and related types.
- **`Graphs`** — re-exported; provides standard graph types.

## See also

- [`file-Graphs.md`](file-Graphs.md) — re-exported by this package
- [`file-IntegralClosure.md`](file-IntegralClosure.md), [`file-Elimination.md`](file-Elimination.md) — imported (auto-loaded)
- [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) — auto-loaded; used for component analysis
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- `StatGraphs` package — re-exported; provides `MixedGraph` type
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — computation-engine catalogue
- Pearl, *Probabilistic Reasoning in Intelligent Systems*, for graphical-model theory background
- Sullivant et al., *Algebraic Statistics*, for the trek-separation foundations
