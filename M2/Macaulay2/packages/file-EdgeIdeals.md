# `EdgeIdeals.m2` — (hyper)graphs and their edge ideals

The `EdgeIdeals` package implements **edge ideals of (hyper)graphs**
— the central data structure of combinatorial commutative algebra
applied to graph theory. Provides parallel `Graph` and `HyperGraph`
types specifically tuned for edge-ideal analysis, plus the operations
that bridge between graph-theoretic invariants and the algebraic
invariants of `R / I(G)` (the Stanley-Reisner ring of the
independence complex).

Sister to [`Graphs`](file-Graphs.md): both define `Graph` types, but
`Graphs` is **graph-theory-first** (40+ named families, layout/display,
chromatic invariants) while `EdgeIdeals` is **edge-ideal-first**
(hypergraphs, good leaves, line graphs, the algebraic side of the
combinatorial-commutative-algebra interface).

- File: `EdgeIdeals.m2` (5 132 lines — single file, no aux directory)
- Authors: Chris Francisco, Andrew Hoefel, Adam Van Tuyl
- Version: 1.0.2 (March 2011)
- Re-exports: [`SimplicialComplexes`](file-SimplicialComplexes.md)
- Imports: `GenericInitialIdeal`, [`PrimaryDecomposition`](file-PrimaryDecomposition.md)
- **JSAG-certified** (vol. 1, 2009)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Types

```
Graph                       -- an ordinary graph (uniform 2-edges)
HyperGraph                  -- a hypergraph (edges of varying sizes)
```

Note: this package **redefines** `Graph` (vs the parallel definition
in the [`Graphs`](file-Graphs.md) package). Code that uses both must
disambiguate. The packages share many names; loading both in one
session usually shadows the second-loaded one.

### Construction

```m2
graph(R, edges)             -- graph from a ring + list of edges
hyperGraph(R, edges)        -- hypergraph (edges may have any size)
completeGraph(R)            -- complete graph
completeMultiPartite(R, …)
cycle n
antiCycle n
```

### Edge-ideal and dual ideals

```m2
edgeIdeal G                 -- the edge ideal I(G) ⊂ R
coverIdeal G                -- the dual ideal (vertex covers); the Alexander dual
edgeIdealToHyperGraph I     -- recover the (hyper)graph from a squarefree monomial ideal
```

### Standard graph data

```m2
edges G, vertices G
isEdge(G, e)
getEdge(G, i), getEdgeIndex(G, e)
adjacencyMatrix G
incidenceMatrix G
degreeVertex(G, v)
neighbors(G, v)
isolatedVertices G
```

### Properties

```m2
chromaticNumber G
cliqueNumber G
independenceNumber G
allOddHoles G               -- odd-length induced cycles
allEvenHoles G              -- even-length induced cycles
hasOddHole G
getCliques(G), getMaxCliques(G)
```

### Boolean predicates

```m2
isBipartite G
isChordal G
isPerfect G
isConnected G               -- (for hypergraphs)
isConnectedGraph G          -- (for graphs)
isForest G
isLeaf, isGoodLeaf, hasGoodLeaf, getGoodLeaf, getGoodLeafIndex
isCM G                      -- is the edge ideal Cohen-Macaulay?
isSCM G                     -- sequentially Cohen-Macaulay?
isGraph H                   -- is this hypergraph uniform with 2-edges?
isEdge(G, e)
```

### Derivative (hyper)graphs

```m2
complementGraph G
lineGraph G                 -- L(G) — edges become vertices
inducedGraph(G, S), inducedHyperGraph(H, S)
deleteEdges(G, L)
connectedGraphComponents G
```

### Simplicial complex bridges

```m2
cliqueComplex G             -- the clique complex (faces = cliques)
independenceComplex G       -- the independence complex (faces = independent sets)
hyperGraphToSimplicialComplex H
```

The independence complex is the **Stanley-Reisner-side dual** to the
edge ideal: `I(G)` is exactly the Stanley-Reisner ideal of
`independenceComplex G`. This is the formal reason combinatorial
commutative algebra works.

## The good-leaf machinery

A distinctive feature of this package: the **good-leaf** concept for
hypergraphs (from work of Häkkä, Hà-Van Tuyl, and others). A leaf in
an ordinary graph is an edge with a degree-1 vertex; for hypergraphs
there are multiple notions of "leaf". The package implements
**`getGoodLeaf`**, `isGoodLeaf`, `hasGoodLeaf`, `getGoodLeafIndex`
— the leaf concept tuned for Hà-Van Tuyl's "splitting edge" theorems
about Betti numbers of edge ideals.

For a forest hypergraph (every connected component has a good leaf
at every step), the resolution of `I(G)` admits a recursive
splitting decomposition. The package's `getGoodLeaf` extracts the
leaf used in that decomposition.

## Single-file architecture

Like [`Graphs`](file-Graphs.md), this package is a single 5 132-line
file with no auxiliary directory. The section-comment-based layout:

| Section (approx) | Topic |
|---|---|
| Lines 1-150 | Package header, exports |
| Lines 150-800 | Type definitions (`Graph`, `HyperGraph`), constructors |
| Lines 800-1400 | Edge-ideal / cover-ideal builders |
| Lines 1400-2500 | Properties (chromatic, clique, independence, holes, leaves) |
| Lines 2500-3500 | Predicates (`isCM`, `isSCM`, `isChordal`, `isBipartite`, …) |
| Lines 3500-4200 | Operations (complement, line graph, induced subgraphs) |
| Lines 4200-5132 | Documentation `doc ///…///` and `TEST ///…///` blocks |

## When `EdgeIdeals` vs `Graphs`?

The two packages overlap. Pick `EdgeIdeals` when:

- You need **`HyperGraph` support** (this package; `Graphs` does ordinary graphs only).
- Your goal is the **edge ideal** and its algebraic invariants (this package's primary focus; resolution / Betti numbers / Cohen-Macaulay tests).
- You care about **`getGoodLeaf` / Hà-Van Tuyl splitting** for resolution analysis.
- You want `cliqueComplex G` / `independenceComplex G` as `SimplicialComplexes` objects (this package's bridge is built-in).

Pick `Graphs` when:

- You need the **40+ named graph families** (Kneser, Johnson, Petersen, etc.) — `EdgeIdeals` has only a handful.
- You want **`displayGraph G`** with Graphviz output.
- You're doing **pure graph theory** (chromatic polynomial, max flow, articulation points).
- You don't need hypergraph support.

For mixed workflows, load both but be aware of the name conflict on `Graph`, `graph`, `edgeIdeal`, etc.

## `isCM` and `isSCM` — Cohen-Macaulay tests

```m2
G = cycleGraph(R, 5)
isCM G        -- true iff R / I(G) is Cohen-Macaulay
isSCM G       -- true iff R / I(G) is sequentially Cohen-Macaulay
```

These are the most-called user-facing operations from a research
standpoint. The algorithm reduces to checking whether the
`independenceComplex G` is pure (for `isCM`) or shellable (for
`isSCM`). The package handles the reduction internally; for very
large graphs, expect slowdown proportional to the size of the
clique complex (often exponential).

## When this is slow

| Symptom | Try |
|---|---|
| `allOddHoles G` slow | Hole enumeration is NP-hard; bound the input size or check `hasOddHole G` first |
| `isCM G` very slow | `R / I(G)` Cohen-Macaulay testing involves a depth computation; for chordal graphs this is fast, otherwise expect slowdown |
| `coverIdeal G` slow | Alexander duality on a large monomial ideal; pre-compute the support sizes and route through the engine `monideal` path |
| `getGoodLeaf` slow on a large hypergraph | Walks through possible leaves; for forest hypergraphs there's a faster recursive variant — see the source |

## Sister packages and tooling

| Package | Relationship |
|---|---|
| [`Graphs`](file-Graphs.md) | Different `Graph` type; broader graph-theory toolkit |
| [`SimplicialComplexes`](file-SimplicialComplexes.md) | Re-exported by this package; `cliqueComplex` / `independenceComplex` return these objects |
| [`PrimaryDecomposition`](file-PrimaryDecomposition.md) | Imported; used internally for some hole-detection algorithms |
| `GenericInitialIdeal` | Imported; used in some `isCM` / `isSCM` paths |
| [`MinimalPrimes`](file-MinimalPrimes.md) (auto-loaded) | Used internally for primary decomposition of cover ideals |

## See also

- [`file-Graphs.md`](file-Graphs.md) — sister package; pick which based on your workflow needs
- [`file-SimplicialComplexes.md`](file-SimplicialComplexes.md) — re-exported by this package; provides `cliqueComplex` / `independenceComplex` types
- [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) — imported (auto-loaded)
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- Engine `MonomialIdeal`: [`e/file-monideal.md`](../e/file-monideal.md) — backs edge-ideal operations
- [JSAG 2009 article](https://msp.org/jsag/2009/1-1/p01.xhtml) — Francisco-Hoefel-Van Tuyl: *EdgeIdeals: a package for (hyper)graphs*
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — monomial-ideal computation catalogue
