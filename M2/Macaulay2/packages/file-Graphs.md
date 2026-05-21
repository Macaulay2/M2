# `Graphs.m2` — graphs, digraphs, and graph-theoretic invariants

The `Graphs` package implements **graph theory** in M2: undirected
`Graph`, directed `Digraph`, a battery of named graph families
(complete / cycle / path / Petersen / Kneser / barbell / cocktail-party
/ ladder / Hopf / friendship / lollipop / Prism / Cayley / Johnson /
…), graph-theoretic invariants (chromatic number, clique number,
girth, diameter, connectivity, independence number), graph operations
(union, disjoint union, join, complement, product, line graph,
barycentric subdivision), and bridging operations into the
commutative-algebra side (`monomialGraph`, edge ideals through
`SimplicialComplexes`).

Single-file package — **all 5 542 lines live in one `.m2` file**.

- File: `Graphs.m2` (5 542 lines — no auxiliary directory)
- Authors: Jack Burkart, David Cook II, Caroline Jansen, Amelia Taylor, Augustine O'Keefe
- Version: 0.3.4 (May 2021)
- Imports: [`PrimaryDecomposition`](file-PrimaryDecomposition.md)
- Re-exports: [`SimplicialComplexes`](file-SimplicialComplexes.md)
- Configuration: `DotBinary => "dot"` — Graphviz `dot` for rendering

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API (~147 symbols)

### Types

```
Graph                    -- an undirected graph
Digraph                  -- a directed graph
```

### Construction

```m2
graph(vertices, edges)               -- explicit
graph(L)                              -- from a list of edges
digraph(vertices, directed-edges)
digraph(L)
simpleGraph G                         -- remove loops + multi-edges
graph(EntryMode => "edges" | "neighbors", …)
                                      -- input format selector
Singletons => {a, b, …}               -- vertices with no edges
```

### Basic data

```m2
vertexSet G            -- the vertex set (typically an indexed list)
edges G                -- the edge set
adjacencyMatrix G      -- a 0/1 Matrix
degreeMatrix G         -- diagonal matrix of degrees
incidenceMatrix G      -- a 0/1 Matrix
laplacianMatrix G      -- D − A (degree matrix minus adjacency matrix)
degreeSequence G       -- sorted list of degrees
```

### Display

```m2
displayGraph G         -- render via Graphviz `dot` into an SVG and open
showTikZ G             -- generate TikZ code
writeDotFile(G, fname) -- write a .dot file
```

### Named graph families (~40+ constructors)

```m2
completeGraph n
cycleGraph n
pathGraph n
generalizedPetersenGraph(n, k)
kneserGraph(n, k)             -- vertices = k-subsets of [n]; edges = disjoint pairs
johnsonGraph(n, k)
hopfGraph                     -- specific named graphs
crownGraph n
friendshipGraph n
ladderGraph n, circularLadder n, prismGraph n
lollipopGraph(m, n)
barbellGraph(m, n)
cocktailParty n
doubleStar(m, n)
completeMultipartiteGraph {a_1, …, a_n}
graphLibrary "name"           -- many named small graphs (Petersen, Tutte, etc.)
monomialGraph(I)              -- graph whose edges are pairs of variables in supports of monomial generators of I
```

### Operations on graphs

```m2
union(G, H), G + H            -- union (same vertex set)
disjointUnion(G, H), G ++ H   -- disjoint union
join(G, H), G * H             -- graph join
G ** H                        -- Cartesian product
complementGraph G
underlyingGraph D             -- forget directions in a digraph
digraphTranspose D            -- reverse all edges
lineGraph G                   -- L(G)
barycenter G                  -- barycentric subdivision
```

### Manipulation

```m2
addVertex(G, v), addVertices(G, L)
addEdge(G, e), addEdges(G, L)
removeVertex(G, v)
deleteEdges(G, L)
contract(G, e)                -- edge contraction
```

### Properties (numerical / structural)

```m2
chromaticNumber G
cliqueNumber G                -- largest clique
independenceNumber G
girth G                       -- shortest cycle length
diameter G                    -- longest shortest path
edgeConnectivity G
vertexConnectivity G
chromaticPolynomial G
spanningTree G
```

### Boolean predicates

```m2
isConnected G
isAcyclic G                   -- (for digraphs)
isTree G
isComplete G
isCyclic G
isBipartite G
isChordal G
isEulerian G
isPerfect G
isRegular G
isStrongComponent D
isIndependentSet(G, L)
isClique(G, L)
isStrongConnected D
```

### Cut properties

```m2
articulationPoints G
bridges G
minimumCut(G, source, sink)
maxFlow(G, source, sink)
```

### Algorithmic core (BFS / DFS / paths)

```m2
breadthFirstSearch(G, v)
depthFirstSearch(G, v)
findPaths(G, src, sink)
distance(G, u, v)
neighbors(G, v)
descendents(D, v)             -- digraph reachability
predecessors(D, v)
```

### Edge ideals (bridge to commutative algebra)

```m2
edgeIdeal G                   -- the squarefree monomial ideal of edges
coverIdeal G                  -- the dual (vertex covers)
edgeRing G                    -- the quotient by edgeIdeal
```

Edge ideals connect graphs to **Stanley-Reisner theory** (see [`file-SimplicialComplexes.md`](file-SimplicialComplexes.md)) — the package's `PackageExports => {"SimplicialComplexes"}` makes this seamless.

## Single-file architecture

Unlike many packages, `Graphs` is one big 5 542-line file with no
aux directory. The internal structure is just **section comments**
splitting the file into nine areas matching the export categories:

| Section | Lines (approx) | Topic |
|---|---|---|
| Header | 1-80 | `newPackage`, exports, configuration |
| Data types + constructors | 80-500 | `Graph`, `Digraph`, `graph`, `digraph` |
| Basic data | 500-1100 | `adjacencyMatrix`, `edges`, `vertexSet`, etc. |
| Display | 1100-1400 | `displayGraph`, `writeDotFile`, `showTikZ` |
| Derivative graphs | 1400-1800 | `lineGraph`, `complementGraph`, `barycenter`, etc. |
| Enumerators | 1800-3000 | The 40+ named graph constructors |
| Cut / connectivity | 3000-3500 | `articulationPoints`, `bridges`, `maxFlow`, etc. |
| Properties + predicates | 3500-4800 | `chromaticNumber`, `cliqueNumber`, `is*` predicates |
| Operations | 4800-5300 | Union, join, contract, complement |
| Documentation | 5300-5542 | `doc ///…///` blocks for every exported function |

The file is the entire package — search by section comment for navigation. The doc and tests are interleaved through the file rather than in a separate folder.

## Why single-file?

The 2010 / 2014 author lists show two waves of development; both kept the single-file structure because:

1. The operations are **mostly small** — `chromaticNumber` is a few lines, `displayGraph` is a Graphviz invocation, etc.
2. Cross-cutting helpers (BFS, DFS, neighbor enumeration) are needed by many functions; spreading them across files would make for many imports.
3. The package's user base is mostly **researchers running examples** — they don't routinely modify the source, so the lack of structure isn't a maintenance burden.

For packages with more internal structure, see e.g. [`Polyhedra`](file-Polyhedra.md) or [`Complexes`](file-Complexes.md).

## Graphviz integration

`displayGraph G` invokes the system `dot` binary (configurable via the `Configuration => { "DotBinary" => "dot" }` package option). It writes a temporary `.dot` file, runs Graphviz to produce an SVG, and opens it.

Common failure modes:
- "command not found: dot" — install Graphviz (`apt install graphviz` / `brew install graphviz`).
- Network rendering — `displayGraph` always opens via the **default browser**; M2's `displayHelp` machinery handles that.
- For pure TikZ output (no `dot` dependency), use `showTikZ G` and paste into a LaTeX document.

## Edge ideals — the bridge to commutative algebra

```m2
G = cycleGraph 5
I = edgeIdeal G       -- (x_1 x_2, x_2 x_3, x_3 x_4, x_4 x_5, x_5 x_1)
R = ring I            -- ZZ/101[x_1..x_5]
res I                 -- the resolution of the edge ideal
```

`edgeIdeal G` returns a squarefree monomial ideal in a polynomial
ring whose variables are the vertices and whose generators are the
edges. This is exactly the **Stanley-Reisner ideal of the independence
complex** of `G`. Operations on `edgeIdeal G`:

- `dim` → the independence number + 1 (clique cover number duality)
- `coverIdeal G` (the Alexander dual) → encodes vertex covers
- `betti res edgeIdeal G` → Betti numbers reveal combinatorial information

This is the entry point to **edge-ideal research** that occupies a big slice of combinatorial commutative algebra. The package's re-export of `SimplicialComplexes` makes the bridge two-way: a graph yields an independence complex, and the complex yields back the same edge ideal.

## When this is slow

| Symptom | Try |
|---|---|
| `chromaticNumber G` slow on >20-vertex graphs | Chromatic number is NP-hard; for large graphs use `chromaticPolynomial G` and evaluate (also NP-hard, but caches differently) |
| `cliqueNumber G` slow | Same — NP-hard; check `isPerfect G` first (perfect graphs have `cliqueNumber = chromaticNumber`) |
| `displayGraph` errors | Verify `dot` is on `PATH`; check `Configuration => { "DotBinary" => "dot" }` |
| `findPaths` very slow | Path enumeration grows combinatorially; use `distance(G, u, v)` if only the shortest length is needed |
| `edgeIdeal G` over a large graph | Edge ideal can have thousands of generators; pass to engine via `monomialIdeal` rather than `ideal` for the fast monomial path |

## Notable users

| Package | What it uses |
|---|---|
| [`EdgeIdeals`](EdgeIdeals.m2) (not yet deep-dived) | Built on this package; specialised edge-ideal operations |
| [`StatGraphs`](StatGraphs.m2) | Graphical models for algebraic statistics |
| Many combinatorial-commutative-algebra research packages | Use `Graph` / `edgeIdeal` constructions as inputs |

## See also

- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [`file-SimplicialComplexes.md`](file-SimplicialComplexes.md) — re-exported by this package; provides the independence-complex bridge
- [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) — imported; used internally for edge-ideal analysis
- Engine `MonomialIdeal`: [`e/file-monideal.md`](../e/file-monideal.md) — backs `edgeIdeal` operations
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — `monomialIdeal` ops catalogue
