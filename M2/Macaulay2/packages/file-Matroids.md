# `Matroids.m2` — matroid theory in M2

The `Matroids` package implements **matroid theory** in M2 — the
combinatorial structure that abstracts the "linear independence"
relation from vector spaces while generalizing to graphs, hyperplane
arrangements, and dozens of other domains. Provides a unified
`Matroid` type with constructors from multiple sources (matrices,
graphs, ideals, axiomatic data), all major matroid operations
(deletion, contraction, dualisation, direct sum, parallel/series
connection), structural invariants (rank function, Tutte polynomial,
characteristic polynomial), and a database of named matroids.

JSAG-certified (vol. 9, 2019).

- File: `Matroids.m2` (1 445 lines)
- Auxiliary directory: `Matroids/` (data + tests)
- Author: Justin Chen
- Version: 1.7.0 (February 2024)
- Re-exports: [`Graphs`](file-Graphs.md), [`Posets`](file-Posets.md)
- Imports: [`Complexes`](file-Complexes.md)
- ~97 exported symbols

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### The `Matroid` type and constructors

```m2
Matroid                              -- the type
matroid B                            -- from a list of bases B
matroid M                            -- from a matrix M (column matroid)
matroid G                            -- from a graph G (cycle matroid)
matroid I                            -- from a monomial ideal I (independence matroid)
matroid(E, B)                        -- explicit ground set + bases
ParallelEdges => …                   -- option: treat parallel graph edges as parallel matroid elements
Loops => …                           -- option: treat self-loops as matroid loops
```

### Ground-set / basis / circuit / flat queries

```m2
groundSet M                          -- the ground set
indicesOf(M, subset)                 -- positional indices in the ground set
bases M                              -- list of bases (size-r subsets, r = rank)
nonbases M
circuits M                            -- list of circuits (minimal dependent subsets)
fundamentalCircuit(M, B, e)          -- the circuit of e + (B \ {e})
loops M, coloops M                   -- size-1 dependent / coloops
flats M                              -- list of flats by rank
latticeOfFlats M                     -- as a `Poset` (re-exported from `Posets`)
closure(M, S)                        -- closure (smallest flat containing S)
isDependent(M, S)                    -- predicate
```

### Operations (deletion, contraction, minors)

```m2
restriction(M, S), deletion(M, S)    -- M | S — restrict to S; M \ S — delete S
contraction(M, S)                    -- M / S
minor(M, S, T)                       -- M / S | T (contract S then restrict to T)
hasMinor(M, N)                       -- does M have a minor isomorphic to N?
dualMatroid M                        -- the dual matroid
directSum(M, N)                      -- ⊕
seriesConnection, parallelConnection -- the two standard non-direct combinations
twoSum                                -- a special operation
relabel(M, perm)                      -- ground-set relabeling
```

### Structural predicates

```m2
isBinary M, isTernary M               -- representable over F_2 / F_3
is3Connected M                        -- 3-connectivity
isPaving M
isUniform M
isLoopless M
isSimple M
isRegular M
isQuotient(M, N)
isModularElement(M, x)
isFlag(M, F)
isMatroid input                       -- predicate testing whether axioms hold
```

### Invariants

```m2
rank M                                -- the matroid rank (also rank(M, S) for a subset)
tuttePolynomial M                     -- the Tutte polynomial T(M; x, y)
characteristicPolynomial M            -- via Möbius function of latticeOfFlats
chromaticPolynomial M                 -- when M is a cycle matroid
flagfVector M                         -- flag f-vector
rankFunction M                        -- the rank function as a list
sortedCircuits M                      -- circuits sorted by size
```

### Realisability and representability

```m2
isRepresentable(M, field)             -- is M F-representable?
realRepresentation M                  -- realize M as a matrix when possible
characteristicSet M                   -- set of characteristics over which M is representable
isLinearMatroid M                     -- shortcut
```

### Named-matroid database

```m2
allMatroids n                         -- all (small) matroids of rank ≤ n
specificMatroid "name"                -- named matroid by string lookup
uniformMatroid(r, n)                  -- U_{r,n}
graphicMatroid G                      -- alias for matroid G
projectiveGeometry(d, q)
affineGeometry(d, q)
spike, swirl, theta, vamos             -- specific historically-significant examples
fano, nonfano                          -- F_7 / non-Fano
nonpappus, pappus
desargues
fanoMatroid, nonfanoMatroid           -- aliased versions
```

The full database covers ~30+ named matroids that are central reference points in matroid theory (Fano, non-Fano, Pappus, non-Pappus, Vámos, M(K_4), M(K_5), Reye, Wagner — the textbook examples).

### Algebraic bridges

```m2
matroidIdeal M                        -- the matroid (Stanley-Reisner) ideal of M
brokenCircuitComplex M                -- the broken-circuit simplicial complex
characteristicPolynomial M            -- via Whitney's theorem from broken-circuits
chowRing M                            -- the Chow ring of a matroid (Adiprasito-Huh-Katz)
hilbertSeriesChow M
augmentedChowRing M
intersectionRing M
```

The **Chow ring of a matroid** is the central object in
**Adiprasito-Huh-Katz's resolution of the Heron-Rota-Welsh
conjecture** — the package exposes it directly.

## Why matroids are useful

Matroids provide a **uniform combinatorial language** for problems that look different on the surface:

- Linear independence (column matroid of a matrix)
- Graph cycle structure (cycle matroid of a graph)
- Algebraic dependence in field extensions
- Transversal structures (transversal matroid)
- Geometric incidence (hyperplane arrangement intersection structure)

A theorem about matroids translates instantly to all five settings. This is why the **Adiprasito-Huh-Katz proof** of the Heron-Rota-Welsh conjecture (concavity of the characteristic-polynomial coefficients) — proved by establishing a Kähler package structure on the Chow ring of a matroid — implies long-conjectured results in **graph chromatic polynomials** and in **arrangement combinatorics** simultaneously.

## How it fits with other packages

| Operation | This package + sibling |
|---|---|
| Matroid M built from graph G | `matroid G` here; `G` from [`Graphs`](file-Graphs.md) |
| Lattice of flats as a `Poset` | `latticeOfFlats M` returns a `Poset` from [`Posets`](file-Posets.md) (re-exported) |
| Matroid Bergman fan (tropical) | `BergmanFan M` in [`Tropical`](file-Tropical.md) (which imports this package) |
| Matroid Stanley-Reisner ideal | `matroidIdeal M` here; the result is a monomial ideal one can pass to [`MinimalPrimes`](file-MinimalPrimes.md), [`SimplicialComplexes`](file-SimplicialComplexes.md) |
| Chow ring of a matroid | `chowRing M` here; the result is a graded ring one can pass to [`Complexes`](file-Complexes.md), `hilbertSeries`, etc. |

## When this is slow

| Symptom | Try |
|---|---|
| `bases M` slow for high-rank matroid | The basis count grows fast; consider `numBases M` if you only need the count |
| `tuttePolynomial M` slow | The deletion-contraction recursion is exponential in the worst case; use specialised forms (`graphicMatroid`-coloring polynomial via `chromaticPolynomial`) when applicable |
| `chowRing M` slow | The Chow ring presentation has many generators; consider `augmentedChowRing M` which sometimes simplifies |
| `isRepresentable(M, F)` slow | Representation-testing is NP-hard in general; for small matroids it succeeds, large ones it may not terminate quickly |
| `hasMinor(M, N)` slow | The minor-containment problem grows exponentially; bound the size of N first |

## Architecture

Single-file 1 445-line package. Organised by section comments into:

| Section | Topic |
|---|---|
| Header + exports | Lines 1-110 |
| `Matroid` type + constructors | from various inputs |
| Basis/circuit/flat queries | The core combinatorial extractors |
| Operations | Deletion, contraction, minors, duality, sums, connections |
| Predicates | The `is*` family |
| Invariants | Tutte / characteristic / chromatic polynomials |
| Algebraic bridges | matroidIdeal, brokenCircuitComplex, chowRing |
| Named-matroid database | Fano, Vámos, etc. + helper constructors |
| Documentation `doc ///…///` | Trailing |
| Test suite | Trailing |

The aux directory `Matroids/` contains the structured matroid databases (small-matroid enumerations).

## See also

- [`file-Graphs.md`](file-Graphs.md) — re-exported; provides `Graph` for `matroid G` (cycle matroids)
- [`file-Posets.md`](file-Posets.md) — re-exported; `latticeOfFlats M` returns a `Poset`
- [`file-Complexes.md`](file-Complexes.md) — imported (auto-loaded); `chowRing M` works in this category
- [`file-Tropical.md`](file-Tropical.md) — uses this package for `BergmanFan M`
- [`file-SimplicialComplexes.md`](file-SimplicialComplexes.md) — natural pair (matroid independence complex; `brokenCircuitComplex` returns this type)
- [`file-EdgeIdeals.md`](file-EdgeIdeals.md) — `matroidIdeal` connects to edge-ideal Stanley-Reisner workflows
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [JSAG 2019 article](https://msp.org/jsag/2019/9-1/p03.xhtml) — Chen: *Matroids: a Macaulay2 package*
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- Oxley, *Matroid Theory* (2nd ed., OUP 2011) — canonical reference
- Adiprasito-Huh-Katz, *Hodge Theory for Combinatorial Geometries*, Annals 2018 — the Chow ring connection
