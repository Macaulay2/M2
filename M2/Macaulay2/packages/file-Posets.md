# `Posets.m2` — partially ordered sets in M2

The `Posets` package implements **partially ordered sets** with a
substantial library of operations: construction (~15 named families),
derivative posets (intervals, filters, order ideals, dual posets,
distributive lattice), derivative non-poset structures (Hasse
diagram, comparability/incomparability graph, order complex,
Hibi ideal, p-partition ring), enumeration (chain polynomial, Möbius
function, characteristic polynomial), and predicates (is-lattice,
is-distributive, is-CM, is-shellable, …).

**JSAG-certified** (vol. 7, 2015).

- File: `Posets.m2` (6 889 lines — single file, no aux directory)
- Authors: David Cook II, Sonja Mapes, Gwyn Whieldon
- Version: 1.1.3 (May 2021)
- Re-exports: [`Isomorphism`](file-Isomorphism.md) (auto-loaded), [`SimplicialComplexes`](file-SimplicialComplexes.md), [`Graphs`](file-Graphs.md), `FourTiTwo`, [`Complexes`](file-Complexes.md)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API (~109 symbols)

### Types & construction

```
Poset                       -- a partially ordered set
poset(GroundSet, Relations) -- general constructor
poset(M)                    -- from a relation matrix
transitiveClosure           -- complete the partial order
GroundSet, RelationMatrix,
Relations, AntisymmetryStrategy
                            -- option keys
```

### Named posets (~15 families)

```m2
booleanLattice n            -- the Boolean lattice on n atoms (subset poset of [n])
chain n                     -- the chain 1 < 2 < … < n
divisorPoset n              -- divisors of n, ordered by divisibility
dominanceLattice n          -- partitions of n, dominance order
facePoset Δ                 -- face poset of a simplicial complex
intersectionLattice         -- intersection lattice of a hyperplane arrangement
lcmLattice I                -- LCM lattice of generators of a monomial ideal I
ncpLattice n                -- non-crossing partitions of [n]
partitionLattice n          -- partitions of [n], refinement order
plueckerPoset n             -- Plücker coordinates poset
projectivizeArrangement     -- from a hyperplane arrangement
rationalPoset n             -- "Catalan-tree" poset on rational sequences
standardMonomialPoset       -- standard monomials of a monomial ideal
youngSubposet λ             -- Young's lattice up to a partition
```

### Operations

```m2
augmentPoset P              -- add a top and bottom element
adjoinMax P, adjoinMin P    -- add only top (resp. only bottom)
dropElements(P, L)          -- delete elements
closedInterval(P, a, b)
openInterval(P, a, b)
filter(P, a)                -- {x : x ≥ a}
principalFilter(P, a)
orderIdeal(P, a)            -- {x : x ≤ a}
principalOrderIdeal(P, a)
subposet(P, S)              -- induced subposet on S
flagPoset(P, w)             -- flag poset for a weight w
diamondProduct(P, Q)        -- diamond product of posets
removeIsomorphicPosets L    -- deduplicate a list
areIsomorphic(P, Q)         -- with caching
distributiveLattice P       -- the distributive lattice of order ideals
dilworthLattice P           -- Dilworth's lattice of antichains
indexLabeling, labelPoset,
naturalLabeling
```

### Derivative non-poset structures (the algebraic bridges)

```m2
hasseDiagram P              -- the Digraph (from Graphs)
comparabilityGraph P        -- comparability Graph
incomparabilityGraph P
orderComplex P              -- the SimplicialComplex of chains
hibiIdeal P                 -- the Hibi ideal (a monomial ideal in a polynomial ring)
hibiRing P                  -- R/hibiIdeal P
pPartitionRing(P, …)        -- the p-partition ring (a binomial ideal quotient)
```

### Enumeration / Möbius function

```m2
mobiusMatrix P              -- the Möbius function as a matrix
mobiusFunction(P, a, b)
characteristicPolynomial P
chainPolynomial(P, k)
zetaPolynomial P
flagfPolynomial P
flagfVector P
fPolynomial P, fVector P
hPolynomial P, hVector P
rankFunction P, rankPoset P
```

### Predicates

```m2
isAntichain(P, S)
isAtomic P
isBoolean P
isBounded P
isCM P, isShellable P
isComparable, isConnected, isDistributive
isEulerian P, isGraded P, isGeometric, isJoinSemilattice
isJoinSemilattice P, isMeetSemilattice P, isLattice P
isModular P, isUpperSemiModular P, isLowerSemiModular P
isRanked P
```

### Display

```m2
displayPoset P              -- render via Graphviz `dot`
texPoset P                  -- TikZ output
outputTexPoset(P, fname)
```

The Graphviz integration mirrors that of [`Graphs`](file-Graphs.md) (which is re-exported by this package).

## Architecture

Like [`Graphs`](file-Graphs.md), this is a **single-file 6 889-line package** with no aux directory. The structure is section-comment-based:

| Section (approx) | Topic |
|---|---|
| Lines 1-200 | Header, exports, configuration |
| Lines 200-700 | Type definition, constructor, `transitiveClosure` |
| Lines 700-1500 | Named poset constructors (booleanLattice, divisorPoset, …) |
| Lines 1500-2800 | Operations on posets (intervals, filters, order ideals, …) |
| Lines 2800-3500 | Derivative non-poset structures (Hasse diagram, comparability graph, order complex, Hibi ideal) |
| Lines 3500-4500 | Predicates (`isCM`, `isShellable`, `isLattice`, `isDistributive`, …) |
| Lines 4500-5500 | Enumeration (Möbius matrix, chain/zeta polynomials, h-vector) |
| Lines 5500-6000 | Display (Graphviz, TikZ) |
| Lines 6000-6889 | Documentation `doc ///…///` blocks |

## Precomputation strategy

Many poset operations are quadratic-or-worse in poset size; the
package precomputes and caches the **relation matrix** by default:

```m2
setPrecompute true     -- (default) cache RelationMatrix on construction
setPrecompute false    -- compute lazily, save memory for very large posets
```

The configuration option `DefaultPrecompute => true | false` sets the
session default. For research workflows with many small/medium
posets, leave it on. For experiments with very large posets where
you only need a few operations, turn it off.

The `Precompute` option on individual `poset` constructors overrides
the session default per-call.

## The algebraic side: Hibi ideals and p-partition rings

Posets connect to commutative algebra via:

```m2
P = booleanLattice 3
I = hibiIdeal P         -- monomial ideal in QQ[x_{a,b} for each comparable a < b]
R = hibiRing P          -- the quotient by hibiIdeal
```

The **Hibi ideal** is the squarefree monomial ideal of incomparable
pairs. Its quotient (the Hibi ring) is a well-studied Stanley-Reisner
ring; its Hilbert series is the order polynomial of `P`. Key
properties:

- `R = hibiRing P` is **Cohen-Macaulay** iff `P` is a meet-semilattice
  with certain restrictions.
- The Hilbert series encodes the **h-vector** of `P`, which carries
  most combinatorial information about the poset.

`pPartitionRing P` is the more refined binomial-ideal-quotient
construction.

## Bridges to other packages

| Operation | Returns | Bridges to |
|---|---|---|
| `hasseDiagram P` | `Digraph` | [`Graphs`](file-Graphs.md) |
| `comparabilityGraph P` | `Graph` | [`Graphs`](file-Graphs.md) |
| `orderComplex P` | `SimplicialComplex` | [`SimplicialComplexes`](file-SimplicialComplexes.md) |
| `hibiIdeal P` / `hibiRing P` | `Ideal` / `Ring` | Standard M2 commutative algebra |
| `lcmLattice I` | `Poset` (from a monomial ideal `I`) | Reverse bridge |

The package re-exports `Isomorphism`, `SimplicialComplexes`, `Graphs`, `FourTiTwo`, `Complexes` — anyone using `Posets` automatically has access to those types and operations.

## Möbius function & enumeration

```m2
mobiusMatrix P            -- the |P|×|P| matrix M with M(a,b) = μ(a, b)
mobiusFunction(P, a, b)   -- a single Möbius value
characteristicPolynomial P  -- a polynomial in ZZ[t]
```

The Möbius function is the central enumerative invariant: it generalises Euler's `μ` function (for `divisorPoset n` it gives exactly the number-theoretic Möbius) and is the inversion partner for the zeta function.

`isEulerian P` checks the **Euler condition** (`χ(P_{<a}) = 0` for every `a`); Eulerian posets carry extra structure (Dehn-Sommerville relations on the f-vector).

## When this is slow

| Symptom | Try |
|---|---|
| Construction of a 100-element poset takes seconds | `setPrecompute false` to skip the `RelationMatrix` precomputation |
| `isCM P` very slow | Check `isLattice P` first — for lattices, fast structural tests apply |
| `mobiusMatrix P` slow on a poset with many incomparable pairs | Precomputed once and cached, but the cache itself is `O(|P|^2)` |
| `displayPoset` errors | Verify `dot` (Graphviz) is on `PATH`; see [`file-Graphs.md`](file-Graphs.md) for the shared rendering machinery |
| `lcmLattice I` very slow for `I` with many generators | The LCM lattice grows exponentially; bound the number of generators first |

## See also

- [`file-Graphs.md`](file-Graphs.md) — re-exported; provides Hasse-diagram visualisation
- [`file-SimplicialComplexes.md`](file-SimplicialComplexes.md) — re-exported; `orderComplex` returns these objects
- [`file-Isomorphism.md`](file-Isomorphism.md) — re-exported (auto-loaded); used internally for `areIsomorphic`
- [`file-Complexes.md`](file-Complexes.md) — re-exported (auto-loaded); used internally
- [`FourTiTwo.m2`](FourTiTwo.m2) — re-exported; lattice point algorithms for some poset operations
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- Engine `MonomialIdeal`: [`e/file-monideal.md`](../e/file-monideal.md) — backs `hibiIdeal`
- [JSAG 2015 article](https://msp.org/jsag/2015/7-1/p02.xhtml) — Cook-Mapes-Whieldon: *Partially ordered sets in Macaulay2*
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — combinatorial / monomial-ideal catalogue
