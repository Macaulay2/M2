# Computation engines

A **catalogue of every long-running computation M2 supports** —
Gröbner bases, resolutions, Hilbert series, LLL, numerical AG,
polynomial factoring, and more. Lists the backends, strategy
selectors, and decision criteria.

Pairs with [`RING-ZOO.md`](RING-ZOO.md) (which rings exist) by
covering what you can *do* with rings.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Ring zoo](RING-ZOO.md) · [Engine: computations](M2/Macaulay2/e/computations.md)

## The `Computation` framework

Every long-running engine algorithm is a **resumable
Computation** — see
[`e/file-comp.md`](M2/Macaulay2/e/file-comp.md):

```cpp
Computation                  (abstract base)
   │
   ├── GBComputation          (gb-default, gb-f4, ...)
   ├── ResolutionComputation  (Schreyer, res-a*, ...)
   ├── HilbertComputation
   ├── LLLComputation
   └── NAGComputation
```

The interpreter creates one, optionally sets stop conditions
(degree limit, basis-element limit, ...), starts it, and can
query partial results.

See [`e/architecture.md`](M2/Macaulay2/e/architecture.md) for
the framework architecture.

## Gröbner bases

### Default (`gb-default`) — Buchberger-style

```m2
R = QQ[x, y, z]
I = ideal(x^2 - y, x*y - z)
G = gb I
```

- **Strategy**: `Strategy => "Default"` (implicit).
- **Engine**: `comp-gb-default` ([`e/file-gb-default.md`](M2/Macaulay2/e/file-gb-default.md)).
- **Best for**: most generic GB workloads over fields.
- **Algorithm**: classical Buchberger with optimisations (criteria
  for redundant pairs, normal-form caching).

### F4 — Macaulay-matrix reduction (original)

```m2
G = gb(I, Algorithm => F4)
```

- **Engine**: [`e/f4/architecture.md`](M2/Macaulay2/e/f4/architecture.md).
- **Best for**: GB over `Z/p` with many polynomials.
- **Algorithm**: Faugère F4 — Macaulay-matrix sweeps.

### F4 refactored (`gb-f4`) — modern variant

```m2
G = gb(I, Strategy => "F4")
```

- **Engine**: [`e/gb-f4/architecture.md`](M2/Macaulay2/e/gb-f4/architecture.md).
- **Differences from original**: cleaner type separation, struct-
  of-arrays polynomial representation, typed-integer family.
- **Status**: opt-in; under development.

### mathicgb — signature-based GB

```m2
G = gb(I, Strategy => "MathicGB")
```

- **Engine**: mathicgb submodule (Stillman/Roune; see
  [`M2/submodules/file-submodules.md`](M2/submodules/file-submodules.md)).
- **Best for**: large GBs where the signature-based criteria
  prune effectively.

### NC Gröbner bases

```m2
needsPackage "AssociativeAlgebras"
R = QQ<|x, y, z|>
I = ideal(x*y + y*x)
NCGB I
```

- **Engine**: [`e/NCAlgebras/architecture.md`](M2/Macaulay2/e/NCAlgebras/architecture.md).
- **Two variants**: classical NC-Groebner and NC-F4.
- **Best for**: non-commutative algebra computations.

### NC-F4

- **Engine**: [`e/NCAlgebras/file-NCF4.md`](M2/Macaulay2/e/NCAlgebras/file-NCF4.md).
- **Best for**: large NC GBs where matrix-style reduction
  helps.

### BIBasis — Boolean involutive

```m2
needsPackage "BIBasis"
R = ZZ/2[x_1..x_5]
involutiveBasis(I)
```

- **Engine**: [`e/bibasis/architecture.md`](M2/Macaulay2/e/bibasis/architecture.md).
- **Best for**: Boolean polynomial rings `F_2[x_i]/(x_i^2 - x_i)`.
- **Algorithm**: Janet involutive division (Zinin 2006-2011).

### Toric (binomial) GB

```m2
G = gb(I, Algorithm => Toric)
```

- **Engine**: [`e/file-gb-variants.md`](M2/Macaulay2/e/file-gb-variants.md)
  (covers `gb-toric.{cpp,hpp}`).
- **Best for**: toric ideals (binomial generators).
- **Algorithm**: optimised for binomial structure.

### Sugarless / homogeneous

```m2
G = gb(I, Algorithm => Sugarless)
G = gb(I, Algorithm => Homogeneous2)
```

- **Engine**: [`e/file-gb-variants.md`](M2/Macaulay2/e/file-gb-variants.md).
- **Best for**: specific input shapes (sugarless = no "sugar"
  selection; homog2 = homogeneous polynomials, alternative
  variant).

### Gröbner walk

```m2
G = gb(I, Algorithm => Walk)
```

- **Engine**: [`e/file-gb-variants.md`](M2/Macaulay2/e/file-gb-variants.md).
- **Best for**: converting a GB between monomial orders
  (typically faster than recomputing from scratch).

### Reduced GB

```m2
G = gb I
G = reducedGroebnerBasis I
```

- **Engine**: [`e/file-reducedgb.md`](M2/Macaulay2/e/file-reducedgb.md).
- **Variants**: field, ZZ, local order, marked (each in its own
  source file).
- **Best for**: getting the canonical (reduced) form of a GB.

### Strategy selection

```m2
gb(I, Strategy => "Default")     -- pick automatically
gb(I, Strategy => "F4")
gb(I, Strategy => "MathicGB")
gb(I, Strategy => "MGB")          -- alternative MathicGB syntax
gb(I, Strategy => "Toric")
```

Plus options applicable to all:

```m2
gb(I, DegreeLimit => 5)
gb(I, BasisElementLimit => 100)
gb(I, SyzygyLimit => 50)
gb(I, PairLimit => 1000)
gb(I, ChangeMatrix => true)       -- track the change-of-basis matrix
gb(I, Syzygies => true)            -- compute syzygy module too
```

See [`e/groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md) for
the full option set.

## Free resolutions

### Schreyer-frame (modern, default)

```m2
M = coker matrix{{x, y}, {y, x}}
C = resolution M
```

- **Strategy**: `Strategy => 4` (the default for most rings).
- **Engine**: [`e/schreyer-resolution/architecture.md`](M2/Macaulay2/e/schreyer-resolution/architecture.md).
- **Best for**: modern resolutions over commutative rings.
- **Algorithm**: F4-style sweeps using a Schreyer frame.

### Older engines (`res-a0`, `res-a1`, `res-a2`)

```m2
C = resolution(M, Strategy => 0)   -- res-a0
C = resolution(M, Strategy => 1)   -- res-a1
C = resolution(M, Strategy => 2)   -- res-a2
```

- **Engine**: [`e/file-res-old.md`](M2/Macaulay2/e/file-res-old.md).
- **Best for**: regression testing; specific edge cases where
  modern engine has issues.

### `Eschreyer` (predecessor)

- **Engine**: [`e/file-Eschreyer.md`](M2/Macaulay2/e/file-Eschreyer.md).
- **Status**: predecessor to the modern Schreyer engine;
  obsolete.

### NC free resolutions

```m2
-- Requires NCAlgebras infrastructure plus NCResolutions
```

- **Engine**: [`e/NCResolutions/architecture.md`](M2/Macaulay2/e/NCResolutions/architecture.md).
- **Status**: early implementation; bounded by `max_level`.

### Strategy selection

```m2
resolution(M, Strategy => 0..4)        -- pick algorithm
resolution(M, LengthLimit => 5)        -- compute first 5 modules
resolution(M, DegreeLimit => 10)       -- only degrees ≤ 10
resolution(M, FastNonminimal => true)  -- skip minimisation
```

### Betti tables

```m2
B = betti C
```

- **Engine**: [`e/file-betti.md`](M2/Macaulay2/e/file-betti.md).

## Hilbert series and Hilbert function

```m2
H = hilbertSeries M
P = hilbertPolynomial M
F = hilbertFunction(N, M)    -- value at degree N
```

- **Engine**: [`e/file-hilb.md`](M2/Macaulay2/e/file-hilb.md).
- **Algorithm**: Bigatti / efficient combinatorial.
- **Use for**: dimension counts at each graded piece.

## LLL lattice reduction

```m2
M = matrix{{1, 2, 3}, {4, 5, 6}}
N = LLL M           -- reduced basis
```

- **Engine**: [`e/file-LLL.md`](M2/Macaulay2/e/file-LLL.md).
- **Backends**:
  - **NTL** — default (default option).
  - **fplll** — alternative via [`e/file-fplll.md`](M2/Macaulay2/e/file-fplll.md).
- **Strategy selection**: `LLL(M, Strategy => "NTL")` or
  `Strategy => "FPLLL"`.

## Numerical algebraic geometry (NAG)

```m2
needsPackage "NumericalAlgebraicGeometry"
F = polySystem ...
sols = solveSystem F
```

- **Engine**: [`e/file-NAG.md`](M2/Macaulay2/e/file-NAG.md).
- **Algorithm**: homotopy continuation, witness sets, sample
  paths.
- **Backends**:
  - **M2 native** — built into the engine.
  - **Bertini** (external program, via package).
  - **PHCpack** (external program, via package).

### Straight-line programs

```m2
-- Internally used by NAG for repeated evaluation
```

- **Engine**: [`e/file-SLP.md`](M2/Macaulay2/e/file-SLP.md),
  [`e/file-SLP-defs.md`](M2/Macaulay2/e/file-SLP-defs.md),
  [`e/file-SLP-imp.md`](M2/Macaulay2/e/file-SLP-imp.md).

### Point arrays

```m2
P = pointArray points
```

- **Engine**: numerical AG infrastructure for clustering points.

## Polynomial factoring and GCD

```m2
f = (x^2 + 1) * (x^2 - 2)
factor f
```

- **Engine**: Factory library (external).
- **Boundary**: [`e/interface/file-factory-interface.md`](M2/Macaulay2/e/interface/file-factory-interface.md).
- **Coefficient rings supported**: `ZZ`, `QQ`, `Z/p`, algebraic
  extensions.

## Univariate root finding

```m2
f = x^4 - 2*x^3 + 3*x - 1
roots f
```

- **Engine**: MPSolve via [`e/interface/file-polyroots.md`](M2/Macaulay2/e/interface/file-polyroots.md).
- **Algorithm**: Bini's certified algorithm.
- **Precision**: tunable; works with `RR_N` for any `N`.

## CRT / rational reconstruction

```m2
needsPackage "ChineseRemainder"
crt(values, moduli)
```

- **Engine**: [`e/file-cra.md`](M2/Macaulay2/e/file-cra.md).
- **Use for**: lifting modular computations back to `ZZ`/`QQ`.

## Primary decomposition

```m2
needsPackage "PrimaryDecomposition"
primaryDecomposition I
```

- **Engine**: primary decomposition uses
  [`e/file-assprime.md`](M2/Macaulay2/e/file-assprime.md) for
  associated primes, plus algorithmic strategies for
  decomposition.

## Monomial ideals

```m2
I = monomialIdeal(x^2, y^3)
intersect(I, J)
saturate(I, J)
```

- **Engine**: [`e/file-monideal.md`](M2/Macaulay2/e/file-monideal.md),
  [`e/file-monideal-minprimes.md`](M2/Macaulay2/e/file-monideal-minprimes.md).
- **Specialised**: fast algorithms exploiting monomial structure.

## Linear algebra

### Dense matrix algorithms

```m2
A = mutableMatrix(R, 3, 3)
rank A
det A
nullSpace A
```

- **Engine variants** by ring:
  - `DMat<ARingZZp>` — Z/p via FFPACK or FLINT.
  - `DMat<ARingQQ>` — QQ via FLINT (fraction-free).
  - `DMat<ARingGF*>` — GF via FLINT.
  - `DMat<ARingRR/CC>` — floats via LAPACK.

**Deep dives**:
[`e/file-mat-linalg.md`](M2/Macaulay2/e/file-mat-linalg.md),
[`e/file-dmat-lu-variants.md`](M2/Macaulay2/e/file-dmat-lu-variants.md).

### Sparse matrix algorithms

```m2
A = mutableMatrix(R, 3, 3, Dense => false)
```

- **Engine**: [`e/file-smat.md`](M2/Macaulay2/e/file-smat.md).
- **Best for**: matrices that are mostly zero.

## Cone and polyhedral computations

```m2
needsPackage "Polyhedra"
C = posHull matrix{{1, 0}, {1, 1}}
```

- **Engine**: [`e/interface/file-cone-interface.md`](M2/Macaulay2/e/interface/file-cone-interface.md).
- **Backends**:
  - **CDDlib** (external program).
  - **lrslib** (external program).
- **Used by**: `Polyhedra`, `NormalToricVarieties` packages.

## Schubert calculus

```m2
needsPackage "Schubert2"
G = flagBundle(...)
```

- **Engine**: implemented mostly at M2 level using the engine's
  Schur ring machinery.
- **Schur engine**: [`e/file-schur.md`](M2/Macaulay2/e/file-schur.md),
  [`e/file-schur2.md`](M2/Macaulay2/e/file-schur2.md),
  [`e/file-schurSn.md`](M2/Macaulay2/e/file-schurSn.md).

## Combinatorial helpers

```m2
binomial(10, 3)
partitions(5)
```

- **Engine**: [`e/file-comb.md`](M2/Macaulay2/e/file-comb.md).

## Common Computation options

All `Computation`-based operations accept:

```m2
op(input, DegreeLimit => N)              -- compute up to total degree N
op(input, BasisElementLimit => N)         -- stop after N basis elements
op(input, SyzygyLimit => N)               -- stop after N syzygies
op(input, PairLimit => N)                 -- stop after N S-pairs
op(input, CodimensionLimit => N)          -- stop when codim ≥ N
op(input, StopBeforeComputation => true)  -- return computation object without running
op(input, Strategy => "...")              -- algorithm variant
```

See [`e/file-comp.md`](M2/Macaulay2/e/file-comp.md) for the
abstract base class behaviour.

## Decision tree: which engine?

```
What are you computing?
  Gröbner basis
    Over field?
      Coefficient is Z/p? → F4 (Strategy => F4) or default
      Coefficient is QQ? → default (uses modular runs internally)
      Coefficient is GF? → default or F4
      Coefficient is field of characteristic 0 not QQ? → default
    Over ZZ? → default (handles ZZ-specific reductions)
    Toric (binomial)? → Algorithm => Toric
    Need to convert orders? → Algorithm => Walk
    Boolean? → BIBasis package
    Non-commutative? → AssociativeAlgebras package
    Default behaviour fine? → default

  Free resolution
    Over a field? → default (Schreyer-frame)
    Need a specific older algorithm for testing? → Strategy => 0..2
    Non-commutative? → NCResolutions

  Hilbert series / polynomial → hilbertSeries (Bigatti algorithm)

  LLL → NTL (default) or fplll (Strategy => "FPLLL")

  Numerical solving → NumericalAlgebraicGeometry package

  Polynomial factoring → factor (Factory library)

  Univariate roots → roots (MPSolve, certified)

  Primary decomposition → primaryDecomposition (M2 algorithm)

  Linear algebra → built-in (engine picks backend by ring)
```

## Comparison tables

### GB engines

| Engine | Ring | Algorithm | Best for | Status |
|---|---|---|---|---|
| `gb-default` | Any commutative | Buchberger + opts | Generic workloads | Stable |
| `f4/` | `Z/p` mostly | F4 Macaulay matrix | Z/p with many polys | Production |
| `gb-f4/` | `Z/p` mostly | F4 refactored | Future default | Maturing |
| mathicgb | `Z/p` | Signature-based | Large GBs with pruning | Stable |
| Toric | Binomial | Specialised | Toric ideals | Stable |
| NC GB | NC algebras | Word-overlap | NC ideals | Stable |
| NC-F4 | NC algebras | F4 for words | Large NC GBs | Maturing |
| BIBasis | Boolean polys | Janet involutive | Boolean systems | Stable |

### Resolution engines

| Engine | Algorithm | Best for | Status |
|---|---|---|---|
| `schreyer-resolution/` | F4-style + Schreyer | Modern default | Production |
| `res-a2` | Older Schreyer | Backup | Stable |
| `res-a1` | Older | Backup | Stable |
| `res-a0` | Original | Backup | Stable |
| `Eschreyer` | Predecessor | Obsolete | Maintained for compat |
| `NCResolutions/` | NC resolution | NC modules | Early |

## How to add a new computation

Per [`TOUR.md`](TOUR.md) Path F:

1. **Subclass `Computation`** in `e/`:
   ```cpp
   class MyComputation : public Computation {
       virtual void start_computation() override;
       virtual void get_result(int &returncode) override;
       virtual int complete_thru_degree() const override;
   };
   ```
2. **Wire it via `interface/`** — add an entry point matching the
   `IM2_<X>_make` convention.
3. **Bind in `d/`** — add `.d` binding calling the new function.
4. **Wrap in `m2/`** — add user-facing method.
5. **Test** in both `e/unit-tests/` (gtest) and `tests/normal/`
   (M2-level).

See [`e/file-comp.md`](M2/Macaulay2/e/file-comp.md) for the base
class.

## Stop conditions

The Computation framework lets users **interrupt and resume**:

```m2
G = gb(I, DegreeLimit => 3)        -- stop at degree 3
G = gb(I, BasisElementLimit => 20) -- stop after 20 elements
G = gb(I)                           -- resume; M2 reuses cached state
```

Each subsequent call to `gb` on the same ideal **reuses** the
already-computed state. M2's `Computation` framework tracks the
state in a hash table.

## Performance: which engine fastest?

Highly input-dependent. Rough heuristics:

- **GB over `Z/p`** (large basis): F4 or gb-f4.
- **GB over `QQ`**: default with modular reductions.
- **GB over `ZZ`**: default (others don't support ZZ as well).
- **NC GB**: NC-F4 if matrices fit; else classical NC-GB.
- **Resolutions over `Z/p`**: Schreyer (default).
- **Boolean polynomial GBs**: BIBasis (orders of magnitude
  faster).

When uncertain, M2's default is **usually right**. Override with
`Strategy =>` only when benchmarking or working around a known
issue.

## Used by

- M2 user code (the user-facing operations: `gb`, `resolution`,
  `LLL`, etc.).
- Engine developers extending the algorithms.
- Distribution maintainers wondering which optional libraries
  enable which engines.

## Package deep dives for computation-heavy packages

The packages that wrap or extend these engine computations now have
dedicated deep-dive docs. Use these when the M2-level entry point
matters more than the engine algorithm:

| Computation area | Package deep dive |
|---|---|
| Minimal primes, radicals, `isPrime` | [`MinimalPrimes`](M2/Macaulay2/packages/file-MinimalPrimes.md) — splitting-based with `AnnotatedIdeal` + hooks strategy system |
| Primary decomposition, associated primes | [`PrimaryDecomposition`](M2/Macaulay2/packages/file-PrimaryDecomposition.md) — three strategies: Shimoyama-Yokoyama (default), EHV via Ext, GTZ family |
| Saturation, ideal quotient, annihilator | [`Saturation`](M2/Macaulay2/packages/file-Saturation.md) — `addHook` strategy tables: Iterate / Quotient / Linear / Monomial / Eliminate / Bayer / GRevLex |
| Variable elimination, Sylvester resultants | [`Elimination`](M2/Macaulay2/packages/file-Elimination.md) — short module, but imported by every algebraic-decomposition package |
| Free resolutions, Ext, Tor, Yoneda product | [`Complexes`](M2/Macaulay2/packages/file-Complexes.md) — the `HomologicalAlgebraPackage`; 5 strategy variants for `freeResolution` |
| Truncations of modules + `effCone` / `nefCone` | [`Truncations`](M2/Macaulay2/packages/file-Truncations.md) — re-exported by `Complexes` |
| Sheaf cohomology, sheaf Ext / Hom | [`Varieties`](M2/Macaulay2/packages/file-Varieties.md) — built on `Complexes`; routes `HH^i F` through truncation + module Ext |
| Integral closure of rings and ideals | [`IntegralClosure`](M2/Macaulay2/packages/file-IntegralClosure.md) — six strategies (Radical / RadicalCodim1 / AllCodimensions / SimplifyFractions / StartWithOneMinor / Vasconcelos) + char-p variant `icFracP` |
| Rees algebras, associated graded, multiplicity | [`ReesAlgebra`](M2/Macaulay2/packages/file-ReesAlgebra.md) — JSAG-certified; foundational for ideal integral closure |
| LLL lattice reduction with backend dispatch | [`LLLBases`](M2/Macaulay2/packages/file-LLLBases.md) — NTL / fpLLL / Cohen backends + BKZ / Givens / FP-precision overlays via bit-encoded `Strategy` |
| Inverse systems (artinian Gorenstein construction) | [`InverseSystems`](M2/Macaulay2/packages/file-InverseSystems.md) |
| Module isomorphism (probabilistic) | [`Isomorphism`](M2/Macaulay2/packages/file-Isomorphism.md) — random-map approach with degree-shift detection |
| Numerical algebraic geometry | [`NumericalAlgebraicGeometry`](M2/Macaulay2/packages/file-NumericalAlgebraicGeometry.md) — JSAG-certified; M2engine / BERTINI / PHCPACK / HOM4PS2 backends |
| Convex polyhedra, cones, fans | [`Polyhedra`](M2/Macaulay2/packages/file-Polyhedra.md) — JSAG-certified; the largest single package (~44 000 lines) |
| Normal toric varieties, toric divisors | [`NormalToricVarieties`](M2/Macaulay2/packages/file-NormalToricVarieties.md) — the 5-divisor-group diagram + smooth-Fano database through dim 6 |
| Schubert calculus, intersection theory | [`Schubert2`](M2/Macaulay2/packages/file-Schubert2.md) — "abstract varieties" by their Chow rings; M2 successor to the Maple Schubert package |
| Simplicial complexes, Stanley-Reisner | [`SimplicialComplexes`](M2/Macaulay2/packages/file-SimplicialComplexes.md) — JSAG-certified; named examples (Klein bottle, Poincaré sphere, …) + monomial-ideal resolutions (Taylor, Scarf, Buchberger, Lyubeznik) |
| Local-ring computations (`R_P`, singularity analysis) | [`LocalRings`](M2/Macaulay2/packages/file-LocalRings.md) — `localRing`, `liftUp`, `hilbertSamuelFunction`, `localResolution`; lift-and-descend pattern |
| D-modules: Bernstein-Sato polynomials, multiplier ideals, D-module restriction/localisation, de Rham / local / intersection cohomology | [`BernsteinSato`](M2/Macaulay2/packages/file-BernsteinSato.md) — `globalBFunction f`, `multiplierIdeal(f, c)`, `Dresolution`, `Drestriction`, `Dlocalize`, `DHom`, `DeRham`, `localCohom`, `intersectionCohom`, `WeylClosure`; ~6 600 lines across 15 algorithmic modules |
| Graph theory + edge ideals (commutative-algebra bridge) | [`Graphs`](M2/Macaulay2/packages/file-Graphs.md) — `Graph`/`Digraph` types, 40+ named graph families, chromatic/clique/independence numbers, `edgeIdeal`/`coverIdeal`; single-file 5 542-line package |
| (Hyper)graphs tuned for edge-ideal workflows + Cohen-Macaulay tests | [`EdgeIdeals`](M2/Macaulay2/packages/file-EdgeIdeals.md) — JSAG-certified; `HyperGraph` type, `cliqueComplex`/`independenceComplex` bridges, `getGoodLeaf` splitting, `isCM`/`isSCM` tests |
| Characteristic-p commutative algebra: F-pure threshold, F-jumping exponents, Frobenius `nu` invariants | [`FrobeniusThresholds`](M2/Macaulay2/packages/file-FrobeniusThresholds.md) — JSAG-certified; `fpt f`, `isFPT`, `compareFPT`, `frobeniusNu`, Frobenius powers/roots; char-p counterpart of `BernsteinSato` |
| Partially ordered sets (combinatorial / Möbius / Hibi-ring) | [`Posets`](M2/Macaulay2/packages/file-Posets.md) — JSAG-certified; 15 named families, Möbius function, Hibi ideal, order complex, Cohen-Macaulay / shellability predicates |
| Algebraic statistics: vanishing ideals of graphical / Markov / Gaussian models | [`GraphicalModels`](M2/Macaulay2/packages/file-GraphicalModels.md) — `markovRing` / `gaussianRing`, `discreteVanishingIdeal` / `gaussianVanishingIdeal`, `trekIdeal`/`trekSeparation`, three Markov properties |
| D-module infrastructure: Weyl algebra construction, GB with weight orders, Fourier transform, Stafford-Smith make-cyclic, factoring | [`WeylAlgebras`](M2/Macaulay2/packages/file-WeylAlgebras.md) — `makeWeylAlgebra`, `gbw`, `Ddim`, `isHolonomic`, `Fourier`/`FourierInverse`/`Dtransposition`, `makeCyclic M`, `factorWA f` |
| Holonomic systems: GKZ hypergeometric systems, canonical-series solutions, differential-operator rings | [`HolonomicSystems`](M2/Macaulay2/packages/file-HolonomicSystems.md) — `gkz(A, β)`, `AppellF1`, `cssExpts`, `indicialIdeal`, `solveFrobeniusIdeal`, `diffOps` |
| Bertini numerical solver interface (zero-dim / pos-dim / parameter homotopies, adaptive precision) | [`Bertini`](M2/Macaulay2/packages/file-Bertini.md) — `bertiniZeroDimSolve`, `bertiniPosDimSolve`, `bertiniParameterHomotopy`, `bertiniTrackHomotopy`; backend for `Software => BERTINI` in NAG |

For symbol-level lookups (given an M2 function or engine class name, find its deep dive), see [`SYMBOLS.md`](SYMBOLS.md).

## Related

- [`README.md`](README.md) — repository TOC.
- [`GLOSSARY.md`](GLOSSARY.md) — terminology (`Computation`, F4,
  Schreyer frame, etc.).
- [`RING-ZOO.md`](RING-ZOO.md) — sister catalogue of rings.
- [`DEPENDENCIES.md`](DEPENDENCIES.md) — external libraries each
  engine needs.
- [`SYMBOLS.md`](SYMBOLS.md) — symbol-to-doc reverse index.
- [`M2/Macaulay2/e/groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md)
  — engine-area overview (now with M2-strategy → engine-algorithm mapping table).
- [`M2/Macaulay2/e/resolutions.md`](M2/Macaulay2/e/resolutions.md)
  — engine-area overview (now with M2-strategy → engine-implementation mapping).
- [`M2/Macaulay2/e/computations.md`](M2/Macaulay2/e/computations.md)
  — engine-area overview (now with M2-operation → engine-entry mapping).
- [`M2/Macaulay2/e/architecture.md`](M2/Macaulay2/e/architecture.md)
  — engine architectural reference.
- All engine-subdir architectures
  ([`f4/`](M2/Macaulay2/e/f4/architecture.md),
  [`gb-f4/`](M2/Macaulay2/e/gb-f4/architecture.md),
  [`schreyer-resolution/`](M2/Macaulay2/e/schreyer-resolution/architecture.md),
  [`NCAlgebras/`](M2/Macaulay2/e/NCAlgebras/architecture.md),
  [`NCResolutions/`](M2/Macaulay2/e/NCResolutions/architecture.md),
  [`bibasis/`](M2/Macaulay2/e/bibasis/architecture.md)).
- M2 user docs — `help "computing Gröbner bases"`,
  `help resolution`, etc.
