# Research-domain index

The 34 documented M2 packages organised by **mathematical research
domain**. Use this when you know your area but aren't sure which
packages serve it.

For symbol-level lookup, see [`SYMBOLS.md`](SYMBOLS.md).
For computation-by-engine lookup, see [`COMPUTATIONS.md`](COMPUTATIONS.md).
For ring-type-based lookup, see [`RING-ZOO.md`](RING-ZOO.md).
For doc-by-name lookup, see [`INDEX.md`](INDEX.md).
For day-to-day commands, see [`CHEATSHEET.md`](CHEATSHEET.md).

[← repository TOC](README.md)

## Domain ↔ documented packages

### Commutative algebra (the M2 core)

The foundational toolkit. The first six are **auto-loaded** so every
M2 session has them without `needsPackage`.

| Operation | Package | Auto? |
|---|---|---|
| Minimal primes, radicals, `isPrime` | [`MinimalPrimes`](M2/Macaulay2/packages/file-MinimalPrimes.md) | ✓ |
| Primary decomposition, associated primes | [`PrimaryDecomposition`](M2/Macaulay2/packages/file-PrimaryDecomposition.md) | ✓ |
| Saturation, ideal quotient, annihilator | [`Saturation`](M2/Macaulay2/packages/file-Saturation.md) | ✓ |
| Variable elimination, Sylvester resultants | [`Elimination`](M2/Macaulay2/packages/file-Elimination.md) | ✓ |
| Integral closure of rings and ideals | [`IntegralClosure`](M2/Macaulay2/packages/file-IntegralClosure.md) | ✓ |
| Rees algebras, associated graded, multiplicity | [`ReesAlgebra`](M2/Macaulay2/packages/file-ReesAlgebra.md) | ✓ |
| Module isomorphism (probabilistic) | [`Isomorphism`](M2/Macaulay2/packages/file-Isomorphism.md) | ✓ |
| Inverse systems (artinian Gorenstein) | [`InverseSystems`](M2/Macaulay2/packages/file-InverseSystems.md) | ✓ |
| Operations over local rings `R_P` | [`LocalRings`](M2/Macaulay2/packages/file-LocalRings.md) | — |

### Homological algebra

| Operation | Package | Auto? |
|---|---|---|
| Free resolutions, Ext, Tor, Yoneda | [`Complexes`](M2/Macaulay2/packages/file-Complexes.md) | ✓ (as `HomologicalAlgebraPackage`) |
| Truncations of modules + `effCone`/`nefCone` | [`Truncations`](M2/Macaulay2/packages/file-Truncations.md) | ✓ (re-exported by Complexes) |
| Modules over local rings (with `localResolution` etc.) | [`LocalRings`](M2/Macaulay2/packages/file-LocalRings.md) | — |

### Algebraic geometry (general)

| Operation | Package | Auto? |
|---|---|---|
| Variety types (`Spec`, `Proj`), sheaves, sheaf cohomology | [`Varieties`](M2/Macaulay2/packages/file-Varieties.md) | ✓ |
| Intersection theory for "abstract" varieties (Chern classes, Schubert calculus, blowup) | [`Schubert2`](M2/Macaulay2/packages/file-Schubert2.md) | — |

### Toric geometry

| Operation | Package | Auto? |
|---|---|---|
| Convex polyhedra, cones, fans, polyhedral complexes | [`Polyhedra`](M2/Macaulay2/packages/file-Polyhedra.md) | — (effectively auto-loaded via Truncations) |
| Normal toric varieties, toric divisors, smooth Fano database | [`NormalToricVarieties`](M2/Macaulay2/packages/file-NormalToricVarieties.md) | — |
| 4ti2 interface: toric Markov / Groebner / circuits / Graver / Hilbert bases | [`FourTiTwo`](M2/Macaulay2/packages/file-FourTiTwo.md) | — |
| Normaliz interface: Hilbert basis / integral closure / Ehrhart polynomials / torus invariants (OpenMP-threaded) | [`Normaliz`](M2/Macaulay2/packages/file-Normaliz.md) | — |
| Gfan interface: Gröbner fans, tropical varieties, fan refinements | [`gfanInterface`](M2/Macaulay2/packages/file-gfanInterface.md) | — |
| Research-level tropical geometry: tropical varieties / prevarieties / cycles / stable intersection / Bergman fans | [`Tropical`](M2/Macaulay2/packages/file-Tropical.md) | — |

### Combinatorial commutative algebra

| Operation | Package | Auto? |
|---|---|---|
| Simplicial complexes + Stanley-Reisner + named topological examples | [`SimplicialComplexes`](M2/Macaulay2/packages/file-SimplicialComplexes.md) | — |
| Graph theory + edge ideals (broad toolkit) | [`Graphs`](M2/Macaulay2/packages/file-Graphs.md) | — |
| Graphs + hypergraphs tuned for edge-ideal workflows (Cohen-Macaulay tests, good-leaf splitting) | [`EdgeIdeals`](M2/Macaulay2/packages/file-EdgeIdeals.md) | — |
| Partially ordered sets (Hibi ideal, Möbius function, lattice predicates) | [`Posets`](M2/Macaulay2/packages/file-Posets.md) | — |
| Matroid theory (Tutte polynomial, Chow ring, named matroids) | [`Matroids`](M2/Macaulay2/packages/file-Matroids.md) | — |

### Numerical algebraic geometry

| Operation | Package | Auto? |
|---|---|---|
| Homotopy continuation, witness sets, numerical irreducible decomposition | [`NumericalAlgebraicGeometry`](M2/Macaulay2/packages/file-NumericalAlgebraicGeometry.md) | — |
| Bertini numerical-solver interface (backend of `Software => BERTINI`) | [`Bertini`](M2/Macaulay2/packages/file-Bertini.md) | — |
| PHCpack polyhedral-homotopy interface (backend of `Software => PHCPACK`) | [`PHCpack`](M2/Macaulay2/packages/file-PHCpack.md) | — |
| Lattice basis reduction (used by NAG and resolution paths) | [`LLLBases`](M2/Macaulay2/packages/file-LLLBases.md) | ✓ |

### D-modules and characteristic-p singularities

| Operation | Package | Auto? |
|---|---|---|
| Weyl algebra infrastructure: construction, weight-order GB, Fourier, Stafford-Smith make-cyclic | [`WeylAlgebras`](M2/Macaulay2/packages/file-WeylAlgebras.md) | — |
| Holonomic systems: GKZ hypergeometric systems, canonical-series solutions, differential-operator rings | [`HolonomicSystems`](M2/Macaulay2/packages/file-HolonomicSystems.md) | — |
| Bernstein-Sato polynomials, multiplier ideals, D-module operations (char 0; built on WeylAlgebras+HolonomicSystems) | [`BernsteinSato`](M2/Macaulay2/packages/file-BernsteinSato.md) | — |
| F-pure threshold, F-jumping exponents, Frobenius `nu` (char `p`) | [`FrobeniusThresholds`](M2/Macaulay2/packages/file-FrobeniusThresholds.md) | — |

### Algebraic statistics

| Operation | Package | Auto? |
|---|---|---|
| Graphical-model vanishing ideals (discrete + Gaussian), trek combinatorics | [`GraphicalModels`](M2/Macaulay2/packages/file-GraphicalModels.md) | — |

### Documentation, packaging, infrastructure

| Operation | Package | Auto? |
|---|---|---|
| The `doc ///…///` DSL — used by every package | [`SimpleDoc`](M2/Macaulay2/packages/file-SimpleDoc.md) | ✓ |
| The main M2 user documentation | [`Macaulay2Doc`](M2/Macaulay2/packages/file-Macaulay2Doc.md) | (structural — distributed with M2) |
| Doc styling + grammar generation | [`Style`](M2/Macaulay2/packages/file-Style.md) | (structural) |
| Engine M2-level tests | [`EngineTests`](M2/Macaulay2/packages/file-EngineTests.md) | (structural; in PACKAGES_DEVEL) |
| Package writing conventions | [`file-package-conventions.md`](M2/Macaulay2/packages/file-package-conventions.md) | (meta-doc) |

### Number-theoretic utilities

| Operation | Package | Auto? |
|---|---|---|
| The `GF(q)` database backing `GF` constructors | `ConwayPolynomials` (auto) — covered in [`file-utility-packages.md`](M2/Macaulay2/packages/file-utility-packages.md) | ✓ |
| Online math-database lookups (`oeis`, `isc`) | `OnlineLookup` (auto) — covered in [`file-utility-packages.md`](M2/Macaulay2/packages/file-utility-packages.md) | ✓ |
| BibTeX citation entry generation | `PackageCitations` (auto) — covered in [`file-utility-packages.md`](M2/Macaulay2/packages/file-utility-packages.md) | ✓ |

### Miscellaneous utilities (auto-loaded utility batch)

The five small auto-loaded utility packages live in a single
batched doc: [`file-utility-packages.md`](M2/Macaulay2/packages/file-utility-packages.md). Coverage:

- `Classic` — classic-Macaulay polynomial parser (`poly "x2y - 3xz3"`)
- `ConwayPolynomials` — `GF(q)` Conway polynomial database
- `OnlineLookup` — `oeis L`, `isc x`
- `PackageCitations` — `cite "Pkg"` BibTeX generation
- `TangentCone` — tangent cone of an ideal at the origin

## What's documented vs what's not

The **~400-package M2 distribution** spans many more research domains
than these 34. Packages without dedicated deep dives:

- `Bertini`, `PHCpack` — external-library wrappers for NAG backends (used by [`NumericalAlgebraicGeometry`](M2/Macaulay2/packages/file-NumericalAlgebraicGeometry.md))
- `WeylAlgebras`, `HolonomicSystems` — D-module infrastructure under [`BernsteinSato`](M2/Macaulay2/packages/file-BernsteinSato.md)
- `BIBasis`, `EngineTests`, `NAGtypes`, `SLPexpressions`, `NumericalLinearAlgebra` — packages re-exported from documented ones
- `FourTiTwo`, `Normaliz`, `gfanInterface` — external-library wrappers used by [`Polyhedra`](M2/Macaulay2/packages/file-Polyhedra.md) etc.
- `Truncations`, `Markov`, `AlgebraicStatistics` — used by documented packages
- ~360 application-specific packages (e.g. `Quaternions`, `Tropical`, `KostkaPolynomials`, `Posets`-based add-ons, `Macaulay2Doc/*` subpackages)

Each of those is documented through M2's built-in `viewHelp` system and (when applicable) via their JSAG publications. The deep-dive coverage focuses on what users land on most often: the auto-loaded core, the largest non-auto-loaded packages, and the packages that anchor specific research-domain workflows.

## How to extend this index

If you write a new package deep dive or fill in one of the gaps above:

1. Drop it under `M2/Macaulay2/packages/file-<PackageName>.md` following the conventions of the existing ones (see e.g. [`file-GraphicalModels.md`](M2/Macaulay2/packages/file-GraphicalModels.md) for a recent template).
2. Add a row in this domain index under the appropriate section.
3. Wire it into the four other top-level catalogues per the loop's rule:
   - [`README.md`](README.md) — per-folder file-deep-dives table + total counts
   - [`INDEX.md`](INDEX.md) — counts in the header
   - [`PACKAGES.md`](PACKAGES.md) — auto-loaded table or non-auto-loaded table + coverage tally
   - [`SYMBOLS.md`](SYMBOLS.md) — the "M2 functions from packages" section
   - [`COMPUTATIONS.md`](COMPUTATIONS.md) — the package-deep-dives table

The `file-MinimalPrimes.md`, `file-Polyhedra.md`, `file-Graphs.md`,
etc. all follow the same shape: introduction → exported API by category → architecture → "when this is slow" → cross-references → see-also.

## See also

- [`README.md`](README.md) — repository top-level TOC
- [`PACKAGES.md`](PACKAGES.md) — package ecosystem reference
- [`SYMBOLS.md`](SYMBOLS.md) — symbol-to-doc reverse index
- [`COMPUTATIONS.md`](COMPUTATIONS.md) — computation-engine catalogue
- [`RING-ZOO.md`](RING-ZOO.md) — ring-type catalogue
- [`INDEX.md`](INDEX.md) — flat doc index
- [`CHEATSHEET.md`](CHEATSHEET.md) — day-to-day commands
- [`TOUR.md`](TOUR.md) — audience-specific reading orders
- M2 user documentation — `help "<topic>"`, `viewHelp` for each package
