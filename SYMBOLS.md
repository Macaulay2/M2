# Symbol-to-doc index

Given an engine **class name** or M2 **function name**, find the
corresponding source file and deep-dive doc. Covers the most-
referenced symbols — see [`INDEX.md`](INDEX.md) for a complete flat
catalogue of every doc, and [`GLOSSARY.md`](GLOSSARY.md) for
terminology.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) ·
[Index](INDEX.md) · [Cheatsheet](CHEATSHEET.md)

## Engine C++ classes

### Ring hierarchy

| Symbol | Defined in | Deep dive |
|---|---|---|
| `Ring` | `e/ring.hpp` | [`e/file-ring.md`](M2/Macaulay2/e/file-ring.md) |
| `ARing` | `e/ring.hpp` (forward in `e/aring.hpp`) | [`e/coefficient-rings.md`](M2/Macaulay2/e/coefficient-rings.md) |
| `ConcreteRing<ARingType>` | `e/aring-glue.hpp` | [`e/file-aring-glue.md`](M2/Macaulay2/e/file-aring-glue.md) |
| `PolynomialRing` | `e/polyring.hpp` | [`e/file-polyring.md`](M2/Macaulay2/e/file-polyring.md) |
| `QuotientRing` | `e/qring.hpp` | [`e/file-qring.md`](M2/Macaulay2/e/file-qring.md) |
| `FractionField` | `e/frac.hpp` | [`e/file-frac.md`](M2/Macaulay2/e/file-frac.md) |
| `LocalRing` | `e/localring.hpp` | [`e/file-localring.md`](M2/Macaulay2/e/file-localring.md) |
| `WeylAlgebra` | `e/weylalg.hpp` | [`e/file-weylalg.md`](M2/Macaulay2/e/file-weylalg.md) |
| `SkewPolynomialRing` | `e/skewpoly.hpp` | [`e/file-skewpoly.md`](M2/Macaulay2/e/file-skewpoly.md) |
| `SolvableAlgebra` | `e/solvable.hpp` | [`e/file-solvable.md`](M2/Macaulay2/e/file-solvable.md) |
| `SchurRing` | `e/schur.hpp` | [`e/file-schur.md`](M2/Macaulay2/e/file-schur.md) |
| `Tower` | `e/tower.hpp` | [`e/file-tower.md`](M2/Macaulay2/e/file-tower.md) |
| `M2FreeAlgebra` / `…Quotient` | `e/M2FreeAlgebra.hpp` | [`e/file-M2FreeAlgebra.md`](M2/Macaulay2/e/file-M2FreeAlgebra.md), [`e/file-M2FreeAlgebraQuotient.md`](M2/Macaulay2/e/file-M2FreeAlgebraQuotient.md) |

### Coefficient `ARing` backends

| Symbol | Defined in | Deep dive |
|---|---|---|
| `ARingZZ`, `ARingZZGMP` | `e/aring-zz-gmp.hpp` | [`e/file-aring-zz-gmp.md`](M2/Macaulay2/e/file-aring-zz-gmp.md) |
| `ARingZZpFlint` | `e/aring-zz-flint.hpp` | [`e/file-aring-zz-flint.md`](M2/Macaulay2/e/file-aring-zz-flint.md) |
| `ARingZZp` | `e/aring-zzp.hpp` | [`e/file-aring-zzp.md`](M2/Macaulay2/e/file-aring-zzp.md) |
| `ARingZZpFFPACK` | `e/aring-zzp-ffpack.hpp` | [`e/file-aring-zzp-ffpack.md`](M2/Macaulay2/e/file-aring-zzp-ffpack.md) |
| `ARingQQGMP`, `ARingQQFlint` | `e/aring-qq.hpp` | [`e/file-aring-qq.md`](M2/Macaulay2/e/file-aring-qq.md) |
| `ARingGFM2`, `…Flint`, `…FlintBig` | `e/aring-gf.hpp`, `e/aring-gf-flint.hpp`, `e/aring-gf-flint-big.hpp` | [`e/file-aring-gf-flint.md`](M2/Macaulay2/e/file-aring-gf-flint.md), [`e/file-aring-gf-flint-big.md`](M2/Macaulay2/e/file-aring-gf-flint-big.md) |
| `ARingRR` (double) | `e/aring-RR.hpp` | [`e/file-aring-RR.md`](M2/Macaulay2/e/file-aring-RR.md) |
| `ARingRRR` (MPFR) | `e/aring-RRR.hpp` | [`e/file-aring-RRR.md`](M2/Macaulay2/e/file-aring-RRR.md) |
| `ARingRRi` (MPFI) | `e/aring-RRi.hpp` | [`e/file-aring-RRi.md`](M2/Macaulay2/e/file-aring-RRi.md) |
| `ARingCC` (`complex<double>`) | `e/aring-CC.hpp` | [`e/file-aring-CC.md`](M2/Macaulay2/e/file-aring-CC.md) |
| `ARingCCC` (MPC) | `e/aring-CCC.hpp` | [`e/file-aring-CCC.md`](M2/Macaulay2/e/file-aring-CCC.md) |
| `ARingTower` | `e/aring-tower.hpp` | [`e/file-aring-tower.md`](M2/Macaulay2/e/file-aring-tower.md) |

The full ring zoo with a "when to use which" decision tree lives in
[`RING-ZOO.md`](RING-ZOO.md).

### Ring elements, maps, modules, matrices

| Symbol | Defined in | Deep dive |
|---|---|---|
| `RingElement` | `e/relem.hpp` | [`e/file-relem.md`](M2/Macaulay2/e/file-relem.md) |
| `ring_elem` (legacy union) | `e/ring.hpp` | [`e/file-ring.md`](M2/Macaulay2/e/file-ring.md) |
| `ElementType` (modern, per-`aring`) | each `aring-*.hpp` | [`e/coefficient-rings.md`](M2/Macaulay2/e/coefficient-rings.md) |
| `RingMap` | `e/ringmap.hpp` | [`e/file-ringmap.md`](M2/Macaulay2/e/file-ringmap.md) |
| `FreeModule` | `e/freemod.hpp` | [`e/file-freemod.md`](M2/Macaulay2/e/file-freemod.md) |
| `Matrix` | `e/matrix.hpp` | [`e/file-matrix.md`](M2/Macaulay2/e/file-matrix.md) |
| `MutableMatrix` | `e/mutablemat.hpp` | [`e/file-mutablemat.md`](M2/Macaulay2/e/file-mutablemat.md) |
| `DMat<RingType>` (dense) | `e/dmat.hpp` | [`e/file-dmat.md`](M2/Macaulay2/e/file-dmat.md) |
| `SMat<RingType>` (sparse) | `e/smat.hpp` | [`e/file-smat.md`](M2/Macaulay2/e/file-smat.md) |
| `MutableComplex` | `e/mutablecomplex.hpp` | [`e/file-mutablecomplex.md`](M2/Macaulay2/e/file-mutablecomplex.md) |

### Monomials and monoids

| Symbol | Defined in | Deep dive |
|---|---|---|
| `Monoid` | `e/monoid.hpp` | [`e/file-monoid.md`](M2/Macaulay2/e/file-monoid.md) |
| `Monomial` | `e/monomial.hpp` | [`e/file-monomial.md`](M2/Macaulay2/e/file-monomial.md) |
| `MonomialIdeal` | `e/monideal.hpp` | [`e/file-monideal.md`](M2/Macaulay2/e/file-monideal.md) |
| `ExponentList` | `e/ExponentList.hpp` | [`e/file-ExponentList.md`](M2/Macaulay2/e/file-ExponentList.md) |
| `ExponentVector` | `e/ExponentVector.hpp` | [`e/file-ExponentVector.md`](M2/Macaulay2/e/file-ExponentVector.md) |
| `MonomialOrdering` (engine C type) | `e/monordering.h` | [`e/file-monordering.md`](M2/Macaulay2/e/file-monordering.md) |

### Polynomial value types

| Symbol | Defined in | Deep dive |
|---|---|---|
| `Polynomial` (modern) | `e/Polynomial.hpp` | [`e/file-Polynomial.md`](M2/Macaulay2/e/file-Polynomial.md) |
| `Nterm` (legacy linked-list) | `e/poly.hpp` | [`e/file-poly.md`](M2/Macaulay2/e/file-poly.md) |
| `BasicPoly`, `BasicPolyList` | `e/BasicPoly.hpp` | [`e/file-BasicPoly.md`](M2/Macaulay2/e/file-BasicPoly.md) |
| `polyheap` (geometric heap) | `e/geopoly.hpp` | [`e/file-geopoly-hpp.md`](M2/Macaulay2/e/file-geopoly-hpp.md) |
| `geobucket<F,V>` | `e/geobucket.hpp` | [`e/file-geobucket.md`](M2/Macaulay2/e/file-geobucket.md) |

### Computation framework

| Symbol | Defined in | Deep dive |
|---|---|---|
| `Computation` (abstract base) | `e/comp.hpp` | [`e/file-computation-framework.md`](M2/Macaulay2/e/file-computation-framework.md) |
| `GBComputation` | `e/comp-gb.hpp` | [`e/file-comp-gb.md`](M2/Macaulay2/e/file-comp-gb.md) |
| `GBDeclared` | `e/comp-gb-declared.hpp` | [`e/file-comp-gb-declared-proxy.md`](M2/Macaulay2/e/file-comp-gb-declared-proxy.md) |
| `ResolutionComputation` | `e/comp-res.hpp` | [`e/file-comp-res.md`](M2/Macaulay2/e/file-comp-res.md) |
| `gb-default` Buchberger workhorse | `e/gb-default.hpp` | [`e/file-gb-default.md`](M2/Macaulay2/e/file-gb-default.md) |
| `F4Computation` (original F4) | `e/f4/F4Computation.hpp` | [`e/f4/file-f4-computation.md`](M2/Macaulay2/e/f4/file-f4-computation.md) |
| `mathicgb`-driven GB | `e/mathicgb-interface.hpp` | [`e/file-mathicgb-interface.md`](M2/Macaulay2/e/file-mathicgb-interface.md) |
| Schreyer-frame resolution | `e/schreyer-resolution/res-schreyer-frame.hpp` | [`e/schreyer-resolution/file-res-schreyer-frame.md`](M2/Macaulay2/e/schreyer-resolution/file-res-schreyer-frame.md) |
| BIBasis (Boolean involutive) | `e/bibasis/bibasis.hpp` | [`e/bibasis/file-bibasis.md`](M2/Macaulay2/e/bibasis/file-bibasis.md) |

Full strategy catalogue: [`COMPUTATIONS.md`](COMPUTATIONS.md).

### Memory and infrastructure

| Symbol | Defined in | Deep dive |
|---|---|---|
| `EngineObject` (GC base) | `e/hash.hpp` | [`e/file-hash.md`](M2/Macaulay2/e/file-hash.md) |
| `MutableEngineObject` | `e/hash.hpp` | [`e/file-hash.md`](M2/Macaulay2/e/file-hash.md) |
| `our_new_delete` | `e/newdelete.hpp` | [`e/file-newdelete.md`](M2/Macaulay2/e/file-newdelete.md) |
| `MemoryBlock<T>` | `e/MemoryBlock.hpp` | [`e/file-MemoryBlock.md`](M2/Macaulay2/e/file-MemoryBlock.md) |
| `FastAllocator` (bibasis slab) | `e/bibasis/allocator.hpp` | [`e/bibasis/file-allocator.md`](M2/Macaulay2/e/bibasis/file-allocator.md) |
| `stash` (typed pool) | `e/mem.hpp` | [`e/file-mem.md`](M2/Macaulay2/e/file-mem.md) |
| `M2_string` / `M2_arrayint` (cross-lang) | `e/interface/m2-types.h` | [`e/interface/file-m2-types-interface.md`](M2/Macaulay2/e/interface/file-m2-types-interface.md) |

Full memory model: [`MEMORY.md`](MEMORY.md).

### NC algebras and resolutions

| Symbol | Defined in | Deep dive |
|---|---|---|
| `FreeAlgebra` | `e/NCAlgebras/FreeAlgebra.hpp` | [`e/NCAlgebras/file-FreeAlgebra.md`](M2/Macaulay2/e/NCAlgebras/file-FreeAlgebra.md) |
| `FreeAlgebraQuotient` | `e/NCAlgebras/FreeAlgebraQuotient.hpp` | [`e/NCAlgebras/file-FreeAlgebraQuotient.md`](M2/Macaulay2/e/NCAlgebras/file-FreeAlgebraQuotient.md) |
| `NCGroebner` | `e/NCAlgebras/NCGroebner.hpp` | [`e/NCAlgebras/file-NCGroebner.md`](M2/Macaulay2/e/NCAlgebras/file-NCGroebner.md) |
| `NCReductionPoly` | `e/NCAlgebras/NCReduction.hpp` | [`e/NCAlgebras/file-NCReduction.md`](M2/Macaulay2/e/NCAlgebras/file-NCReduction.md) |
| `OverlapTable` (suffix-tree overlap) | `e/NCAlgebras/OverlapTable.hpp` | [`e/NCAlgebras/file-OverlapTable.md`](M2/Macaulay2/e/NCAlgebras/file-OverlapTable.md) |
| `NCResolutionComputation` | `e/NCResolutions/nc-res-computation.hpp` | [`e/NCResolutions/file-nc-res-computation.md`](M2/Macaulay2/e/NCResolutions/file-nc-res-computation.md) |

## M2-level functions (frequently looked up)

| M2 function | Engine entry | Wrapper (m2/) | Binding (d/) |
|---|---|---|---|
| `gb` | `rawGB` → `IM2_GB_make` in `interface/groebner.h` | [`m2/file-gb.md`](M2/Macaulay2/m2/file-gb.md) | [`d/file-engine-dd.md`](M2/Macaulay2/d/file-engine-dd.md) |
| `resolution` | `rawResolution` | [`m2/file-modules2.md`](M2/Macaulay2/m2/file-modules2.md) (defined in `modules2.m2`) | same |
| `matrix` | `rawMatrix` | [`m2/file-matrix.md`](M2/Macaulay2/m2/file-matrix.md) | same |
| `mingens` | `rawMinimalGenerators` | [`m2/file-gb.md`](M2/Macaulay2/m2/file-gb.md) (alongside the GB API) | same |
| `hilbertSeries` | `rawHilbert` | [`m2/file-hilbert.md`](M2/Macaulay2/m2/file-hilbert.md) | same |
| `LLL` | `rawLLL` | covered in engine: [`e/file-LLL.md`](M2/Macaulay2/e/file-LLL.md) | same |
| `lift` / `promote` | `rawLift` / `rawPromote` | [`m2/file-rings.md`](M2/Macaulay2/m2/file-rings.md) | same |
| `installPackage` | (no engine) | [`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md) | — |
| `loadPackage` | (no engine) | [`m2/file-packages.md`](M2/Macaulay2/m2/file-packages.md) | — |
| `viewHelp` / `help` | (no engine) | [`m2/file-help.md`](M2/Macaulay2/m2/file-help.md) | — |
| `task` / `schedule` | (supervisor) | [`m2/file-threads.md`](M2/Macaulay2/m2/file-threads.md) | [`d/file-threads.md`](M2/Macaulay2/d/file-threads.md) |

## M2 functions from documented packages

Operations from the 27 packages with dedicated deep dives. **Auto-loaded** packages are available without `needsPackage`:

| M2 function | Package (auto?) | Deep dive |
|---|---|---|
| `minimalPrimes I` / `minprimes I` | `MinimalPrimes` (auto) | [`packages/file-MinimalPrimes.md`](M2/Macaulay2/packages/file-MinimalPrimes.md) |
| `radical I` | `MinimalPrimes` (auto) | same |
| `isPrime I` | `MinimalPrimes` (auto) override | same |
| `primaryDecomposition I` | `PrimaryDecomposition` (auto) | [`packages/file-PrimaryDecomposition.md`](M2/Macaulay2/packages/file-PrimaryDecomposition.md) |
| `associatedPrimes I` / `ass I` | `PrimaryDecomposition` (auto) | same |
| `localize(I, P)`, `primaryComponent(I, P)` | `PrimaryDecomposition` (auto) | same |
| `isPrimary I`, `irreducibleDecomposition I`, `topComponents I` | `PrimaryDecomposition` (auto) | same |
| `saturate(I, J)`, `quotient(I, J)`, `annihilator M` | `Saturation` (auto) | [`packages/file-Saturation.md`](M2/Macaulay2/packages/file-Saturation.md) |
| `eliminate(v, I)` | `Elimination` (auto) | [`packages/file-Elimination.md`](M2/Macaulay2/packages/file-Elimination.md) |
| `resultant(f, g, x)`, `discriminant(f, x)`, `sylvesterMatrix(f, g, x)` | `Elimination` (auto) | same |
| `freeResolution M`, `res M`, `resolution M` | `Complexes` (auto) | [`packages/file-Complexes.md`](M2/Macaulay2/packages/file-Complexes.md) |
| `Ext^i(M, N)`, `Tor_i(M, N)` | `Complexes` (auto) | same |
| `Hom(M, N)`, `koszulComplex`, `eagonNorthcottComplex` | `Complexes` (auto) | same |
| `yonedaProduct`, `yonedaExtension`, `yonedaMap` | `Complexes` (auto) | same |
| `naiveTruncation`, `canonicalTruncation`, `constantStrand` | `Complexes` (auto) | same |
| `nullHomotopy`, `isNullHomotopic`, `isQuasiIsomorphism` | `Complexes` (auto) | same |
| `truncate(d, M)` | `Truncations` (re-exported by `Complexes`) | [`packages/file-Truncations.md`](M2/Macaulay2/packages/file-Truncations.md) |
| `effCone R`, `nefCone R`, `effGenerators R`, `nefGenerators R` | `Truncations` (re-exported by `Complexes`) | same |
| `doc ///...///`, `multidoc`, `packageTemplate` | `SimpleDoc` (auto) | [`packages/file-SimpleDoc.md`](M2/Macaulay2/packages/file-SimpleDoc.md) |
| `arXiv`, `stacksProject`, `wikipedia` (HTML helpers) | `SimpleDoc` (auto) | same |
| `Variety`, `AffineVariety`, `ProjectiveVariety`, `Spec R`, `Proj R` | `Varieties` (auto) | [`packages/file-Varieties.md`](M2/Macaulay2/packages/file-Varieties.md) |
| `CoherentSheaf`, `sheaf M`, `sheafHom`, `sheafExt^i` | `Varieties` (auto) | same |
| `tangentSheaf X`, `cotangentSheaf X`, `canonicalBundle X` | `Varieties` (auto) | same |
| `HH^i F` (sheaf cohomology), `hh^(p,q)`, `OO_X`, `F(d)` | `Varieties` (auto) | same |
| `integralClosure R`, `integralClosure I` | `IntegralClosure` (auto) | [`packages/file-IntegralClosure.md`](M2/Macaulay2/packages/file-IntegralClosure.md) |
| `conductor R`, `icMap R`, `icFractions R`, `icFracP R` | `IntegralClosure` (auto) | same |
| `reesIdeal I`, `reesAlgebra I`, `associatedGradedRing I`, `specialFiber I` | `ReesAlgebra` (auto) | [`packages/file-ReesAlgebra.md`](M2/Macaulay2/packages/file-ReesAlgebra.md) |
| `analyticSpread I`, `multiplicity I`, `minimalReduction I`, `distinguished I J` | `ReesAlgebra` (auto) | same |
| `LLL M`, `kernelLLL M`, `hermite M`, `gcdLLL L`, `gramm M` | `LLLBases` (auto) | [`packages/file-LLLBases.md`](M2/Macaulay2/packages/file-LLLBases.md) |
| `inverseSystem M`, `toDividedPowers p`, `fromDividedPowers p` | `InverseSystems` (auto) | [`packages/file-InverseSystems.md`](M2/Macaulay2/packages/file-InverseSystems.md) |
| `fromDual M`, `toDual(d, I)` (legacy aliases) | `InverseSystems` (auto) | same |
| `isIsomorphic(N, M)`, `isomorphism(N, M)`, `checkDegrees(A, B)` | `Isomorphism` (auto) | [`packages/file-Isomorphism.md`](M2/Macaulay2/packages/file-Isomorphism.md) |
| `poly "x2y - 3xz3"` (classic-Macaulay polynomial syntax) | `Classic` (auto) | [`packages/file-utility-packages.md`](M2/Macaulay2/packages/file-utility-packages.md) |
| `conwayPolynomial(p, n)` | `ConwayPolynomials` (auto) | same |
| `oeis L`, `isc x` (online lookup) | `OnlineLookup` (auto) | same |
| `cite "Pkg"` (BibTeX entry) | `PackageCitations` (auto) | same |
| `tangentCone I` (engine path via homogenisation) | `TangentCone` (auto) | same |
| `convexHull V`, `coneFromVData R`, `polyhedronFromHData(M, v)` | `Polyhedra` (re-exported chain) | [`packages/file-Polyhedra.md`](M2/Macaulay2/packages/file-Polyhedra.md) |
| `Polyhedron`, `Cone`, `Fan`, `PolyhedralComplex` (types) | `Polyhedra` | same |
| `vertices P`, `rays C`, `halfspaces P`, `hilbertBasis C`, `latticePoints P` | `Polyhedra` | same |
| `normalToricVariety(rays, cones)`, `affineSpace n`, `toricProjectiveSpace n` | `NormalToricVarieties` | [`packages/file-NormalToricVarieties.md`](M2/Macaulay2/packages/file-NormalToricVarieties.md) |
| `hirzebruchSurface n`, `kleinschmidt(d, L)`, `smoothFanoToricVariety(d, i)` | `NormalToricVarieties` | same |
| `weilDivisorGroup X`, `cartierDivisorGroup X`, `classGroup X`, `picardGroup X` | `NormalToricVarieties` | same |
| `flagBundle(L, V)`, `schubertCycle(λ, G)`, `chern V`, `ch V`, `todd V` | `Schubert2` | [`packages/file-Schubert2.md`](M2/Macaulay2/packages/file-Schubert2.md) |
| `AbstractVariety`, `abstractProjectiveSpace n`, `tangentBundle X`, `blowup f` | `Schubert2` | same |
| `solveSystem F`, `track(start, target, ...)`, `refine(F, sols)` | `NumericalAlgebraicGeometry` | [`packages/file-NumericalAlgebraicGeometry.md`](M2/Macaulay2/packages/file-NumericalAlgebraicGeometry.md) |
| `numericalIrreducibleDecomposition I`, `parameterHomotopy(F, ...)` | `NumericalAlgebraicGeometry` | same |
| `simplicialComplex {…}`, `link(D, σ)`, `star(D, σ)`, `barycentricSubdivision D` | `SimplicialComplexes` | [`packages/file-SimplicialComplexes.md`](M2/Macaulay2/packages/file-SimplicialComplexes.md) |
| `localRing(S, P)`, `liftUp M`, `hilbertSamuelFunction(R, M, n)`, `localResolution`, `localsyz`, `localMingens`, `localPrune`, `setMaxIdeal` (legacy) | `LocalRings` | [`packages/file-LocalRings.md`](M2/Macaulay2/packages/file-LocalRings.md) |
| `globalBFunction f`, `multiplierIdeal(f, c)`, `jumpingCoefficients f`, `bFunction I`, `localBFunction(f, P)`, `Dresolution M`, `Drestriction(M, w)`, `Dlocalize(M, f)`, `DHom(M, N)`, `DeRham M`, `localCohom(I, M)`, `intersectionCohom M`, `WeylClosure I`, `annFs f` | `BernsteinSato` | [`packages/file-BernsteinSato.md`](M2/Macaulay2/packages/file-BernsteinSato.md) |
| `graph(…)`, `digraph(…)`, `Graph`, `Digraph` (types) | `Graphs` | [`packages/file-Graphs.md`](M2/Macaulay2/packages/file-Graphs.md) |
| `adjacencyMatrix G`, `edges G`, `vertexSet G`, `laplacianMatrix G`, `degreeSequence G` | `Graphs` | same |
| `completeGraph n`, `cycleGraph n`, `pathGraph n`, `generalizedPetersenGraph(n,k)`, `kneserGraph(n,k)`, `johnsonGraph(n,k)`, `barbellGraph`, `cocktailParty`, `friendshipGraph`, `lollipopGraph`, … (40+ named families) | `Graphs` | same |
| `chromaticNumber G`, `cliqueNumber G`, `independenceNumber G`, `girth G`, `diameter G`, `chromaticPolynomial G`, `spanningTree G` | `Graphs` | same |
| `edgeIdeal G`, `coverIdeal G`, `edgeRing G` (commutative-algebra bridge) | `Graphs` | same |
| `displayGraph G`, `writeDotFile(G, fname)`, `showTikZ G` (Graphviz / TikZ rendering) | `Graphs` | same |
| `HyperGraph` type, `hyperGraph(R, edges)`, `cliqueComplex G`, `independenceComplex G`, `edgeIdeal G`, `coverIdeal G` (edge-ideal-first variant) | `EdgeIdeals` | [`packages/file-EdgeIdeals.md`](M2/Macaulay2/packages/file-EdgeIdeals.md) |
| `isCM G`, `isSCM G`, `isChordal G`, `isPerfect G`, `getGoodLeaf`, `hasGoodLeaf`, `allOddHoles G`, `allEvenHoles G`, `lineGraph G` | `EdgeIdeals` | same |
| `fpt f` (F-pure threshold), `isFPT(t, f)`, `compareFPT(t, f)`, `isFJumpingExponent(t, f)`, `frobeniusNu(e, f)` | `FrobeniusThresholds` | [`packages/file-FrobeniusThresholds.md`](M2/Macaulay2/packages/file-FrobeniusThresholds.md) |
| `FrobeniusPower`, `FrobeniusRoot`, `GlobalFrobeniusRoot`, `isSimpleNormalCrossing`, `ContainmentTest` options | `FrobeniusThresholds` | same |
| `Poset` (type), `poset(G, R)`, `transitiveClosure`, `mobiusMatrix P`, `mobiusFunction(P, a, b)`, `characteristicPolynomial P` | `Posets` | [`packages/file-Posets.md`](M2/Macaulay2/packages/file-Posets.md) |
| `booleanLattice n`, `chain n`, `divisorPoset n`, `dominanceLattice n`, `lcmLattice I`, `partitionLattice n`, `facePoset Δ`, `youngSubposet λ`, etc. (15 named families) | `Posets` | same |
| `hibiIdeal P`, `hibiRing P`, `pPartitionRing P`, `hasseDiagram P`, `orderComplex P`, `comparabilityGraph P` (algebraic/combinatorial bridges) | `Posets` | same |
| `isCM P`, `isShellable P`, `isLattice P`, `isDistributive`, `isAtomic`, `isBoolean`, `isEulerian P`, `isGraded P` | `Posets` | same |
| `markovRing(d_1, …, d_n)`, `gaussianRing G`, `covarianceMatrix R`, `bidirectedEdgesMatrix G`, `directedEdgesMatrix G` | `GraphicalModels` | [`packages/file-GraphicalModels.md`](M2/Macaulay2/packages/file-GraphicalModels.md) |
| `discreteVanishingIdeal(G, R)`, `gaussianVanishingIdeal(G, R)`, `conditionalIndependenceIdeal`, `gaussianParametrization`, `markovMatrices`, `gaussianMatrices` | `GraphicalModels` | same |
| `globalMarkov G`, `localMarkov G`, `pairMarkov G`, `trekIdeal(R, G)`, `trekSeparation(G, A, B)`, `identifyParameters G`, `hiddenMap`, `marginMap` | `GraphicalModels` | same |
| `makeWeylAlgebra R`, `makeWA R`, `Ddim M`, `holonomicRank M`, `isHolonomic M`, `dpairInds W`, `dpairVars W` | `WeylAlgebras` | [`packages/file-WeylAlgebras.md`](M2/Macaulay2/packages/file-WeylAlgebras.md) |
| `gbw(I, w)`, `inw(I, w)`, `Dprune f`, `Fourier`, `FourierInverse`, `Dtransposition`, `makeCyclic M`, `factorWA f` | `WeylAlgebras` | same |
| `createIntRing W`, `IntRing`, `WtoIR`, `IRtoW`, `createThetaRing`, `ThetaRing`, `createHomWeylAlgebra`, `HomWeylAlgebra` | `WeylAlgebras` | same |
| `gkz(A, β)`, `AppellF1`, `eulerOperators(A, β)`, `toricIdealPartials A` (GKZ hypergeometric systems) | `HolonomicSystems` | [`packages/file-HolonomicSystems.md`](M2/Macaulay2/packages/file-HolonomicSystems.md) |
| `cssExpts I`, `cssExptsMult I`, `cssLeadTerm`, `indicialIdeal I`, `solveFrobeniusIdeal I`, `nilssonSupport I`, `truncatedCanonicalSeries`, `distraction f`, `isTorusFixed I` | `HolonomicSystems` | same |
| `diffOps(I, k)`, `putWeylAlgebra W`, `PolyGens`, `BasisElts` (differential operators on R/I) | `HolonomicSystems` | same |
| `bertiniZeroDimSolve F`, `bertiniPosDimSolve F`, `bertiniParameterHomotopy(F, p, vals)`, `bertiniTrackHomotopy(H, start, end)`, `bertiniSample(W, n)`, `bertiniRefineSols`, `bertiniComponentMemberTest`, `bertiniUserHomotopy` | `Bertini` | [`packages/file-Bertini.md`](M2/Macaulay2/packages/file-Bertini.md) |
| `MPType` option (machine / fixed / adaptive precision), `Configuration => { "BERTINIexecutable" => … }`, `subPoint`, `makeB'InputFile`, `importMainDataFile` | `Bertini` | same |
| `solveSystem F`, `solveRationalSystem`, `trackPaths`, `refineSolutions`, `numericalIrreducibleDecomposition F`, `mixedVolume F`, `cascade(F, n)`, `topWitnessSet F`, `factorWitnessSet`, `isWitnessSetMember(p, W)`, `intersectSlice(W, L)`, `realSlice1D`/`realSlice2D` | `PHCpack` | [`packages/file-PHCpack.md`](M2/Macaulay2/packages/file-PHCpack.md) |
| `computingPrecision`, `numThreads`, `randomSeed`, `seeProgress`, `intermediateSolutions`, `StartDimension`, `StartSystem`, `StableMixedVolume`, `versionNumber` (PHCpack tunable options) | `PHCpack` | same |
| `toricMarkov A`, `toricGroebner A`, `toricCircuits A`, `toricGraver A`, `toricGraverDegrees A`, `hilbertBasis A` (Matrix-input override of `Polyhedra`'s) | `FourTiTwo` | [`packages/file-FourTiTwo.md`](M2/Macaulay2/packages/file-FourTiTwo.md) |
| `toBinomial(M, S)`, `getMatrix filename`, `putMatrix(F, B)`, `InputType` option | `FourTiTwo` | same |
| `normalToricRing M`, `intclToricRing M`, `intclMonIdeal I`, `ehrhartRing M`, `torusInvariants`, `finiteDiagInvariants`, `diagInvariants` | `Normaliz` | [`packages/file-Normaliz.md`](M2/Macaulay2/packages/file-Normaliz.md) |
| `normaliz`, `setNmzOption("opt", true)`, `showNmzOptions`, `writeNmzData`, `readNmzData`, `rmNmzFiles`, `allComputations`, `getNumInvs`, `MonomialSubalgebra`, `RationalCone`, `nmzNumberThreads` | `Normaliz` | same |
| `gfan I`, `gfanBuchberger I`, `gfanGroebnerCone L`, `gfanInitialForms(L, w)`, `gfanMarkPolynomialSet L`, `gfanIsMarkedGroebnerBasis L`, `gfanLeadingTerms L`, `gfanHomogenize`, `gfanKrullDimension I`, `gfanSaturation I` | `gfanInterface` | [`packages/file-gfanInterface.md`](M2/Macaulay2/packages/file-gfanInterface.md) |
| `gfanTropicalVariety I`, `gfanTropicalBasis I`, `gfanTropicalStartingCone I`, `gfanTropicalIntersection I`, `gfanTropicalLinearSpace M`, `gfanTropicalRank M`, `gfanTropicalTraverse(C, I)`, `gfanTropicalLifting` (tropical operations) | `gfanInterface` | same |
| `gfanFanCommonRefinement`, `gfanFanProduct`, `gfanFanLink`, `gfanMinkowskiSum`, `gfanSecondaryFan`, `gfanResultantFan`, `gfanLatticeIdeal`, `gfanMinors`, `gfanRender`, `MarkedPolynomialList`, `markedPolynomialList` | `gfanInterface` | same |
| `taylorResolution I`, `scarfSimplicialComplex I`, `buchbergerSimplicialComplex I`, `lyubeznikSimplicialComplex I` | `SimplicialComplexes` | same |
| `kleinBottleComplex`, `dunceHatComplex`, `realProjectiveSpaceComplex n`, `poincareSphereComplex`, `smallManifold(d, n, k)` | `SimplicialComplexes` | same |

For the complete package list with all exported symbols, see each package's deep dive linked above, or the [packages overview](M2/Macaulay2/packages/README.md).

## Interpreter-layer `.d`/`.dd` symbols

| Symbol | Defined in | Deep dive |
|---|---|---|
| `Expr` (discriminated union) | `d/expr.d` | [`d/file-expr.md`](M2/Macaulay2/d/file-expr.md) |
| `Token` | `d/tokens.d` | [`d/file-tokens.md`](M2/Macaulay2/d/file-tokens.md) |
| `Dictionary` | `d/binding.d` | [`d/file-binding.md`](M2/Macaulay2/d/file-binding.md) |
| `BasicFile` (early stderr) | `d/errio.d` | [`d/file-err.md`](M2/Macaulay2/d/file-err.md) |
| `varstring`, `varnet` | `d/varstrin.d`, `d/varnets.d` | [`d/file-strings.md`](M2/Macaulay2/d/file-strings.md), [`d/file-nets.md`](M2/Macaulay2/d/file-nets.md) |
| `Net` (2D char grid) | `d/nets.d` | [`d/file-nets.md`](M2/Macaulay2/d/file-nets.md) |
| `Ccode(t, …)` (escape) | `c/` translator | [`c/file-cprint.md`](M2/Macaulay2/c/file-cprint.md), [`c/architecture.md`](M2/Macaulay2/c/architecture.md) |

## How to look up an unlisted symbol

1. **`.cpp` / `.hpp`:** grep `^class FooBar` or `^struct FooBar` in
   `M2/Macaulay2/e/` (and subdirs). The file's deep dive is
   `file-<basename>.md` in the same directory.
2. **`raw…` engine entry point:** grep `setupfun("rawFooBar"` in
   `M2/Macaulay2/d/*.d`. Then look up the engine side in
   `M2/Macaulay2/e/interface/`.
3. **M2 function:** grep `bar = method(` in `M2/Macaulay2/m2/`.

If nothing matches in step 1 because it's spelled differently
(`ARingZZp_FFPACK` vs `ARingZZpFFPACK`), grep just the substring
across all `.hpp` files: `grep -rn FFPACK M2/Macaulay2/e/*.hpp`.

## See also

- [`README.md`](README.md) — top-level repository TOC
- [`INDEX.md`](INDEX.md) — flat alphabetical doc catalogue
- [`GLOSSARY.md`](GLOSSARY.md) — terminology
- [`RING-ZOO.md`](RING-ZOO.md) — full ring catalogue
- [`COMPUTATIONS.md`](COMPUTATIONS.md) — full computation-engine catalogue
- [`CHEATSHEET.md`](CHEATSHEET.md) — command quick-reference
