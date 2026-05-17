# `M2/Macaulay2/e/` — the Macaulay2 engine

The **engine** is the C++ mathematical kernel of Macaulay2 (~340 source files
here). It supplies the heavy machinery — rings, monoids, matrices, modules,
Gröbner bases, resolutions, Hilbert functions, LLL, numerical AG — and is
linked into the final `M2` binary alongside the
[interpreter](../d/README.md).

Position in the [four-language stack](../../../README.md#the-four-language-stack):

```
.d / .dd  ──▶  .c / .cpp  ──▶  M2-interpreter ──▶ M2
                                     ▲
                                     │ linked
                                  M2-engine ← you are here
                                  [Macaulay2/e/]
```

For a much fuller cross-cutting tour of how the engine is organised, see the
[engine deep-dive](../../../README.md#engine-deep-dive-m2macaulay2e) in the
top-level README.

## Subdirectories

| Directory | Purpose |
|---|---|
| [`interface/`](interface/README.md) | Public C interface — entry points called from `d/engine.dd` |
| [`f4/`](f4/README.md) | Original F4 Gröbner basis engine |
| [`gb-f4/`](gb-f4/README.md) | Refactored F4 Gröbner basis engine |
| [`schreyer-resolution/`](schreyer-resolution/README.md) | F4-style free resolutions via Schreyer frames |
| [`NCAlgebras/`](NCAlgebras/README.md) | Non-commutative free algebras and GB |
| [`NCResolutions/`](NCResolutions/README.md) | Non-commutative free resolutions |
| [`bibasis/`](bibasis/README.md) | Involutive (Janet) bases for Boolean rings |
| [`unit-tests/`](unit-tests/README.md) | gtest suite for the engine |
| [`doxygen-settings/`](doxygen-settings/README.md) | Doxygen config for the developer API docs |

## Single-file deep dives

For especially central classes, there are dedicated single-file walkthroughs.
Convention: `file-<basename>.md` in this directory.

### Foundations

| File doc | Class | Area |
|---|---|---|
| [`file-monoid.md`](file-monoid.md) | `Monoid` | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-aring.md`](file-aring.md) | `aring` framework / dispatcher | [Coefficient rings](coefficient-rings.md) |
| [`file-polyring.md`](file-polyring.md) | `PolynomialRing` | [Polynomial rings](polynomial-rings.md) |
| [`file-freemod.md`](file-freemod.md) | `FreeModule` | [Free modules](free-modules.md) |
| [`file-schorder.md`](file-schorder.md) | `SchreyerOrder` | [Free modules](free-modules.md) |
| [`file-matrix.md`](file-matrix.md) | `Matrix` (immutable) | [Matrices](matrices.md) |
| [`file-mutablemat.md`](file-mutablemat.md) | `MutableMatrix` | [Matrices](matrices.md) |

### Polynomial ring variants

| File doc | Class | Area |
|---|---|---|
| [`file-frac.md`](file-frac.md) | `FractionField` | [Polynomial rings](polynomial-rings.md) |
| [`file-qring.md`](file-qring.md) | `QRingInfo` / `PolyQuotient` | [Polynomial rings](polynomial-rings.md) |
| [`file-localring.md`](file-localring.md) | `LocalRing` | [Polynomial rings](polynomial-rings.md) |
| [`file-weylalg.md`](file-weylalg.md) | `WeylAlgebra` | [Polynomial rings](polynomial-rings.md) |
| [`file-skewpoly.md`](file-skewpoly.md) | `SkewPolynomialRing` | [Polynomial rings](polynomial-rings.md) |
| [`file-solvable.md`](file-solvable.md) | `SolvableAlgebra` (PBW) | [Polynomial rings](polynomial-rings.md) |

### Monomial machinery

| File doc | Class | Area |
|---|---|---|
| [`file-imonorder.md`](file-imonorder.md) | Internal monomial order | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-montable.md`](file-montable.md) | `MonomialTable` | [Monoids & monomials](monoids-and-monomials.md) |

### Matrices

| File doc | Class | Area |
|---|---|---|
| [`file-dmat.md`](file-dmat.md) | `DMat<R>` (dense matrix template) | [Matrices](matrices.md) |

### Ring elements and maps

| File doc | Class | Area |
|---|---|---|
| [`file-relem.md`](file-relem.md) | `RingElement` | [Ring elements & maps](ring-elements-and-maps.md) |
| [`file-ringmap.md`](file-ringmap.md) | `RingMap` | [Ring elements & maps](ring-elements-and-maps.md) |

### Computations

| File doc | Class | Area |
|---|---|---|
| [`file-computation-framework.md`](file-computation-framework.md) | `Computation` (abstract base) | [GB](groebner-bases.md), [res](resolutions.md), [other](computations.md) |
| [`file-comp-gb.md`](file-comp-gb.md) | `GBComputation` | [Gröbner bases](groebner-bases.md) |
| [`file-gb-default.md`](file-gb-default.md) | `gbA` (default GB algorithm) | [Gröbner bases](groebner-bases.md) |
| [`file-gb-variants.md`](file-gb-variants.md) | `gb-homog2`/`gb-sugarless`/`gb-toric`/`gb-walk` | [Gröbner bases](groebner-bases.md) |
| [`file-mathicgb-interface.md`](file-mathicgb-interface.md) | mathicgb bridge | [Gröbner bases](groebner-bases.md) |
| [`file-reducedgb.md`](file-reducedgb.md) | `ReducedGB` family | [Gröbner bases](groebner-bases.md) |
| [`file-spair.md`](file-spair.md) | `s_pair`, `gb_elem` | [Gröbner bases](groebner-bases.md) |
| [`file-gbweight.md`](file-gbweight.md) | `GBWeight` | [Gröbner bases](groebner-bases.md) |
| [`file-assprime.md`](file-assprime.md) | `AssociatedPrimes` | [Other computations](computations.md) |
| [`file-comb.md`](file-comb.md) | `Subsets` (combinatorial helpers) | [Other computations](computations.md) |
| [`file-overflow.md`](file-overflow.md) | Overflow-checked arithmetic | [Utilities](utilities.md) |

### Utilities & monomial encodings

| File doc | Class | Area |
|---|---|---|
| [`file-buffer.md`](file-buffer.md) | `buffer` (append-only byte buffer) | [Utilities](utilities.md) |
| [`file-text-io.md`](file-text-io.md) | `text-io` (wrap / `bignum_text_out`) | [Utilities](utilities.md) |
| [`file-MemoryBlock.md`](file-MemoryBlock.md) | `MemoryBlock` (bump allocator) | [Utilities](utilities.md) |
| [`file-ExponentList.md`](file-ExponentList.md) | `ExponentList` (sparse monomial encoding) | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-Polynomial.md`](file-Polynomial.md) | `Monom` / `Poly` (modern polynomial value type) | [Polynomial rings](polynomial-rings.md) |
| [`file-VectorArithmetic.md`](file-VectorArithmetic.md) | `VectorArithmetic` (templated arithmetic dispatcher) | [Matrices](matrices.md) |
| [`file-error.md`](file-error.md) | `error.{c,h}` (engine error reporting) | [Utilities](utilities.md) |
| [`file-debug.md`](file-debug.md) | `debug.{cpp,hpp}` (debugger-callable printers) | [Utilities](utilities.md) |
| [`file-coeffrings.md`](file-coeffrings.md) | `CoefficientRing*` (registry + `SimpleARing` example) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-glue.md`](file-aring-glue.md) | `ConcreteRing<R>` (bridge from `aring` to `Ring`) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-zz-flint.md`](file-aring-zz-flint.md) | `ARingZZ` (ZZ via FLINT) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md) | `ARingZZpFlint` (Z/p via FLINT) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-RR.md`](file-aring-RR.md) | `ARingRR` (RR via hardware `double`) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-CC.md`](file-aring-CC.md) | `ARingCC` (CC via pair of `double`) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-qq-flint.md`](file-aring-qq-flint.md) | `ARingQQFlint` (QQ via FLINT) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-gf-flint.md`](file-aring-gf-flint.md) | `ARingGFFlint` (small GF via FLINT Zech) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-zz-gmp.md`](file-aring-zz-gmp.md) | `ARingZZGMP` (ZZ via GMP) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-zzp.md`](file-aring-zzp.md) | `ARingZZp` (portable Z/p via log tables) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) | `ARingZZpFFPACK` (Z/p via FFLAS-FFPACK) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-RRR.md`](file-aring-RRR.md) | `ARingRRR` (RR via MPFR) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-CCC.md`](file-aring-CCC.md) | `ARingCCC` (CC via MPFR pair) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-tower.md`](file-aring-tower.md) | `ARingTower` (iterated finite extension) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-RRi.md`](file-aring-RRi.md) | `ARingRRi` (real intervals via MPFI) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-CCi.md`](file-aring-CCi.md) | `ARingCCi` (complex intervals via MPFI) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-m2-gf.md`](file-aring-m2-gf.md) | `ARingGFM2` (native M2 GF, no external dep) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md) | `ARingGFFlintBig` (large GF via FLINT `fq_nmod`) | [Coefficient rings](coefficient-rings.md) |
| [`file-M2FreeAlgebra.md`](file-M2FreeAlgebra.md) | `M2FreeAlgebra` (`Ring` wrapper for NC algebras) | [Polynomial rings](polynomial-rings.md) |
| [`file-skew.md`](file-skew.md) | `SkewMultiplication` (skew-commutative config) | [Polynomial rings](polynomial-rings.md) |
| [`file-ringelem.md`](file-ringelem.md) | `ring_elem` (universal value type) | [Ring elements & maps](ring-elements-and-maps.md) |
| [`file-hash.md`](file-hash.md) | `EngineObject` / `MutableEngineObject` (GC bases) | [Utilities](utilities.md) |
| [`file-exceptions.md`](file-exceptions.md) | Engine C++ exception hierarchy | [Utilities](utilities.md) |
| [`file-engine-h.md`](file-engine-h.md) | `engine.h` aggregating header | (Public interface) |
| [`file-style.md`](file-style.md) | `style.hpp` (comparison codes, `GEOHEAP_SIZE`) | [Utilities](utilities.md) |
| [`file-newdelete.md`](file-newdelete.md) | `our_new_delete` GC allocation hook | [Utilities](utilities.md) |
| [`file-M2FreeAlgebraQuotient.md`](file-M2FreeAlgebraQuotient.md) | `M2FreeAlgebraQuotient` (`Ring` wrapper for NC quotients) | [Polynomial rings](polynomial-rings.md) |
| [`file-BasicPoly.md`](file-BasicPoly.md) | `BasicPoly` / `BasicPolyList` (portable polynomial type) | [Polynomial rings](polynomial-rings.md) |
| [`file-ExponentVector.md`](file-ExponentVector.md) | `ExponentVector` (dense monomial encoding template) | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-det.md`](file-det.md) | Determinants and minors (`DET_BAREISS`/`COFACTOR`/`DYNAMIC`) | [Matrices](matrices.md) |
| [`file-mutablecomplex.md`](file-mutablecomplex.md) | `MutableComplex` (in-place chain complex) | [Matrices](matrices.md) |
| [`file-dpoly.md`](file-dpoly.md) | Univariate polynomials over QQ extensions / finite fields | [Other computations](computations.md) |
| [`file-schur.md`](file-schur.md) | `SchurRing` (Schur function ring) | [Polynomial rings](polynomial-rings.md) |
| [`file-tower.md`](file-tower.md) | `Tower` (legacy tower-of-extensions ring) | [Coefficient rings](coefficient-rings.md) |
| [`file-ntl-interface.md`](file-ntl-interface.md) | Bridge to the NTL library | [Coefficient rings](coefficient-rings.md) |
| [`file-gauss.md`](file-gauss.md) | `GaussElimComputation` (field-coeff Gaussian elim GB) | [Gröbner bases](groebner-bases.md) |
| [`file-hermite.md`](file-hermite.md) | `HermiteComputation` (ZZ Hermite normal form) | [Gröbner bases](groebner-bases.md) |
| [`file-lapack.md`](file-lapack.md) | LAPACK bridge for `RR` / `CC` matrices | [Matrices](matrices.md) |
| [`file-eigen.md`](file-eigen.md) | Eigenvalues / SVD (LAPACK + Eigen3 fallback) | [Matrices](matrices.md) |
| [`file-pfaff.md`](file-pfaff.md) | `PfaffianComputation` (Pfaffians of skew-symmetric matrices) | [Matrices](matrices.md) |
| [`file-matrix-con.md`](file-matrix-con.md) | `MatrixConstructor` (immutable-matrix builder) | [Matrices](matrices.md) |
| [`file-matrix-stream.md`](file-matrix-stream.md) | `MatrixStream` (streaming matrix construction) | [Matrices](matrices.md) |
| [`file-mat-linalg.md`](file-mat-linalg.md) | Templated linear algebra for `DMat<R>` | [Matrices](matrices.md) |
| [`file-mat-arith.md`](file-mat-arith.md) | Templated matrix arithmetic + `MatrixWindow` | [Matrices](matrices.md) |
| [`file-mat-elem-ops.md`](file-mat-elem-ops.md) | `MatElementaryOps<MT>` (row/column ops) | [Matrices](matrices.md) |
| [`file-monomial-sets.md`](file-monomial-sets.md) | Fixed/variable-size monomial sets | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-mat-util.md`](file-mat-util.md) | Generic matrix helpers (`displayMat`) | [Matrices](matrices.md) |
| [`file-poly.md`](file-poly.md) | `PolyRing` (standard commutative polynomial ring) | [Polynomial rings](polynomial-rings.md) |
| [`file-polyquotient.md`](file-polyquotient.md) | `PolyQuotient` (concrete quotient subclass) | [Polynomial rings](polynomial-rings.md) |
| [`file-sagbi.md`](file-sagbi.md) | SAGBI helpers (legacy) | [Gröbner bases](groebner-bases.md) |
| [`file-points.md`](file-points.md) | `PointsComputation<CoeffRing>` (ideal of points) | [Other computations](computations.md) |
| [`file-interreduce.md`](file-interreduce.md) | `Interreducer` | [Gröbner bases](groebner-bases.md) |
| [`file-fractionfreeLU.md`](file-fractionfreeLU.md) | `FF_LUComputation` (Bareiss LU over a domain) | [Matrices](matrices.md) |
| [`file-franzi.md`](file-franzi.md) | `franzi-*` (Boolean-ring GB family) | [Gröbner bases](groebner-bases.md) |
| [`file-mutablemat-defs.md`](file-mutablemat-defs.md) | `MutableMat<Mat>` template internals | [Matrices](matrices.md) |
| [`file-util.md`](file-util.md) | `util.hpp` (M2-side string/array conversions) | [Utilities](utilities.md) |
| [`file-cra-impl.md`](file-cra-impl.md) | `ChineseRemainder` internals | [Other computations](computations.md) |
| [`file-monordering.md`](file-monordering.md) | `MonomialOrdering` constructors (impl) | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-montableZZ.md`](file-montableZZ.md) | `MonomialTableZZ` (ZZ-coeff monomial table) | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-monomial-collection.md`](file-monomial-collection.md) | `IntsSet` / `ModuleMonomSet` | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-monsort.md`](file-monsort.md) | Generic monomial sorter template | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-mem.md`](file-mem.md) | `stash` (size-class slab allocator) | [Utilities](utilities.md) |
| [`file-myalloc.md`](file-myalloc.md) | `StatsAllocator` (debug allocator) | [Utilities](utilities.md) |
| [`file-finalize.md`](file-finalize.md) | Engine-object finalisation hooks | [Utilities](utilities.md) |
| [`file-ring-vecs.md`](file-ring-vecs.md) | `Ring`'s `vec` operations | [Ring elements & maps](ring-elements-and-maps.md) |
| [`file-monideal-minprimes.md`](file-monideal-minprimes.md) | `MinimalPrimes` of a monomial ideal | [Other computations](computations.md) |
| [`file-interrupted.md`](file-interrupted.md) | `system_interrupted()` (Ctrl+C polling) | [Utilities](utilities.md) |
| [`file-int-bag.md`](file-int-bag.md) | `int_bag` (small value + varpower monomial) | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-dmat-zz-flint.md`](file-dmat-zz-flint.md) | `DMat<ARingZZ>` FLINT specialisation | [Matrices](matrices.md) |
| [`file-dmat-zzp-flint.md`](file-dmat-zzp-flint.md) | `DMat<ARingZZpFlint>` FLINT specialisation | [Matrices](matrices.md) |
| [`file-memory-status.md`](file-memory-status.md) | Placeholder memory-stats hooks | [Utilities](utilities.md) |
| [`file-dmat-qq-flint.md`](file-dmat-qq-flint.md) | `DMat<ARingQQFlint>` FLINT specialisation | [Matrices](matrices.md) |
| [`file-dmat-gf-flint.md`](file-dmat-gf-flint.md) | `DMat<ARingGFFlint>` / `ARingGFFlintBig` FLINT specialisations | [Matrices](matrices.md) |
| [`file-dmat-lu.md`](file-dmat-lu.md) | LU decomposition specialisations (`DMatLinAlg<R>`) | [Matrices](matrices.md) |
| [`file-dmat-ffpack.md`](file-dmat-ffpack.md) | Historical FFLAS-FFPACK dispatcher (no longer in use) | [Matrices](matrices.md) |
| [`file-geovec.md`](file-geovec.md) | Geometric heap for `vec` accumulation | [Ring elements & maps](ring-elements-and-maps.md) |
| [`file-matrix-kbasis.md`](file-matrix-kbasis.md) | k-basis of a graded module | [Matrices](matrices.md) |
| [`file-matrix-symm.md`](file-matrix-symm.md) | `SymmMatrix` (symmetric power) | [Matrices](matrices.md) |
| [`file-matrix-sort.md`](file-matrix-sort.md) | `MatrixSorter` (column sort) | [Matrices](matrices.md) |
| [`file-ZZ.md`](file-ZZ.md) | Legacy `ZZ` (`Ring`-based integers) | [Coefficient rings](coefficient-rings.md) |
| [`file-ZZp.md`](file-ZZp.md) | Legacy `Z_mod` (`Ring`-based Z/p) | [Coefficient rings](coefficient-rings.md) |
| [`file-GF.md`](file-GF.md) | Legacy `GF` (`Ring`-based Galois field) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-translate.md`](file-aring-translate.md) | Cross-ring coercion templates | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-wrap.md`](file-aring-wrap.md) | `RElementWrap<RingType>` (typed value wrapper) | [Coefficient rings](coefficient-rings.md) |
| [`file-aring-qq.md`](file-aring-qq.md) | `ARingQQ` typedef + dispatcher | [Coefficient rings](coefficient-rings.md) |
| [`file-BasicPolyListParser.md`](file-BasicPolyListParser.md) | `BasicPolyList` text-format parsers | [Polynomial rings](polynomial-rings.md) |
| [`file-PolynomialStream.md`](file-PolynomialStream.md) | Streaming polynomial-input concept (newf4) | [Polynomial rings](polynomial-rings.md) |
| [`file-polyroots.md`](file-polyroots.md) | Univariate polynomial root finder | [Other computations](computations.md) |
| [`file-schur2.md`](file-schur2.md) | `SchurRing2` (refactored Schur ring) | [Polynomial rings](polynomial-rings.md) |
| [`file-schurSn.md`](file-schurSn.md) | `SchurSnRing` (symmetric-group ring) | [Polynomial rings](polynomial-rings.md) |
| [`file-schur-poly-heap.md`](file-schur-poly-heap.md) | `schur_poly_heap` (Schur accumulator) | [Polynomial rings](polynomial-rings.md) |
| [`file-matrix-ncbasis.md`](file-matrix-ncbasis.md) | Non-commutative `basis` | [Matrices](matrices.md) |
| [`file-SLP-defs.md`](file-SLP-defs.md) | `SLProgram` / `M2SLProgram` declarations | [Other computations](computations.md) |
| [`file-SLP-imp.md`](file-SLP-imp.md) | `SLEvaluatorConcrete<RT>` (templated SLP evaluator) | [Other computations](computations.md) |
| [`file-monomial.md`](file-monomial.md) | `EngineMonomial` (boundary monomial type) | [Monoids & monomials](monoids-and-monomials.md) |
| [`file-godboltTest.md`](file-godboltTest.md) | Standalone Z/p log-table sandbox | (sandbox) |
| [`file-timing.md`](file-timing.md) | `timing.hpp` (engine-side timestamps) | [Utilities](utilities.md) |
| [`file-dmat-qq-interface-flint.md`](file-dmat-qq-interface-flint.md) | FLINT-mat translation for GMP-based `DMat<ARingQQ>` | [Matrices](matrices.md) |
| [`file-Eschreyer-cpp.md`](file-Eschreyer-cpp.md) | `Eschreyer.cpp` implementation notes | [Resolutions](resolutions.md) |
| [`file-aring-ZZ-comparison.md`](file-aring-ZZ-comparison.md) | Cross-reference: the three `ZZ` paths | [Coefficient rings](coefficient-rings.md) |
| [`file-m2tbb.md`](file-m2tbb.md) | `m2tbb.hpp` (TBB wrapper) | [Utilities](utilities.md) |
| [`file-ring.md`](file-ring.md) | `Ring` (legacy ring base class) | [Coefficient rings](coefficient-rings.md) |
| [`file-mat.md`](file-mat.md) | `MutableMatrix` (matrix abstract base) | [Matrices](matrices.md) |
| [`file-smat.md`](file-smat.md) | `SMat<R>` (sparse matrix template) | [Matrices](matrices.md) |
| [`file-mutablemat-imp.md`](file-mutablemat-imp.md) | `MutableMat<Mat>` SLP factory bodies | [Matrices](matrices.md) |
| [`file-comp.md`](file-comp.md) | `Computation` (incremental computation base) | [Other computations](computations.md) |
| [`file-cra.md`](file-cra.md) | `ChineseRemainder` (CRT + rational reconstruction) | [Other computations](computations.md) |
| [`file-reader.md`](file-reader.md) | `Reader<RingType>` (stream → ring element) | [Utilities](utilities.md) |
| [`file-BasicPolyList.md`](file-BasicPolyList.md) | `BasicPolyList` (ring-agnostic polynomial list) | [Polynomial rings](polynomial-rings.md) |
| [`file-geobucket.md`](file-geobucket.md) | `geobucket<F,V>`, `polyheap` (geometric buckets) | [Gröbner bases](groebner-bases.md) |
| [`file-aring-qq-gmp.md`](file-aring-qq-gmp.md) | `ARingQQGMP` (GMP-backed rationals) | [Coefficient rings](coefficient-rings.md) |
| [`file-res-old.md`](file-res-old.md) | `res-a0`/`res-a1`/`res-a2` (older resolution engines) | [Resolutions](resolutions.md) |
| [`file-fplll.md`](file-fplll.md) | `fp_LLL` (fplll lattice reduction) | [Other computations](computations.md) |
| [`file-comp-gb-declared-proxy.md`](file-comp-gb-declared-proxy.md) | `GBDeclared`, `GBProxy` | [Gröbner bases](groebner-bases.md) |
| [`file-complex-h.md`](file-complex-h.md) | `gmp_CC` C primitives | [Coefficient rings](coefficient-rings.md) |
| [`file-defgroups.md`](file-defgroups.md) | Doxygen group definitions | (docs) |
| [`file-dmat-gf-flint-big.md`](file-dmat-gf-flint-big.md) | `DMat<ARingGFFlintBig>` FLINT-native ops | [Matrices](matrices.md) |
| [`file-dmat-lu-variants.md`](file-dmat-lu-variants.md) | `DMatLinAlg<R>` specialisations for Z/p, QQ, GF | [Matrices](matrices.md) |
| [`file-exptable-table.md`](file-exptable-table.md) | Hanson's `Table_T` + exponent-vector wrapper | [Utilities](utilities.md) |
| [`file-mpreal.md`](file-mpreal.md) | Vendored `mpreal` (MPFR C++ wrapper) | (vendored) |
| [`file-overflow-test.md`](file-overflow-test.md) | `safe::*` benchmark binary | [Utilities](utilities.md) |
| [`file-ntl-glue.md`](file-ntl-glue.md) | `ntl-debugio`, `ntl-internal` (NTL glue) | [Utilities](utilities.md) |
| [`file-franzi-brp.md`](file-franzi-brp.md) | Franziska Hinkelmann's boolean-ring GB | [Gröbner bases](groebner-bases.md) |

### Subdirectory file deep dives

Per-file walkthroughs that live alongside their source inside engine
subdirectories:

| Subdir | File doc | Class |
|---|---|---|
| `NCAlgebras/` | [`NCAlgebras/file-FreeMonoid.md`](NCAlgebras/file-FreeMonoid.md) | `FreeMonoid` |
| `NCAlgebras/` | [`NCAlgebras/file-FreeAlgebra.md`](NCAlgebras/file-FreeAlgebra.md) | `FreeAlgebra` |
| `NCAlgebras/` | [`NCAlgebras/file-NCGroebner.md`](NCAlgebras/file-NCGroebner.md) | `NCGroebner` |
| `NCAlgebras/` | [`NCAlgebras/file-NCF4.md`](NCAlgebras/file-NCF4.md) | `NCF4` |
| `NCAlgebras/` | [`NCAlgebras/file-WordTable.md`](NCAlgebras/file-WordTable.md) | `WordTable` |
| `NCAlgebras/` | [`NCAlgebras/file-OverlapTable.md`](NCAlgebras/file-OverlapTable.md) | `OverlapTable` |
| `NCAlgebras/` | [`NCAlgebras/file-NCReduction.md`](NCAlgebras/file-NCReduction.md) | `PolynomialHeap` (NC reduction) |
| `NCAlgebras/` | [`NCAlgebras/file-SuffixTree.md`](NCAlgebras/file-SuffixTree.md) | `SuffixTree` |
| `NCAlgebras/` | [`NCAlgebras/file-FreeAlgebraQuotient.md`](NCAlgebras/file-FreeAlgebraQuotient.md) | `FreeAlgebraQuotient` |
| `NCAlgebras/` | [`NCAlgebras/file-Word.md`](NCAlgebras/file-Word.md) | `Word` (non-owning word view) |
| `NCAlgebras/` | [`NCAlgebras/file-Range.md`](NCAlgebras/file-Range.md) | `Range<T>` (iterator-pair view) |
| `f4/` | [`f4/file-f4-computation.md`](f4/file-f4-computation.md) | `F4Computation` |
| `f4/` | [`f4/file-f4-spairs.md`](f4/file-f4-spairs.md) | `F4SPairSet` |
| `f4/` | [`f4/file-f4-m2-interface.md`](f4/file-f4-m2-interface.md) | `F4toM2Interface` |
| `f4/` | [`f4/file-monhashtable.md`](f4/file-monhashtable.md) | Monomial hash-table traits |
| `f4/` | [`f4/file-varpower-monomial.md`](f4/file-varpower-monomial.md) | F4-internal sparse encoding |
| `f4/` | [`f4/file-ntuple-monomial.md`](f4/file-ntuple-monomial.md) | F4-internal dense encoding |
| `f4/` | [`f4/file-moninfo.md`](f4/file-moninfo.md) | `MonomialInfo` (F4 monomial layout) |
| `f4/` | [`f4/file-f4.md`](f4/file-f4.md) | `F4GB` (the F4 algorithm) |
| `f4/` | [`f4/file-hilb-fcn.md`](f4/file-hilb-fcn.md) | `HilbertController` |
| `f4/` | [`f4/file-memblock.md`](f4/file-memblock.md) | `F4MemoryBlock<T>` |
| `f4/` | [`f4/file-f4-monlookup.md`](f4/file-f4-monlookup.md) | `F4MonomialLookupTableT<Key>` |
| `f4/` | [`f4/file-f4-types.md`](f4/file-f4-types.md) | F4 type vocabulary |
| `bibasis/` | [`bibasis/file-bibasis.md`](bibasis/file-bibasis.md) | `BIBasis` driver |
| `bibasis/` | [`bibasis/file-monom.md`](bibasis/file-monom.md) | `Monom` + ordering specialisations |
| `bibasis/` | [`bibasis/file-janettree.md`](bibasis/file-janettree.md) | `JanetTree<MonomType>` |
| `bibasis/` | [`bibasis/file-polynom.md`](bibasis/file-polynom.md) | `Polynom<MonomType>` |
| `NCResolutions/` | [`NCResolutions/file-nc-res-computation.md`](NCResolutions/file-nc-res-computation.md) | `NCResComputation` |
| `interface/` | [`interface/file-aring-interface.md`](interface/file-aring-interface.md) | aring C entry points |
| `interface/` | [`interface/file-groebner-interface.md`](interface/file-groebner-interface.md) | GB / resolution C entry points |
| `interface/` | [`interface/file-ring-interface.md`](interface/file-ring-interface.md) | Legacy `Ring` C entry points |
| `interface/` | [`interface/file-matrix-interface.md`](interface/file-matrix-interface.md) | `Matrix` C entry points |
| `interface/` | [`interface/file-freemodule-interface.md`](interface/file-freemodule-interface.md) | `FreeModule` C entry points |
| `interface/` | [`interface/file-monoid-interface.md`](interface/file-monoid-interface.md) | `Monoid` C entry points |
| `interface/` | [`interface/file-computation-interface.md`](interface/file-computation-interface.md) | Computation status / stop-condition enums |
| `interface/` | [`interface/file-ringelement-interface.md`](interface/file-ringelement-interface.md) | `RingElement` C entry points |
| `interface/` | [`interface/file-ringmap-interface.md`](interface/file-ringmap-interface.md) | `RingMap` C entry points |
| `interface/` | [`interface/file-monomial-ideal-interface.md`](interface/file-monomial-ideal-interface.md) | `MonomialIdeal` C entry points |
| `interface/` | [`interface/file-mutable-matrix-interface.md`](interface/file-mutable-matrix-interface.md) | `MutableMatrix` C entry points |
| `interface/` | [`interface/file-monomial-ordering-interface.md`](interface/file-monomial-ordering-interface.md) | `MonomialOrdering` enum + constructors |
| `interface/` | [`interface/file-flint-interface.md`](interface/file-flint-interface.md) | FLINT primality / factorisation |
| `interface/` | [`interface/file-cone-interface.md`](interface/file-cone-interface.md) | Cone operations |
| `interface/` | [`interface/file-factory-interface.md`](interface/file-factory-interface.md) | Polynomial GCD / factorisation |
| `interface/` | [`interface/file-cra-interface.md`](interface/file-cra-interface.md) | CRT / rational reconstruction |
| `interface/` | [`interface/file-NAG-interface.md`](interface/file-NAG-interface.md) | Numerical Algebraic Geometry C API |
| `interface/` | [`interface/file-random-interface.md`](interface/file-random-interface.md) | Engine RNG entry points |
| `interface/` | [`interface/file-gmp-util-interface.md`](interface/file-gmp-util-interface.md) | GMP/MPFR allocation helpers |
| `interface/` | [`interface/file-m2-mem-interface.md`](interface/file-m2-mem-interface.md) | Engine memory hooks + debug traps |
| `interface/` | [`interface/file-m2-types-interface.md`](interface/file-m2-types-interface.md) | Base type aliases |
| `gb-f4/` | [`gb-f4/file-GBF4Computation.md`](gb-f4/file-GBF4Computation.md) | `GBF4Computation` |
| `gb-f4/` | [`gb-f4/file-MacaulayMatrix.md`](gb-f4/file-MacaulayMatrix.md) | `MacaulayMatrix` |
| `gb-f4/` | [`gb-f4/file-Basis.md`](gb-f4/file-Basis.md) | `Basis` |
| `gb-f4/` | [`gb-f4/file-SPairs.md`](gb-f4/file-SPairs.md) | `SPairs` (refactored F4) |
| `gb-f4/` | [`gb-f4/file-MonomialHashTable.md`](gb-f4/file-MonomialHashTable.md) | `MonomialHashFunction` + table |
| `gb-f4/` | [`gb-f4/file-MonomialLookupTable.md`](gb-f4/file-MonomialLookupTable.md) | `MonomialLookupTable` (divisibility index) |
| `gb-f4/` | [`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md) | `PolynomialList` |
| `gb-f4/` | [`gb-f4/file-MonomialView.md`](gb-f4/file-MonomialView.md) | `MonomialView` |
| `gb-f4/` | [`gb-f4/file-MonomialTypes.md`](gb-f4/file-MonomialTypes.md) | Typed integers (`newf4` vocabulary) |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-f4-computation.md`](schreyer-resolution/file-res-f4-computation.md) | `F4ResComputation` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-schreyer-frame.md`](schreyer-resolution/file-res-schreyer-frame.md) | `SchreyerFrame` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-poly-ring.md`](schreyer-resolution/file-res-poly-ring.md) | `ResPolyRing` / `ResPolynomial` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-monomial-sorter.md`](schreyer-resolution/file-res-monomial-sorter.md) | `MonomialSorterObject` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-dep-graph.md`](schreyer-resolution/file-res-dep-graph.md) | TBB dependency graph |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-moninfo.md`](schreyer-resolution/file-res-moninfo.md) | `ResMonoid` dispatcher |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-schreyer-order.md`](schreyer-resolution/file-res-schreyer-order.md) | `ResSchreyerOrder` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-f4.md`](schreyer-resolution/file-res-f4.md) | `F4Res` (F4 reduction loop) |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-monomial-types.md`](schreyer-resolution/file-res-monomial-types.md) | Type vocabulary + encoding typedefs |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-f4-monlookup.md`](schreyer-resolution/file-res-f4-monlookup.md) | `ResF4MonomialLookupTableT<Key>` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-f4-m2-interface.md`](schreyer-resolution/file-res-f4-m2-interface.md) | `ResF4toM2Interface` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-memblock.md`](schreyer-resolution/file-res-memblock.md) | `ResMemoryBlock<T>` |
| [`file-comp-res.md`](file-comp-res.md) | `ResolutionComputation` | [Resolutions](resolutions.md) |
| [`file-gbring.md`](file-gbring.md) | `GBRing` / `gbvector` | [Gröbner bases](groebner-bases.md) |
| [`file-Eschreyer.md`](file-Eschreyer.md) | `GBKernelComputation` (older Schreyer) | [Resolutions](resolutions.md) |
| [`file-betti.md`](file-betti.md) | `BettiDisplay` | [Resolutions](resolutions.md) |
| [`file-hilb.md`](file-hilb.md) | Hilbert function (Bigatti) | [Other computations](computations.md) |
| [`file-LLL.md`](file-LLL.md) | LLL lattice reduction | [Other computations](computations.md) |
| [`file-monideal.md`](file-monideal.md) | `MonomialIdeal` | [Other computations](computations.md) |
| [`file-NAG.md`](file-NAG.md) | Numerical AG | [Other computations](computations.md) |
| [`file-SLP.md`](file-SLP.md) | Straight-line programs | [Other computations](computations.md) |

(More single-file deep dives will be added as the per-file documentation
effort proceeds.)

## Top-level files: per-area docs

Files at the top level of `e/` are documented in grouped markdown files, one
per area. These are the **engine deep-dive** references:

| Area | Documentation | Covers |
|---|---|---|
| Coefficient rings | [`coefficient-rings.md`](coefficient-rings.md) | `aring-*`, `ZZ`, `ZZp`, `GF`, `coeffrings` |
| Polynomial rings | [`polynomial-rings.md`](polynomial-rings.md) | `polyring`, `poly`, `qring`, `frac`, `weylalg`, `skewpoly`, `solvable`, `localring`, `BasicPoly*`, `Polynomial*` |
| Monoids & monomials | [`monoids-and-monomials.md`](monoids-and-monomials.md) | `monoid`, `monorder`, `imonorder`, `varpower`, `ntuple`, `montable*`, `ExponentList`, `ExponentVector` |
| Matrices | [`matrices.md`](matrices.md) | `matrix*`, `dmat*`, `smat`, `mat-*`, `mutablemat*` |
| Free modules | [`free-modules.md`](free-modules.md) | `freemod`, `schorder` |
| Gröbner bases | [`groebner-bases.md`](groebner-bases.md) | `comp-gb*`, `gb-*`, `reducedgb*`, `gbring`, `gbweight`, `spair`, `mathicgb-interface` |
| Resolutions | [`resolutions.md`](resolutions.md) | `comp-res`, `res-a0*`, `res-a1*`, `res-a2*`, `Eschreyer`, `betti` |
| Other computations | [`computations.md`](computations.md) | `hilb`, `LLL`, `NAG`, `SLP*`, `assprime`, `monideal`, `comb` |
| Ring elements & maps | [`ring-elements-and-maps.md`](ring-elements-and-maps.md) | `relem`, `ringmap`, `M2FreeAlgebra*` |
| Utilities | [`utilities.md`](utilities.md) | `buffer`, `text-io`, `error`, `debug`, `overflow`, `MemoryBlock` |

## File groups at the top level

> Detailed per-file tables are in the
> [engine deep-dive](../../../README.md#engine-deep-dive-m2macaulay2e).

| Group | Pattern | Description |
|---|---|---|
| Public top header | `engine.h` | The legacy aggregating header. New code goes in [`interface/`](interface/README.md) instead |
| Older interface | `x-*.cpp` | Older flat-layout entry points, slowly migrating into `interface/` |
| Abstract rings | `aring-*.{cpp,hpp}` | One file per coefficient type (ZZ, ZZp, QQ, RR, CC, GF, …) |
| Rings (concrete) | `ZZ.{cpp,hpp}`, `ZZp.{cpp,hpp}`, `GF.{cpp,hpp}`, `poly*.{cpp,hpp}`, `qring.{cpp,hpp}`, `weylalg.{cpp,hpp}`, `skewpoly.{cpp,hpp}`, `solvable.{cpp,hpp}`, `frac.{cpp,hpp}`, … | Specific ring constructions |
| Monoids | `monoid.{cpp,hpp}`, `monorder.cpp`, `imonorder.{cpp,hpp}`, `montable*.{cpp,hpp}`, `ExponentList.{cpp,hpp}`, `ExponentVector.hpp` (top-level); `f4/varpower-monomial.hpp`, `f4/ntuple-monomial.hpp` (F4-internal) | Monoid representation and ordering |
| Matrices | `matrix*.{cpp,hpp}`, `dmat*.{cpp,hpp}`, `mat-*.{cpp,hpp}`, `smat*.{cpp,hpp}` | Dense, sparse, and mutable matrices |
| Modules | `freemod*.{cpp,hpp}`, `schorder.{cpp,hpp}` | Free / Schreyer modules |
| Gröbner basis | `gb-*.{cpp,hpp}`, `comp-gb*.{cpp,hpp}`, `reducedgb*.{cpp,hpp}`, `gbring.{cpp,hpp}`, `gbweight.{cpp,hpp}`, `spair.{cpp,hpp}` | Several GB algorithms and the Computation glue |
| Resolutions | `res-a0*`, `res-a1*`, `res-a2*`, `comp-res.{cpp,hpp}`, `Eschreyer.{cpp,hpp}` | Older resolution implementations |
| Hilbert | `hilb.{cpp,hpp}` | Hilbert function / series |
| Numerics | `LLL.{cpp,hpp}`, `NAG.{cpp,hpp}`, `SLP*.{cpp,hpp}` | LLL, numerical AG, straight-line programs |
| Ideals | `assprime.{cpp,hpp}`, `monideal.{cpp,hpp}` | Associated primes; monomial ideals |
| Memory | `MemoryBlock.hpp`, `newdelete.hpp` (in subdirs) | GC integration helpers |
| Utility | `buffer.{cpp,hpp}`, `text-io.{cpp,hpp}`, `error.{cpp,hpp}`, `debug.{cpp,hpp}`, `overflow.{cpp,hpp}` | Generic helpers |

## Build

```sh
cmake --build M2/BUILD/build --target M2-core              # engine + interpreter glue
cmake --build M2/BUILD/build --target M2-unit-tests        # gtest binary
ctest -R "unit-tests" --output-on-failure                 # run gtests
```

## Adding an engine function (workflow)

1. **Implement** in C++ here, internal headers in subdirectories
   (e.g. a new matrix routine goes in `e/matrix/foo.{cpp,hpp}`).
2. **Expose** through [`interface/<area>.{h,cpp}`](interface/README.md), with
   minimal includes and **no dependency on `engine.h`**.
3. **Bind** in the interpreter at [`d/<area>.dd`](../d/README.md) via the
   `engine.dd` bridge.
4. **Wrap** at the M2 level in [`m2/<area>.m2`](../m2/README.md).
5. **Test** with a gtest in [`unit-tests/<area>.cpp`](unit-tests/README.md).

## Style and memory

- `STYLE.txt` in this directory captures C++ formatting conventions; the
  repository also has a `.clang-format` one level up at `M2/.clang-format`.
- Memory management goes through bdwgc. Use `our_new_delete` / `our_new_gc`
  helpers and the `MemoryBlock` allocator in hot loops.
- Long-running GC barrier concerns between engine and front-end are
  documented in the "Historical notes" below.

## Related TODOs

The engine carries several long-running design TODOs in plain text files
alongside the source:

- `TODO` — general
- `TODO-numerics` — numerical AG cleanup
- `TODO-rings-matrices` — ring/matrix refactor
- `TODO-SLPs` — straight-line programs
- `TODO-reallocate-heap` — GC-related allocator changes

## Topic-specific notes

- [`README-monideals.md`](README-monideals.md) — monomial ideal implementation
  notes.

---

## Historical notes

The remainder of this file preserves the older "Engine Notes" content that
predates this navigation README, so links to specific notes stay valid.

### Fall 2020 Work in Progress

1. Parallel directory structure in e, d, m2

- Main header: `e/engine.h`
  - should be short, mostly include other headers
  - organize interface functions in `e/interface`; eg:

        e/interface/matrix.h   // defining types currently in engine.h
        e/interface/matrix.cpp // previously x-mat.cpp

  - each should be self contained, include minimal dependencies (specifically, not `engine.h`)
  - associated interpreter and top level code should be placed in appropriate files; eg:

        d/matrix.dd
        m2/matrix.m2

- unit tests should be provided for all interface routines; eg:

        e/unit-test/matrix.cpp

- Internal routines
  - should be in respective directories, filename based on the classes; eg:

        e/matrix/matrix.hpp
        e/matrix/dense.hpp

2. GC barrier between engine and front end

- issue: ringelem, our_new_delete vs our_new_gc
- goal: ability to hotswap the GC backend by editing only one file
- benefit: allow easy comparison and benchmarking

3. Computations to be written or rewritten

- e.g. gb, smith normal form, etc.


### Engine
- engine.h and x-*
- newdelete hash
- Arithmetic
 - Flint
 - GMP
 - MPFR
 - Arb, etc.
- Monoids
- Rings
- RingElements
- RingMaps
- Matrices
- FreeModules
- Computations
 - LLL
 - GB
 - Resolution
 - Hilbert*
- NAG
- Util
- Interface


--------------------------------------------------------
-- 12/26/2011 MES
Cleaning up code todo:
1. get gtest working
    i.e. a make file target
2. create a gtest file
    linking test file
3. DONE tabs --> spaces
   DONE put in tab-mode-null into each file
    change copyrights
4. remove as many includes as possible
5. maybe make a set of subdirectories of 'e':
  rings
  matrices
  computations
  util
  commands
  tests (gtest stuff)
6. buffer --> use ostringstream?
7. text-io --> maybe keep these except for bignum_text_out?
8. DONE at some point, merge back in the stuff with Jakob, preferably soon
9.

------------------------------------------------------

these files really use overflow facilities:

    gbring.o
    imonorder.o
    matrix-kbasis.o
    matrix.o
    monoid.o
    monorder.o
    overflow.o
    polyring.o
    varpower.o

these files depends on overflow.hpp

    CC.o
    CCC.o
    Eschreyer.o
    GF.o
    QQ.o
    RR.o
    RRR.o
    ZZ.o
    ZZp.o
    comp-gb-declared.o
    comp-gb.o
    comp-res.o
    debug.o
    frac.o
    freemod.o
    gb-default.o
    gb-homog2.o
    gb-sugarless.o
    gb-toric.o
    gbring.o
    gbweight.o
    imonorder.o
    matrix-kbasis.o
    matrix.o
    monoid.o
    monorder.o
    montable.o
    ntuple.o
    overflow.o
    polyring.o
    qring.o
    reducedgb-ZZ.o
    reducedgb-field-local.o
    reducedgb-field.o
    reducedgb.o
    res-a2-gb.o
    res-a2.o
    schorder.o
    skewpoly.o
    solvable.o
    spair.o
    varpower.o
    weylalg.o
    x-gb.o
    x-mat.o
    x-relem.o

[← back to repository TOC](../../../README.md#under-m2macaulay2)
