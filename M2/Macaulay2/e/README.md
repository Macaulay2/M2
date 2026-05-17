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
| [`file-m2tbb.md`](file-m2tbb.md) | `m2tbb.hpp` (TBB wrapper) | [Utilities](utilities.md) |

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
| `f4/` | [`f4/file-f4-computation.md`](f4/file-f4-computation.md) | `F4Computation` |
| `f4/` | [`f4/file-f4-spairs.md`](f4/file-f4-spairs.md) | `F4SPairSet` |
| `f4/` | [`f4/file-f4-m2-interface.md`](f4/file-f4-m2-interface.md) | `F4toM2Interface` |
| `f4/` | [`f4/file-monhashtable.md`](f4/file-monhashtable.md) | Monomial hash-table traits |
| `f4/` | [`f4/file-varpower-monomial.md`](f4/file-varpower-monomial.md) | F4-internal sparse encoding |
| `f4/` | [`f4/file-ntuple-monomial.md`](f4/file-ntuple-monomial.md) | F4-internal dense encoding |
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
| `gb-f4/` | [`gb-f4/file-GBF4Computation.md`](gb-f4/file-GBF4Computation.md) | `GBF4Computation` |
| `gb-f4/` | [`gb-f4/file-MacaulayMatrix.md`](gb-f4/file-MacaulayMatrix.md) | `MacaulayMatrix` |
| `gb-f4/` | [`gb-f4/file-Basis.md`](gb-f4/file-Basis.md) | `Basis` |
| `gb-f4/` | [`gb-f4/file-SPairs.md`](gb-f4/file-SPairs.md) | `SPairs` (refactored F4) |
| `gb-f4/` | [`gb-f4/file-MonomialHashTable.md`](gb-f4/file-MonomialHashTable.md) | `MonomialHashFunction` + table |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-f4-computation.md`](schreyer-resolution/file-res-f4-computation.md) | `F4ResComputation` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-schreyer-frame.md`](schreyer-resolution/file-res-schreyer-frame.md) | `SchreyerFrame` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-poly-ring.md`](schreyer-resolution/file-res-poly-ring.md) | `ResPolyRing` / `ResPolynomial` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-monomial-sorter.md`](schreyer-resolution/file-res-monomial-sorter.md) | `MonomialSorterObject` |
| `schreyer-resolution/` | [`schreyer-resolution/file-res-dep-graph.md`](schreyer-resolution/file-res-dep-graph.md) | TBB dependency graph |
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
