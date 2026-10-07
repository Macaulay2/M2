# PR #4441 (arings): review checklist

Every changed line gets **two reviewers**, nobody reviews their own changes, and each person's load is roughly the same (target about 5,470 weighted lines).

**How to use this:** find your name below. Each unit you review is a checkbox, with its files (or commits) as sub-boxes. Tick a file when you've reviewed it, and the unit when you're done with it. "With" names the other reviewer on that unit.

**Before merging:** delete this file and every review-notes directory (e.g. `PR4441-CC-CCC-review/`) from the branch, so they do not land in `development`.

## Overview

### By reviewer

| Reviewer | Units | Load |
|---|---|---:|
| [Doug](#doug) | Tests: CC / CCC; Tests: NCGroebner; Tests: other engine | 5,768 |
| [Andrew](#andrew) | ARing conversion API core; ARing backends (other changes); Matrix code + misc engine (other changes); Monomial-ordering move; set_from_* → set rename (5 commits) | 4,641 |
| [Aidan](#aidan) | Interval rings (RRi, CCi); ARing backends (other changes); Tests: ZZ / QQ / ZZp / GF; Tests: monomials | 5,290 |
| [Anton](#anton) | Build system & gcov; Tests: CC / CCC; Tests: matrices | 5,815 |
| [Mike S.](#mike-s) | Tests: monomials; Tests: NCGroebner; Tests: other engine; set_from_* → set rename (5 commits) | 6,160 |
| [Michael B.](#michael-b) | Build system & gcov; Matrix code + misc engine (other changes); Tests: CCi; Tests: ZZ / QQ / ZZp / GF | 4,981 |
| [Dave](#dave) | Test harness; Tests: CCi; Tests: RR / RRR / RRi | 5,712 |
| [Jingyi](#jingyi) | ARing conversion API core; Monomial-ordering move; Test harness | 5,105 |
| [Dima](#dima) | Interval rings (RRi, CCi); Tests: RR / RRR / RRi; Tests: matrices | 5,762 |

### By unit

| Unit | Weighted | Reviewers |
|---|---:|---|
| Build system & gcov | 564 | Anton, Michael B. |
| ARing conversion API core | 1,107 | Andrew, Jingyi |
| Interval rings (RRi, CCi) | 369 | Aidan, Dima |
| ARing backends (other changes) | 100 | Andrew, Aidan |
| Matrix code + misc engine (other changes) | 42 | Andrew, Michael B. |
| Monomial-ordering move | 2,240 | Andrew, Jingyi |
| Test harness | 1,758 | Dave, Jingyi |
| Test docs | 0 | not assigned (frozen) |
| Tests: CC / CCC | 2,509 | Doug, Anton |
| Tests: CCi | 1,303 | Michael B., Dave |
| Tests: RR / RRR / RRi | 2,651 | Dave, Dima |
| Tests: ZZ / QQ / ZZp / GF | 3,072 | Aidan, Michael B. |
| Tests: matrices | 2,742 | Anton, Dima |
| Tests: monomials | 1,749 | Aidan, Mike S. |
| Tests: NCGroebner | 1,421 | Doug, Mike S. |
| Tests: other engine | 1,838 | Doug, Mike S. |
| set_from_* → set rename (5 commits) | 1,152 | Andrew, Mike S. |
| **Total** | 24,617 | |

## Checklists

### Doug

Load: 5,768 weighted lines.

- [ ] **Tests: CC / CCC** — with Anton — 2,509 weighted (2,509 lines × 1)
  - [ ] `e/unit-tests/ARingCCCTest.cpp` +953/−213
  - [ ] `e/unit-tests/ARingCCTest.cpp` +904/−215
  - [ ] `e/unit-tests/RingCCCTest.cpp` +152/−76
- [ ] **Tests: NCGroebner** — with Mike S. — 1,421 weighted (1,421 lines × 1)
  - [ ] `e/unit-tests/NCGroebnerTest.cpp` +652/−769
- [ ] **Tests: other engine** — with Mike S. — 1,838 weighted (1,838 lines × 1)
  - [ ] `e/unit-tests/BRP-test.cpp` +60/−29
  - [ ] `e/unit-tests/BasicPolyListParserTest.cpp` +114/−133
  - [ ] `e/unit-tests/LatticePointsTest.cpp` +95/−72
  - [ ] `e/unit-tests/NewF4Test.cpp` +99/−144
  - [ ] `e/unit-tests/OverflowTest.cpp` +110/−28
  - [ ] `e/unit-tests/PointArray.cpp` +36/−20
  - [ ] `e/unit-tests/PolyRingTest.cpp` +35/−43
  - [ ] `e/unit-tests/QuotientRingTest.cpp` +25/−11
  - [ ] `e/unit-tests/ResTest.cpp` +176/−180
  - [ ] `e/unit-tests/RingTowerTest.cpp` +20/−61
  - [ ] `e/unit-tests/SubsetTest.cpp` +30/−26
  - [ ] `e/unit-tests/WeylAlgebraTest.cpp` +133/−96
  - [ ] `e/unit-tests/basics-test.cpp` +33/−29

### Andrew

Load: 4,641 weighted lines.

- [ ] **ARing conversion API core** — with Jingyi — 1,107 weighted (369 lines × 3)
  - [ ] `e/basic-rings/aring-glue.hpp` +11/−11
  - [ ] `e/basic-rings/aring-translate.hpp` +147/−322
  - [ ] `e/basic-rings/aring.hpp` +7/−9
  - [ ] `e/coeffrings.hpp` +6/−4
- [ ] **ARing backends (other changes)** — with Aidan — 100 weighted (67 lines × 1.5)
  Also check the small changes by Andrew (2 lines).
  - [ ] `e/basic-rings/aring-CC.hpp` +13/−19
  - [ ] `e/basic-rings/aring-CCC.hpp` +13/−25
  - [ ] `e/basic-rings/aring-GF-flint-big.hpp` +9/−15
  - [ ] `e/basic-rings/aring-GF-flint.cpp` +1/−1
  - [ ] `e/basic-rings/aring-GF-flint.hpp` +9/−15
  - [ ] `e/basic-rings/aring-QQ-flint.cpp` +1/−1
  - [ ] `e/basic-rings/aring-QQ-flint.hpp` +6/−12
  - [ ] `e/basic-rings/aring-QQ-gmp.cpp` +1/−1
  - [ ] `e/basic-rings/aring-QQ-gmp.hpp` +7/−6
  - [ ] `e/basic-rings/aring-RR.hpp` +6/−6
  - [ ] `e/basic-rings/aring-RRR.hpp` +6/−5
  - [ ] `e/basic-rings/aring-ZZ-flint.cpp` +3/−3
  - [ ] `e/basic-rings/aring-ZZ-flint.hpp` +5/−15
  - [ ] `e/basic-rings/aring-ZZ-gmp.cpp` +3/−3
  - [ ] `e/basic-rings/aring-ZZ-gmp.hpp` +7/−6
  - [ ] `e/basic-rings/aring-ZZp-ffpack.cpp` +9/−9
  - [ ] `e/basic-rings/aring-ZZp-ffpack.hpp` +4/−10
  - [ ] `e/basic-rings/aring-ZZp-flint.hpp` +9/−15
  - [ ] `e/basic-rings/aring-ZZp.hpp` +7/−13
  - [ ] `e/basic-rings/aring-m2-GF.cpp` +1/−1
  - [ ] `e/basic-rings/aring-m2-GF.hpp` +6/−13
  - [ ] `e/basic-rings/aring-tower.hpp` +7/−13
  - [ ] `e/basic-rings/reader.cpp` +1/−1
  - [ ] `e/basic-rings/vector-arithmetic.hpp` +7/−7
- [ ] **Matrix code + misc engine (other changes)** — with Michael B. — 42 weighted (28 lines × 1.5)
  Also check the small changes by Aidan (4 lines).
  - [ ] `e/BasicPolyListParser.cpp` +4/−6
  - [ ] `e/NAG/NAG.hpp` +1/−1
  - [ ] `e/SLP/SLP-imp.hpp` +11/−11
  - [ ] `e/basic-mutable-matrices/dmat-lu-inplace.hpp` +4/−4
  - [ ] `e/basic-mutable-matrices/dmat-lu-qq.hpp` +4/−4
  - [ ] `e/basic-mutable-matrices/dmat-lu-zzp-flint.hpp` +1/−1
  - [ ] `e/basic-mutable-matrices/dmat-lu.hpp` +18/−18
  - [ ] `e/basic-mutable-matrices/dmat.cpp` +4/−4
  - [ ] `e/basic-mutable-matrices/lapack.cpp` +10/−10
  - [ ] `e/basic-mutable-matrices/mat-arith.hpp` +3/−3
  - [ ] `e/basic-mutable-matrices/mat-elem-ops.hpp` +15/−15
  - [ ] `e/basic-mutable-matrices/mat-util.hpp` +2/−2
  - [ ] `e/basic-mutable-matrices/smat.hpp` +2/−2
  - [ ] `e/cytools/lattice_points.cpp` +10/−0
  - [ ] `e/eigen.cpp` +2/−2
  - [ ] `e/matrices/matrix-con.hpp` +0/−2
  - [ ] `e/rings/dpoly.cpp` +2/−2
  - [ ] `e/rings/dpoly.hpp` +3/−3
  - [ ] `e/rings/tower.cpp` +3/−3
  - [ ] `e/schreyer-resolutions/res-f4-m2-interface.cpp` +2/−2
  - [ ] `tests/normal/subst7.m2` +3/−3
- [ ] **Monomial-ordering move** — with Jingyi — 2,240 weighted (1,493 lines × 1.5)
  Also check the small changes by Doug (9 lines).
  - [ ] `e/interface/monomial-ordering.cpp` +51/−963
  - [ ] `e/interface/monomial-ordering.h` +1/−1
  - [ ] `e/monomials/monordering.cpp` +466/−0
  - [ ] `e/monomials/monordering.hpp` +11/−0
- [ ] **set_from_* → set rename (5 commits)** — with Mike S. — 1,152 weighted (768 lines × 1.5)
  Review with `git show <commit>`; the other units skip these changes.
  - [ ] `fcb62382b6` Rename set_from_* to set() across ARing classes (643 lines)
  - [ ] `dfaf786bee` Rename get_from_* dispatch helpers to try_set (24 lines)
  - [ ] `2d5678944a` Fix mutableMatrix, polynomial coefficients, and det over GFM2 (set vs copy) (82 lines)
  - [ ] `253a427eb4` set() updates for ZZp (17 lines)
  - [ ] `bfc0866d87` Fix broken CCi unit test (set_from_long -> set) (2 lines)

### Aidan

Load: 5,290 weighted lines.

- [ ] **Interval rings (RRi, CCi)** — with Dima — 369 weighted (123 lines × 3)
  - [ ] `e/basic-rings/aring-CCi.hpp` +32/−33
  - [ ] `e/basic-rings/aring-RRi.hpp` +100/−9
- [ ] **ARing backends (other changes)** — with Andrew — 100 weighted (67 lines × 1.5)
  Also check the small changes by Andrew (2 lines).
  - [ ] `e/basic-rings/aring-CC.hpp` +13/−19
  - [ ] `e/basic-rings/aring-CCC.hpp` +13/−25
  - [ ] `e/basic-rings/aring-GF-flint-big.hpp` +9/−15
  - [ ] `e/basic-rings/aring-GF-flint.cpp` +1/−1
  - [ ] `e/basic-rings/aring-GF-flint.hpp` +9/−15
  - [ ] `e/basic-rings/aring-QQ-flint.cpp` +1/−1
  - [ ] `e/basic-rings/aring-QQ-flint.hpp` +6/−12
  - [ ] `e/basic-rings/aring-QQ-gmp.cpp` +1/−1
  - [ ] `e/basic-rings/aring-QQ-gmp.hpp` +7/−6
  - [ ] `e/basic-rings/aring-RR.hpp` +6/−6
  - [ ] `e/basic-rings/aring-RRR.hpp` +6/−5
  - [ ] `e/basic-rings/aring-ZZ-flint.cpp` +3/−3
  - [ ] `e/basic-rings/aring-ZZ-flint.hpp` +5/−15
  - [ ] `e/basic-rings/aring-ZZ-gmp.cpp` +3/−3
  - [ ] `e/basic-rings/aring-ZZ-gmp.hpp` +7/−6
  - [ ] `e/basic-rings/aring-ZZp-ffpack.cpp` +9/−9
  - [ ] `e/basic-rings/aring-ZZp-ffpack.hpp` +4/−10
  - [ ] `e/basic-rings/aring-ZZp-flint.hpp` +9/−15
  - [ ] `e/basic-rings/aring-ZZp.hpp` +7/−13
  - [ ] `e/basic-rings/aring-m2-GF.cpp` +1/−1
  - [ ] `e/basic-rings/aring-m2-GF.hpp` +6/−13
  - [ ] `e/basic-rings/aring-tower.hpp` +7/−13
  - [ ] `e/basic-rings/reader.cpp` +1/−1
  - [ ] `e/basic-rings/vector-arithmetic.hpp` +7/−7
- [ ] **Tests: ZZ / QQ / ZZp / GF** — with Michael B. — 3,072 weighted (3,072 lines × 1)
  - [ ] `e/unit-tests/ARingGFTest.cpp` +130/−115
  - [ ] `e/unit-tests/ARingQQFlintTest.cpp` +73/−49
  - [ ] `e/unit-tests/ARingQQGmpTest.cpp` +232/−50
  - [ ] `e/unit-tests/ARingQQTest.hpp` +463/−0
  - [ ] `e/unit-tests/ARingZZGmpTest.cpp` +460/−0
  - [ ] `e/unit-tests/ARingZZTest.cpp` +382/−26
  - [ ] `e/unit-tests/ARingZZpTest.cpp` +472/−269
  - [ ] `e/unit-tests/RingQQTest.cpp` +50/−18
  - [ ] `e/unit-tests/RingZZTest.cpp` +153/−111
  - [ ] `e/unit-tests/RingZZpTest.cpp` +50/−9
- [ ] **Tests: monomials** — with Mike S. — 1,749 weighted (1,749 lines × 1)
  - [ ] `e/unit-tests/EngineMonomialTest.cpp` +101/−0
  - [ ] `e/unit-tests/ExponentListTest.cpp` +225/−0
  - [ ] `e/unit-tests/ExponentVectorTest.cpp` +116/−0
  - [ ] `e/unit-tests/MonoidTest.cpp` +66/−47
  - [ ] `e/unit-tests/MonomialCollectionsTest.cpp` +214/−0
  - [ ] `e/unit-tests/MonomialIdealTest.cpp` +368/−0
  - [ ] `e/unit-tests/MonomialOrderingTest.cpp` +180/−0
  - [ ] `e/unit-tests/MonomialSortTest.cpp` +44/−0
  - [ ] `e/unit-tests/MonomialTableTest.cpp` +216/−0
  - [ ] `e/unit-tests/MonomialTestHelpers.hpp` +172/−0

### Anton

Load: 5,815 weighted lines.

- [ ] **Build system & gcov** — with Michael B. — 564 weighted (376 lines × 1.5)
  - [ ] `.gitignore` +5/−0
  - [ ] `CMakeLists.txt` +12/−0
  - [ ] `bin/Makefile.in` +3/−0
  - [ ] `d/Makefile.in` +3/−0
  - [ ] `e/CMakeLists.txt` +32/−1
  - [ ] `e/Makefile.common.in` +5/−1
  - [ ] `e/Makefile.files.in` +1/−0
  - [ ] `e/unit-tests/Makefile.files` +28/−3
  - [ ] `e/unit-tests/Makefile.in` +3/−1
  - [ ] `system/Makefile.in` +3/−0
  - [ ] `Makefile.in` +17/−2
  - [ ] `cmake/configure.cmake` +0/−5
  - [ ] `cmake/gcov.cmake` +96/−12
  - [ ] `cmake/scc.cmake` +14/−5
  - [ ] `configure.ac` +51/−6
  - [ ] `include/config.Makefile.in` +4/−0
  - [ ] `m4/ax_check_compile_flag.m4` +63/−0
- [ ] **Tests: CC / CCC** — with Doug — 2,509 weighted (2,509 lines × 1)
  - [ ] `e/unit-tests/ARingCCCTest.cpp` +953/−213
  - [ ] `e/unit-tests/ARingCCTest.cpp` +904/−215
  - [ ] `e/unit-tests/RingCCCTest.cpp` +152/−76
- [ ] **Tests: matrices** — with Dima — 2,742 weighted (2,742 lines × 1)
  - [ ] `e/unit-tests/ARingMatrixTest.hpp` +374/−0
  - [ ] `e/unit-tests/DMatCCCTest.cpp` +30/−0
  - [ ] `e/unit-tests/DMatCCTest.cpp` +46/−0
  - [ ] `e/unit-tests/DMatCCiTest.cpp` +30/−0
  - [ ] `e/unit-tests/DMatGFFlintBigTest.cpp` +37/−0
  - [ ] `e/unit-tests/DMatGFFlintTest.cpp` +31/−0
  - [ ] `e/unit-tests/DMatGFM2Test.cpp` +37/−0
  - [ ] `e/unit-tests/DMatQQFlintTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatQQGMPTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatRRRTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatRRTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatRRiTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZGMPTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZpFFPACKTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZpFlintTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZpTest.cpp` +162/−66
  - [ ] `e/unit-tests/MatrixIOTest.cpp` +71/−182
  - [ ] `e/unit-tests/MatrixShape.hpp` +73/−0
  - [ ] `e/unit-tests/MatrixTest.hpp` +487/−0
  - [ ] `e/unit-tests/SMatTest.cpp` +791/−0
  - [ ] `e/unit-tests/SMatTest.hpp` +127/−0

### Mike S.

Load: 6,160 weighted lines.

- [ ] **Tests: monomials** — with Aidan — 1,749 weighted (1,749 lines × 1)
  - [ ] `e/unit-tests/EngineMonomialTest.cpp` +101/−0
  - [ ] `e/unit-tests/ExponentListTest.cpp` +225/−0
  - [ ] `e/unit-tests/ExponentVectorTest.cpp` +116/−0
  - [ ] `e/unit-tests/MonoidTest.cpp` +66/−47
  - [ ] `e/unit-tests/MonomialCollectionsTest.cpp` +214/−0
  - [ ] `e/unit-tests/MonomialIdealTest.cpp` +368/−0
  - [ ] `e/unit-tests/MonomialOrderingTest.cpp` +180/−0
  - [ ] `e/unit-tests/MonomialSortTest.cpp` +44/−0
  - [ ] `e/unit-tests/MonomialTableTest.cpp` +216/−0
  - [ ] `e/unit-tests/MonomialTestHelpers.hpp` +172/−0
- [ ] **Tests: NCGroebner** — with Doug — 1,421 weighted (1,421 lines × 1)
  - [ ] `e/unit-tests/NCGroebnerTest.cpp` +652/−769
- [ ] **Tests: other engine** — with Doug — 1,838 weighted (1,838 lines × 1)
  - [ ] `e/unit-tests/BRP-test.cpp` +60/−29
  - [ ] `e/unit-tests/BasicPolyListParserTest.cpp` +114/−133
  - [ ] `e/unit-tests/LatticePointsTest.cpp` +95/−72
  - [ ] `e/unit-tests/NewF4Test.cpp` +99/−144
  - [ ] `e/unit-tests/OverflowTest.cpp` +110/−28
  - [ ] `e/unit-tests/PointArray.cpp` +36/−20
  - [ ] `e/unit-tests/PolyRingTest.cpp` +35/−43
  - [ ] `e/unit-tests/QuotientRingTest.cpp` +25/−11
  - [ ] `e/unit-tests/ResTest.cpp` +176/−180
  - [ ] `e/unit-tests/RingTowerTest.cpp` +20/−61
  - [ ] `e/unit-tests/SubsetTest.cpp` +30/−26
  - [ ] `e/unit-tests/WeylAlgebraTest.cpp` +133/−96
  - [ ] `e/unit-tests/basics-test.cpp` +33/−29
- [ ] **set_from_* → set rename (5 commits)** — with Andrew — 1,152 weighted (768 lines × 1.5)
  Review with `git show <commit>`; the other units skip these changes.
  - [ ] `fcb62382b6` Rename set_from_* to set() across ARing classes (643 lines)
  - [ ] `dfaf786bee` Rename get_from_* dispatch helpers to try_set (24 lines)
  - [ ] `2d5678944a` Fix mutableMatrix, polynomial coefficients, and det over GFM2 (set vs copy) (82 lines)
  - [ ] `253a427eb4` set() updates for ZZp (17 lines)
  - [ ] `bfc0866d87` Fix broken CCi unit test (set_from_long -> set) (2 lines)

### Michael B.

Load: 4,981 weighted lines.

- [ ] **Build system & gcov** — with Anton — 564 weighted (376 lines × 1.5)
  - [ ] `.gitignore` +5/−0
  - [ ] `CMakeLists.txt` +12/−0
  - [ ] `bin/Makefile.in` +3/−0
  - [ ] `d/Makefile.in` +3/−0
  - [ ] `e/CMakeLists.txt` +32/−1
  - [ ] `e/Makefile.common.in` +5/−1
  - [ ] `e/Makefile.files.in` +1/−0
  - [ ] `e/unit-tests/Makefile.files` +28/−3
  - [ ] `e/unit-tests/Makefile.in` +3/−1
  - [ ] `system/Makefile.in` +3/−0
  - [ ] `Makefile.in` +17/−2
  - [ ] `cmake/configure.cmake` +0/−5
  - [ ] `cmake/gcov.cmake` +96/−12
  - [ ] `cmake/scc.cmake` +14/−5
  - [ ] `configure.ac` +51/−6
  - [ ] `include/config.Makefile.in` +4/−0
  - [ ] `m4/ax_check_compile_flag.m4` +63/−0
- [ ] **Matrix code + misc engine (other changes)** — with Andrew — 42 weighted (28 lines × 1.5)
  Also check the small changes by Aidan (4 lines).
  - [ ] `e/BasicPolyListParser.cpp` +4/−6
  - [ ] `e/NAG/NAG.hpp` +1/−1
  - [ ] `e/SLP/SLP-imp.hpp` +11/−11
  - [ ] `e/basic-mutable-matrices/dmat-lu-inplace.hpp` +4/−4
  - [ ] `e/basic-mutable-matrices/dmat-lu-qq.hpp` +4/−4
  - [ ] `e/basic-mutable-matrices/dmat-lu-zzp-flint.hpp` +1/−1
  - [ ] `e/basic-mutable-matrices/dmat-lu.hpp` +18/−18
  - [ ] `e/basic-mutable-matrices/dmat.cpp` +4/−4
  - [ ] `e/basic-mutable-matrices/lapack.cpp` +10/−10
  - [ ] `e/basic-mutable-matrices/mat-arith.hpp` +3/−3
  - [ ] `e/basic-mutable-matrices/mat-elem-ops.hpp` +15/−15
  - [ ] `e/basic-mutable-matrices/mat-util.hpp` +2/−2
  - [ ] `e/basic-mutable-matrices/smat.hpp` +2/−2
  - [ ] `e/cytools/lattice_points.cpp` +10/−0
  - [ ] `e/eigen.cpp` +2/−2
  - [ ] `e/matrices/matrix-con.hpp` +0/−2
  - [ ] `e/rings/dpoly.cpp` +2/−2
  - [ ] `e/rings/dpoly.hpp` +3/−3
  - [ ] `e/rings/tower.cpp` +3/−3
  - [ ] `e/schreyer-resolutions/res-f4-m2-interface.cpp` +2/−2
  - [ ] `tests/normal/subst7.m2` +3/−3
- [ ] **Tests: CCi** — with Dave — 1,303 weighted (1,303 lines × 1)
  - [ ] `e/unit-tests/ARingCCiTest.cpp` +1305/−0
- [ ] **Tests: ZZ / QQ / ZZp / GF** — with Aidan — 3,072 weighted (3,072 lines × 1)
  - [ ] `e/unit-tests/ARingGFTest.cpp` +130/−115
  - [ ] `e/unit-tests/ARingQQFlintTest.cpp` +73/−49
  - [ ] `e/unit-tests/ARingQQGmpTest.cpp` +232/−50
  - [ ] `e/unit-tests/ARingQQTest.hpp` +463/−0
  - [ ] `e/unit-tests/ARingZZGmpTest.cpp` +460/−0
  - [ ] `e/unit-tests/ARingZZTest.cpp` +382/−26
  - [ ] `e/unit-tests/ARingZZpTest.cpp` +472/−269
  - [ ] `e/unit-tests/RingQQTest.cpp` +50/−18
  - [ ] `e/unit-tests/RingZZTest.cpp` +153/−111
  - [ ] `e/unit-tests/RingZZpTest.cpp` +50/−9

### Dave

Load: 5,712 weighted lines.

- [ ] **Test harness** — with Jingyi — 1,758 weighted (1,172 lines × 1.5)
  - [ ] `e/unit-tests/ARingTest.hpp` +511/−192
  - [ ] `e/unit-tests/RingElem.cpp` +11/−4
  - [ ] `e/unit-tests/RingElem.hpp` +24/−14
  - [ ] `e/unit-tests/RingTest.hpp` +146/−92
  - [ ] `e/unit-tests/fromStream.cpp` +25/−52
  - [ ] `e/unit-tests/util-polyring-creation.cpp` +43/−72
  - [ ] `e/unit-tests/util-polyring-creation.hpp` +10/−8
- [ ] **Tests: CCi** — with Michael B. — 1,303 weighted (1,303 lines × 1)
  - [ ] `e/unit-tests/ARingCCiTest.cpp` +1305/−0
- [ ] **Tests: RR / RRR / RRi** — with Dima — 2,651 weighted (2,651 lines × 1)
  Also check the small changes by Doug (2 lines).
  - [ ] `e/unit-tests/ARingRRRTest.cpp` +921/−44
  - [ ] `e/unit-tests/ARingRRTest.cpp` +768/−36
  - [ ] `e/unit-tests/ARingRRiTest.cpp` +492/−194
  - [ ] `e/unit-tests/RingRRRTest.cpp` +147/−59

### Jingyi

Load: 5,105 weighted lines.

- [ ] **ARing conversion API core** — with Andrew — 1,107 weighted (369 lines × 3)
  - [ ] `e/basic-rings/aring-glue.hpp` +11/−11
  - [ ] `e/basic-rings/aring-translate.hpp` +147/−322
  - [ ] `e/basic-rings/aring.hpp` +7/−9
  - [ ] `e/coeffrings.hpp` +6/−4
- [ ] **Monomial-ordering move** — with Andrew — 2,240 weighted (1,493 lines × 1.5)
  Also check the small changes by Doug (9 lines).
  - [ ] `e/interface/monomial-ordering.cpp` +51/−963
  - [ ] `e/interface/monomial-ordering.h` +1/−1
  - [ ] `e/monomials/monordering.cpp` +466/−0
  - [ ] `e/monomials/monordering.hpp` +11/−0
- [ ] **Test harness** — with Dave — 1,758 weighted (1,172 lines × 1.5)
  - [ ] `e/unit-tests/ARingTest.hpp` +511/−192
  - [ ] `e/unit-tests/RingElem.cpp` +11/−4
  - [ ] `e/unit-tests/RingElem.hpp` +24/−14
  - [ ] `e/unit-tests/RingTest.hpp` +146/−92
  - [ ] `e/unit-tests/fromStream.cpp` +25/−52
  - [ ] `e/unit-tests/util-polyring-creation.cpp` +43/−72
  - [ ] `e/unit-tests/util-polyring-creation.hpp` +10/−8

### Dima

Load: 5,762 weighted lines.

- [ ] **Interval rings (RRi, CCi)** — with Aidan — 369 weighted (123 lines × 3)
  - [ ] `e/basic-rings/aring-CCi.hpp` +32/−33
  - [ ] `e/basic-rings/aring-RRi.hpp` +100/−9
- [ ] **Tests: RR / RRR / RRi** — with Dave — 2,651 weighted (2,651 lines × 1)
  Also check the small changes by Doug (2 lines).
  - [ ] `e/unit-tests/ARingRRRTest.cpp` +921/−44
  - [ ] `e/unit-tests/ARingRRTest.cpp` +768/−36
  - [ ] `e/unit-tests/ARingRRiTest.cpp` +492/−194
  - [ ] `e/unit-tests/RingRRRTest.cpp` +147/−59
- [ ] **Tests: matrices** — with Anton — 2,742 weighted (2,742 lines × 1)
  - [ ] `e/unit-tests/ARingMatrixTest.hpp` +374/−0
  - [ ] `e/unit-tests/DMatCCCTest.cpp` +30/−0
  - [ ] `e/unit-tests/DMatCCTest.cpp` +46/−0
  - [ ] `e/unit-tests/DMatCCiTest.cpp` +30/−0
  - [ ] `e/unit-tests/DMatGFFlintBigTest.cpp` +37/−0
  - [ ] `e/unit-tests/DMatGFFlintTest.cpp` +31/−0
  - [ ] `e/unit-tests/DMatGFM2Test.cpp` +37/−0
  - [ ] `e/unit-tests/DMatQQFlintTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatQQGMPTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatRRRTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatRRTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatRRiTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZGMPTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZpFFPACKTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZpFlintTest.cpp` +24/−0
  - [ ] `e/unit-tests/DMatZZpTest.cpp` +162/−66
  - [ ] `e/unit-tests/MatrixIOTest.cpp` +71/−182
  - [ ] `e/unit-tests/MatrixShape.hpp` +73/−0
  - [ ] `e/unit-tests/MatrixTest.hpp` +487/−0
  - [ ] `e/unit-tests/SMatTest.cpp` +791/−0
  - [ ] `e/unit-tests/SMatTest.hpp` +127/−0

## Reference: unit sizes and authors

| Unit | Lines | × | Weighted | Authors (lines attributed) |
|---|---:|---:|---:|---|
| Build system & gcov | 376 | 1.5 | 564 | Doug 286, Aidan 32, Andrew 22 |
| ARing conversion API core | 369 | 3 | 1,107 | Doug 536 |
| Interval rings (RRi, CCi) | 123 | 3 | 369 | Michael B. 85, Doug 36, Andrew 16 |
| ARing backends (other changes) | 67 | 1.5 | 100 | Doug 139, Andrew 2 |
| Matrix code + misc engine (other changes) | 28 | 1.5 | 42 | Doug 88, Mike S. 10, Aidan 4 |
| Monomial-ordering move | 1,493 | 1.5 | 2,240 | Aidan 1479, Doug 9 |
| Test harness | 1,172 | 1.5 | 1,758 | Andrew 351, Doug 298, Anton 88 |
| Test docs | 733 | 1 | 0 | Anton 596, Andrew 137 |
| Tests: CC / CCC | 2,509 | 1 | 2,509 | Andrew 2044 |
| Tests: CCi | 1,303 | 1 | 1,303 | Andrew 1289, Doug 16 |
| Tests: RR / RRR / RRi | 2,651 | 1 | 2,651 | Andrew 1391, Mike S. 651, Michael B. 216, Aidan 39, Doug 2 |
| Tests: ZZ / QQ / ZZp / GF | 3,072 | 1 | 3,072 | Andrew 1039, Doug 1026, Anton 316, Dave 71 |
| Tests: matrices | 2,742 | 1 | 2,742 | Andrew 1113, Doug 899, Aidan 507 |
| Tests: monomials | 1,749 | 1 | 1,749 | Andrew 1667, Doug 29 |
| Tests: NCGroebner | 1,421 | 1 | 1,421 | Andrew 494 |
| Tests: other engine | 1,838 | 1 | 1,838 | Andrew 823 |
| set_from_* → set rename (5 commits) | 768 | 1.5 | 1,152 | Doug 768 |
| **Total** | 22,414 | | 24,617 | |

The rename unit covers commits `fcb62382b6`, `dfaf786bee`, `2d5678944a`, `253a427eb4` and `bfc0866d87`. The first four are MichaelABurr/M2#63 ("Rename set_from_* methods to set"). `2d5678944a` is the fallout of the rename: once `set_from_long` became `set(long)`, `set(result, 1)` on GF(2^m) picked the element overload, so element copies were switched to `copy()`. `bfc0866d87` is a later one-line test fix.

## How this was built

- The diff is `git diff` from the merge-base with `development` (`ea715ee2fc`) to the PR head (`9edb15912a`, after #4733, MichaelABurr/M2#98 and `development` were merged in): 146 files, +16,859/−5,555.
- The rename commits' lines (768) are subtracted from the file units they touch, so the file units' line counts are approximate where later commits edited the same lines.
- Jingyi is treated as a co-author of the CC, CCC and CCi tests: Andrew's commits `b064509e41` and `52e3e20f14` say they were "built alongside Jingyi", even though blame only shows Andrew.
- Authorship comes from `git blame -w` on the surviving added lines. For code that was deleted outright, it comes from the commit author (Aidan for the monomial-ordering removal, Doug for the `aring-translate.hpp` rewrite).
- Anyone with 10 or more lines in a unit is kept off it. Smaller touches don't exclude anyone; the checklists name them so the other reviewer on that unit checks them.
- The load is (added + deleted lines) × a difficulty factor, shown in the × column: 1 for tests; 1.5 for build files, the rename, the backend and matrix-code leftovers, the test harness and the monomial-ordering move (mostly relocated code); 3 for the conversion-dispatch rewrite and the interval rings.
- The test docs (`AGENTS.md`, `STYLE.md`, `README.anton-dima`) are not assigned; they were frozen.
- The assignment started from a small search that minimizes load above the target, subject to the exclusion rule, and was then adjusted by hand.
