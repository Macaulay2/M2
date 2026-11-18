# PR #4441 — Tests: other engine — Doug's review

Files: `e/unit-tests/BRP-test.cpp`, `BasicPolyListParserTest.cpp`, `LatticePointsTest.cpp`, `NewF4Test.cpp`, `OverflowTest.cpp`, `PointArray.cpp`, `PolyRingTest.cpp`, `QuotientRingTest.cpp`, `ResTest.cpp`, `RingTowerTest.cpp`, `SubsetTest.cpp`, `WeylAlgebraTest.cpp`, `basics-test.cpp`. With Mike S.
Details: `PR4441-other-engine-review-claude.md`.

## Points for the PR

1. **Deleted maintainer notes (§6, §13).**
   - Mike's ~45-line stream/collector design notes at the end of `NewF4Test.cpp`, mostly still unimplemented.
   - The smaller remarks in `PolyRingTest.cpp` and `RingTowerTest.cpp`.
   - The `box_enum` comment saying the bug is upstream (vendored code) and was reported there; #4702 doesn't mention it.
2. **`Nothing.ideal` (§4).** A placeholder name now holds an unrelated buffer check; delete it, since `Buffer.make1`/`make2` cover buffers.
3. **`ResTest` placeholder names (§4).** `encode5`, `encode6`, `outOfRange` and `encodeBoundary` were empty stubs copied from `SubsetTest`, and they've been filled with new tests to fit; give them names that describe what they test.
4. **`checkMultiplication` branches on `identitySize == 6` (minor).** It's a stand-in for dense vs sparse.
5. **`SCOPED_TRACE` in 116,280-iteration loops (§17, minor).** `Subsets.encode4`/`encode6` take ~180 ms, about a tenth of the suite; stream the index into the `EXPECT` instead.
6. **Openers that don't match their tests (minor).** `PolyRing.create1` claims a grevlex check it doesn't make; two pairs of tests share identical openers.
7. **Small fixes.** A `// 840` comment fragment left by clang-format (`WeylAlgebraTest.cpp:169`); `<cstdlib>` is missing for `std::abs` (`LatticePointsTest.cpp:195`).

## For Doug / Mike only

8. **Ask Mike** which of the deleted notes he still wants.
9. **`concatenateSubsets` had a real test bug, now fixed.** It decoded out-of-range indices and stopped each row at the first zero; worth saying in the PR.
10. `OverflowTest` uses `volatile` in its old tests but not in its new ones; harmless, but inconsistent.

## Good

11. Every print-only test now asserts something:
    - `matrixstream`, `GroebnerBasis.simple`, `RingTower.elems`, `encodeBoundary` and `ResMonoid*` all check their results;
    - `BRP.Divisibility` no longer tests the `^` operator;
    - the parser tests check terms, not just counts;
    - `OverflowTest` gained boundary and packed-lane cases;
    - the `#if 0` string test is back.

## Status

12. Built with Autotools on Ubuntu 24.04. The 124 enabled tests pass, also shuffled; the full suite has 879 passed, 4 skipped and 26 disabled. The one disabled test fails as intended. CMake and coverage were not run.

*Summary drafted by Claude Opus 5.5 from the full review; to be edited by Doug.*
