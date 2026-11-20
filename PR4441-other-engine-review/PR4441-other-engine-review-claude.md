# PR #4441 — draft review comments, unit "Tests: other engine"

Reviewed against `9edb15912a`. Rules: `e/unit-tests/STYLE.md` (§ numbers below), `e/unit-tests/AGENTS.md`.
Files: `BRP-test.cpp`, `BasicPolyListParserTest.cpp`, `LatticePointsTest.cpp`, `NewF4Test.cpp`, `OverflowTest.cpp`, `PointArray.cpp`, `PolyRingTest.cpp`, `QuotientRingTest.cpp`, `ResTest.cpp`, `RingTowerTest.cpp`, `SubsetTest.cpp`, `WeylAlgebraTest.cpp`, `basics-test.cpp`.
Almost all of the change is `75d5e785ce` (Andrew, retrofit, GPT-6). `b913c4636b` removed a skip-only placeholder from `ResTest.cpp`, and `526fc7411a` touched `OverflowTest.cpp`.

---

**`e/unit-tests/NewF4Test.cpp:162`** (end of file)

The retrofit deleted about 45 lines of Mike's design notes on the stream/collector API: the TODOs for a variable-name accessor, `readMatrix`, `polynomialListFromStream` and a monomial sort, the sketch of `MatrixStream`, `BasicPolyListStream` and `PolynomialListStream`, and "What tests to make here?". Some of it has been done since: `Monoid::variableNames()` exists, and `PolyRingTest.cpp` now uses it. But there's still no `readMatrix` and no `polynomialListFromStream`. §6 says "do not … relocate existing commentary without cause", and §13 treats such comments as hypotheses to check, not to drop. Restore the block, or move it to an issue or to `fromStream.hpp`, but don't just delete it.

Smaller cases of the same thing:
- `PolyRingTest.cpp:42`: "Plan: this should be a simple constructor call!" and the remark about how hard the monomial order is to build. Both are still true of `create1`.
- `RingTowerTest.cpp`: the note on the planned string format for tower polynomials (variables `a..zA..Z`, then `[1]`, `[2]`, …).
- `LatticePointsTest.cpp:170`: the old comment said the root-count bug is in `box_enum.h`, that it was reported to the upstream author, and gave the one-line fix. `cytools/README.md` says `box_enum` is vendored from natemacfadden/latticepts. Neither the new comment nor #4702 mentions that, so whoever picks up #4702 may patch the vendored copy without knowing about the upstream report. Add one line to the issue and to the comment.

---

**`e/unit-tests/basics-test.cpp:12`**

`Nothing.ideal` was a placeholder (`EXPECT_EQ(true, testfcn())`). The retrofit kept the name and put a new buffer check inside: "Appending an empty string leaves the accumulated text unchanged." The test is now named for neither buffers nor anything else it does. §4 says to keep existing names and also that "the test name must identify the behavior described by its opening comment", and here the two rules conflict. Here deletion is better: `Buffer.make1`/`make2` already cover `buffer`. If the empty-append case matters, add it to `Buffer.make2`.

---

**`e/unit-tests/ResTest.cpp:164`, `:170`, `:176`, `:186`**

These four names were empty placeholders (`TEST(ResMonoidDense, encode5) {}`, …), apparently copied from `SubsetTest.cpp`, where `encode5`, `outOfRange` and `encodeBoundary` mean something else. The retrofit filled them with new tests that fit the names as well as it could: "encode5" became "5 variables", and "outOfRange" became "component 2^28 fails `check_monomial`". The new tests are reasonable, but the names come from another file. These are new tests, so per §4 they should get new camelCase names (e.g. `firstAndLastVariableRoundTrip`, `componentGuardBit`). A fifth placeholder, `concatenateResMonoidDense`, was removed in `b913c4636b`, which is right.

---

**`e/unit-tests/ResTest.cpp:83`** — minor

`checkMultiplication` chooses its size check by testing whether `identitySize == 6`, as a stand-in for "is this the dense monoid". Pass a flag or a size rule from the two call sites, or put the dense and sparse size checks in `ResMonoidDense.mult` / `ResMonoidSparse.mult` themselves.

---

**`e/unit-tests/SubsetTest.cpp:60`, `:94`** — minor (§17)

The retrofit added a `SCOPED_TRACE` inside every loop. In `encode4` and `encode6` that means building a trace message 116,280 times. In the scratch build they take 103 ms and 80 ms, about a tenth of the whole 1.8 s suite. I didn't time them without the traces. Stream the index into the failing `EXPECT` instead (`EXPECT_EQ(i, j) << "index " << i;`), which costs nothing when the check passes.

---

**`e/unit-tests/PolyRingTest.cpp:42`, `:19`/`:30`** — minor

`create1` now says "Equal degrees select the standard graded reverse lexicographic order", but nothing checks the ordering. `createDegreesRing` and `createDegreesRing2` have the same opener word for word, and so do `create1` and `createSimple`. §4: the opener must describe what that test checks.

---

**`e/unit-tests/WeylAlgebraTest.cpp:169`** — minor

clang-format wrapped the arithmetic in the comment, leaving `// 840` alone on the next line. Shorten the comment so it fits on one line.

---

**`e/unit-tests/LatticePointsTest.cpp:195`** — minor

`std::abs(int)` is used, but `<cstdlib>` isn't included. It compiles today only through other headers.

---

### For Doug / Mike only (not PR comments)

- **Mike wrote most of what was deleted** (the `NewF4Test.cpp` notes, the `PolyRingTest.cpp` and `RingTowerTest.cpp` remarks, the `box_enum` comment). He's the other reviewer on this unit, so he can say which of them he still wants.
- **The `Subsets.concatenateSubsets` loop was a test bug, and it's now fixed.** The old inner loop decoded 35 indices into 2-element subsets of a 7-set, which has only 21, and it hit `break` on the first zero, so it skipped the rest of each row. The new loop runs `j < 21` and uses `continue`. That's a real improvement, so say so in the PR rather than leave it buried in a style commit.
- **`OverflowTest`** no longer needs `volatile`. The old `INT32_MIN - x + x` was itself signed-overflow UB in the test setup, and `overflow.hpp` does its arithmetic unsigned, so literal arguments are safe; the new boundary tests use plain literals. The five old tests still use `volatile` locals. That's harmless, but it's inconsistent within one file.

### What complies well

- Every print-only test now asserts something:
  - `NewF4.matrixstream` compares each coefficient and monomial against the parsed input;
  - `GroebnerBasis.simple` checks the three-element reduced basis;
  - `RingTower.elems` checks (a+b+2)² against its expansion;
  - `Subsets.encodeBoundary` checks the removed element;
  - `ResMonoid*` dropped `dump()` for real round-trip checks.
- `BRP.Divisibility` had been testing the C++ `^` operator (`EXPECT_EQ(14 ^ 8, 6)`); it now calls `isDivisibleBy`.
- The parser tests that only checked "doesn't throw" or the polynomial count now check the terms. `expectTerm` gained bounds checks before it indexes the monomial array.
- `OverflowTest` gained real boundary and packed-lane coverage, including "a failed `add_to` leaves the destination unchanged".
- The `LatticePoints` TODO became a disabled test linked to #4702, and it does fail when enabled.
- `Util.m2strings_basic` was under `#if 0`; it's back and passes.
- `Util.m2array2stdvec_big` now states the truncation the code performs (`static_cast<int>` in `util.hpp:57`) instead of asserting only that the round trip fails.

### Verification

- Build: Autotools (`--with-system-libs`, gcc 13, Ubuntu 24.04) in a scratch tree, `make -C Macaulay2/e/unit-tests testMain`.
- The 124 enabled tests in these 13 files pass, also with `--gtest_shuffle --gtest_random_seed=4441`. Full suite: 883 run, 879 passed, 4 skipped, 26 disabled.
- `LatticePoints.DISABLED_infeasibleRootNodeCount` fails when enabled (`n_nodes` is 0).
- Not done: CMake and coverage. The authorship of the deleted comments comes from `git blame` at `ea715ee2fc`.
