# PR #4441 — draft review comments, unit "Tests: NCGroebner"

Reviewed against `9edb15912a`. Rules: `e/unit-tests/STYLE.md` (§ numbers below), `e/unit-tests/AGENTS.md`.
The file was changed by one commit, `75d5e785ce` (Andrew, "Apply unit-test style guide to older engine tests", GPT-6).

---

**`e/unit-tests/NCGroebnerTest.cpp:649`** (and `:878`, issue #4701)

When `SuffixTree.DISABLED_suffixtree1` is enabled on Linux (gcc 13, autotools), it does not fail an assertion. It segfaults in the first `insert` (`:671`), inside `SuffixTree::extendedLocus` (`SuffixTree.cpp:364`). Under valgrind both disabled suffix-tree tests show invalid reads from the very first insertions: `SuffixTreeNode`'s constructor, called from `SuffixTree::splitArc` (`SuffixTree.cpp:310`), reads an arc label from a block that has already been freed along with a `std::map` entry.

So the symptoms #4701 reports from macOS (missing overlaps, null suffix links) are most likely reads of freed memory, not logic errors in overlap detection. The expected values in the tests may well be right. Suggested changes:
- Amend #4701 (or file a new issue and link it) to say this is a use-after-free in `SuffixTree::insert`/`splitArc`, with the valgrind trace.
- Fix the comments above both tests to match. §11 asks the comment to explain the bug; "loses the baba self-overlap" describes a symptom that varies by platform.

`SuffixTree` isn't used in production (`NCGroebner.hpp:24` and `NCF4.hpp:105` use `WordTable`; the `SuffixTree` members are commented out), so this isn't urgent. But a disabled test that segfaults is not the documented defect that §11 describes.

---

**`e/unit-tests/NCGroebnerTest.cpp:699`** (also `:582`, reached from `:888`)

`ASSERT_EQ` in the first scope returns from the whole test. So the later blocks of both suffix-tree tests (596 overlaps, superwords, left overlaps, `subword`) have never been run. #4701 says so itself ("those later paths are not validated"). That's fine while the tests are disabled, but the comment's "Re-enable when … the later pattern-set assertions pass" can't be checked by anyone today. Use `EXPECT_EQ` here (§5: `ASSERT_*` only when later steps cannot continue; they can).

---

**`e/unit-tests/NCGroebnerTest.cpp:887–889`**

clang-format reflowed the Emacs local-variables footer into

```
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests
// runNCGroebnerTest  " indent-tabs-mode: nil End:
```

The compile-command string is split across two lines and `End:` is no longer on its own line, so Emacs won't parse it. Restore the three original lines (or wrap them in `// clang-format off`/`on`). This is the only file in the PR with the problem.

---

**`e/unit-tests/NCGroebnerTest.cpp:121`** (opener at `:104`)

The opener says "Allocation growth and shrinking must preserve every still-live stored value", but the values are written only *after* `shrinkLastAllocate` (`:125`). So the test can't detect shrinking losing or moving data. That is the contract the two production callers rely on: `f4/f4-spairs.cpp:56` writes the lcm into a `max_monomial_size()` array and then shrinks it, and `gb-f4/MonomialView.cpp:72` fills its result, shrinks, and then writes through `rng.first`. Both need the address to stay put and the prefix to survive.

`shrinkLastAllocate` frees the top array and allocates it again. If the address changes, it only prints `"ooops: location changed"` (`MemoryBlock.hpp`), and the test doesn't check the address either. Suggested change: fill the whole array before shrinking, then `ASSERT_EQ` that `range.first` is unchanged and `EXPECT_EQ` the first four values. The growth half of the opener is checked by the read-back loop and is fine (§13).

---

**`e/unit-tests/NCGroebnerTest.cpp:138`**

`NCReduction.tryit` used to call `tryOutMathicCode()`. It now runs `checkPolynomialHeap` on `NaiveGeobucket` and `NaiveDedupGeobucket`, which `NaivePolynomialHeap` (`:164`) and `NaiveDedupPolynomialHeap` (`:157`) already do. So:
- `tryit` duplicates two other tests, and its new opener ("Both queue-backed reduction heaps…") describes them, not the name;
- `tryOutMathicCode` (`NCAlgebras/NCReduction.cpp:1089`, declared in `NCGroebner.hpp:17`) now has no callers anywhere.

Drop `tryit`. Either delete `tryOutMathicCode` or keep a test that calls it, but don't leave it as dead code because of a test change.

---

**`e/unit-tests/NCGroebnerTest.cpp:175`** (issue #4700)

The disabled test does fail as described (it prints `yzxz-yzxz-2yzy^2…`). But the retrofit deleted Mike and Frank's note from the old heap tests: "strangely enough, this doesn't give an error unless you use *all* the terms; stopping F early will not cause the error." That is the most useful clue for whoever fixes #4700. It belongs in the comment above the disabled test (§6, "do not … relocate existing commentary without cause"), or in the issue.

---

**`e/unit-tests/NCGroebnerTest.cpp:369`**

The old comment was a question: "TODO: did the offset change on our recent heft changes?" It became a statement: "One heft word and one length word precede the three-letter leading word." The test passes, so the offsets are what they are, but §13 asks for comments to be checked, and the commit doesn't say who checked that layout. Mike should confirm it. Also, these three `EXPECT_TRUE(… == … && … == …)` checks report nothing useful when they fail. `EXPECT_EQ` on the two offsets would show the values (§5).

---

**`e/unit-tests/NCGroebnerTest.cpp:397`** — minor

`NCGroebner.sorting` used to print the iteration order of a `std::map<std::pair<int,bool>,int>`, with "TODO: Make this into a test". It now asserts the iteration order of `OverlapTable::overlapMap()`, which is that same `std::map` type (`OverlapTable.hpp:18`). So it tests `std::map`'s ordering of `std::pair`, not anything in the engine. Either delete the test, or test something `OverlapTable` decides, such as what `nextDegreeOverlaps` returns when a degree has both generator and non-generator overlaps.

---

**`e/unit-tests/NCGroebnerTest.cpp:336`, `:350–352`, `:380`** — minor

Several element comparisons are still `EXPECT_TRUE(f == h)` / `EXPECT_TRUE(A->compare_elems(…) == GT)`. Elsewhere in the file the retrofit switched to `EXPECT_EQ`, which prints both polynomials on failure (§5). The same applies here.

---

### For Doug / Mike only (not PR comments)

- **Suffix-tree expected values.** Mike wrote the suffix tree and is the other reviewer here. Once the use-after-free is understood, he is the one to say whether the expected values (596 overlaps, the superword and left-overlap lists) are still what the code should produce, and whether experimental code that nobody calls should have disabled tests and an issue at all, rather than staying commented out as he left it ("temporarily … working on speeding up the suffix tree code").
- **#4701 provenance.** The issue was written from a macOS run. Its description of what is wrong comes from that run; Linux behaves differently. Worth a comment on the issue before anyone tries to fix the "missing overlaps".
- **`MonomialOrdering.create`** (`:182`) lives in this file but tests only `MonomialOrderings`, which now has its own `MonomialOrderingTest.cpp`. Leave it alone in this PR, but it's a candidate to move.

### What complies well

- The four copies of the heap test were folded into `checkPolynomialHeap` / `checkOverlappingHeapTerms`. They now assert intermediate values after each `removeLeadTerm` instead of printing them, and the debug output is gone.
- The `MemoryBlock` test now reads every allocation back at the end, so it checks that later allocations, including ones in new arena chunks, don't overwrite earlier ones. The old version only checked the range sizes, and accepted either size for any allocation. (Shrinking is not covered; see `:121` above.)
- `prefix_suffix` became a case table. The `FreeAlgebra.polyarithmetic` scopes each set up their own inputs instead of reusing `f`, `g`, `h` through `setZero`.
- `FreeAlgebraQuotient` was a leaked `new`; it's now on the stack, and `GB` is declared first, so it outlives the quotient. The suffix trees are `unique_ptr`s.
- The `#if 0` blocks and the unused `operator<<` overloads are gone. The `subwords`/`subword` checks that had been commented out are restored inside the disabled test.
- `WordTable.skylanin` and `SuffixTree.suffixtree2` now share one template, as the old "this test is identical to the one above" comment wanted.
- Both linked issues (#4700, #4701) exist and are open, and their reproductions match the tests.

### Verification

- Build: Autotools (`--with-system-libs`, gcc 13, Ubuntu 24.04) in a scratch tree, `make -C Macaulay2/e/unit-tests testMain`.
- The 19 enabled tests in this file pass, also with `--gtest_shuffle --gtest_random_seed=4441`. Full suite: 883 run, 879 passed, 4 skipped, 26 disabled.
- `--gtest_also_run_disabled_tests`: `DISABLED_deduplicatingHeapCancellation` and `DISABLED_suffixtree2` fail on their assertions; `DISABLED_suffixtree1` segfaults (exit 139). Both suffix-tree tests are run under valgrind as above.
- Not done: CMake, coverage, and finding the exact free site in `SuffixTree.cpp`.
