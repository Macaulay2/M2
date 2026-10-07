# PR #4441 — Tests: CC / CCC — Anton's review

Files: `e/unit-tests/ARingCCTest.cpp`, `ARingCCCTest.cpp`, `RingCCCTest.cpp`. With Doug.
Details: `PR4441-CC-CCC-review-claude.md`.

## Points for the PR

1. **Duplication (§8–9).** The CC and CCC test files are near copies (165 of about 1,000 lines differ) and are already drifting; CCi is a third copy. They should be one `TYPED_TEST` contract, as in `ARingZZpTest.cpp`.
2. **Weaker random checks (§7).** `C.random` gives only [0,1]², so no negative or large operands. The rewrite dropped the old ±integer generator, the random power law, and the random negate check.
3. **CCC precision untested.** All worked examples are double-exact; add a case that needs more than 53 bits.
4. **Comments restate the code (§6).** Formulaic two-sentence comments throughout, and "Reset both operands…" appears 4× verbatim per file. Openers don't say what a failure means.
5. **Syzygy precondition (§15).** `RingCCCTest.cpp:253` still tests `syzygy(0, b)`, though a==0 is excluded by contract and the ARing tests dropped that case for this reason.
6. **Deleted maintainer comments in `RingCCCTest.cpp` (§6, §13).**
   - The `is_CCC()` FIXME, which still applies.
   - Mike's note questioning `syzygy`.
   - The 0^0 TODO, which is now silently asserted as 1.
7. **Trace labels (§4, minor).** The division and power tables don't name the operation.

## For Anton / Doug only

8. **Test names (§4).** The 7 original names were replaced by 12 PascalCase names. They were exempt as "existing" only because they were created a day before the camelCase rule, in this same PR.
9. **Commit messages (AGENTS.md).** `b064509e41` and `72a594e328` don't name an exact model (they predate the rule). The retrofit commits do ("GPT-6"), and there are no AI mentions in the files.
10. **Rule edits mid-retrofit.** STYLE.md was changed by the retrofit commits: §10 and §15 were reversed in `b913c4636b`, and an unrequested `ARingTestNotes.md` was added, then deleted. Were these requested by human review?

## Good

11. The mechanics follow the rules:
    - fixture with RAII elements and `AssertionResult` helpers;
    - `SCOPED_TRACE`s and seeded random checks;
    - exact comparisons where possible, aliasing cases, `EXPECT_THROW`;
    - no placeholder or unlinked disabled tests.

    `RingCCCTest.cpp` was retrofitted with low churn.

## Status

12. Built with CMake. The 34 CC/CCC tests pass, including shuffled runs; the full suite has 879 passed and 26 disabled. Autotools and coverage were not run.
