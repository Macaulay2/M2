# PR #4441 — Tests: CC / CCC — Anton's review

This is a human reaction to the one-line summaries of issues elaborated in [Claude's review](PR4441-CC-CCC-review-claude.md).

Files: `e/unit-tests/ARingCCTest.cpp`, `ARingCCCTest.cpp`, `RingCCCTest.cpp`. 
Details: `PR4441-CC-CCC-review-claude.md`.

## Points for the PR

1. **Duplication (§8–9).** The CC and CCC test files are near copies (165 of about 1,000 lines differ) and are already drifting; CCi is a third copy. They should be one `TYPED_TEST` contract, as in `ARingZZpTest.cpp`. 

This has to be fixed. Duplication of hundreds lines of test code is a maintenance burden.

2. **Weaker random checks (§7).** `C.random` gives only [0,1]², so no negative or large operands. The rewrite dropped the old ±integer generator, the random power law, and the random negate check.

This is likely a mishap: `C.random` behavior is documented but unexpected by many people.

3. **CCC precision untested.** All worked examples are double-exact; add a case that needs more than 53 bits.

Fair comment. Why not?

4. **Comments restate the code (§6).** Formulaic two-sentence comments throughout, and "Reset both operands…" appears 4× verbatim per file. Openers don't say what a failure means.

There is a passage on comments in STYLE.md, which asks them to be concise... but I guess doesn't ask for them to be rich in info. (Change comments? Update STYLE.md and then change comments?)

5. **Syzygy precondition (§15).** `RingCCCTest.cpp:253` still tests `syzygy(0, b)`, though a==0 is excluded by contract and the ARing tests dropped that case for this reason.

Not sure what is going on here... perhaps a response for the next item could clarify things. 

6. **Deleted maintainer comments in `RingCCCTest.cpp` (§6, §13).**
   - The `is_CCC()` FIXME, which still applies.
   - Mike's note questioning `syzygy`.
   - The 0^0 TODO, which is now silently asserted as 1.

I guess these were dropped too silently. We should do one of the following in each situation like this:
  - write a DISABLED test (and an issue referring to it) 
  - write a comment (in the source? in the commit message?) explaining why the old comment is no longer relevant
  - ??? (there are probably more ways)
7. **Trace labels (§4, minor).** The division and power tables don't name the operation.
  This is minor, but easy to fix.

