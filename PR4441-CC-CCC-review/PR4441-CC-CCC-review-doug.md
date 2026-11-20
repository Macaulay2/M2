# PR #4441 — Tests: CC / CCC — Doug's review

Files: `e/unit-tests/ARingCCTest.cpp`, `ARingCCCTest.cpp`, `RingCCCTest.cpp`. With Anton.
Details: `PR4441-CC-CCC-review-doug-claude.md`. Shared points: `PR4441-CC-CCC-review-anton.md`.

## Points for the PR

1. **Agree with Anton's points 1–7.** I re-checked them at `9edb15912a`.
2. **Three tolerance models (§7).** CC is relative, CCC and `RingCCCTest` are absolute. The absolute ones will break once Anton's point 2 widens the random inputs, so they should become relative, and each helper should say why it uses the tolerance it does.
3. **Exact identities checked with tolerance (minor, §7).** Commutativity in `RandomizedProperties` is exact for CCC; for CC it's exact unless gcc contracts to FMA. Use exact checks for CCC, and add a comment to CC.
4. **`power_mpz` ±2¹⁰⁰ throws even for `i` (§12).** Fine as documenting the range, but the comment should call it an implementation limit, not the contract.
5. **Hand-typed seed in traces (minor).** `SCOPED_TRACE("seed 0x52494e47")` repeats the default seed 22 times across the PR; that's for a follow-up helper, not this PR.

## For Doug / Anton only

6. At top level, `ii^(2^100)` is wrong (`.837706+.546122*ii`, also in `CC_100`), and no existing issue turned up. That's outside the PR; decide separately whether to file it.
7. The 0^0 = 1 assertion agrees with top level, so Anton's point 6 needs only a comment.

## Status

8. Built with Autotools on Ubuntu 24.04, which complements Anton's CMake run. The 34 CC/CCC tests pass.

*Summary drafted by Claude Opus 5.5 from the full review; to be edited by Doug.*
