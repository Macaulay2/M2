# PR #4441 — draft review comments, unit "Tests: CC / CCC" (second reviewer)

Reviewed against `9edb15912a`. Rules: `e/unit-tests/STYLE.md` (§ numbers below).
These are the points that [the first review](PR4441-CC-CCC-review-claude.md) doesn't already make. I re-checked its points 1–7 against the same commit and agree with all of them.

---

**`e/unit-tests/ARingCCTest.cpp:56`, `ARingCCCTest.cpp:51`, `RingCCCTest.cpp:21`**

The three files use three different tolerance models:
- `ARingCC::near`: relative error 2⁻⁴⁷ of the larger modulus, never scaled below 1.
- `ARingCCC::near`: absolute error 2^(6 − precision) on each component separately.
- `RingCCCTest.cpp` `almostEqual`: absolute error 2^(−nbits), with `nbits` chosen per test (98 for add/subtract).

All three are fine for operands in [0,1]², which is all that `C.random` produces (first review, point 2). But the two absolute tolerances will fail spuriously once the random inputs are widened to include large values, as the first review suggests. If point 2 is fixed, the CCC tolerances should become relative as well. Either way, §7 asks each tolerance to explain its allowance; one sentence per helper saying why it is absolute or relative would do.

---

**`e/unit-tests/ARingCCTest.cpp:871–878`** (also `ARingCCCTest.cpp`, same block) — minor

Commutativity of addition and multiplication is checked with `near()`. Addition commutes exactly. Multiplication does too as written: `mult` computes `(ac − bd, ad + bc)`, and swapping the operands rounds the same two products (`aring-CC.hpp:257`, and the mpfr version in `aring-CCC.hpp:288`). For CCC, `hasValue`-style exact equality would be the stronger check (§7: "check exact answers with exact comparisons when they are representable"). For CC, gcc may contract `ad + bc` into an FMA (it does by default on arm64), which makes the swapped product round differently. So the tolerance is defensible there, but only if a comment says so. Associativity and distributivity do need the tolerance.

---

**`e/unit-tests/ARingCCTest.cpp:584–596`** (also `ARingCCCTest.cpp:651–663`)

`power_mpz` with exponent ±2¹⁰⁰ is expected to throw `engine_error`, even for base `i`, whose powers are exactly ±1 or ±i. That is a reasonable way to document the supported range, but the comment ("Both signs of 2^100 exceed the supported integer exponent range") should say it is a limitation of the implementation, not of the mathematics, so that nobody reads it as the intended contract (§12).

Not this PR: at top level the same power gives a wrong answer instead of an error. `ii^(2^100)` prints `.837706+.546122*ii`, and `(ii_(CC_100))^(2^100)` prints `.99423-.10727*ii`, while `ii^(2^31)` correctly prints `1`. I found no existing issue (searched `ii^`, "complex power", "power large exponent CC").

---

**`e/unit-tests/RingCCCTest.cpp:93`, `:116`, `:134`** (also `ARingCCTest.cpp:859`, `ARingCCCTest.cpp:910`) — minor

The trace label repeats the seed by hand: `seedRingRandom(); SCOPED_TRACE("seed 0x52494e47");`. The value is the default argument in `RingTest.hpp:53`, so if the default ever changes, every trace will report the wrong seed. The same pattern occurs 22 times in the PR. A helper that seeds the generator and adds the trace itself would remove the duplication. This is shared-harness work, so it belongs in a follow-up, not here.

---

### For Doug / Anton only (not PR comments)

- The first review's point 6 (deleted comments) matches what I found. On the 0^0 assertion: top level agrees, since `0_CC^0` and `0_(CC_100)^0` both give `1`. So the comment only needs to say that this is intended.
- Whether to file the top-level `ii^(2^100)` result is a separate decision; it's outside this PR.

### Verification

- Build: Autotools (`--with-system-libs`, gcc 13, Ubuntu 24.04) in a scratch tree. This is the build system the first review didn't run.
- `--gtest_filter='ARingCC.*:ARingCCC.*:RingCCC.*'`: 34/34 pass, the same count as the CMake run.
- Top-level results are from `M2` 1.26.06-100-gb4b4c163a1 (development).
- These files are identical at `d66c5054a3` and `9edb15912a`.
