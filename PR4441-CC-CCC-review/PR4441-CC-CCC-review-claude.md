# PR #4441 — draft review comments, unit "Tests: CC / CCC"

Reviewed against `9edb15912a`. Rules: `e/unit-tests/STYLE.md` (§ numbers below), `e/unit-tests/AGENTS.md`.

---

**`e/unit-tests/ARingCCCTest.cpp:19`** (also `ARingCCTest.cpp:25`, and `ARingCCiTest.cpp`)

These two fixtures and their tests are near copies: with the class names swapped, the files differ in about 165 of about 1,000 lines. They are already drifting. CC checks nonfinite values inside `ApproximationTolerance`, while CCC has a separate `comparisonHelpersRejectNaN`. CC also has an `eval: existing error` case that CCC lacks.

STYLE §8–9 asks for a shared contract in this situation: one `TYPED_TEST` over `ARingCC`/`ARingCCC` (and CCi where the contract holds), with a traits struct for construction, `hasValue`/`near`/`describe`, and the precisions. `ARingZZpTest.cpp` is the model. Type-specific cases (CCC's real/imaginary-part setters, the CC-only `eval` error branch) can stay as plain `TEST_F`s.

---

**`e/unit-tests/ARingCCTest.cpp:864`** (also `ARingCCCTest.cpp:915`)

`C.random` only yields values in [0,1]² (`randomMpfr`, `interface/random.cpp:240`). So the randomized properties never see a negative or large operand, and §7 says "mixed signs expose sign errors". The code this replaced used `ARingElementGenerator`, which mixed in the integers −25..24.

The rewrite also dropped two random checks:
- the power law a^e1·a^e2 = a^(e1+e2) (old `power_and_invert`);
- the random (−a)+a = 0 check (old `negate`).

`Powers` now checks only exact powers of i and (3+2i)^3.

Suggested fix: draw operands from a signed, wider range (e.g. scale and shift `random`, or restore the generator), and put the power law back into `RandomizedProperties`.

---

**`e/unit-tests/ARingCCCTest.cpp:23`**

The fixture is 100-bit, but every worked example uses double-exact inputs and answers. A CCC that silently worked in double precision would pass all of them. Please add at least one known-answer case that needs more than 53 bits, e.g. 1 + 2^-80 ≠ 1, or 3·(1/3) within 2^(6−prec) of 1 after `set(mpq 1/3)`.

---

**`e/unit-tests/ARingCCTest.cpp:427`** (repeated at 439, 451 and 463, and in the same four places in `ARingCCCTest.cpp`)

"Reset both operands before storing the answer over an input." appears four times verbatim, and it describes the next three lines. More generally, almost every scope carries a two-sentence "Do X. Check Y." comment that restates the code. For example, line 377: "Matching numbers should compare as equal. The result must be zero."

§6: "A comment earns its place by saying what the code cannot … Never restate the next line." Please trim to the comments that give a reason (a choice of input, a precondition, a rounding allowance). Also make the test-level openers say what a failure would mean.

---

**`e/unit-tests/RingCCCTest.cpp:253`**

`ARingCCC::syzygy` documents "no need to consider the case a==0 or b==0". `b913c4636b` removed `zeroInputSyzygy` from the ARing tests for exactly that reason (§15: honor documented preconditions). This test still calls `syzygy(0, b)`, and the retrofit gave the call an explicit label ("zero first operand"). Either drop this case, or change the documented contract and say so.

---

**`e/unit-tests/RingCCCTest.cpp:59` and lines `200` and `234`**

The retrofit deleted three pieces of maintainer commentary that still carry information (§6 "do not … relocate existing commentary without cause"; §13):
- `// FIXME: not implemented: EXPECT_TRUE(R->is_CCC());`. This is still true: `ConcreteRing` does not override `is_CCC`.
- The `0^0 == 1 too?` TODO. It was replaced by `ringEquals(R, R->one(), R->power(a, 0))`, which now asserts 0^0 = 1 (the generator yields 0 at index 25) without saying so. That is fine as a decision, but it should be stated in a comment.
- Mike's NOTE asking whether `RingCCC::syzygy` should be removed.

Please restore the FIXME and the syzygy note, and say in a comment that the 0^0 = 1 assertion is intentional.

---

**`e/unit-tests/ARingCCTest.cpp:515`** (also lines 570, and `ARingCCCTest.cpp:582, 637`) — minor

The trace labels here are bare row names ("real divisor", "i: square"). §4 asks for "the operation and the input condition", as the reciprocal table does (`"reciprocal: " << sample.name`).

---

### For Anton/Doug only (not PR comments)

- **Test names.** All 7 original names in `ARingCC`/`ARingCCC` were replaced by 12 PascalCase names. That is compliant with §4 only because those names count as "existing": they date from `b064509e41`, one day before the camelCase rule. Decide whether that exemption should apply to names introduced in the same PR.
- **Commit messages.** `b064509e41` ("Codex") and `72a594e328` (nothing) don't name an exact model. They predate the AGENTS.md rule; the retrofit commits do name it ("GPT-6"). No AI mentions appear in the test files.
- **Rule edits.** The retrofit commits edited STYLE.md as they applied it. `b913c4636b` reversed the §10 skip guidance and the §15 zero-operand guidance, and `e5854ce500` created an unrequested `ARingTestNotes.md`, deleted three minutes later. Confirm these rule changes came from human review.

### What complies well

- Structure: an anonymous namespace, a fixture with RAII elements, and `AssertionResult` helpers that report the expected value, the actual value and the tolerance.
- Traces and randomness: a `SCOPED_TRACE` on every scope, and a seeded random test that traces each trial and its inputs. Randomized checks are kept separate from worked examples.
- Choice of checks: exact comparison wherever the answer is exact, aliasing and in-place cases, and `EXPECT_THROW` for the documented `power_mpz` overflow.
- Restraint: no skip placeholders or unlinked `DISABLED_` tests, and `invert`'s a≠0 precondition is honored.
- The CC `eval: existing error` case tests a real branch (`if (not error())` in `aring-CC.hpp`), not a coverage trick.
- `RingCCCTest.cpp` was retrofitted with low churn. It kept its test names and swapped the exact-`is_equal` subtract helper for a tolerant loop, which §15 sanctions.

### Verification

- Build: CMake (Ninja, RelWithDebInfo, gcov off) in `M2/BUILD/anton/builds.tmp/cmake`, target `M2-unit-tests`.
- `--gtest_filter='ARingCC.*:ARingCCC.*:RingCCC.*'`: 34/34 pass (13 `ARingCC`, 12 `ARingCCC`, 9 `RingCCC`). They also pass with `--gtest_shuffle` and seeds 1, 915 and 4441.
- Full suite: 879 passed, 26 disabled.
- Not done: the Autotools build, and a gcov/gcovr coverage run. So the claim that the randomized checks got weaker comes from reading the code (`randomMpfr` returns values in [0,1]; the old generator and checks were dropped); it is not measured coverage.
- The CC↔CCC overlap was measured with `diff <(sed 's/ARingCCC/ARingCC/g; s/ACCC_/ACC_/g' ARingCCCTest.cpp) ARingCCTest.cpp`, which gives 165 differing lines.
