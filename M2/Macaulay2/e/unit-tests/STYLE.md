# Writing unit tests for the Macaulay2 engine

Conventions for adding or revising engine unit tests in this directory.
For C++ formatting and include order see `Macaulay2/e/STYLE.txt`, which applies
here too.

Apply these rules to the tests you are changing. Do not reformat unrelated
tests to bring them into agreement with this guide.

## 1. Tests in this directory

This guide applies to `Macaulay2/e/unit-tests/`: C++ tests written with
GoogleTest and linked against `M2-engine`. Add or extend tests here to exercise
the engine component under test directly through its C++ interface.

Use the fixtures and helpers available in this directory for setup (see
section 16). If a behavior cannot be exercised here, document the coverage gap
and its cause. Keep test additions within this directory.

## 2. Adding a test file

The two build systems keep separate lists, and both must be updated:

1. put `<Component>Test.cpp` in `Macaulay2/e/unit-tests/`
2. add `unit-tests/<Component>Test.cpp` to the `M2-unit-tests` source list in
   `../CMakeLists.txt`
3. add `<Component>Test` (no extension) to `UNITTEST_CCFILES` in `Makefile.files`
   in this directory, preserving the continuation backslashes
4. build and run under both

Existing registered sources need no new entry for additional tests.

Forgetting one is the most common mistake here, and it is silent: the file still
compiles when you build it by hand, so nothing complains. Before the style
retrofit, `ARingGFTest.cpp` was commented out of both lists and went stale for
years without a failing build:

```
# in Macaulay2/e/CMakeLists.txt
#    unit-tests/ARingGFTest.cpp # TODO: needs rework - ARingGFFlint API ...

# in Macaulay2/e/unit-tests/Makefile.files
#    ARingGFTest \
```

Include the component header, GoogleTest, and the standard headers the test uses
directly. Put file-local fixtures and helpers in an anonymous namespace in new
code; most existing files predate this and need not be retrofitted.

## 3. Building and running

```sh
cmake --build <build-dir> --target M2-unit-tests
ctest --test-dir <build-dir> -R '<pattern>' --output-on-failure
```

CTest names carry the prefix `unit-tests:`. You can also run the binary
directly, which is quicker when iterating:

```sh
./Macaulay2/e/M2-unit-tests --gtest_filter='ARingZZp*'
```

For Autotools, run `make -k check` from the unit-tests directory in the
configured build tree. Verify both registrations when changing source lists: a
successful run in one build system does not validate the other.

Write commands relative to the build directory. Do not hardcode a personal one.

## 4. Structure of a test

Name tests in camelCase, and only for new tests — leave existing names alone.
Bundle related small cases under one name rather than writing a test per
assertion: real, imaginary and mixed reciprocals belong together under
`arithmetic`. Split a theme only when it becomes hard to follow.

Choosing the macro:

| | when |
|---|---|
| `TEST` | one-off checks, no shared setup |
| `TEST_F` | several cases share setup or assertion helpers |
| `TYPED_TEST` | several implementations share one contract (section 9) |

gtest builds a **fresh fixture for each `TEST_F`**, but not for each scope inside
it. Tests must run independently and in any order. Declare only commonly used
state in the fixture; keep case-specific values local.

Lay out setup, the operation, and the checks in that order, separated by blank
lines where it helps. Set each case's inputs explicitly; never rely on values
left behind by an earlier case. Use small scopes for cases with their own setup,
and name them with `SCOPED_TRACE`, including the operation and the input
condition — `"init set: source changes"` in `ARingCCCTest.cpp`. The test name
must identify the behavior described by its opening comment; scope and table
case names must identify the conditions described by their nearby comments.
Structure each case so its setup establishes those conditions and its assertions
check the claimed outcome.

### Layout example

From `ARingCCCTest.cpp`, using its `ARingCCC` fixture and `Ring` alias. The
existing name `Construction` is preserved here; new test names use camelCase.
The two scopes match the requested-precision and default-precision cases
explained in their comments.

```cpp
TEST_F(ARingCCC, Construction)
{
  // Check the ring name and the precision it reports. The default settings
  // and requested settings should agree with the values used to create the
  // ring.

  {
    // Try several requested precisions. Both the stored precision and the
    // printed name should reflect the request.
    SCOPED_TRACE("create: requested precision");
    for (unsigned long precision : {53UL, 100UL, 200UL})
      {
        SCOPED_TRACE(::testing::Message() << "precision " << precision);
        Ring ring(precision);

        EXPECT_EQ(ring.get_precision(), precision);
        EXPECT_EQ(ringName(ring), "ACCC_" + std::to_string(precision));
      }
  }

  {
    // Create the ring without settings. Check the defaults reported to
    // callers.
    SCOPED_TRACE("create: default precision");
    Ring defaultRing;

    EXPECT_EQ(defaultRing.get_precision(), 53);
    EXPECT_EQ(defaultRing.characteristic(), 0);
    EXPECT_EQ(ringName(defaultRing), "ACCC_53");
  }
}
```

For repeated steps with different inputs, define a local case struct with a
descriptive `name`, the inputs, and the expected output; in the loop set
`SCOPED_TRACE(sample.name)`, prepare fresh inputs, and check. Use separate tables
when the steps or the expected failure behavior differ; do not select unrelated
operations with a switch.

## 5. Assertions

Every test asserts something. A body that only prints is a crash check wearing a
costume — it passes whatever the answer is. Do not add calls with no useful
assertion, and do not write unreachable-state tricks to move coverage numbers.

Use `EXPECT_*` for independent checks so one run reports several problems. Use
`ASSERT_*` only when later steps cannot meaningfully continue. Beware that
`ASSERT_*` returns early: it will skip an `R.clear(a)` that follows it.

Check documented error paths with `EXPECT_THROW` and the documented exception
type. Prefer small helpers returning `testing::AssertionResult` over helpers
returning `bool`, and report expected and actual values — for approximate
comparisons, the error and the tolerance too.

## 6. Be brief

Tests are read far more often than written, and a test nobody can scan is a test
nobody maintains. Keep the code and the commentary short enough to take in at a
glance.

Start a test body with one or two sentences on what it checks and what a failure
would mean. Add similarly short notes before distinct scopes and case tables,
explaining why those inputs were chosen or what the group is meant to catch.
Explain table columns, unusual setup, and rounding allowances when they are not
obvious.

A comment earns its place by saying what the code cannot: a precondition, the
reason for a constant, the defect being pinned. Never restate the next line, and
never narrate every assertion. Prefer one sentence to three. If an explanation
genuinely needs a paragraph, put it in the commit message or PR description.

Never create Markdown notes, reports, summaries, or other documentation files
unless the user explicitly requests them. Keep explanations in relevant test
comments, commit messages, or PR descriptions; a request to change tests does
not authorize adding a separate Markdown file.

Do not reflow or relocate existing commentary without cause. Churn costs review
attention and hides the real change.

## 7. Choosing inputs

Start with small, deterministic inputs whose answers are known independently.
Choose values that reveal mistakes: distinct entries expose swapped positions,
empty and single-element inputs expose boundary handling, mixed signs expose
sign errors.

Check exact answers with exact comparisons when they are representable. Use a
justified, precision-aware tolerance for results affected by rounding; explain
the allowance, and do not widen it just to make a test pass. For interval
results, distinguish exact endpoints from containment — a valid interval may be
wider than the expected point, so pick the check that matches the contract.

Exercise in-place operations where supported (the answer replacing an input),
boundaries, conversions, and error cases. Do not assume related implementations
support the same operations.

Keep randomized property checks separate from worked examples. On failure, show
the trial and its inputs, and record a seed where the random API allows.
Random checks supplement known-answer cases; they do not replace them.

## 8. Templated helpers

Checks shared by several implementations of one concept belong in a templated
helper header, not copied into each test file. `ARingTest.hpp` holds the ones
every ARing must satisfy — `testAdd`, `testDivide`, `testAxioms` and the rest —
as function templates over the ring type. Eleven test files include it:
`ARingZZpTest.cpp` and `ARingGFTest.cpp` both call `testFiniteField`, the
aggregate for finite fields, while the ZZ, QQ, RR and CC tests take the subset
that applies to them.

Scope and architecture pull in different directions here, and both matter: do
not refactor shared helpers as a side effect of an unrelated task, but when
duplication is the thing you are addressing, lift it into a helper. The cost of
never doing so is not bulk, it is drift — the copies diverge, and the weaker one
quietly becomes the standard.

## 9. Several implementations of one contract

Once the checks live in a helper, run them over every implementation from one
list, with a traits struct supplying construction and limits.

These excerpts from `ARingZZpTest.cpp` show one factory specialization and the
fixture registration; the file also defines factories for FFPACK and Flint.

```cpp
template <typename RT>
struct ARingFactory;

template <>
struct ARingFactory<M2::ARingZZp>
{
  static const char* name() { return "ARingZZp"; }
  // two newarray_atomic(int, p) tables and an O(p^2) primitive-root search
  static const char* limit() { return "table size; p <= 32749"; }
  static bool supports(unsigned long p) { return p <= 32749; }
  static std::unique_ptr<M2::ARingZZp> make(unsigned long p)
  {
    return std::unique_ptr<M2::ARingZZp>(new M2::ARingZZp(p));
  }
};
```

```cpp
template <typename RT>
class ZZpRing : public ::testing::Test
{
};

typedef ::testing::
    Types<M2::ARingZZp, M2::ARingZZpFFPACK, M2::ARingZZpFlint>
        ZZpTypes;
TYPED_TEST_SUITE(ZZpRing, ZZpTypes);
```

`supports()` becomes the single declared place each implementation's limits
live, instead of being scattered across call sites or implied by absence. Use
`SCOPED_TRACE` inside the loop so a failure names the case it came from.

`ARingZZpTest.cpp` does this over three ZZ/p classes and twelve moduli.

## 10. Declare exclusions; never omit them

If a case does not apply, say so in code, with a reason, at runtime —
`GTEST_SKIP()`, or a printed line from the `supports()` predicate. A commented
out call reports nothing and rots unnoticed. Before the style retrofit,
`ARingZZTest.cpp` dropped two checks this way, invisible in test output:

```cpp
  testDivide(R, ntrials);
  //  testReciprocal(R, ntrials); // this test is not applicable, as this is not
  //  a field
  //  testPower(R, ntrials);  // this test can't work, as it expects a finite
  //  field
  testAxioms(R, ntrials);
```

The reasons given are sound; the problem is that only a reader of that file will
ever learn them.

## 11. Recording a known defect

Prefix the test `DISABLED_`. In a comment block immediately above the test or
at the start of its body, explain the bug, why the test is disabled, and what
would allow it to be re-enabled. That same block must include a direct URL to
the specific posted issue in the
[Macaulay2/M2 issue tracker](https://github.com/Macaulay2/M2/issues).
A link to the tracker alone or an unposted issue placeholder is insufficient.
Before filing a report, search both open and closed issues for the component,
operation, and observed failure, and read plausible matches. Link the existing
issue when it describes the same defect; file a new report only when that
pre-check finds no match. Do this before adding the disabled test.
This commentary is allowed to be longer than usual.

Then run it and confirm it fails:

```sh
./Macaulay2/e/M2-unit-tests --gtest_also_run_disabled_tests
```

A `DISABLED_` test that passes documents nothing, and is worse than no test
because it looks like coverage. This is not hypothetical — one such test passed,
and chasing why is what found the actual bug.

## 12. Pin the relationship, not the wrong value

When pinning behavior you believe is wrong, do not assert the bad constant:
whoever fixes the bug then has to edit the test, which invites editing it
wrongly. Assert the property that exposes it.

From `ARingZZpTest.cpp`, in
`ARingZZpFFPACK.advertisedMaxModulusIsBelowTheRealOne`:

```cpp
  EXPECT_LT(static_cast<double>(M2::ARingZZpFFPACK::getMaxModulus()),
            static_cast<double>(M2::ARingZZpFFPACK::FieldType::maxCardinality()));
```

This characterizes the discrepancy without hardcoding the advertised value.
For a disabled regression test, assert the intended corrected behavior so that
fixing the linked bug makes the test pass (section 11).

## 13. Comments are hypotheses

Verify them against the code before believing them, especially "this fails" and
"commented out because". Several such comments in this directory described tests
that were not commented out and did pass.

One of them blamed a ring routine for failing above 2^63; the routine was
correct, and the fault was in the test helper. Had it been believed, the fix
would have gone in the wrong file. Find the root cause first.

## 14. Coverage

Aim for strong line, function and branch coverage of the code under test, and
use gcovr results to find behavior nobody exercises. Report remaining gaps and
their causes rather than hiding them; never add production changes purely to
move the number.

Coverage and test count move independently. One recent change cut the count from
283 to 268 while coverage rose, by collapsing duplicated tests into a typed
matrix that covered more. The count is not the metric.

Build with `-DGCOV=ON`, then:

```sh
cmake --build <build-dir> --target coverage-reset   # clear counters
./Macaulay2/e/M2-unit-tests
cmake --build <build-dir> --target coverage-report  # or run gcovr directly
```

Point gcovr at `Macaulay2/e/CMakeFiles`, not at the engine object directory
alone. Most of an ARing class is inline code in the `.hpp`, which gcov
attributes to whichever translation unit instantiated it, including the test
TUs. Narrow the search and the headers report 0% on an implausibly small line
count, which looks like a result and is not.

## 15. Ring elements

- Pair `init` with `clear`. Some element types are plain data; others own
  resources. Prefer objects that clean up after themselves.
- `set(c, a)` is **not** a copy. Where `ElementType` is an integral type it binds
  to the integer-coercion overload and reinterprets the element's value. Use
  `init_set`; `copy()` is not available on every class.
- Do not hold a characteristic or cardinality in `long` or `int`. The types
  differ per class and the values reach 2^64.
- Exponent types differ too (`int`, `int32_t`, `long`). Large exponents go
  through `power_mpz`.
- Honor documented preconditions even when nothing enforces them, and make
  them explicit in case names and structure. `ARingZZp::subtract_multiple`
  says "we assume: a, b are NONZERO!!" and has no assert; a zero argument
  silently returns a wrong answer. Name the valid case to identify the nonzero
  operands, explain the precondition in its nearby comment, and establish it
  in setup or guard the operation as `ARingZZpTest.cpp` does below. Put any
  regression for zero operands in a separately named test with its own bug
  explanation and issue link if disabled (section 11).
- Exact vs approximate: RR and CC cannot use exact `is_equal`, and define their
  own tolerant helpers rather than reusing the shared ones.

The guarded case in `ZZpRing.elementOperations` (`ARingZZpTest.cpp`):

```cpp
      if (!R.is_zero(a) && !R.is_zero(b))
        {
          R.set(c, 7);
          R.subtract_multiple(c, a, b);
          R.set(e, 7);
          R.mult(d, a, b);
          R.subtract(e, e, d);
          EXPECT_TRUE(R.is_equal(c, e));
        }
```

## 16. Reuse what is here

| | |
|---|---|
| `ARingTest.hpp`, `RingTest.hpp`, `DMatTest.hpp` | shared templated checks |
| `util-polyring-creation.hpp` | `simplePolynomialRing`, `simpleWeylAlgebra`, `simpleQuotientRing`, `idealFromStrings` |
| `RingElem.hpp` | value-semantics element wrapper; prefer it to raw `RingElement*`, its header says why |
| `util.hpp` | `stdvector_to_M2_arrayint` |

For private or protected members, use the friend accessor pattern: declare
`friend class WeylAlgebraTestAccessor` in the engine header, and define the
accessor with static forwarders in the test. This excerpt from
`WeylAlgebraTest.cpp` omits the other forwarders:

```cpp
class WeylAlgebraTestAccessor {
 public:
  static const Ring* coefficientRing(const WeylAlgebra* W) {
    return W->getCoefficients();
  }
  static int nderivatives(const WeylAlgebra* W) {
    return W->_nderivatives;
  }
};
```

Caution: `ARingTest.hpp` and `RingTest.hpp` share an include guard and define
different values of `ntrials`, so one file must not include both.

## 17. Keep the suite fast

The whole suite runs in well under a second, which is why people run it. Keep it
that way. `ntrials` governs the randomized helpers; raise it locally when hunting
something, not in a commit.

Fixtures are not free either: a fresh one is built per `TEST_F`, and some ring
constructors are expensive — `ARingZZp` allocates two size-`p` tables and runs an
O(p^2) primitive-root search, `ARingZZpFlint` factors `p-1`.
