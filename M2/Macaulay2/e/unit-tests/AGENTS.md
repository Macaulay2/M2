# Engine unit-test conventions

Use these conventions when adding or revising engine unit tests in this
directory. Tests should explain the expected behavior and help a reader locate
the cause of a failure. This document defines the format without requiring a
particular existing test file as a template.

Apply these rules to the tests being changed. Do not reformat unrelated tests
solely to bring them into agreement with this guide.

## Organize tests by theme

- Keep tests in the existing file for the component when possible. Avoid new
  helper files or changes to shared helpers unless the task needs them. Name
  a new component test source `<Component>Test.cpp`.
- Include the component header, GoogleTest, and the standard headers used by
  the test directly. Keep file-local fixtures and helpers in an anonymous
  namespace.
- Use a small GoogleTest fixture named for the component when cases share
  setup or assertion helpers. Declare only commonly used state in the fixture;
  keep case-specific values local. Use `TEST` when no fixture is needed.
- GoogleTest creates a fresh fixture for each `TEST_F`, but not for each scope
  inside it. Tests must run independently and in any order.
- Give each test a short theme name, such as `Storage`, `Conversions`,
  `Comparisons`, `Arithmetic`, `Powers`, or `Formatting`.
- Group related cases in one `TEST_F` block. For example, real, imaginary, and
  mixed-value reciprocals belong together in `Arithmetic`, rather than in
  separate tests for each input.
- Within a theme, use small scopes for cases with their own setup. A short
  sequence of related checks can stay together without extra braces.
- Split a theme only when it becomes hard to follow or covers separate
  behavior. Avoid both one giant test and a separate test for every assertion.

## Make each case easy to debug

- Lay out setup, the operation, and checks in that order. Separate these steps
  with blank lines when it helps the reader.
- Set each case's inputs explicitly. Do not rely on values left by an unrelated
  earlier case in the same test.
- Use `SCOPED_TRACE` to name scoped cases and table rows. Include the operation
  and the relevant input condition, such as `divide: imaginary divisor`.
- For straight-line checks, append a short operation label with `<<` when the
  failure would otherwise be ambiguous.
- Use `EXPECT_*` for independent checks so a run can report several problems.
  Use `ASSERT_*` when later steps cannot safely or meaningfully continue after
  failure.
- Prefer small helpers returning `testing::AssertionResult` over helpers that
  return only a boolean. Report expected and actual values; for approximate
  comparisons, also report the error and tolerance.
- Use objects that clean up their resources automatically where possible.
  Pair explicit initialization and cleanup for GMP and similar objects. Avoid
  fatal assertions that skip required cleanup.

## Explain the purpose in plain language

- Start each test body with a comment explaining what it checks and what a
  failure would mean. Keep the comment to one or two short sentences.
- Add similarly short comments before distinct scopes, calculation groups,
  and case tables. Explain why the inputs were chosen or what the group is
  meant to catch, rather than repeating the next line of code.
- Explain table columns, unusual setup, rounding allowances, and helper
  behavior when they are not obvious.
- Prefer familiar wording. For example, say "the answer replaces an input"
  when explaining aliasing, or "find multipliers whose products add to zero"
  when explaining a syzygy check.
- Keep comments useful to a colleague investigating a failure. Do not narrate
  every assertion, add long background essays, or describe work done by an AI.

## Example layout

This complete example uses a standard container to demonstrate the layout
without depending on an engine API. In actual tests, exercise the engine
component being changed; do not add tests of the standard library itself.
The two cases share a `Storage` theme but each supplies its own inputs.

```cpp
#include <gtest/gtest.h>
#include <vector>

namespace {

class IntegerSequence : public ::testing::Test
{
 protected:
  std::vector<int> values;
};

TEST_F(IntegerSequence, Storage)
{
  // Check that copying and clearing preserve the right values.
  // Changes to the original must not damage a saved copy.

  {
    // Change the original after copying it. The copy should keep both
    // numbers in their original order.
    SCOPED_TRACE("copy: original changes");
    values = {2, 7};

    const auto saved = values;
    values[0] = 9;

    EXPECT_EQ(saved, (std::vector<int>{2, 7}));
    EXPECT_EQ(values, (std::vector<int>{9, 7}));
  }

  {
    // Clear a nonempty sequence, then use it again. No old entries should
    // remain when a new value is added.
    SCOPED_TRACE("clear: reuse storage");
    values = {3, 8};

    values.clear();

    EXPECT_TRUE(values.empty()) << "after clearing";

    values.push_back(5);

    EXPECT_EQ(values, (std::vector<int>{5})) << "after reuse";
  }
}

}  // namespace
```

For repeated steps with different inputs, define a local case structure with
named fields: a descriptive `name`, the inputs, and the expected output. In the
loop, set `SCOPED_TRACE(sample.name)`, prepare fresh inputs, perform the operation,
and check the expected output. Use separate tables when the steps or expected
failure behavior differ; do not select unrelated operations with a switch.

## Choose meaningful inputs and checks

- Start with small, deterministic inputs and independently known answers.
  Choose values that reveal mistakes: distinct entries expose swapped positions;
  empty and single-element inputs expose boundary handling; mixed signs expose
  sign errors. Use the cases that apply to the component.
- Use a small named case table when several inputs share the same steps.
  Keep expected answers visible in the table. Avoid large loops that mix
  unrelated operations or require complicated logic to interpret a failure.
- Check exact answers with exact comparisons when they are representable.
  Use a justified, precision-aware tolerance for results affected by rounding.
  Explain the allowance; do not widen it just to make a test pass.
- For interval results, distinguish exact endpoints from containment checks.
  A valid interval may be wider than the expected point, so choose the check
  that matches the operation's contract.
- Exercise supported in-place operations, boundaries, conversions, and error
  cases where they represent distinct behavior. Check the documented error
  result or exception type. Do not assume related implementations support the
  same operations.
- Keep randomized property checks separate from worked examples. On failure,
  show the trial and inputs, and record a seed when the random API supports it.
  Random checks supplement known-answer cases; they do not replace them.

## Coverage, builds, and scope

- Aim for strong line, function, and branch coverage of the associated code.
  Use gcovr results to find missing behavior, then add readable cases that
  assert an observable result.
- Do not add calls with no useful assertion, unreachable-state tricks, or
  production changes just to improve coverage numbers. Report remaining gaps
  and their causes rather than hiding them.
- Keep changes within the requested component. Treat production fixes and
  broad shared-helper refactors as separate work unless included in the task.
- When adding `<Component>Test.cpp`, register it in both build systems:
  add `<Component>Test` (without the extension) to `UNITTEST_CCFILES` in
  `Makefile.files` in this directory, and add
  `unit-tests/<Component>Test.cpp` to the `M2-unit-tests` source list in
  `../CMakeLists.txt`. Preserve Makefile continuation backslashes.
  Existing registered sources need no new build entry for additional tests.
- Build and run the affected tests after changing test code. For an existing
  configured CMake build, use `cmake --build <build-dir> --target M2-unit-tests`
  and `ctest --test-dir <build-dir> -R '<affected-test-pattern>'
  --output-on-failure`. CTest names have the prefix `unit-tests:`.
- For Autotools, run `make -k check` from the unit-tests directory in the
  configured build tree. Verify both build registrations when changing source
  lists; a successful run in one build system does not validate the other.
- Report which checks ran and any limits on validation. For comment-only
  edits, checking the diff for unintended code changes is sufficient; do not
  claim tests or coverage were rerun if they were not.
