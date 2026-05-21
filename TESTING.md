# Testing M2

This document is the **end-to-end testing reference** for M2.
The test infrastructure is split across multiple subsystems —
this file unifies them, explains when each is used, and tells
you how to run them.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md) · [Startup](STARTUP.md)

## Six test infrastructures

```
┌──────────────────────────────────────────────────────────┐
│   1. e/unit-tests/    — engine gtest (C++)                │
│   2. M2 --check N      — interpreter-level self-tests     │
│   3. packages/check    — per-package M2 TEST blocks       │
│   4. tests/normal/     — CTest default-tier regression    │
│   5. tests/slow/       — CTest slow regression            │
│   6. tests/ComputationsBook/ — book-examples regression   │
└──────────────────────────────────────────────────────────┘
```

Plus three secondary suites: `tests/engine/` (CI-skipped),
`tests/quarantine/` (broken tests on hold), `system/tests.cpp`
(standalone supervisor tests).

## 1. Engine gtest suite — `e/unit-tests/`

**Source**:
[`M2/Macaulay2/e/unit-tests/README.md`](M2/Macaulay2/e/unit-tests/README.md)
+ per-file deep dives.

**What it tests**: the C++ engine directly. No interpreter, no
`.d` translation layer. Exercises:

- Every coefficient ring (`ARingZZ`, `ARingZZp`, `ARingQQ*`,
  `ARingGF*`, `ARingRR*`, `ARingCC*`).
- Both legacy `Ring` and modern `aring` framework.
- `DMat` linear algebra over each ring.
- Polynomial-ring construction.
- F4 GB (newer `gb-f4` engine).
- NC GB.
- Schreyer-resolution monoid.

**Build target**: `M2-unit-tests`.

**Run**:

```sh
cd M2/BUILD/build
cmake --build . --target M2-unit-tests
ctest --output-on-failure -R "unit-tests"
# Or directly:
./Macaulay2/e/unit-tests/M2-unit-tests
# Or filtering:
./Macaulay2/e/unit-tests/M2-unit-tests --gtest_filter='ARingZZ*'
```

**Speed**: < 1 minute on a modern machine.

**When to run**: every PR (CI runs this); locally when touching
engine C++ code.

**Adding a test**: drop a `.cpp` file using `TEST(Suite,
TestName) { ... }` next to existing tests. Auto-discovered by
CMake. Per-file conventions:
[`unit-tests/file-test-harness.md`](M2/Macaulay2/e/unit-tests/file-test-harness.md).

## 2. Interpreter-level self-test — `M2 --check N`

**Source**: the `--check` infrastructure is documented in
[`M2/Macaulay2/m2/file-testing.md`](M2/Macaulay2/m2/file-testing.md)
and the embedded test strings in
[`M2/Macaulay2/bin/file-startup.md`](M2/Macaulay2/bin/file-startup.md).

**What it tests**: a curated set of M2 expressions whose expected
outputs are baked into the binary. Three difficulty levels:

```sh
M2 -q --check 1   # fast
M2 -q --check 2   # medium
M2 -q --check 3   # slow
```

**Speed**: ~1 min (level 1), ~5 min (level 2), ~30 min (level 3).

**When to run**: CI runs all three on every PR; locally when
testing a binary smoke-test.

**Why this exists**: catches bugs that only manifest end-to-end
(M2 source → lexer → parser → engine → output) without requiring
the test harness's machinery.

## 3. Per-package tests — `check "PackageName"`

**Source**:
[`M2/Macaulay2/packages/README.md`](M2/Macaulay2/packages/README.md)
+ [`file-package-conventions.md`](M2/Macaulay2/packages/file-package-conventions.md).

**What it tests**: each user package's `TEST ///...///` blocks.
Every distributed package has them; `check "Foo"` runs Foo's
tests from inside M2.

**Run from inside M2**:

```m2
check "Foo"             -- run Foo's TEST blocks
check_FormatNN "Foo"    -- variants for specific test formats
```

**Run from shell**:

```sh
ctest -R "check-Foo"        # CMake
make check-Foo              # autotools
```

**Speed**: depends on the package — most are seconds, some are
minutes.

**When to run**:

- When editing a package.
- CI runs every distributed package's `check` on PRs touching
  packages.

**Adding a test**: add a `TEST ///...///` block to the package's
`.m2` file. The block contains M2 code that uses `assert(...)` to
verify behaviour.

## 4. CTest default-tier — `tests/normal/`

**Source**:
[`tests/normal/file-normal-tests-catalogue.md`](M2/Macaulay2/tests/normal/file-normal-tests-catalogue.md).

**What it tests**: 373 `.m2` test scripts covering the broad
correctness surface — every ring, every algorithm, edge cases,
specific bug regressions.

**Run**:

```sh
cd M2/BUILD/build
ctest --output-on-failure -R "normal"   # all
ctest --output-on-failure -R "normal/gb-"  # filter
```

**Speed**: ~5-10 minutes total.

**When to run**: every PR (CI); locally when touching cross-cutting
engine or M2-layer code.

**Naming conventions**: see
[`tests/normal/file-normal-tests-catalogue.md`](M2/Macaulay2/tests/normal/file-normal-tests-catalogue.md)
— mix of `0-prefix.m2` (bootstrap), `B*.m2` (bug regressions),
descriptive names (`adjoint.m2`, `RRi.m2`).

**Adding a test**: drop `<name>.m2` into the directory. Open with
a `-- fixed: YYYY-MM-DD` comment if it's a bug regression. Use
`assert(...)` calls for invariants. Auto-discovered by CMake.

## 5. CTest slow-tier — `tests/slow/`

**Source**:
[`tests/slow/file-slow-tests-catalogue.md`](M2/Macaulay2/tests/slow/file-slow-tests-catalogue.md).

**What it tests**: 12 slow-running regression tests — large GBs,
deep resolutions, stress workloads. Each takes minutes-to-hours
individually.

**Run**:

```sh
ctest --output-on-failure -R "slow"
```

**Speed**: ~1 hour total.

**When to run**: not on every PR. Maintainers run periodically on
beefy machines, and locally when touching code that might affect
a known-slow path.

## 6. Book-examples regression — `tests/ComputationsBook/`

**Source**:
[`tests/ComputationsBook/file-computations-book-catalogue.md`](M2/Macaulay2/tests/ComputationsBook/file-computations-book-catalogue.md).

**What it tests**: 12 chapters of examples from the book
*Computations in Algebraic Geometry with Macaulay 2*. Each
chapter's examples are replayed and outputs diffed against
canonical expected outputs.

**Run**:

```sh
ctest -R "ComputationsBook"
ctest -R "ComputationsBook/varieties"   # one chapter
```

**Speed**: minutes to tens of minutes.

**When to run**: every PR (CI); locally when touching anything
that might change book-example outputs (monomial ordering, normal
forms, GB strategies).

**Why it matters**: the book is M2's canonical reference. Silent
output drift breaks the book's reproducibility — this suite
catches it.

## Secondary suites (lower priority)

### `tests/engine/` — engine integration (CI-skipped)

35 `.m2` scripts exercising `raw…()` engine entry points. Many
were written against engine behaviour that has since drifted; CI
skips them per
[issue #1213](https://github.com/Macaulay2/M2/issues/1213).
Useful for manual verification of specific engine paths.

[`tests/engine/file-engine-tests-catalogue.md`](M2/Macaulay2/tests/engine/file-engine-tests-catalogue.md).

### `tests/quarantine/` — temporarily-disabled tests

9 tests that once passed but currently fail. The fixes haven't
been prioritised; moved here to keep CI green while preserving
the known-failing cases. See
[`tests/file-small-suites-catalogue.md`](M2/Macaulay2/tests/file-small-suites-catalogue.md).

### `system/tests.cpp` — standalone supervisor tests

Tests the thread supervisor in isolation (no engine, no
interpreter). Catches lost-tasks, race conditions, deadlocks.
See [`system/file-tests.md`](M2/Macaulay2/system/file-tests.md).

### Other tiny suites

`tests/goals/`, `tests/gigantic/`, `tests/threads/`,
`tests/rationality/` — see
[`tests/file-small-suites-catalogue.md`](M2/Macaulay2/tests/file-small-suites-catalogue.md).

### `EngineTests` user package

[`packages/file-EngineTests.md`](M2/Macaulay2/packages/file-EngineTests.md)
— M2-level engine test suite. Listed in `PACKAGES_DEVEL` (not
shipped to users) but run by CI.

## Test architecture: what tests what

```
                          ┌─────────────────────┐
                          │  User M2 input      │
                          └──────────┬──────────┘
                                     ▼
                          ┌─────────────────────┐
                          │  M2 interpreter     │  ← M2 --check N
                          │  + Core M2          │     packages/check "Foo"
                          └──────────┬──────────┘     tests/normal/
                                     │                tests/slow/
                                     ▼                tests/ComputationsBook/
                          ┌─────────────────────┐
                          │  Engine boundary    │  ← tests/engine/ (skipped)
                          │  (interface/)       │
                          └──────────┬──────────┘
                                     ▼
                          ┌─────────────────────┐
                          │  Engine internals   │  ← e/unit-tests/ (gtest)
                          │  (C++ classes)      │
                          └──────────┬──────────┘
                                     ▼
                          ┌─────────────────────┐
                          │  Thread supervisor  │  ← system/tests.cpp
                          └─────────────────────┘
```

Each test suite hits the stack at a different layer. Together
they catch bugs at every level.

## When to add a test where

| Bug surface | Test location |
|---|---|
| Engine internal C++ class | [`e/unit-tests/`](M2/Macaulay2/e/unit-tests/README.md) |
| Engine-boundary C ABI | `EngineTests` package or [`tests/engine/`](M2/Macaulay2/tests/engine/README.md) |
| M2-level operation on a specific ring | [`tests/normal/`](M2/Macaulay2/tests/normal/README.md) |
| Package-specific feature | `TEST ///...///` block in the package's `.m2` |
| Cross-package interaction | [`tests/normal/`](M2/Macaulay2/tests/normal/README.md) |
| Output formatting / pretty-printing | [`tests/normal/`](M2/Macaulay2/tests/normal/README.md) or `--check` |
| Book-example output drift | (auto — Computations Book runs each PR) |
| Threading correctness | [`system/tests.cpp`](M2/Macaulay2/system/file-tests.md) |
| Stress / scale | [`tests/slow/`](M2/Macaulay2/tests/slow/README.md) or [`tests/gigantic/`](M2/Macaulay2/tests/README.md) |
| Bug regression | [`tests/normal/`](M2/Macaulay2/tests/normal/README.md) with `-- fixed: DATE` header |

## CI matrix

`.github/workflows/test_build.yml` runs **four combinations** on
every PR to `stable` or `development`:

| Build system | OS |
|---|---|
| autotools | ubuntu-24.04 |
| autotools | macos-15 |
| cmake | ubuntu-24.04 |
| cmake | macos-15 |

For each combination CI runs:

1. **Build**: `M2-engine`, `M2-binary`, `M2-core`, `M2-emacs`.
2. **Tests**:
   - `ctest -R "unit-tests"` (engine gtest).
   - `M2 -q --check 1`, `M2 -q --check 2`, `M2 -q --check 3`.
   - `ctest -R "ComputationsBook"`.
   - `ctest -R "normal"` (default-tier).
   - `make check` (per-package tests).

Total CI runtime per combination: ~30-60 minutes.

## Running tests locally

The minimal workflow:

```sh
# Initial build
cmake -GNinja -S M2 -B M2/BUILD/build
cmake --build M2/BUILD/build --target build-libraries build-programs
cmake --build M2/BUILD/build --target M2-core M2-emacs M2-unit-tests

# Run different test tiers
cd M2/BUILD/build
ctest --output-on-failure -R "unit-tests"             # ~30 sec
./M2 -q --check 1                                      # ~1 min
ctest --output-on-failure -R "ComputationsBook"        # ~10 min
ctest --output-on-failure -R "normal"                  # ~10 min

# All of the above:
ctest --output-on-failure -j$(nproc)
```

For autotools builds, swap `ctest` for `make check` from the
build directory.

## Test code conventions

### M2-level tests (`.m2` files)

```m2
-- fixed: 2024-05-15
-- (regression test for issue #NNNN)

R = QQ[x, y, z]
I = ideal(x^2 - y, x*y - z)
G = gb I
assert(numgens G == 3)
assert(class G === GroebnerBasis)
```

Use `assert(...)` for invariants. The test passes if no
assertion fails.

### C++ tests (`unit-tests/*.cpp`)

```cpp
#include <gtest/gtest.h>
#include "ARingTest.hpp"
#include "aring-zzp.hpp"

TEST(ARingZZp, basicArithmetic) {
  M2::ARingZZp R(101);
  M2::ARingZZp::ElementType a, b, c;
  R.init(a); R.init(b); R.init(c);
  R.set_from_long(a, 50);
  R.set_from_long(b, 60);
  R.add(c, a, b);
  EXPECT_EQ(R.get_int(c), 9);  // 50 + 60 mod 101
}
```

Standard gtest conventions. `EXPECT_*` for soft asserts,
`ASSERT_*` for hard. See
[`unit-tests/file-aring-zz-tests.md`](M2/Macaulay2/e/unit-tests/file-aring-zz-tests.md)
for canonical patterns.

### Package tests (`TEST ///...///`)

```m2
TEST ///
R = QQ[x, y]
M = matrix{{x, y}, {y, x}}
assert(det M == x^2 - y^2)
///
```

Inside a package's `.m2` file, after `beginDocumentation()`.

## Debugging test failures

For a failing M2-level test:

```sh
# Run interactively to see all output:
M2 --script tests/normal/the-test.m2
```

For a failing gtest:

```sh
# Run just that test:
./Macaulay2/e/unit-tests/M2-unit-tests --gtest_filter='SuiteName.TestName'
# Or with valgrind:
valgrind --suppressions=$M2/files/M2-suppressions.supp \
    ./Macaulay2/e/unit-tests/M2-unit-tests --gtest_filter='SuiteName.*'
```

For a failing book-example test:

```sh
# Diff the captured output:
diff tests/ComputationsBook/<chapter>/chapter.out \
     tests/ComputationsBook/<chapter>/chapter.out.expected
```

## Code coverage

Coverage instrumentation is opt-in via the CMake option:

```sh
cmake -GNinja -S M2 -B M2/BUILD/cov -DCOVERAGE=ON
cmake --build M2/BUILD/cov --target M2-core M2-unit-tests
cmake --build M2/BUILD/cov --target coverage
# Produces HTML report in M2/BUILD/cov/coverage/
```

See [`cmake/file-misc-cmakes.md`](M2/cmake/file-misc-cmakes.md)
for the coverage module.

## Linting

```sh
# Spell-check (CI runs this):
codespell --ignore-words=.codespell_ignore M2/Macaulay2/packages

# clang-tidy / clang-format / cppcheck / IWYU (opt-in):
cmake --build M2/BUILD/build --target clang-tidy-all
cmake --build M2/BUILD/build --target clang-format-all
```

See [`cmake/file-misc-cmakes.md`](M2/cmake/file-misc-cmakes.md)
for lint module wiring.

## Related

- [`README.md`](README.md) — repository TOC.
- [`TOUR.md`](TOUR.md) — Path B (engine debugger) and Path C
  (package author) reference test infrastructure.
- [`STARTUP.md`](STARTUP.md) — `--check` runs after the boot
  sequence completes.
- [`MEMORY.md`](MEMORY.md) — Valgrind suppression file referenced
  here.
- [`tests/README.md`](M2/Macaulay2/tests/README.md) — overview of
  CTest suites.
- [`e/unit-tests/README.md`](M2/Macaulay2/e/unit-tests/README.md)
  — gtest suite.
- [`packages/file-EngineTests.md`](M2/Macaulay2/packages/file-EngineTests.md)
  — alternate engine test path.
