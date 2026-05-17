# `M2/Macaulay2/tests/normal/` — default-tier regression test suite

This directory contains the **default-tier regression tests** the
CTest target runs — currently 377 `.m2` test files covering general
Macaulay2 behaviour. The tests are simple top-level M2 scripts that
exercise specific features; CI runs them on every commit.

[← back to tests overview](../README.md)

## What's tested

The `normal/` suite covers the **broad correctness surface** of the
engine and front-end:

- Ring construction over every supported coefficient ring.
- Module / matrix construction and arithmetic.
- Gröbner bases (default algorithm, various strategies).
- Resolutions across the algorithm variants.
- Hilbert function / Betti tables.
- Cross-engine boundary correctness (M2 ↔ engine values).
- Reading and writing every output format the engine exposes.

The naming convention is loose — most test files are named after the
feature they exercise (e.g. `4-b.m2`, `B44.m2`, `0-homog.m2`).

## How tests run

Each `.m2` file is invoked with `M2 --no-prompts --silent` (or
similar). The test passes if M2 returns 0 and produces no
unexpected output; it fails otherwise.

The mechanism is in `Makefile.in` (autotools) and `CMakeLists.txt`
(CMake) at this directory's parent.

## Test categories

| Pattern | Approximate topic |
|---|---|
| `0-*.m2`, `000-*.m2`, `00*.m2` | Bootstrap / basic sanity |
| `4-*.m2`, `4a.m2`, `4b.m2`, `4d.m2` | Engine boundary tests |
| `B*.m2` | Bug regressions |
| `gb-*.m2`, `res-*.m2`, `hilb-*.m2` | Algorithm-specific tests |
| Named files (`schur.m2`, `lapack.m2`, etc.) | Per-feature tests |

## Triggering

```sh
ctest -R "normal"      # CMake
make -C tests/normal   # autotools
```

Both run every `.m2` file in this directory.

## Catalogue

[`file-normal-tests-catalogue.md`](file-normal-tests-catalogue.md) — full
walkthrough of test patterns, naming conventions, what gets tested, and
how to add a new test. Covers all 373 test files structurally.

## Related

- [`../README.md`](../README.md) — overall test-suite overview.
- [`../engine/README.md`](../engine/README.md) (when added) — engine
  integration tests, currently skipped in CI.
- [`../slow/README.md`](../slow/README.md) (when added) — slower
  tests run less often.
- [`../../e/unit-tests/`](../../e/unit-tests/README.md) — C++ unit tests.
