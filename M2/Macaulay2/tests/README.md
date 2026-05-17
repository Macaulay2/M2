# `M2/Macaulay2/tests/` — top-level CTest suites

These are the **integration test suites** invoked by CTest in the CMake build.
Each subdirectory is its own test group with its own CMake registration.
Package-level tests live with each package under
[`Macaulay2/packages/`](../packages/README.md); the engine-level gtest suite
lives under [`Macaulay2/e/unit-tests/`](../e/README.md).

## Suites

| Directory | Purpose | Notes |
|---|---|---|
| `engine/` | Engine integration tests written in M2 | Currently skipped in CI; see issue [#1213](https://github.com/Macaulay2/M2/issues/1213) |
| `ComputationsBook/` | Examples from *Computations in Algebraic Geometry with Macaulay 2* | Run with `ctest -R ComputationsBook` |
| `normal/` | Default-tier regression tests |  |
| `slow/` | Slower regression tests | Not always run in CI |
| `quarantine/` | Tests temporarily disabled, awaiting fixes |  |
| `goals/` | Worked exercises and "goal" computations |  |
| `gigantic/` | Stress tests that may need lots of RAM / time |  |
| `threads/` | Concurrency / thread-supervisor tests |  |
| `rationality/` | Tests centered on rationality questions |  |

Plus build glue: `CMakeLists.txt`, `Makefile.in`, `Makefile.test.in`.

## Running

```sh
cd M2/BUILD/build
ctest --output-on-failure -R "ComputationsBook"   # single suite by regex
ctest --output-on-failure                            # all enabled suites
```

From inside an installed M2 you can also run the language-level self-check at
three difficulty levels:

```sh
M2 -q --check 1   # fast
M2 -q --check 2   # medium
M2 -q --check 3   # slow
```

CI runs all three.

## Related

- [`Macaulay2/e/unit-tests/`](../e/README.md) — gtest suite for the C++ engine.
- [`Macaulay2/packages/`](../packages/README.md) — per-package tests run via
  `check "Foo"` or `ctest -R check-Foo`.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
