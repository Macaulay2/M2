# `M2/Macaulay2/tests/CMakeLists.txt` — top-level CMake registration of tests

`CMakeLists.txt` at the root of the `tests/` directory is the
**CMake-side dispatcher** that registers every test suite under
`tests/` with CTest. Reading it is the fastest way to see what tests
the engine runs in CI.

This file is referenced as documentation by the per-suite READMEs;
this companion markdown file gives the same overview without
requiring users to read raw CMake syntax.

[← back to tests overview](README.md)

## What the CMakeLists.txt does

For each subdirectory (`normal/`, `slow/`, `engine/`, `ComputationsBook/`,
`gigantic/`, `goals/`, `quarantine/`, `rationality/`, `threads/`), the
file:

1. Reads the directory's `Makefile.in` (templated) to find the list
   of `.m2` test scripts.
2. For each `.m2` script, registers a CTest test that invokes M2 on
   that script and checks the exit code.
3. Groups the resulting tests into named CTest labels matching the
   subdirectory name.

This lets the user invoke any single suite via `ctest -R <suite>`.

## CI policy

The companion CI workflow (`.github/workflows/test_build.yml`) runs:

- **CMake build path**: `ctest -j1 --output-on-failure -R "ComputationsBook"`.
- **`M2 --check N` for N=1,2,3** in addition to CTest.

The CMakeLists.txt registers more suites than CI actually runs;
the additional ones are reachable manually.

## Adding a new test

1. Drop the `.m2` file into the most appropriate subdirectory.
2. The `Makefile.in` in that subdirectory should already glob for
   `.m2` files — no edit needed.
3. CTest will pick up the new test on the next configure step.

To create a **new suite** (a new subdirectory), edit `CMakeLists.txt`
to register it and add a `Makefile.in` to the new directory.

## Related

- [`README.md`](README.md) — overall test-suite overview.
- Each per-suite README (linked from above).
- `.github/workflows/test_build.yml` — CI invocation.
- [`../e/unit-tests/`](../e/unit-tests/README.md) — C++
  gtest suite (registered separately).
