# `M2/cmake/` — CMake modules

This directory contains the CMake support code that drives the **CMake-based**
build of Macaulay2. (The parallel autotools build does not use any of these
files; its equivalents live in [`M2/libraries/`](../libraries/README.md) and
the various `Makefile.in` files.)

## Top-level orchestration

| File | Role |
|---|---|
| `configure.cmake` | Aggregates user-facing build options (`BUILD_SHARED`, package set selection, dev features, etc.) |
| `prechecks.cmake` | Sanity checks before the rest of configure runs |
| `flavor.cmake` | Detects compiler / platform "flavor" and sets defaults |
| `check-libraries.cmake` | Enumerates the libraries M2 needs and reports what was found |
| `build-libraries.cmake` | Describes how to download and build third-party libs from source when the system doesn't supply them |
| `scc.cmake` | Builds and invokes [`scc1`](../Macaulay2/c/README.md) on the `.d` sources |
| `startup.cmake` | Substitutes installation paths into [`Macaulay2/bin/startup.c.cmake`](../Macaulay2/bin/README.md) |
| `coverage.cmake`, `profiling.cmake` | Optional code-coverage and profiling toggles |
| `darwin.cmake` | macOS-specific tweaks |
| `latex.cmake` | LaTeX detection for documentation builds |
| `packaging.cmake` | CPack configuration for binary distribution |
| `stackcollapse-m2.sh` | Helper script for profiling (`perf` → flame-graph) |

## `Find*.cmake` — library detection

Each `Find<Lib>.cmake` teaches CMake's `find_package` how to locate a specific
dependency:

`FindBDWGC`, `FindCDDLIB`, `FindEAntic`, `FindFFI`, `FindFactory`, `FindFlint`,
`FindFrobby`, `FindGDBM`, `FindGLPK`, `FindGMP`, `FindHistory`, `FindJansson`,
`FindMPFI`, `FindMPFR`, `FindMPSolve`, `FindMSolve`, `FindMathic`,
`FindMathicgb`, `FindMemtailor`, `FindNTL`, `FindNauty`, `FindNormaliz`,
`FindReadline`, `FindSphinx`, `FindTBB`.

If a `Find*` script fails, the corresponding library is added to the
`build-libraries` target list and built from source by
`build-libraries.cmake` (often by pulling it via the matching
[submodule](../submodules/README.md)).

## Workflow

```sh
cmake -GNinja -S M2 -B M2/BUILD/build
cmake --build M2/BUILD/build --target build-libraries build-programs
cmake --build M2/BUILD/build --target M2-core M2-emacs
cmake --build M2/BUILD/build --target install-packages check-packages
```

See the root [`CLAUDE.md`](../../CLAUDE.md) for the canonical command list.

## Related

- [`M2/libraries/`](../libraries/README.md) — autotools equivalents (per-lib
  `Makefile.in`).
- [`M2/submodules/`](../submodules/README.md) — vendored sources used when
  `build-libraries` has to compile from scratch.

[← back to repository TOC](../../README.md#under-m2)
