# `M2/cmake/` — CMake modules

This directory contains the CMake support code that drives the **CMake-based**
build of Macaulay2. (The parallel autotools build does not use any of these
files; its equivalents live in [`M2/libraries/`](../libraries/README.md) and
the various `Makefile.in` files.)

## Top-level orchestration

| File | Role | Deep dive |
|---|---|---|
| `configure.cmake` | User-facing build options, install layout, compiler flags, feature detection | [`file-configure-cmake.md`](file-configure-cmake.md) |
| `check-libraries.cmake` | Library-detection driver (calls every `Find*.cmake`) | [`file-check-libraries-cmake.md`](file-check-libraries-cmake.md) |
| `build-libraries.cmake` | Build-from-source fallback (`ExternalProject_Add`) | [`file-build-libraries-cmake.md`](file-build-libraries-cmake.md) |
| `scc.cmake` | `scc1` invocation macro for `.d`/`.dd` files | [`file-scc-cmake.md`](file-scc-cmake.md) |
| `startup.cmake` | C-escapes `startup.m2` into `startup.c` | [`file-startup-cmake.md`](file-startup-cmake.md) |
| `prechecks.cmake` | clang-tidy / clang-format / cppcheck / IWYU / valgrind detection | [`file-misc-cmakes.md`](file-misc-cmakes.md) |
| `flavor.cmake` | OS / distro detection for package metadata | [`file-misc-cmakes.md`](file-misc-cmakes.md) |
| `darwin.cmake` | macOS cross-compile toolchain file | [`file-misc-cmakes.md`](file-misc-cmakes.md) |
| `packaging.cmake` | CPack configuration for `.deb`/`.rpm`/`.tar.gz`/`.dmg` | [`file-misc-cmakes.md`](file-misc-cmakes.md) |
| `coverage.cmake`, `profiling.cmake`, `latex.cmake`, `stackcollapse-m2.sh` | Optional dev-tool integrations | [`file-misc-cmakes.md`](file-misc-cmakes.md) |

## `Find*.cmake` — library detection

Each `Find<Lib>.cmake` teaches CMake's `find_package` how to locate a specific
dependency:

`FindBDWGC`, `FindCDDLIB`, `FindEAntic`, `FindFFI`, `FindFactory`, `FindFlint`,
`FindFrobby`, `FindGDBM`, `FindGLPK`, `FindGMP`, `FindHistory`, `FindJansson`,
`FindMPFI`, `FindMPFR`, `FindMPSolve`, `FindMSolve`, `FindMathic`,
`FindMathicgb`, `FindMemtailor`, `FindNTL`, `FindNauty`, `FindNormaliz`,
`FindReadline`, `FindSphinx`, `FindTBB`.

Common-shape deep dive: [`file-find-cmakes.md`](file-find-cmakes.md).

If a `Find*` script fails, the corresponding library is added to the
`build-libraries` target list and built from source by
`build-libraries.cmake` (often by pulling it via the matching
[submodule](../submodules/README.md)).

**Coverage:** every CMake module in this directory has a dedicated deep-dive doc (some grouped: the 25 `Find*.cmake` share one doc, and the small misc modules share one).

## Workflow

```sh
cmake -GNinja -S M2 -B M2/BUILD/build
cmake --build M2/BUILD/build --target build-libraries build-programs
cmake --build M2/BUILD/build --target M2-core M2-emacs
cmake --build M2/BUILD/build --target install-packages check-packages
```

See the project [Wiki](https://github.com/Macaulay2/M2/wiki) and `.github/workflows/test_build.yml` for the canonical command list.

## Related

- [`M2/libraries/`](../libraries/README.md) — autotools equivalents (per-lib
  `Makefile.in`).
- [`M2/submodules/`](../submodules/README.md) — vendored sources used when
  `build-libraries` has to compile from scratch.

[← back to repository TOC](../../README.md#under-m2)
