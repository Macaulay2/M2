# `build-libraries.cmake` — build-from-source fallback driver

`build-libraries.cmake` is the **build-from-source driver** for
every external library M2 needs. When the system version isn't
available (or the user asked for a build-from-source), this
script downloads, configures, builds, and installs it.

Part of [`cmake/`](README.md).

[← cmake/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Header

```cmake
###############################################################################
## This script is responsible for dependencies between libraries and programs
## that we build and contains build instructions for them.
##
## - build all: cmake --build . --target build-libraries  build-programs
## - test all:  cmake --build . --target check-components check-components-slow (very slow)
## - clean all: cmake --build . --target clean-stamps

## Set the timestamp of the extracted content to the time of extraction
cmake_policy(SET CMP0135 NEW)

include(ExternalProject) # configure, patch, build, install, or test at build time
set(M2_SOURCE_URL https://macaulay2.com/Downloads/OtherSourceCode)
```

The script uses CMake's `ExternalProject_Add` machinery. For each
library it sets up:

1. **Download** from `M2_SOURCE_URL` (M2's CDN of vendored
   tarballs) or from a submodule.
2. **Configure** with appropriate flags.
3. **Build** with `make -j`.
4. **Install** into a local prefix.
5. **Patch** if M2 maintains local patches.

`M2_SOURCE_URL` points at the macaulay2.com mirror so M2 can
build reproducibly even if an upstream project moves or vanishes.

## Why this script is huge

Each library has its own quirks:

- **NTL** needs `NTL_GMP_LIP=on` and `NTL_THREADS=on`.
- **MPFR** needs `--with-gmp=...` pointing at M2's GMP.
- **Factory** needs special `--without-Singular`.
- **Normaliz** needs `--with-flint`.
- **fflas-ffpack** depends on Givaro depends on GMP.

The script encodes all this. It's typically 1000+ lines.

## Build dependencies

The script orders libraries so each is built before its
dependents:

```
GMP → MPFR → MPFI
GMP → NTL → Factory
GMP → FLINT → ARB
GMP → Givaro → fflas-ffpack
```

`ExternalProject_Add(NAME ... DEPENDS dep1 dep2)` declares
these dependencies; CMake then serialises the build order.

## Targets exposed

- **`build-libraries`** — convenience: builds everything in the
  build queue.
- **`build-programs`** — builds external programs (4ti2, gfan,
  ...) used by some packages.
- **`check-components`** — runs each library's self-tests.

## Reconfigure after build

After `build-libraries` completes, CMake **re-runs configure**.
The first run found "GMP missing"; now GMP is built and present,
so the second pass picks it up and links against it.

## Used by

- The CMake build, when libraries are missing.
- Developers wanting reproducible builds (force-build everything).

## Related

- [`README.md`](README.md) — cmake/ overview.
- [`file-check-libraries-cmake.md`](file-check-libraries-cmake.md)
  — populates the build queue.
- [`../libraries/README.md`](../libraries/README.md) — autotools
  equivalent.
- [`../submodules/README.md`](../submodules/README.md) — submodules
  this script pulls from.
