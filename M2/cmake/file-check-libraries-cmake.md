# `check-libraries.cmake` — system-library detection driver

`check-libraries.cmake` is the **library detection driver** that
runs every `Find*.cmake` to discover what's installed on the
build machine. Libraries it can't find get added to the
`build-libraries` queue.

Part of [`cmake/`](README.md).

[← cmake/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Header

```cmake
###############################################################################
## This script is responsible for finding location of libraries and programs.
## - force building those components:
##     cmake -DBUILD_LIBRARES="X Y Z" -DBUILD_PROGRAMS="A B C" .
##
## - list NTL variables:        cmake -LA . | grep NTL
##    reconfigure NTL variables: cmake -U*NTL* .

# These are some of the libraries linked with Macaulay2 in Macaulay2/{d,e,bin}/CMakeLists.txt
# Others, like TBB::tbb, FFI::ffi, and Boost::regex, are linked as imported libraries in those files.
# TODO: turn all these libraries into imported libraries and find incompatibilities another way.
set(PKGLIB_LIST    FFLAS_FFPACK GIVARO)
set(LIBRARIES_LIST MPSOLVE FROBBY NORMALIZ FACTORY FLINT NTL MPFI MPFR GMP BDWGC LAPACK)
set(LIBRARY_LIST   READLINE HISTORY GDBM JANSSON)
```

Three categorisation lists:

- **`PKGLIB_LIST`** — pkg-config-based libs (FFLAS_FFPACK,
  GIVARO).
- **`LIBRARIES_LIST`** — main math libs (FLINT, NTL, MPFR, ...).
- **`LIBRARY_LIST`** — auxiliary libs (Readline, GDBM, ...).

Each list gets a slightly different detection pattern (pkg-config
vs `find_package` vs `find_library`).

## What it does

For each library in each list:

1. Run the matching `Find<LibName>.cmake` from this directory.
2. Check if a system version is available and version-compatible.
3. If found → import it (e.g., `target_link_libraries(...
   GMP::GMP)`).
4. If not found → add to `BUILD_LIBRARIES` list for build-time
   compilation.

The user can force a build-from-source even when the system has
the lib:

```sh
cmake -DBUILD_LIBRARIES="NTL MPFR GMP" -S M2 -B M2/BUILD/build
```

Useful when the system version is too old, has the wrong ABI, or
the developer wants a specific patch.

## How forcing works

After this script runs, the build target `build-libraries` knows
what to compile. Then `build-libraries.cmake` actually does the
compile.

The two-phase split makes incremental rebuilds work cleanly:
forcing one library to build from source doesn't disturb the
others.

## Imported targets vs `_LIBRARIES` variables

Old-style CMake uses `${NAME_LIBRARIES}` variables. New-style
uses imported targets (`NAME::NAME`). The TODO comment shows the
migration is incomplete — TBB, FFI, and Boost are already
imported targets but the math libraries here still use the older
variable approach.

## Used by

- The top-level `CMakeLists.txt`, included after
  [`configure.cmake`](file-configure-cmake.md).
- Indirectly, every CMakeLists.txt under `Macaulay2/` that links
  against these libraries.

## Related

- [`README.md`](README.md) — cmake/ overview.
- [`file-configure-cmake.md`](file-configure-cmake.md) — runs
  before.
- [`file-build-libraries-cmake.md`](file-build-libraries-cmake.md)
  — handles the build-from-source fallback.
- [`file-find-cmakes.md`](file-find-cmakes.md) — the 25 `Find*.cmake`
  modules this driver invokes.
- [`../libraries/README.md`](../libraries/README.md) — autotools
  equivalent.
