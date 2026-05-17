# `Find*.cmake` — library detection modules

The 25 `Find<Lib>.cmake` modules in this directory teach CMake's
`find_package` how to locate each external dependency M2 needs.
Each follows the same template: probe for headers, libraries,
and a version test; report success or failure.

Part of [`cmake/`](README.md).

[← cmake/ overview](README.md) · [← top-level repo TOC](../../README.md)

## The modules

Mathematical libraries:

| Module | Library | What M2 uses it for |
|---|---|---|
| `FindGMP.cmake` | GMP | arbitrary-precision integers / rationals |
| `FindMPFR.cmake` | MPFR | arbitrary-precision reals |
| `FindMPFI.cmake` | MPFI | interval reals (built on MPFR) |
| `FindMPSolve.cmake` | MPSolve | polynomial root finding |
| `FindMSolve.cmake` | msolve | F4 GB external |
| `FindNTL.cmake` | NTL | number theory, LLL, polynomial factoring |
| `FindFlint.cmake` | FLINT | fast finite-field linear algebra |
| `FindFactory.cmake` | Factory | polynomial GCD / factoring |
| `FindFrobby.cmake` | Frobby | monomial-ideal operations |
| `FindNormaliz.cmake` | Normaliz | rational cone / polytope ops |
| `FindNauty.cmake` | nauty | graph isomorphism (for packages) |
| `FindCDDLIB.cmake` | cddlib | polyhedral computations |
| `FindGLPK.cmake` | GLPK | linear programming |
| `FindEAntic.cmake` | E-ANTIC | exact real arithmetic |

Linear algebra / acceleration:

| Module | Library |
|---|---|
| `FindMathic.cmake` | Mathic — monomial ideal algorithms |
| `FindMathicgb.cmake` | Mathicgb — F4 GB engine |
| `FindMemtailor.cmake` | Memtailor — fast pool allocator |
| `FindTBB.cmake` | Intel TBB — threading |

System integration:

| Module | Library |
|---|---|
| `FindBDWGC.cmake` | Boehm-Demers-Weiser GC |
| `FindGDBM.cmake` | GDBM — key-value database |
| `FindJansson.cmake` | Jansson — JSON parsing |
| `FindReadline.cmake` | GNU Readline |
| `FindHistory.cmake` | Readline's history library |
| `FindFFI.cmake` | libffi — generic FFI |

Build tooling:

| Module | Tool |
|---|---|
| `FindSphinx.cmake` | Sphinx (for `docs/`) |

## Common shape

Each module:

```cmake
# FindFOO.cmake
find_path(FOO_INCLUDE_DIR foo/foo.h)
find_library(FOO_LIBRARY foo)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FOO
  REQUIRED_VARS FOO_INCLUDE_DIR FOO_LIBRARY
  VERSION_VAR FOO_VERSION)

if(FOO_FOUND AND NOT TARGET FOO::FOO)
  add_library(FOO::FOO UNKNOWN IMPORTED)
  set_target_properties(FOO::FOO PROPERTIES
    IMPORTED_LOCATION "${FOO_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${FOO_INCLUDE_DIR}")
endif()
```

Producing an imported target `FOO::FOO` that downstream
`target_link_libraries(...)` calls can use.

## Version checking

Many libraries have **version-specific bug fixes** M2 depends on.
Each `Find*.cmake` parses the library's header to extract its
version:

```cmake
file(READ "${FOO_INCLUDE_DIR}/foo/version.h" _vh)
string(REGEX MATCH "FOO_VERSION \"([0-9.]+)\"" _ "${_vh}")
set(FOO_VERSION "${CMAKE_MATCH_1}")
```

If the version is too old, the module reports
`FOO_FOUND=FALSE` and [`build-libraries.cmake`](file-build-libraries-cmake.md)
takes over.

## Used by

- [`file-check-libraries-cmake.md`](file-check-libraries-cmake.md)
  — runs each `Find*.cmake`.
- Downstream `CMakeLists.txt` files that `find_package(FOO
  REQUIRED)`.

## Related

- [`README.md`](README.md) — cmake/ overview.
- [`file-check-libraries-cmake.md`](file-check-libraries-cmake.md)
  — driver.
- [`file-build-libraries-cmake.md`](file-build-libraries-cmake.md)
  — fallback when these can't find the library.
- [`../libraries/README.md`](../libraries/README.md) — autotools
  per-library scripts.
