# `configure.cmake` — top-level configure module

`configure.cmake` is the **top-level orchestration module** for
the CMake build. Aggregates user-facing options, sets up
installation layout, compiler flags, and feature detection.

Part of [`cmake/`](README.md).

[← cmake/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Five-section layout

```cmake
## This file has multiple sections:
#    1. Define configure options and cached variables
#    2. Detect and print information about build system
#    3. Define variables for installation directories and Macaulay2 Layout
#    4. Define compiler and linker flags and feature
#    5. Detect type sizes and existence of symbols, headers, and functions
```

### Section 1 — Cached options

The user-facing knobs. Each is a `set(... CACHE ...)` so it appears
in `ccmake` and can be overridden via `-D<NAME>=<VAL>`:

- **`CMAKE_BUILD_TYPE`** — `Debug` (default), `Release`,
  `RelWithDebInfo`, `MinSizeRel`.
- **`BUILD_TESTING`** — opt-in test builds.
- **`BUILD_DOCS`** — opt-in Sphinx docs.
- **`BUILD_SHARED`** — produce shared libraries.
- **`PACKAGES`** / **`PACKAGES_DEVEL`** — which packages to install.

A comment block at the top documents the build-type aliases:

```
# use CMAKE_BUILD_TYPE=Debug                 instead of DEBUG
# use CMAKE_BUILD_TYPE=Release               for releases
# use CMAKE_BUILD_TYPE=RelWithDebInfo        instead of PROFILING
# use CMAKE_BUILD_TYPE=RelMinSize            for minimized release
```

### Section 2 — Build-system probing

Detects:

- Host OS and architecture.
- Compiler (GCC vs Clang vs AppleClang) and its version.
- CMake version (must be ≥ 3.24).
- Whether `ccache` is available (auto-wired if so).

### Section 3 — Installation layout

Defines all `CMAKE_INSTALL_*` directories using `GNUInstallDirs`
plus M2-specific extras:

- `tail_*dir` — relative paths used by [`bin/M2.in`](../Macaulay2/bin/file-M2-in.md).
- `M2_INSTALL_*` — final destinations.

The "Macaulay2 Layout" is what `M2 --version` reports and
controls where the binary looks for `share/`, `lib/`, etc.

### Section 4 — Compiler / linker flags

- `-std=c++17` (with bump to `-std=c++20` planned).
- `-Wall -Wextra` (or `/W4` on MSVC).
- `-fPIC` for shared builds.
- Platform-specific tweaks (macOS Homebrew libc++ flags).
- LTO toggles where supported.

### Section 5 — Feature detection

`check_include_file`, `check_function_exists`,
`check_type_size` calls that populate `M2/config.h`:

- `HAVE_STDINT_H`, `HAVE_PTHREAD_H`, `HAVE_DLFCN_H`, ...
- `SIZEOF_VOID_P`, `SIZEOF_LONG`, ...
- `HAVE_BACKTRACE_SYMBOLS`, ...

These macros flow downstream into engine and interpreter code via
`#include <M2/config.h>`.

## Where this fits in

```
cmake -GNinja -S M2 -B M2/BUILD/build
   ↓
CMakeLists.txt (top-level)
   ↓ include(cmake/prechecks.cmake)
   ↓ include(cmake/configure.cmake)    ← THIS FILE
   ↓ include(cmake/check-libraries.cmake)
   ↓ include(cmake/build-libraries.cmake)
   ↓ add_subdirectory(Macaulay2)
```

Every section here runs before any source is touched.

## Used by

- The top-level `CMakeLists.txt`.
- All downstream CMakeLists in `Macaulay2/`.

## Related

- [`README.md`](README.md) — cmake/ overview.
- [`file-check-libraries-cmake.md`](file-check-libraries-cmake.md)
  — library detection.
- [`file-misc-cmakes.md`](file-misc-cmakes.md) — pre-check steps
  (`prechecks`), OS detection (`flavor`), packaging, profiling, etc.
- [`../include/`](../include/README.md) — where the resulting
  `config.h` lives.
