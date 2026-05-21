# `CMakeLists.txt` — top-level CMake entry point

`CMakeLists.txt` at `M2/` is the **CMake build's entry point**.
Includes every other CMake module in [`cmake/`](cmake/README.md),
sets the C/C++ standards, and adds `Macaulay2/` as a subdirectory
to descend into.

Part of [the M2 source root](README.md).

[← M2/ overview](README.md) · [← repo root](../README.md)

## Header

```cmake
###############################################################################
## CMake is a cross-platform system for generating build environments using
## native tools such as Make and Ninja or IDEs such as Xcode and Visual Studio.
##
## See INSTALL-CMake.md for a comprehensive guide to building M2 using CMake.
## Take a look at cmake/configure.cmake for a list of options
## and read cmake/check-libraries.cmake for a list of requirements.
##
## Short instructions:
##  1. cmake -GNinja -S . -B BUILD/cmake
##  2. cmake --build BUILD/cmake --target build-libraries build-programs
##  2. cmake --build BUILD/cmake --target M2-engine M2-binary M2-core M2-emacs
##  2. cmake --build BUILD/cmake --target install-packages check-packages
##  6. cmake --install BUILD/cmake
###############################################################################

cmake_minimum_required(VERSION 3.24...3.30)
cmake_policy(VERSION 3.24)
cmake_policy(SET CMP0096 NEW) # preserve leading zeros in version number

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 11)
```

The header doubles as a **quick-start guide** — the four `cmake
--build` commands are exactly the CI sequence.

## Required CMake version

```cmake
cmake_minimum_required(VERSION 3.24...3.30)
```

- **3.24** is the minimum.
- **3.30** is the tested maximum. Newer versions may work but
  haven't been validated.

The version-range form (`X...Y`) means CMake will use policies
appropriate for `Y` if available, falling back to `X`.

## `cmake_policy(SET CMP0096 NEW)` — leading-zero preservation

```cmake
cmake_policy(SET CMP0096 NEW) # preserve leading zeros in version number
```

CMake by default strips leading zeros from version numbers
(`1.26.05` → `1.26.5`). M2's `VERSION` file uses zero-padded
months (`05` = May). This policy keeps the padding intact so
version strings match across the codebase.

## C++17 / C11

Currently:

```cmake
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 11)
```

The comment `Use the C++20 standard` in some places hints at an
in-flight C++20 migration. The README header at `defgroups.h`
([`Macaulay2/e/file-defgroups.md`](Macaulay2/e/file-defgroups.md))
also mentions this.

## What happens after the header

The rest of `CMakeLists.txt`:

1. **In-source-build refuse** — abort if `CMAKE_SOURCE_DIR ==
   CMAKE_BINARY_DIR`.
2. **Include each cmake/ module** in order: configure, prechecks,
   check-libraries, build-libraries, ...
3. **`add_subdirectory(Macaulay2)`** — descend into the source.
4. **Top-level targets** — define `M2-core`, `M2-engine`,
   `M2-binary`, etc.

The actual descent into source code is in `Macaulay2/CMakeLists.txt`,
which in turn descends into `Macaulay2/{c, d, e, m2, bin, system,
packages, ...}`.

## Used by

- The CMake build, invoked as `cmake -S M2 -B M2/BUILD/build`.

## Related

- [`README.md`](README.md) — M2/ overview.
- [`cmake/file-configure-cmake.md`](cmake/file-configure-cmake.md)
  — the configure module included first.
- [`file-configure-ac.md`](file-configure-ac.md) — autotools
  counterpart.
- [`file-VERSION.md`](file-VERSION.md) — version source the
  `CMP0096` policy preserves.
