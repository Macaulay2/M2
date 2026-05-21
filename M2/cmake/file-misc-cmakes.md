# `prechecks.cmake`, `flavor.cmake`, `darwin.cmake`, `packaging.cmake`, `coverage.cmake`, `profiling.cmake`, `latex.cmake`, `stackcollapse-m2.sh` — misc support

Small support modules. Each does one specific thing.

Part of [`cmake/`](README.md).

[← cmake/ overview](README.md) · [← top-level repo TOC](../../README.md)

## `prechecks.cmake` — lint / quality tool detection

```cmake
find_program(CLANG_TIDY		NAMES clang-tidy)
find_program(CLANG_FORMAT	NAMES clang-format)
find_program(CPPCHECK		NAMES cppcheck)
find_program(CPPLINT		NAMES cpplint)
find_program(IWYU		NAMES iwyu include-what-you-use)
find_program(VALGRIND		NAMES valgrind)

set(CLANG_TIDY_CHECKS
  -*,clang-analyzer-*,cppcoreguidelines-*,performance-*,modernize-*)
```

Detects optional dev tools and sets up CMake targets:

- **`clang-tidy-all`** runs the configured `CLANG_TIDY_CHECKS`.
- **`clang-format-all`** reformats according to project style.
- **`cppcheck-all`**, **`iwyu-all`** — additional static analysis.

If none of these tools are installed, the precheck just silently
skips them — the build still works.

## `flavor.cmake` — OS / distro detection

```cmake
## This file's single purpose is to set the ISSUE parameter
# with format [FLAVOR]-[RELEASE] which can be overriden
# with -DISSUE=Catch-22 and unset with -UISSUE

if(NOT DEFINED ISSUE)
  find_program(LSB_RELEASE	lsb_release)
  find_program(SW_VERS		sw_vers)
  ...
```

Determines the **OS flavour** (Ubuntu 22.04, macOS 15, Fedora 40,
...) for use in distribution-package metadata. Tries multiple
detection methods because no single one works across all UNIX
flavours:

- `lsb_release` — Linux Standard Base.
- `sw_vers` — macOS.
- `/etc/os-release` — modern systemd-based distros.
- `/etc/issue`, `/etc/system-release` — fallbacks.

The result lands in `${ISSUE}` and gets propagated into deb / rpm
package metadata.

## `darwin.cmake` — macOS cross-compile config

```cmake
set(CMAKE_SYSTEM_NAME 		Darwin)
set(CMAKE_SYSTEM_PROCESSOR	x86_64)

set(CMAKE_STAGING_PREFIX ${CMAKE_CURRENT_LIST_DIR}/usr-dist)
```

A **toolchain file** for cross-compiling from Linux to macOS.
Used by developers without an actual Mac handy who need to build
macOS binaries (via osxcross or similar).

Most macOS users build natively and never see this file.

## `packaging.cmake` — CPack configuration

```cmake
###############################################################################
## This script is responsible for generating CPackConfig.cmake in the build
## directory, which in turn is used by CPack to create distribution packages.

set(CPACK_SOURCE_IGNORE_FILES "BUILD")

set(CPACK_GENERATOR        "TGZ" CACHE STRING "package types to create")
set(CPACK_SOURCE_GENERATOR "TGZ" CACHE STRING "source package types to create")
```

CPack drives `.tar.gz`, `.deb`, `.rpm`, `.dmg` package
generation. The script sets up everything CPack needs:

- Package name, version, vendor, contact.
- Files to include / exclude.
- Per-generator tweaks (CPACK_DEBIAN_*, CPACK_RPM_*).

To build a deb after configure:

```sh
cmake --build . --target package -- CPACK_GENERATOR=DEB
```

## `coverage.cmake` and `profiling.cmake`

`coverage.cmake` adds `--coverage` flag and a `coverage` target
that runs tests + `lcov` to produce HTML coverage reports.

`profiling.cmake` enables `-pg` flag and a `profile` target
suited for `gprof` analysis. Less used now that `perf` /
`callgrind` are the preferred profilers.

## `latex.cmake`

Detects LaTeX for documentation builds that produce PDF (e.g.
the printable M2 reference). Opt-in; if no LaTeX is installed,
PDF output is skipped.

## `stackcollapse-m2.sh`

A `perf` post-processor — converts `perf script` output to the
"folded stacks" format that
[Brendan Gregg's FlameGraph](https://github.com/brendangregg/FlameGraph)
consumes. Used by developers profiling M2 with flame graphs.

## Used by

- The CMake build, when applicable features are enabled.

## Related

- [`README.md`](README.md) — cmake/ overview.
- [`file-configure-cmake.md`](file-configure-cmake.md) — main
  configuration that calls into these.
