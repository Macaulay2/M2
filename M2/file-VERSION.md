# `VERSION` — single source of truth for the project version

`VERSION` is a one-line text file containing the M2 version
number. **Both build systems** (autotools and CMake) read this
file as the canonical version.

Part of [the M2 source root](README.md).

[← M2/ overview](README.md) · [← repo root](../README.md)

## The whole file

```
1.26.05
```

That's it. One line, the version number.

## Version format

`<major>.<year>.<month>` with the month zero-padded:

- **`1`** — major version. Unchanged for many years.
- **`26`** — year identifier (offset from a 2000-era epoch — not
  literally calendar year 2026).
- **`05`** — month (May), zero-padded.

The CMake build needs `cmake_policy(SET CMP0096 NEW)` to preserve
this leading zero (see
[`file-CMakeLists-txt.md`](file-CMakeLists-txt.md)).

## How it propagates

```
M2/VERSION
   ↓ read by
configure.ac (m4_esyscmd_s([cat VERSION]))
   ↓ becomes
PACKAGE_VERSION   (#define in config.h)
   ↓ becomes
M2_version()      (function in version.dd)
   ↓ visible to user as
version#"VERSION"  (M2 HashTable entry)

M2/VERSION
   ↓ read by
cmake/configure.cmake
   ↓ becomes
PROJECT_VERSION  (CMake variable)
   ↓ same downstream path
```

Both build systems pick up `VERSION` at configure time and bake
it into the generated `config.h`.

## When to bump

Release process:

1. Edit `VERSION` — change to the new number.
2. Update `Macaulay2/packages/=distributed-packages` if any
   package versions changed.
3. Update `M2/distributions/...` for release notes.
4. Re-run `autogen.sh` / CMake configure.
5. Tag the release.

Bumping is **only** done by release managers, not contributors.

## Why a separate text file?

Alternatives:

- **`AC_INIT([Macaulay2], [1.26.05], ...)`** in `configure.ac`
  directly. Loses CMake-side access; CMake would need to grep
  `configure.ac`.
- **`set(VERSION "1.26.05")`** in `CMakeLists.txt`. Loses
  autotools-side access; autoconf would need to parse CMake.
- **Generated from git tag.** Loses ability to bump version
  separately from tagging.

A single text file is the **simplest portable approach**: both
build systems can read it with `cat` / `file(READ)`.

## Used by

- `configure.ac` (autotools).
- `cmake/configure.cmake` (CMake).
- `distributions/dmg/...`, RPM spec, `.deb` control — packaging
  metadata.
- Indirectly, every part of M2 that reports a version string.

## Related

- [`README.md`](README.md) — M2/ overview.
- [`file-configure-ac.md`](file-configure-ac.md) — autotools
  consumer.
- [`file-CMakeLists-txt.md`](file-CMakeLists-txt.md) — CMake
  consumer.
- [`Macaulay2/d/file-version.md`](Macaulay2/d/file-version.md) —
  the `version` HashTable exposing this to user code.
- [`BUILD/rpm/file-rpm.md`](BUILD/rpm/file-rpm.md) — RPM packaging
  reads `VERSION`.
