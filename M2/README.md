# `M2/` — Macaulay2 source root

This directory is the top of the Macaulay2 source tree. Everything needed to
build M2 from source lives here. The repository root (one level up) is mostly
just a wrapper.

For installation instructions, see the
[Wiki](https://github.com/Macaulay2/M2/wiki).

---

## Layout

| Path | Purpose |
|---|---|
| [`Macaulay2/`](Macaulay2/README.md) | All source code — the heart of M2 |
| [`cmake/`](cmake/README.md) | CMake modules (`configure.cmake`, `Find*.cmake`, etc.) |
| [`libraries/`](libraries/README.md) | Per-library build wrappers used by the autotools build |
| [`submodules/`](submodules/README.md) | Git submodules for bundled libraries |
| [`distributions/`](distributions/README.md) | Packaging machinery (deb, rpm, dmg, tar) |
| [`include/`](include/README.md) | Generated and shared C/C++ headers |
| [`files/`](files/README.md) | Auxiliary runtime files shipped with M2 |
| [`m4/`](m4/README.md) | Autoconf m4 macros |
| [`check-configure/`](check-configure/README.md) | Configure-time sanity checks |
| [`BUILD/`](BUILD/README.md) | Conventional out-of-tree build location (in-source builds are blocked) |
| `VERSION` | Single source of truth for the project version — [deep dive](file-VERSION.md) |
| `configure.ac` | Autoconf input — [deep dive](file-configure-ac.md) |
| `CMakeLists.txt` | CMake entry point — [deep dive](file-CMakeLists-txt.md) |
| `autogen.sh` | Bootstrap the autotools build — [deep dive](file-autogen-sh.md) |
| `Makefile.in` | Top-level autotools driver — [deep dive](file-Makefile-in.md) |
| `Makefile.doc-dist` | Documentation-only distribution Makefile — [deep dive](file-Makefile-doc-dist.md) |

**Coverage:** every top-level build-system entry point has a dedicated deep-dive doc.

## Build systems

Two **parallel** build systems coexist; pick one per build tree (they do not
share state):

- **CMake** (preferred): `cmake -GNinja -S M2 -B M2/BUILD/build`
- **autotools**: `cd M2/BUILD/build && ../../autogen.sh && ../../configure …`

Full build / test / lint commands are documented in the root
[`CLAUDE.md`](../CLAUDE.md) and tracked in CI by
`.github/workflows/test_build.yml`.

[← back to repository TOC](../README.md#repository-architecture-table-of-contents)
