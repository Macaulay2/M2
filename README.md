Macaulay2
=========

Macaulay2 is a system for computing in commutative algebra, algebraic geometry
and related fields.  The system was originally written by Dan Grayson and Mike
Stillman.  David Eisenbud joined the project a number of years ago, and many
users are writing packages for the system, and some are contributing source
code.  See our web page [Macaulay2.com](https://macaulay2.com/) for more details and for
downloading binary releases.

The structure of this directory is as follows:

* `M2`: contains everything needed by a user to build Macaulay2.

See `CITATION.cff` for information about citing Macaulay2.

### Contributions

Contributions to the code of Macaulay2 are welcome.
The source code is available via our GitHub [repository](https://github.com/Macaulay2/M2),
where you can also report bugs via the [issue tracker](https://github.com/Macaulay2/M2/issues).
For brief instructions, see [here](https://github.com/Macaulay2/M2/wiki/Git-for-Macaulay2-Contributors).

To start working on an existing github "issue", volunteer to work on it, so
you can get "assigned" to the issue, thereby preventing duplication of
effort.

To make a contribution, submit a "pull request" on github.  If the
contribution involves changing an existing package in a non-trivial way, we
will normally contact the authors to get their approval of the change.  If a
new package with mathematical content is submitted, it will normally be
accepted if it can be installed with `installPackage` and the tests pass as
determined by `check`, in the latest version of Macaulay2.

---

## Repository Architecture: Table of Contents

This section is a navigable map of the Macaulay2 source tree. Every directory
listed below either has, or will have, its own `README.md` describing its
purpose, contents, and how it fits into the build. Follow the links to drill
down. Entries marked _(pending)_ do not yet have a per-directory README — they
will be added as this documentation effort proceeds.

### The four-language stack

Macaulay2 is compiled in layers. Reading the architecture in compilation order
is the fastest way to orient yourself:

```
.d / .dd        ──scc1──▶   .c / .cpp     ──C/C++──▶   M2-interpreter ──▶ M2
   │                            │                            ▲
   │                            │                            │ linked
   │                            │                        M2-engine (C++)
[Macaulay2/d/]              [generated]                  [Macaulay2/e/]
   ▲
   │ defines the language scc1 reads
[Macaulay2/c/]
```

| Layer | Directory | Role |
|---|---|---|
| 1. Translator | [`M2/Macaulay2/c/`](M2/Macaulay2/c/README.md) _(pending)_ | The `scc1` compiler-compiler that turns `.d`/`.dd` into C/C++ |
| 2. Interpreter | [`M2/Macaulay2/d/`](M2/Macaulay2/d/README.md) _(pending)_ | Lexer, parser, evaluator, FFI bindings — produces `M2-interpreter` |
| 3. Engine | [`M2/Macaulay2/e/`](M2/Macaulay2/e/README.md) _(pending)_ | C++ math kernel: rings, matrices, Gröbner bases, resolutions |
| 4. Core M2 | [`M2/Macaulay2/m2/`](M2/Macaulay2/m2/README.md) _(pending)_ | `.m2` files loaded at startup that define the Core package |

### Top level

| Path | Contents |
|---|---|
| [`M2/`](M2/README.md) | Source tree root (everything that matters lives here) |
| `VERSION` | Single source of truth for the project version |
| `CITATION.cff` | Citation metadata |

### Under `M2/`

| Path | Purpose |
|---|---|
| [`M2/Macaulay2/`](M2/Macaulay2/README.md) _(pending)_ | All Macaulay2 source code (see breakdown below) |
| [`M2/cmake/`](M2/cmake/README.md) _(pending)_ | CMake modules: `configure.cmake`, `check-libraries.cmake`, `build-libraries.cmake`, `Find*.cmake` |
| [`M2/libraries/`](M2/libraries/README.md) _(pending)_ | Per-library build wrappers used by the **autotools** build |
| [`M2/submodules/`](M2/submodules/README.md) _(pending)_ | Git submodules for bundled libraries (memtailor, mathic, mathicgb, bdwgc, flint, frobby, fflas-ffpack, givaro, googletest) |
| [`M2/distributions/`](M2/distributions/README.md) _(pending)_ | Packaging machinery (deb, rpm, dmg, tar); templates the end-user `INSTALL` |
| [`M2/include/`](M2/include/README.md) _(pending)_ | Generated and shared C/C++ headers |
| [`M2/files/`](M2/files/README.md) _(pending)_ | Auxiliary files bundled with the distribution |
| [`M2/m4/`](M2/m4/README.md) _(pending)_ | Autoconf m4 macros |
| [`M2/check-configure/`](M2/check-configure/README.md) _(pending)_ | Configure-time sanity checks |
| `M2/BUILD/` | Conventional out-of-tree build location (in-source builds are blocked) |

### Under `M2/Macaulay2/`

| Path | Purpose |
|---|---|
| [`M2/Macaulay2/c/`](M2/Macaulay2/c/README.md) _(pending)_ | `scc1` translator; the spec for the `.d` language lives in `c/README` |
| [`M2/Macaulay2/d/`](M2/Macaulay2/d/README.md) _(pending)_ | Interpreter sources (`.d`/`.dd`); FFI to Python, MySQL, libffi, XML, GMP, MPFR |
| [`M2/Macaulay2/e/`](M2/Macaulay2/e/README.md) _(pending)_ | Engine: ~340 C++ files. Public interface in `engine.h` + `e/interface/` |
| [`M2/Macaulay2/m2/`](M2/Macaulay2/m2/README.md) _(pending)_ | Core M2 sources; load order controlled by `loadsequence` |
| [`M2/Macaulay2/packages/`](M2/Macaulay2/packages/README.md) _(pending)_ | ~400 distributed packages; `=distributed-packages` lists what ships |
| [`M2/Macaulay2/bin/`](M2/Macaulay2/bin/README.md) _(pending)_ | Final `M2-binary` linkage and `startup.c` shim |
| [`M2/Macaulay2/system/`](M2/Macaulay2/system/README.md) _(pending)_ | Thread supervisor (`M2-supervisor`) |
| [`M2/Macaulay2/editors/`](M2/Macaulay2/editors/README.md) _(pending)_ | Editor grammar generation (prism, pygments, vim, emacs); `M2-emacs` submodule lives here |
| [`M2/Macaulay2/docs/`](M2/Macaulay2/docs/README.md) _(pending)_ | Sphinx config for the C++ engine developer docs |
| [`M2/Macaulay2/tests/`](M2/Macaulay2/tests/README.md) _(pending)_ | Top-level CTest suites: `engine`, `ComputationsBook`, `normal`, `slow`, `threads`, `rationality`, `gigantic`, `quarantine`, `goals` |
| [`M2/Macaulay2/man/`](M2/Macaulay2/man/README.md) _(pending)_ | Man pages |
| [`M2/Macaulay2/html-check-links/`](M2/Macaulay2/html-check-links/README.md) _(pending)_ | HTML link checker used by `make check` |

### Cross-cutting concerns

These topics span multiple directories — once the per-directory READMEs land,
they will cross-link to one another along these axes:

- **Adding an engine function:** `e/` → `e/interface/` → `d/<area>.dd` → `m2/<area>.m2` → `e/unit-tests/<area>.cpp`
- **Adding a package:** `packages/Foo.m2` (+ optional `packages/Foo/`) → append name to `packages/=distributed-packages` → register CMake-side deps in `packages/CMakeLists.txt` if it needs external libs
- **Build systems:** CMake (`M2/CMakeLists.txt` + `M2/cmake/`) and autotools (`M2/configure.ac` + `M2/libraries/`) run in parallel; either produces a working `M2`, but they do **not** share build state
- **Memory model:** Boehm GC throughout; `.d` uses `Type` / `atomicType`, C++ engine uses `our_new_delete` / `our_new_gc`

### Documentation status

This table of contents is the entry point for an ongoing effort to document
every subdirectory of the Macaulay2 source tree. Per-directory READMEs are
being filled in incrementally — see the `_(pending)_` markers above for what
remains. When a README is completed, the marker will be removed.

---

### Copyright

Copyright (C) 1993-2026 [The Macaulay2 Authors](
https://github.com/Macaulay2/M2/wiki/The-Macaulay2-Authors)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see https://www.gnu.org/licenses/.

Macaulay2 binaries are licensed under GPL-3.0 due to linking with LGPL-3.0 libraries (FLINT, MPFR).
See https://www.gnu.org/licenses/gpl-faq.html#AllCompatibility
