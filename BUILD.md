# Build pipeline

This document walks **end-to-end through M2's build** — from
`cmake -GNinja -S M2 -B M2/BUILD/build` to a working `M2`
executable. The build-time companion to [`STARTUP.md`](STARTUP.md)
(runtime). Both build systems (CMake preferred, autotools
parallel) are covered.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md) · [Testing](TESTING.md)

## The seven build phases

```
1. Configure       — detect compiler, libraries, options
2. Translate       — scc1 turns .d/.dd into .c/.cpp
3. Build libraries — fetch / build external libs if missing
4. Compile         — C/C++ files → .o objects
5. Link            — .o files + libraries → M2-binary
6. Install         — copy to install prefix
7. Optional steps  — install-packages, check-packages, docs
```

Each phase has CMake and autotools equivalents. The CMake side is
preferred; this doc describes it primarily and notes autotools
differences where they matter.

## Quick start

```sh
cmake -GNinja -S M2 -B M2/BUILD/build              # configure
cmake --build M2/BUILD/build --target \
    build-libraries build-programs                  # phase 3
cmake --build M2/BUILD/build --target \
    M2-core M2-emacs                                # main build
cmake --build M2/BUILD/build --target \
    install-packages check-packages                 # phase 7
cmake --install M2/BUILD/build                      # phase 6
```

By default the build type is `Debug`. For Release:

```sh
cmake -GNinja -S M2 -B M2/BUILD/build -DCMAKE_BUILD_TYPE=Release
```

Requires CMake ≥ 3.24, C++17, C11. The full requirement list and
expected library versions are detected during phase 1.

## Phase 1: Configure

**CMake source**:
[`M2/cmake/file-configure-cmake.md`](M2/cmake/file-configure-cmake.md),
[`M2/cmake/file-check-libraries-cmake.md`](M2/cmake/file-check-libraries-cmake.md).

**Autotools source**:
[`M2/file-configure-ac.md`](M2/file-configure-ac.md),
[`M2/file-autogen-sh.md`](M2/file-autogen-sh.md).

The configure phase:

1. **Detect compiler** — gcc / clang / AppleClang version,
   `ccache` (auto-wired if present).
2. **Set install layout** — `bindir`, `libdir`, `datadir`,
   `docdir`, `pre_*` paths (relative paths for the wrapper
   script — see
   [`bin/file-M2-in.md`](M2/Macaulay2/bin/file-M2-in.md)).
3. **Detect libraries** — run every
   [`Find<Lib>.cmake`](M2/cmake/file-find-cmakes.md) module
   (25 of them). Each tries to find a system-installed copy of
   GMP / MPFR / FLINT / NTL / Factory / FFPACK / GiVaro / ...
4. **Mark missing libraries for build-from-source** — failed
   detections go into the `BUILD_LIBRARIES` list.
5. **Feature detection** — `HAVE_*` macros for headers,
   functions, type sizes. These populate
   `M2/include/M2/config.h` via
   [`M2/include/file-configuration-in.md`](M2/include/file-configuration-in.md).
6. **Process every `.in` template** — `Makefile.in`,
   `startup.c.cmake`, `M2.in`, etc. become real files with
   `@VAR@` placeholders substituted.

**Output**: a populated `BUILD/build/` tree with Ninja / Makefile
rules. Re-running `cmake` is idempotent for caching.

**Failure modes**:

- "GMP version mismatch" — the configure detected a system GMP
  but its version is incompatible. Force build-from-source:
  `cmake -DBUILD_LIBRARIES="GMP MPFR" ...` (see
  [`build-libraries.cmake`](M2/cmake/file-build-libraries-cmake.md)).
- "compiler too old" — check the version requirements in
  [`configure.cmake`](M2/cmake/file-configure-cmake.md).
- Missing tools (autotools, automake, libtool) when running
  `autogen.sh`.

## Phase 2: Translate (`scc1`)

**CMake source**: [`M2/cmake/file-scc-cmake.md`](M2/cmake/file-scc-cmake.md).

**Translator source**:
[`M2/Macaulay2/c/architecture.md`](M2/Macaulay2/c/architecture.md).

For every `.d` / `.dd` file in
[`M2/Macaulay2/d/`](M2/Macaulay2/d/architecture.md), the build:

1. **Builds `scc1` first** — the translator binary in
   [`M2/Macaulay2/c/`](M2/Macaulay2/c/architecture.md).
2. **Runs `scc1`** on each `.d` / `.dd` to produce `.c` / `.cpp`
   plus a `.sig` (signature file) and `.dep` (Make
   dependencies).
3. **Chains dependencies** — files that `use foo;` depend on
   `foo.sig`. The
   [`scc.cmake`](M2/cmake/file-scc-cmake.md) module wires this.

**Build target**: implicit — the generated `.c` / `.cpp` files
appear in the build tree and become inputs to phase 4.

**Why this is separate**: without `scc1`, the rest of phase 4
couldn't compile because there are no `.c` / `.cpp` files for
the interpreter yet. Phase 2 must precede phase 4.

**Failure modes**:

- `scc1` syntax error in a `.d` file — points at the source line.
- Missing `.sig` for a `use foo;` — typically caused by stale
  build state; `cmake --build M2/BUILD/build --target clean` and
  rebuild.

## Phase 3: Build libraries (`build-libraries` target)

**CMake source**:
[`M2/cmake/file-build-libraries-cmake.md`](M2/cmake/file-build-libraries-cmake.md).

**Autotools source**:
[`M2/libraries/README.md`](M2/libraries/README.md) +
[`M2/libraries/file-Makefile-library-in.md`](M2/libraries/file-Makefile-library-in.md).

For each library marked as missing in phase 1:

1. **Download source tarball** from
   `https://macaulay2.com/Downloads/OtherSourceCode/` (mirrored
   for reproducibility) or fetch a submodule.
2. **Apply patches** if any (per-library patches kept in
   [`M2/libraries/<lib>/`](M2/libraries/file-per-library-subdirs.md)).
3. **Configure** the library (e.g., `./configure --prefix=...
   --without-X --with-Y`).
4. **Build** (`make -j$(nproc)`).
5. **Install** to a local prefix (`M2/BUILD/build/usr-host/`).

After phase 3 completes, **CMake reruns configure** — this time
the previously-missing libraries are found in
`M2/BUILD/build/usr-host/`, so they get imported as native
targets.

**Build target**: `build-libraries build-programs`.

**Optional**: `build-libraries` only runs if needed. On a
system with all libraries installed (Ubuntu with `apt install
libgmp-dev libmpfr-dev libflint-dev` and friends), this phase is
a near-no-op.

**Failure modes**: most often related to specific library
versions. The build wrapper in
[`M2/libraries/`](M2/libraries/README.md) has per-library
notes and patches.

## Phase 4: Compile

The bulk of build time. Both `.c` / `.cpp` files
(scc1-generated) and engine `.cpp` files (hand-written) compile
through the system C / C++ compiler.

Major build targets and their source directories:

| Target | What | Source |
|---|---|---|
| `scc1` | The `.d` / `.dd` translator | [`M2/Macaulay2/c/`](M2/Macaulay2/c/architecture.md) |
| `M2-interpreter` | Interpreter library | [`M2/Macaulay2/d/`](M2/Macaulay2/d/architecture.md) (scc1-translated) |
| `M2-engine` | Engine library | [`M2/Macaulay2/e/`](M2/Macaulay2/e/architecture.md) |
| `M2-supervisor` | Thread supervisor library | [`M2/Macaulay2/system/`](M2/Macaulay2/system/architecture.md) |
| `M2-binary` (or `M2.exe`) | The actual executable | [`M2/Macaulay2/bin/`](M2/Macaulay2/bin/README.md) |
| `M2-unit-tests` | gtest binary | [`M2/Macaulay2/e/unit-tests/`](M2/Macaulay2/e/unit-tests/README.md) |
| `M2-core` | M2-level "Core" package | [`M2/Macaulay2/m2/`](M2/Macaulay2/m2/architecture.md) |
| `M2-emacs` | Emacs grammar files | [`M2/Macaulay2/editors/`](M2/Macaulay2/editors/README.md) |

The compiler invocations use:

- `-std=c++17` for C++.
- `-std=c11` for C.
- `-O2` (Release) or `-O0 -g` (Debug).
- `-fPIC` for shared builds.
- Library include paths via `target_include_directories(...)`.
- The `<M2/config.h>` from phase 1 contains the `HAVE_*` flags.

**Failure modes**:

- "header not found" — phase 3 didn't install the library
  expected, or PATH for the include is wrong. Re-run cmake.
- "ABI mismatch" — different libraries with different ABIs were
  detected (e.g., system FLINT and system NTL with incompatible
  C++ standard library). Build problematic libs from source.

## Phase 5: Link

`M2-binary` is the final target — linking pulls together:

```
M2-binary
   ←  bin/main.o + bin/timestamp.o + startup.c (generated)
   ←  M2-interpreter (.a/.so)
   ←  M2-engine (.a/.so)
   ←  M2-supervisor (.a)
   ←  bdwgc, GMP, MPFR, MPFI, FLINT, NTL, Factory, ...
   ←  libffi, libxml2, libreadline, libgdbm, jansson, ...
   ←  pthread, dl, m
```

(Approximate — exact list per
[`M2/Macaulay2/bin/file-main.md`](M2/Macaulay2/bin/file-main.md).)

The wrapper script
[`bin/M2.in`](M2/Macaulay2/bin/file-M2-in.md) is templated at
install time to point at the right library directory.

**Failure modes**:

- "undefined symbol" — usually means a library was found at
  configure time but disappeared at link time. Check the build
  dir paths.
- "duplicate symbol" — two libraries provide the same symbol;
  most common when both system and bundled copies are linked.

## Phase 6: Install

```sh
cmake --install M2/BUILD/build
```

(Or `make install` for autotools.)

Copies the binary and supporting files to the install prefix
(default `/usr/local`):

| Component | Destination |
|---|---|
| `M2-binary` | `${bindir}/M2-binary` |
| Wrapper `M2` (script) | `${bindir}/M2` |
| Bundled libraries | `${libdir}/<distro-specific-path>/` |
| Package source `.m2` | `${datadir}/Macaulay2/` |
| Package HTML docs | `${docdir}/Macaulay2/` |
| Info database | `${datadir}/info/Macaulay2/` |
| Man page | `${mandir}/man1/M2.1` |
| Emacs files | `${datadir}/emacs/site-lisp/Macaulay2/` |

The relative paths between binary and resources are baked into
the binary (via `startup.c`) at configure time. Once installed,
M2 finds its data without env variables.

**Failure modes**: typical Linux/macOS install issues — needs
write permission to the prefix, `install -d` failures, etc.

## Phase 7: Optional steps

### `install-packages` — package installation

```sh
cmake --build M2/BUILD/build --target install-packages
```

For each distributed package
([`M2/Macaulay2/packages/`](M2/Macaulay2/packages/README.md)):

1. Run M2 with the package's source.
2. Execute every `Example ...` block, capturing output.
3. Render HTML docs from `doc ///...///` blocks.
4. Build the GDBM info database.

This is **slow** (~30 min for all ~400 packages) because every
example actually runs. The result is the bundled doc set users
get.

See [`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md).

### `check-packages` — package tests

```sh
cmake --build M2/BUILD/build --target check-packages
```

Runs every package's `TEST ///...///` blocks. See
[`TESTING.md`](TESTING.md) section 3.

### `docs` (developer-facing API docs)

```sh
cmake -DBUILD_DOCS=ON -GNinja -S M2 -B M2/BUILD/build
cmake --build M2/BUILD/build --target docs
```

Builds Sphinx + Doxygen API docs of the engine. See
[`docs/file-CMakeLists.md`](M2/Macaulay2/docs/file-CMakeLists.md).

### Tests (see [`TESTING.md`](TESTING.md))

Runs the unit tests, normal-tier regression, `--check N`,
ComputationsBook, etc.

### `package` (CPack distribution artifacts)

```sh
cmake --build M2/BUILD/build --target package
```

Produces `.tar.gz` / `.deb` / `.rpm` / `.dmg`. See
[`cmake/file-misc-cmakes.md`](M2/cmake/file-misc-cmakes.md) and
[`distributions/file-distributions.md`](M2/distributions/file-distributions.md).

## Two build systems coexist

Both produce the same final M2 binary. They share `M2/VERSION`,
`M2/include/configuration.in`, and the per-library configure
options — but otherwise build independently.

| Aspect | CMake | autotools |
|---|---|---|
| Entry point | [`CMakeLists.txt`](M2/file-CMakeLists-txt.md) | [`configure.ac`](M2/file-configure-ac.md) |
| Configuration | [`cmake/configure.cmake`](M2/cmake/file-configure-cmake.md) | [`configure.ac`](M2/file-configure-ac.md) + [`m4/`](M2/m4/README.md) |
| Library detection | [`Find*.cmake`](M2/cmake/file-find-cmakes.md) | [`m4/ax_*.m4`](M2/m4/file-autoconf-archive.md) + autoconf checks |
| Library build | [`build-libraries.cmake`](M2/cmake/file-build-libraries-cmake.md) | [`libraries/<lib>/Makefile.in`](M2/libraries/file-per-library-subdirs.md) |
| Top-level driver | Ninja or make | `Makefile.in` |
| `scc1` invocation | [`scc.cmake`](M2/cmake/file-scc-cmake.md) | per-`.d` rules in `d/Makefile.in` |
| Startup baking | [`startup.cmake`](M2/cmake/file-startup-cmake.md) | shell-script driven |
| Packaging | CPack | [`distributions/Makefile.in`](M2/distributions/file-distributions.md) |
| CI usage | Ubuntu + macOS | Ubuntu + macOS |

In CI all four combinations run. Locally, pick one and commit to
it — mixing CMake and autotools builds in the same `M2/BUILD/`
subdirectory corrupts the build state.

## Build artifacts catalogue

```
M2/BUILD/build/                    ← build root
├── M2/Macaulay2/
│   ├── c/scc1                     ← translator binary
│   ├── d/*.c, *.cpp, *.sig        ← scc1-generated
│   ├── d/M2-interpreter.a         ← static lib
│   ├── e/M2-engine.a              ← static lib
│   ├── e/unit-tests/M2-unit-tests ← test binary
│   ├── system/M2-supervisor.a     ← static lib
│   ├── bin/M2-binary              ← final executable
│   ├── bin/M2                     ← wrapper script
│   ├── bin/startup.c              ← generated
│   ├── m2/*.m2                    ← installed Core sources
│   └── editors/M2-symbols.el      ← generated grammar
├── usr-host/                       ← built-from-source libs
│   ├── lib/libgmp.a (if built)
│   ├── lib/libmpfr.a (if built)
│   └── ...
├── doxygen/                        ← if BUILD_DOCS=ON
└── CMakeCache.txt                  ← CMake state
```

## Incremental rebuilds

After editing source:

| Change | Recompile minimum |
|---|---|
| One `.cpp` file in `e/` | the affected object, then re-link M2-binary |
| One `.d` file in `d/` | scc1 reruns on it, then the resulting `.c` recompiles, then re-link |
| One `.m2` file in `m2/` | nothing in the binary — just copy / regenerate startup.c |
| `M2/VERSION` | re-run cmake (configure phase), then full rebuild |
| `configure.ac` (autotools) | re-run `autogen.sh && configure`, then full rebuild |
| A `cmake/*.cmake` module | re-run `cmake`, then incremental rebuild |

`ccache` (auto-detected) makes most C / C++ recompiles instant
after the first build.

## Build timing

Approximate on a modern 8-core machine:

| Phase | Time |
|---|---|
| Phase 1 (configure, all libs installed) | ~30 sec |
| Phase 1 (configure, libs missing) | ~30 sec + phase 3 |
| Phase 2 (scc1 build + translate) | ~1 min |
| Phase 3 (build-libraries, all sources) | ~30-60 min |
| Phase 4 (compile, full) | ~5-10 min |
| Phase 4 (compile, incremental) | seconds |
| Phase 5 (link) | ~10 sec |
| Phase 6 (install) | ~10 sec |
| Phase 7 (install-packages) | ~30 min |

Total from-scratch on a clean system: ~30 min (if libs are
installed) or ~1.5 h (if building all libs from source).

## Build options to know

```sh
-DCMAKE_BUILD_TYPE=Debug|Release|RelWithDebInfo|MinSizeRel
-DBUILD_SHARED=ON|OFF                 # shared vs static engine
-DBUILD_TESTING=ON|OFF                # test targets
-DBUILD_DOCS=ON|OFF                   # Sphinx + Doxygen
-DBUILD_LIBRARIES="X Y Z"             # force these libs from source
-DBUILD_PROGRAMS="A B C"              # force these programs from source
-DCOVERAGE=ON                         # code coverage instrumentation
-DPACKAGES="Foo Bar"                  # restrict installed packages
```

Full list:
[`cmake/file-configure-cmake.md`](M2/cmake/file-configure-cmake.md).

## Debugging the build

If a build fails:

1. **Check the CI log** — `.github/workflows/test_build.yml`
   gives a known-working configuration. Match your local
   environment to it.
2. **Look at `CMakeCache.txt`** — the cached configure result.
   Any wrong path here propagates downstream.
3. **Increase verbosity** — `cmake --build M2/BUILD/build --target X --verbose`
   shows the actual compiler invocations.
4. **Re-run from scratch** — `rm -rf M2/BUILD/build` and start
   over. Mixed-state caches cause weird errors.
5. **Pin libraries** — use `-DBUILD_LIBRARIES="..."` to bypass
   tricky system installations.

## Used by

- New contributors getting set up.
- Distribution maintainers packaging M2.
- Anyone touching the build system.

## Related

- [`README.md`](README.md) — repository TOC.
- [`STARTUP.md`](STARTUP.md) — runtime walkthrough (build → run
  pair).
- [`TESTING.md`](TESTING.md) — testing reference (phase 7).
- [`MEMORY.md`](MEMORY.md) — memory model the build's compile
  flags affect.
- [`TOUR.md`](TOUR.md) — Path E (build maintainer) has more
  detail on packaging.
- [`M2/cmake/README.md`](M2/cmake/README.md) — CMake module
  catalogue.
- [`M2/libraries/README.md`](M2/libraries/README.md) — autotools
  library-build wrappers.
- [`M2/submodules/README.md`](M2/submodules/README.md) — vendored
  upstream sources.
- [`M2/file-CMakeLists-txt.md`](M2/file-CMakeLists-txt.md) /
  [`M2/file-configure-ac.md`](M2/file-configure-ac.md) — top-level
  build entry points.
- Project [Wiki](https://github.com/Macaulay2/M2/wiki) — install
  instructions for end users.
