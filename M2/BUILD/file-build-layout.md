# BUILD/ — staging-area conventions

`M2/BUILD/` is the **conventional location for build trees** and
release-artefact construction. Not a source directory — its
contents (apart from this doc and a few release-helper Makefiles)
are generated at build time.

Part of [`BUILD/`](README.md).

[← BUILD/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Why a dedicated dir

CMake refuses to do **in-source builds** (a guard in the top-level
`CMakeLists.txt`). Autotools doesn't refuse, but mixing
generated and source files makes `git status` chaotic.

`BUILD/` provides a known location away from the source tree:

```
M2/
├── Macaulay2/        ← sources
├── cmake/            ← sources
├── libraries/        ← sources
├── BUILD/            ← build trees go HERE
│   └── build/        ← canonical CI/dev build
└── ...
```

CI uses `M2/BUILD/build` exactly, so commands documented in
[`CLAUDE.md`](../../CLAUDE.md) work both locally and in CI.

## Subdirectory conventions

```
BUILD/
├── tarfiles/        downloaded library source archives (shared between trees)
├── docker/          Docker-based build images
├── rpm/             RPM build environment
├── build/           the typical "give it your CI a try" tree
├── <user>/          per-developer trees (anton, dan, david, frank, mahrud, mike)
└── README           historical note
```

The per-developer subdirs (`anton/`, `dan/`, ...) historically
contained per-developer Makefile customisations — different
warning levels, alternative compilers, profiling configs. Most
are now empty stubs; the convention is preserved.

## `tarfiles/` — shared download cache

When [`libraries/`](../libraries/README.md) downloads source
tarballs, they go into `BUILD/tarfiles/`. Sharing this dir across
multiple build trees means re-running `make fetch` only re-downloads
if the file is missing.

The `M2_SOURCE_URL = https://macaulay2.com/Downloads/OtherSourceCode`
constant in `cmake/build-libraries.cmake` and
`libraries/Makefile.library.in` points at the upstream cache; the
local copy lands here.

## `docker/`

Docker-based build / packaging recipes. The
[BUILD/docker/README.md](docker/README.md) lists subdirs for:

- **`ubuntu/`** — Compiling M2 in a clean Ubuntu container.
- **`debian/`** — `.deb` packaging.
- **`fedora/`** — `.rpm` packaging (Fedora).
- **`rhel/`** — `.rpm` packaging (Red Hat Enterprise).
- **`testbot/`** — GitHub Actions test runners.
- **`brew/`** — Homebrew bottle builder.
- **`nightly/`** — nightly-build test container.
- **`actions/`** — CI action testing.

Each is a `Dockerfile` plus build glue (`Makefile`, `README.md`).
The top-level `BUILD/docker/Dockerfile` is the **release demo**:
install from PPA, ready to go.

## `rpm/` — Fedora/RHEL packaging

```
BUILD/rpm/
├── Makefile
├── README.md
└── Macaulay2.spec
```

The `Macaulay2.spec` file (RPM spec) is heavily-commented and
declares M2's RPM build dependencies, build/install steps, and
metadata. The `Makefile` orchestrates running `rpmbuild` in a
Docker container.

This is **separate from** the autotools `make rpm` target — this
one builds proper Fedora-compatible RPMs from a clean container,
while the autotools target produces simpler RPMs from the local
host.

## Per-developer subdirs

```
BUILD/anton/ ─── BUILD/dan/ ─── BUILD/david/ ─── BUILD/frank/ ─── BUILD/mahrud/ ─── BUILD/mike/
```

Each was/is a personal sandbox for the named developer. Some are
empty (`anton/`, `david/`, `frank/`), some have actual content:

- **`dan/`** — multiple Makefiles for GCC variants
  (`Makefile.gcc9`, `Makefile.gcc10`, `Makefile.clang`),
  plus an overnight benchmark Makefile.
- **`mahrud/`** — utility scripts: changelog generator,
  package-review script, profiling helpers (`profiling.sh`,
  `simd_ops.sh`).
- **`mike/`** — `universal.txt` (macOS universal binary build
  notes).

## `build/` — historical README

Just contains an older plain-text `README` describing the
intended use. Generally:

```sh
mkdir -p M2/BUILD/build
cd M2/BUILD/build
../../configure   # autotools
# OR
cmake -GNinja -S .. -B .   # CMake
make
```

## Used by

- CI workflows referencing `M2/BUILD/build`.
- Release managers building packages.
- Individual developers building locally.

## Related

- [`README.md`](README.md) — BUILD/ overview.
- [`docker/`](docker/) — Docker-based builds.
- [`rpm/`](rpm/) — RPM packaging.
- [`../distributions/README.md`](../distributions/README.md) —
  autotools-side packaging.
- [`../cmake/file-misc-cmakes.md`](../cmake/file-misc-cmakes.md)
  — CPack-based packaging.
