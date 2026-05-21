# The 9 submodules — git submodules catalogue

The 9 subdirectories of `M2/submodules/` are **git submodules**
pointing at upstream repos for libraries M2 builds from source.
This doc catalogues them in detail.

Part of [`submodules/`](README.md).

[← submodules/ overview](README.md) · [← top-level repo TOC](../../README.md)

## The nine submodules

| Submodule | Upstream | Purpose in M2 | License |
|---|---|---|---|
| `bdwgc` | github.com/ivmai/bdwgc | The garbage collector, pervasive | MIT-style |
| `flint` | github.com/flintlib/flint | Number theory, finite-field linalg, Arb | LGPL |
| `frobby` | github.com/Macaulay2/frobby | Monomial-ideal algorithms | GPL |
| `fflas_ffpack` | github.com/linbox-team/fflas-ffpack | Finite-field BLAS/LAPACK | LGPL |
| `givaro` | github.com/linbox-team/givaro | Finite-field arithmetic | LGPL |
| `googletest` | github.com/google/googletest | C++ test framework for `e/unit-tests/` | BSD-3 |
| `mathic` | github.com/Macaulay2/mathic | Generic algebraic data structures | LGPL |
| `mathicgb` | github.com/Macaulay2/mathicgb | Signature-based GB | LGPL |
| `memtailor` | github.com/Macaulay2/memtailor | Custom allocator | LGPL |

## Why submodules

When a library lives upstream as a git repo, M2 has two choices:

1. **Download a release tarball** at build time
   (via [`libraries/`](../libraries/README.md)).
2. **Submodule the repo** here, so the source is in M2's tree.

Submodules win when:

- M2 maintains patches that don't yet exist upstream (need easy
  rebase).
- The upstream release cadence is slow but the upstream `main`
  has fixes M2 wants.
- M2 needs to **track specific commits**, not just versions.

Tarballs win when:

- The library is stable enough that version pinning is sufficient.
- The upstream isn't on git (rare nowadays).

## Three Macaulay2-owned submodules

The `Macaulay2/*` submodules — `frobby`, `mathic`, `mathicgb`,
`memtailor` — are projects M2 effectively owns:

- **frobby** — originally Bjarke H. Roune's monomial-ideal library;
  Macaulay2 forked to maintain.
- **mathic** / **mathicgb** / **memtailor** — Stillman & Roune's
  trio: data structures, F4 GB engine, custom allocator.

For these, M2 commits patches directly upstream; the submodule
pin is updated as the upstream advances.

## bdwgc — the GC

The Boehm-Demers-Weiser garbage collector is **pervasive** in M2:

- Engine objects (`our_new_gc`).
- Interpreter (`scc1`-emitted code uses `getmem` → `GC_malloc`).
- Supervisor (worker threads register with the GC).
- Even the html-check-links tool.

Vendoring as a submodule means M2 can pin a known-good bdwgc
version even when distros ship newer versions with regressions.

## flint — the big math lib

FLINT 3.x merged in Arb (interval arithmetic) and is now M2's
primary fast-arithmetic backend. M2 uses:

- `fmpz_*` / `fmpq_*` — fast `ZZ`/`QQ`.
- `nmod_*` — `Z/p`.
- `fq_nmod_*` — `GF(p^n)`.
- `arb_*` — interval reals.
- `nmod_mat_*` — Z/p matrices.

Submodule because M2 sometimes maintains in-flight patches
between flint releases.

## fflas_ffpack + givaro

These two are paired — fflas_ffpack provides finite-field BLAS
on top of givaro's generic-field arithmetic. Used for very fast
matrix operations over `Z/p` and `GF(p^n)` (frequently faster
than FLINT's nmod_mat in practice).

## googletest

The test framework for [`e/unit-tests/`](../Macaulay2/e/unit-tests/README.md).
Submoduled so the test build doesn't need a system gtest.

## `M2-emacs` is elsewhere

The `M2-emacs` submodule lives at
[`Macaulay2/editors/emacs/`](../Macaulay2/editors/README.md), not
here. Reason: it's "source" rather than "library" — it ships as an
emacs package, not as a linked library.

## Used by

- `git submodule update --init --recursive` initialises these.
- [`../libraries/file-per-library-subdirs.md`](../libraries/file-per-library-subdirs.md)
  — autotools build path.
- [`../cmake/file-build-libraries-cmake.md`](../cmake/file-build-libraries-cmake.md)
  — CMake build path.

## Related

- [`README.md`](README.md) — submodules/ overview.
- [`../libraries/README.md`](../libraries/README.md) — autotools
  build wrappers consume these.
- [`../cmake/README.md`](../cmake/README.md) — CMake-side equivalent.
