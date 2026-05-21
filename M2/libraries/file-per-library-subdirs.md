# Per-library subdirectories — conventions & catalogue

The 36 subdirectories of `libraries/` are **per-library build
wrappers** for the autotools build. Each contains one `Makefile.in`
following the template plus optional patch files. This doc
catalogues them by role.

Part of [`libraries/`](README.md).

[← libraries/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Library / program distinction

- **Library** — code linked directly into the `M2` binary
  (statically or dynamically).
- **Program** — code that runs as a separate executable, called
  from M2 via `run`/`getRun`.

Both live here but get packaged differently.

## Mathematical core (libraries)

| Subdir | Purpose | License |
|---|---|---|
| `gmp/` | Arbitrary-precision integers / rationals | LGPL |
| `mpfr/` | Arbitrary-precision reals | LGPL |
| `mpfi/` | Interval reals (on MPFR) | LGPL |
| `flint/` | Fast finite-field linear algebra; merged Arb | LGPL |
| `ntl/` | Shoup's number theory library | GPL/LGPL |
| `factory/` | Polynomial GCD / factorisation | GPL |
| `frobby/` | Monomial-ideal algorithms | GPL |
| `mpsolve/` | Polynomial root finding | GPL |
| `eigen/` | C++ linear algebra | MPL |
| `givaro/` | Finite-field generic arithmetic | LGPL |
| `fflas_ffpack/` | Finite-field BLAS / LAPACK | LGPL |
| `linbox/` | Exact linear algebra | LGPL |
| `lapack/` | Numerical linear algebra | BSD |
| `mathic/`, `mathicgb/`, `memtailor/` | Stillman/Roune monomial-ideal + F4 GB | LGPL |

## Optional analysis (libraries)

| Subdir | Purpose |
|---|---|
| `tbb/` | Intel TBB threading |
| `fplll/` | Lattice basis reduction |
| `mpsolve/` | Polynomial roots |

## System integration (libraries)

| Subdir | Purpose |
|---|---|
| `gc/` | Boehm-Demers-Weiser garbage collector |
| `gdbm/` | Key-value database for doc info |
| `readline/` | Line editing |

## Programs (run from M2)

| Subdir | What it provides |
|---|---|
| `4ti2/` | Integer programming / toric ideals |
| `cddlib/` | Polyhedral computations |
| `cohomcalg/` | Cohomology of toric varieties |
| `csdp/` | Semidefinite programming |
| `gfan/` | Gröbner fans / tropical varieties |
| `glpk/` | Linear programming |
| `lrslib/` | Vertex enumeration of polytopes |
| `msolve/` | Polynomial-system solver |
| `nauty/` | Graph isomorphism |
| `normaliz/` | Affine monoids / cones |
| `polymake/` | Polytope research |
| `topcom/` | Triangulations |

## Internal "libraries"

| Subdir | Purpose |
|---|---|
| `M2/` | A pseudo-library: builds M2 itself within the libraries system |
| `Macaulay2-docs/` | Pre-built docs as a "library" |
| `gtest/` | Google Test (build-time dependency for unit tests) |

## Conventions

Each subdir contains:

- **`Makefile.in`** — derived from
  [`Makefile.template`](file-Makefile-template.md). Sets
  `URL`, `VERSION`, `CONFIGOPTIONS`, `LICENSEFILES`, etc.
- **`patch-<version>` files** (optional) — M2-local patches
  applied during build.
- **No actual source** — the source comes from `URL` or the
  matching submodule in
  [`../submodules/`](../submodules/README.md).

A subdir with multiple `patch-X.Y.Z` files means M2 has carried
patches across several upstream releases — handy historical
artefact for tracking long-running fixes.

## Used by

- The autotools build orchestrated by
  [`file-Makefile-in.md`](file-Makefile-in.md).
- Developers adding patches or version bumps to a library.

## Related

- [`README.md`](README.md) — libraries/ overview.
- [`file-Makefile-library-in.md`](file-Makefile-library-in.md) —
  shared recipe each Makefile.in includes.
- [`file-Makefile-template.md`](file-Makefile-template.md) —
  starter template.
- [`../submodules/README.md`](../submodules/README.md) — git
  submodules supplying source for many of these.
- [`../cmake/file-find-cmakes.md`](../cmake/file-find-cmakes.md)
  — CMake equivalent (`Find*.cmake` modules).
