# External dependencies

This document catalogues **every external library or program**
M2 uses — what it's for, version constraints, what happens if
unavailable, and where in the M2 source it appears.

Pairs with [`BUILD.md`](BUILD.md) (the build pipeline that
fetches and links them) and
[`libraries/file-per-library-subdirs.md`](M2/libraries/file-per-library-subdirs.md)
(per-library build wrappers).

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Build](BUILD.md)

## Required (M2 won't build without these)

These are essential. Configure fails without them; no fallback.

### GMP — GNU Multi-Precision arithmetic

**What it does**: arbitrary-precision integers (`mpz_t`) and
rationals (`mpq_t`). The substrate every other math library
builds on.

**M2 uses**: every `aring-zz-gmp`, `aring-qq-gmp` file plus
pervasive use throughout the interpreter (`gmp.d`,
`ballarith.d`).

**Version**: 6.x recommended; 5.x typically works.

**Deep dives**:
[`e/file-aring-zz-gmp.md`](M2/Macaulay2/e/file-aring-zz-gmp.md),
[`e/file-aring-qq-gmp.md`](M2/Macaulay2/e/file-aring-qq-gmp.md),
[`d/file-gmp.md`](M2/Macaulay2/d/file-gmp.md).

**Glossary**: [GMP entry](GLOSSARY.md#external-libraries).

### MPFR — multi-precision floating point

**What it does**: arbitrary-precision floats with correct
rounding. The substrate for `RR`/`CC` rings.

**M2 uses**: `aring-RRR.{cpp,hpp}`, `aring-CCC.{cpp,hpp}`,
plus the user-level `RR`/`CC` types.

**Version**: 4.x recommended.

**Deep dives**:
[`e/file-aring-RRR.md`](M2/Macaulay2/e/file-aring-RRR.md),
[`e/file-aring-CCC.md`](M2/Macaulay2/e/file-aring-CCC.md).

### MPFI — multi-precision interval arithmetic

**What it does**: interval reals built on MPFR. Used for
certified-arithmetic results.

**M2 uses**: `aring-RRi.{cpp,hpp}`, `aring-CCi.{cpp,hpp}`,
plus the `ballarith.d` bindings.

**Version**: 1.5.x recommended.

**Deep dives**:
[`e/file-aring-RRi.md`](M2/Macaulay2/e/file-aring-RRi.md),
[`d/file-ballarith.md`](M2/Macaulay2/d/file-ballarith.md).

### FLINT — Fast Library for Number Theory

**What it does**: fast finite-field linear algebra, polynomial
arithmetic over `ZZ`/`QQ`/`Z/p`/`GF(p^n)`. Merged Arb (interval
arithmetic, formerly separate) in FLINT 3.x.

**M2 uses**: `aring-zz-flint`, `aring-qq-flint`,
`aring-zzp-flint`, `aring-gf-flint*`, plus `dmat-lu-zzp-flint`
for fast linear algebra. The single most-used external library
in the engine.

**Version**: 3.x. FLINT 2.x worked historically; M2 has migrated
to 3.x with Arb merged.

**Deep dives**:
[`e/file-aring-zz-flint.md`](M2/Macaulay2/e/file-aring-zz-flint.md),
[`e/file-aring-qq-flint.md`](M2/Macaulay2/e/file-aring-qq-flint.md),
[`e/file-aring-zzp-flint.md`](M2/Macaulay2/e/file-aring-zzp-flint.md),
[`e/file-aring-gf-flint.md`](M2/Macaulay2/e/file-aring-gf-flint.md),
[`e/file-aring-gf-flint-big.md`](M2/Macaulay2/e/file-aring-gf-flint-big.md),
[`e/file-dmat-lu-variants.md`](M2/Macaulay2/e/file-dmat-lu-variants.md).

**Submodule**:
[`M2/submodules/file-submodules.md`](M2/submodules/file-submodules.md)
— FLINT is a git submodule.

### Boehm-Demers-Weiser GC (bdwgc)

**What it does**: M2's conservative garbage collector. Pervasive
— every `our_new_delete`-derived class, every `getmem` call.

**M2 uses**: see [`MEMORY.md`](MEMORY.md) Layer 1.

**Version**: 8.x.

**Submodule**: bdwgc is a git submodule
([`M2/submodules/file-submodules.md`](M2/submodules/file-submodules.md)).

### libgdbm — GNU DBM

**What it does**: simple key-value database. M2 uses it to store
the **documentation cache** (`rawdocumentation-<endian>-<wordsize>.db`).

**M2 uses**: `interface/m2-types.{h,cpp}`,
`gdbm_interface.c`.

**Deep dive**: [`d/file-c-glue.md`](M2/Macaulay2/d/file-c-glue.md)
(covers the C-side glue) +
[`DOCUMENTATION-SYSTEM.md`](DOCUMENTATION-SYSTEM.md) (consumer).

### Factory — polynomial factoring

**What it does**: multivariate polynomial GCD and factoring
over various coefficient rings.

**M2 uses**: `interface/factory.{h,cpp}` —
[`e/interface/file-factory-interface.md`](M2/Macaulay2/e/interface/file-factory-interface.md).

**Version**: 4.x.

### Boost (Stacktrace + Regex + Math + Multiprecision)

**What it does**:

- **`Boost.Stacktrace`** — crash-time backtraces in `bin/main.cpp`.
- **`Boost.Regex`** — M2's `match`/`replace`/`separate`
  (`d/regex.dd`).
- **`Boost.Math`** — special functions (`beta`, `erf`, `gamma`,
  ...) via `d/boostmath.dd`.
- **`Boost.Multiprecision`** — wraps MPFR for `Boost.Math`.

**Deep dives**:
[`d/file-regex-dd.md`](M2/Macaulay2/d/file-regex-dd.md),
[`d/file-boostmath.md`](M2/Macaulay2/d/file-boostmath.md),
[`bin/file-main.md`](M2/Macaulay2/bin/file-main.md).

### BLAS + LAPACK

**What it does**: dense numerical linear algebra over `double`
and `complex<double>`.

**M2 uses**: floating-point matrix paths in `dmat.cpp`,
`dmat-lu-*.cpp`, NAG.

**Variants**: M2 detects (in priority order):
- Apple Accelerate (macOS).
- OpenBLAS.
- ATLAS.
- Generic `-lblas -llapack`.

**Deep dive**: [`e/file-lapack.md`](M2/Macaulay2/e/file-lapack.md).

### NTL — Victor Shoup's number theory library

**What it does**: `ZZ` arithmetic with specific optimisations,
LLL lattice reduction, polynomial factoring over `ZZ`.

**M2 uses**: `ntl-internal.cpp`, `ntl-debugio.cpp`, plus the
LLL path.

**Deep dives**:
[`e/file-ntl-glue.md`](M2/Macaulay2/e/file-ntl-glue.md),
[`e/file-LLL.md`](M2/Macaulay2/e/file-LLL.md).

### memtailor + mathic + mathicgb

**What they do**: Stillman/Roune's trio:

- **memtailor** — custom memory allocator for the others.
- **mathic** — generic data-structure templates.
- **mathicgb** — signature-based GB engine.

**M2 uses**: optional GB backend (selectable via `Strategy =>`).

**Submodules**: all three are M2's git submodules
([`M2/submodules/file-submodules.md`](M2/submodules/file-submodules.md)).

### FFLAS-FFPACK + Givaro

**What they do**: ultra-optimised finite-field BLAS-style linear
algebra.

- **Givaro** — generic finite-field arithmetic.
- **FFLAS-FFPACK** — BLAS/LAPACK-style routines on top of Givaro.

**M2 uses**: `aring-zzp-ffpack`, `dmat-lu-zzp-ffpack`. Fastest
dense linear algebra over `Z/p`.

**Deep dives**:
[`e/file-aring-zzp-ffpack.md`](M2/Macaulay2/e/file-aring-zzp-ffpack.md),
[`e/file-dmat-lu-variants.md`](M2/Macaulay2/e/file-dmat-lu-variants.md).

**Submodules**: fflas_ffpack + givaro both submoduled.

## Optional libraries (M2 builds without them, with reduced functionality)

These are detected at configure time. If absent, the relevant
M2 feature degrades gracefully or errors informatively.

### MPSolve — polynomial root finder

**What it does**: certified roots of univariate polynomials over
`CC` to arbitrary precision.

**M2 uses**: `interface/polyroots.cpp` — implements `rawRoots`.

**Without**: `rawRoots` returns an error.

**Deep dive**: [`e/interface/file-polyroots.md`](M2/Macaulay2/e/interface/file-polyroots.md).

### Frobby — monomial ideal computations

**What it does**: fast algorithms on monomial ideals (intersect,
primary decomposition, Hilbert).

**M2 uses**: some monomial-ideal operations.

**Without**: M2 falls back to slower pure-M2 implementations.

### Normaliz — affine monoid / cone computations

**What it does**: lattice-points-in-cones, semigroup
computations.

**M2 uses**: the `Normaliz` package wraps it.

**Without**: the `Normaliz` package errors at use time.

### CDDlib — polyhedral computations

**What it does**: vertex enumeration, half-space description
conversions, polyhedral algorithms.

**M2 uses**: the `Polyhedra` and related packages.

### MSolve — polynomial system solver

**What it does**: F5-style GB and real-root counting for
polynomial systems over `QQ`.

**M2 uses**: optional GB backend via the `MSolve` package.

### E-ANTIC — exact real arithmetic

**What it does**: algebraic numbers as exact entities.

**M2 uses**: niche; some research packages.

### nauty — graph isomorphism

**What it does**: canonical labellings and isomorphism testing
for graphs.

**M2 uses**: the `Graphs` user package.

### GLPK — linear programming

**What it does**: simplex / interior-point LP.

**M2 uses**: `interface/cone.cpp` for cone-dimension checks; the
`Polyhedra` package.

### fplll — lattice basis reduction

**What it does**: LLL / BKZ on integer lattices.

**M2 uses**: alternative `LLL M` backend (selectable via `Strategy`).

**Without**: M2 falls back to NTL's LLL.

**Deep dive**: [`e/file-fplll.md`](M2/Macaulay2/e/file-fplll.md).

### TBB — Intel Threading Building Blocks

**What it does**: task-graph parallelism, `parallel_for`,
concurrent containers.

**M2 uses**: Schreyer-resolution dependency-graph cell scheduling.

**Without**: M2 falls back to sequential resolution. M2 still
works, just sequential where it could be parallel.

**Deep dive**: [`e/file-m2tbb.md`](M2/Macaulay2/e/file-m2tbb.md).

### libffi — generic foreign-function interface

**What it does**: dynamically call C functions from M2 with
runtime-known signatures.

**M2 uses**: the M2-level `ffi.dd` exposes this to user code.

**Deep dive**: [`d/file-ffi.md`](M2/Macaulay2/d/file-ffi.md).

### libxml2 — XML parsing

**What it does**: parses XML; used for MathML output and TeXmacs
frontend protocol.

**M2 uses**: `d/xml.d` plus `m2/file-mathml.md`.

**Deep dive**: [`d/file-xml.md`](M2/Macaulay2/d/file-xml.md).

### Jansson — JSON parsing

**What it does**: JSON parse / serialise.

**M2 uses**: `d/json.d` for the M2-level JSON API.

**Deep dive**: [`d/file-json.md`](M2/Macaulay2/d/file-json.md).

### Python (CPython)

**What it does**: embedded Python interpreter callable from M2.

**M2 uses**: `d/python.d` exposes Python to M2 user code.

**Without**: the `PythonInterpreter` package is unavailable.

**Deep dive**: [`d/file-python.md`](M2/Macaulay2/d/file-python.md).

### MySQL client

**What it does**: connects to MySQL databases.

**M2 uses**: `d/mysql.d` exposes a low-level MySQL client API.

**Without**: M2 builds `d/mysqldummy.d` instead — calls return
errors.

**Deep dive**: [`d/file-mysql.md`](M2/Macaulay2/d/file-mysql.md).

### Readline + history

**What they do**: line-editing in the REPL.

**M2 uses**: the interactive prompt.

**Without**: M2 falls back to plain stdin (no line editing).

### eigen — C++ linear algebra

**What it does**: dense linear algebra in C++ templates.

**M2 uses**: some matrix paths use eigen for eigenvalue
computations.

**Deep dive**: [`e/file-eigen.md`](M2/Macaulay2/e/file-eigen.md)
(if present in your tree).

## Optional programs (called via `run` / `getRun`)

These are **external executables** rather than libraries. M2
spawns them via OS process creation; they communicate via files
or stdin/stdout.

### 4ti2 — integer programming / toric ideals

**M2 uses**: the `FourTiTwo` package.

### gfan — Gröbner fans / tropical varieties

**M2 uses**: the `gfanInterface` package.

### Bertini — homotopy continuation

**M2 uses**: the `Bertini` package.

### CohomCalg — cohomology of toric varieties

**M2 uses**: the `CohomCalg` package.

### CSDP — semidefinite programming solver

**M2 uses**: the `SemidefiniteProgramming` package.

### lrslib — vertex / facet enumeration

**M2 uses**: the `Polyhedra` package via `lrs`/`redund`.

### Polymake — polytope research

**M2 uses**: the `Polymake` user package.

### TOPCOM — triangulations

**M2 uses**: the `Topcom` package.

## Build-time tools

These run during the build but aren't linked into M2.

### Bison + Flex

**What they do**: parser/lexer generators.

**M2 uses**:
- [`c/grammar.y`](M2/Macaulay2/c/file-grammar.md) (the `.d`
  language).
- [`html-check-links/grammar.y`](M2/Macaulay2/html-check-links/file-grammar.md)
  + `lex.l`.

**Version**: any modern.

### autotools (autoconf, automake, libtool)

**What it does**: generates `configure` from `configure.ac`.

**M2 uses**: the autotools build system (parallel to CMake).
Triggered by `autogen.sh`
([`M2/file-autogen-sh.md`](M2/file-autogen-sh.md)).

**Version**: autoconf 2.69+.

### CMake

**What it does**: alternative build system (preferred).

**Version**: 3.24+. See
[`M2/file-CMakeLists-txt.md`](M2/file-CMakeLists-txt.md).

### Doxygen + Sphinx + Breathe

**What they do**: generate C++ engine API documentation.

**M2 uses**: opt-in `docs` target — see
[`M2/Macaulay2/docs/`](M2/Macaulay2/docs/README.md).

**Without**: the engine API docs aren't built. M2 itself works
fine.

### ccache (optional)

**What it does**: caches C/C++ compile output.

**M2 uses**: auto-detected and wired in during phase 1 of build
([`BUILD.md`](BUILD.md)).

**Without**: builds take longer; everything else works.

### Google Test (gtest)

**What it does**: C++ test framework.

**M2 uses**: `e/unit-tests/` test binary.

**Submodule**: googletest is M2's submodule.

## Vendored

Some libraries M2 vendors rather than detect:

| Library | Why vendored |
|---|---|
| **mpreal** (`e/mpreal.h`) | Single-header MPFR C++ wrapper; not packaged separately |
| **Hanson's table** (`e/table.h`) | From David Hanson's *C Interfaces and Implementations*, 1996 |
| **autoconf-archive ax_*.m4** macros | `M2/m4/ax_*.m4` — pinned versions for reproducibility |

See [`e/file-mpreal.md`](M2/Macaulay2/e/file-mpreal.md),
[`e/file-exptable-table.md`](M2/Macaulay2/e/file-exptable-table.md),
[`m4/file-autoconf-archive.md`](M2/m4/file-autoconf-archive.md).

## Decision tree: required vs optional

```
Is the library used by Core M2 (every session needs it)?
  yes → REQUIRED (GMP, MPFR, FLINT, BDWGC, ...)
  no
    │
Does failure mean M2 errors at use time?
      yes → OPTIONAL with informative error (MPSolve, fplll, ...)
      no
        │
Does M2 silently degrade?
          yes → OPTIONAL with graceful fallback (TBB, Readline, fplll → NTL, ...)
          no → OPTIONAL with package-level error (Normaliz, gfan, ...)
```

## How M2 finds libraries

```
1. Configure tries every Find*.cmake
   (cmake/file-find-cmakes.md) → produces NAME_FOUND variables.

2. If a required library isn't found:
   - CMake: error and stop
   - autotools: same

3. If an optional library isn't found:
   - CMake: set NAME_FOUND=FALSE; the feature is disabled
   - autotools: similar

4. If --DBUILD_LIBRARIES="X Y Z" is set:
   - Skip detection; force build-from-source.

5. build-libraries phase (BUILD.md phase 3) builds any missing
   library from source via libraries/<lib>/Makefile.in (autotools)
   or build-libraries.cmake (CMake).
```

Configure-time detection details:
[`cmake/file-check-libraries-cmake.md`](M2/cmake/file-check-libraries-cmake.md).
Build-from-source fallback:
[`cmake/file-build-libraries-cmake.md`](M2/cmake/file-build-libraries-cmake.md)
or
[`libraries/file-per-library-subdirs.md`](M2/libraries/file-per-library-subdirs.md).

## Version drift

M2 is sensitive to **specific versions** of math libraries.
GMP/MPFR/FLINT all have had API/ABI breaks. The version assertions
in [`STARTUP.md`](STARTUP.md) Phase 4 catch these at startup.

To pin a specific version:

```sh
# Force build-from-source instead of using system version
cmake -DBUILD_LIBRARIES="GMP" ...
```

Compatible versions are pinned in `libraries/<lib>/Makefile.in`
or `cmake/build-libraries.cmake`.

## License compatibility

All required libraries are GPL/LGPL/permissive — compatible with
M2's GPL distribution. Specifically:

| Library | License |
|---|---|
| GMP | LGPL-3 |
| MPFR | LGPL-3 |
| MPFI | LGPL-3 |
| FLINT | LGPL-3 |
| BDWGC | MIT-style |
| Factory | GPL-3 |
| Boost | Boost license (permissive) |
| LAPACK/BLAS | BSD |
| NTL | GPL/LGPL |
| Givaro/FFLAS-FFPACK | LGPL-3 |
| mpreal | LGPL-3 |
| Bison/Flex | GPL (build tool — doesn't affect M2 license) |

The binary M2 is **GPL-3** because of the LGPL-3 libraries it
links against — see the license header in
[`README.md`](README.md).

## Used by

- Distribution maintainers checking version compatibility.
- Developers debugging "library not found" errors.
- Anyone wondering why a specific feature exists.

## Related

- [`README.md`](README.md) — repository TOC.
- [`BUILD.md`](BUILD.md) — how dependencies get found / built.
- [`GLOSSARY.md`](GLOSSARY.md#external-libraries) — terminology
  for each library.
- [`STARTUP.md`](STARTUP.md) — version assertions at startup
  (phase 4).
- [`MEMORY.md`](MEMORY.md) — how BDWGC integrates with M2.
- [`THREADING.md`](THREADING.md) — TBB's role.
- [`M2/libraries/file-per-library-subdirs.md`](M2/libraries/file-per-library-subdirs.md)
  — full per-library build wrappers (autotools).
- [`M2/cmake/file-find-cmakes.md`](M2/cmake/file-find-cmakes.md)
  — the 25 `Find*.cmake` modules.
- [`M2/submodules/file-submodules.md`](M2/submodules/file-submodules.md)
  — the 9 git submodules.
- [`M2/Macaulay2/d/`](M2/Macaulay2/d/README.md) — FFI bindings to
  the FFI-shape dependencies (Python, libxml2, MySQL, libffi,
  Jansson, Boost.Regex).
