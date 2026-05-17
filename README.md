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
listed below has its own `README.md` describing its purpose, contents, and how
it fits into the build. Follow the links to drill down. The
[engine deep-dive](#engine-deep-dive-m2macaulay2e) below adds a second
level of detail for `M2/Macaulay2/e/` specifically.

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
| 1. Translator | [`M2/Macaulay2/c/`](M2/Macaulay2/c/README.md) | The `scc1` compiler-compiler that turns `.d`/`.dd` into C/C++ |
| 2. Interpreter | [`M2/Macaulay2/d/`](M2/Macaulay2/d/README.md) | Lexer, parser, evaluator, FFI bindings — produces `M2-interpreter` |
| 3. Engine | [`M2/Macaulay2/e/`](M2/Macaulay2/e/README.md) | C++ math kernel: rings, matrices, Gröbner bases, resolutions — see [deep-dive](#engine-deep-dive-m2macaulay2e) |
| 4. Core M2 | [`M2/Macaulay2/m2/`](M2/Macaulay2/m2/README.md) | `.m2` files loaded at startup that define the Core package |

### Top level

| Path | Contents |
|---|---|
| [`M2/`](M2/README.md) | Source tree root (everything that matters lives here) |
| `VERSION` | Single source of truth for the project version |
| `CITATION.cff` | Citation metadata |

### Under `M2/`

| Path | Purpose |
|---|---|
| [`M2/Macaulay2/`](M2/Macaulay2/README.md) | All Macaulay2 source code (see breakdown below) |
| [`M2/cmake/`](M2/cmake/README.md) | CMake modules: `configure.cmake`, `check-libraries.cmake`, `build-libraries.cmake`, `Find*.cmake` |
| [`M2/libraries/`](M2/libraries/README.md) | Per-library build wrappers used by the **autotools** build |
| [`M2/submodules/`](M2/submodules/README.md) | Git submodules for bundled libraries (memtailor, mathic, mathicgb, bdwgc, flint, frobby, fflas-ffpack, givaro, googletest) |
| [`M2/distributions/`](M2/distributions/README.md) | Packaging machinery (deb, rpm, dmg, tar); templates the end-user `INSTALL` |
| [`M2/include/`](M2/include/README.md) | Generated and shared C/C++ headers |
| [`M2/files/`](M2/files/README.md) | Auxiliary files bundled with the distribution |
| [`M2/m4/`](M2/m4/README.md) | Autoconf m4 macros |
| [`M2/check-configure/`](M2/check-configure/README.md) | Configure-time sanity checks |
| `M2/BUILD/` | Conventional out-of-tree build location (in-source builds are blocked) |

### Under `M2/Macaulay2/`

| Path | Purpose |
|---|---|
| [`M2/Macaulay2/c/`](M2/Macaulay2/c/README.md) | `scc1` translator; the spec for the `.d` language lives in `c/README` |
| [`M2/Macaulay2/d/`](M2/Macaulay2/d/README.md) | Interpreter sources (`.d`/`.dd`); FFI to Python, MySQL, libffi, XML, GMP, MPFR |
| [`M2/Macaulay2/e/`](M2/Macaulay2/e/README.md) | Engine: ~340 C++ files. Public interface in `engine.h` + `e/interface/` |
| [`M2/Macaulay2/m2/`](M2/Macaulay2/m2/README.md) | Core M2 sources; load order controlled by `loadsequence` |
| [`M2/Macaulay2/packages/`](M2/Macaulay2/packages/README.md) | ~400 distributed packages; `=distributed-packages` lists what ships |
| [`M2/Macaulay2/bin/`](M2/Macaulay2/bin/README.md) | Final `M2-binary` linkage and `startup.c` shim |
| [`M2/Macaulay2/system/`](M2/Macaulay2/system/README.md) | Thread supervisor (`M2-supervisor`) |
| [`M2/Macaulay2/editors/`](M2/Macaulay2/editors/README.md) | Editor grammar generation (prism, pygments, vim, emacs); `M2-emacs` submodule lives here |
| [`M2/Macaulay2/docs/`](M2/Macaulay2/docs/README.md) | Sphinx config for the C++ engine developer docs |
| [`M2/Macaulay2/tests/`](M2/Macaulay2/tests/README.md) | Top-level CTest suites: `engine`, `ComputationsBook`, `normal`, `slow`, `threads`, `rationality`, `gigantic`, `quarantine`, `goals` |
| [`M2/Macaulay2/man/`](M2/Macaulay2/man/README.md) | Man pages |
| [`M2/Macaulay2/html-check-links/`](M2/Macaulay2/html-check-links/README.md) | HTML link checker used by `make check` |

### Cross-cutting concerns

These topics span multiple directories — once the per-directory READMEs land,
they will cross-link to one another along these axes:

- **Adding an engine function:** `e/` → `e/interface/` → `d/<area>.dd` → `m2/<area>.m2` → `e/unit-tests/<area>.cpp`
- **Adding a package:** `packages/Foo.m2` (+ optional `packages/Foo/`) → append name to `packages/=distributed-packages` → register CMake-side deps in `packages/CMakeLists.txt` if it needs external libs
- **Build systems:** CMake (`M2/CMakeLists.txt` + `M2/cmake/`) and autotools (`M2/configure.ac` + `M2/libraries/`) run in parallel; either produces a working `M2`, but they do **not** share build state
- **Memory model:** Boehm GC throughout; `.d` uses `Type` / `atomicType`, C++ engine uses `our_new_delete` / `our_new_gc`

### Documentation status

This table of contents is the entry point for an ongoing effort to document
every subdirectory of the Macaulay2 source tree. Top-level per-directory
READMEs are complete; the [engine deep-dive](#engine-deep-dive-m2macaulay2e)
below is the next layer being filled in.

---

## Engine deep-dive: `M2/Macaulay2/e/`

The engine is the C++ mathematical kernel of Macaulay2. It is the largest and
oldest part of the codebase (~340 source files in `e/`) and the part most
people new to the project find hardest to navigate. This section is a guided
tour.

### Per-area docs (quick navigation)

For each top-level area of the engine, there is a dedicated markdown file
alongside the source. Use these as your entry point when you know which area
you care about.

| Area | Doc | Covers |
|---|---|---|
| Coefficient rings | [coefficient-rings.md](M2/Macaulay2/e/coefficient-rings.md) | `aring-*`, `ZZ`, `ZZp`, `GF`, `coeffrings` |
| Polynomial rings | [polynomial-rings.md](M2/Macaulay2/e/polynomial-rings.md) | `polyring`, `poly`, `qring`, `frac`, `weylalg`, `skewpoly`, `solvable`, `localring`, `BasicPoly*` |
| Monoids & monomials | [monoids-and-monomials.md](M2/Macaulay2/e/monoids-and-monomials.md) | `monoid`, `monorder`, `imonorder`, `varpower`, `ntuple`, `montable*`, `ExponentList`, `ExponentVector` |
| Matrices | [matrices.md](M2/Macaulay2/e/matrices.md) | `matrix*`, `dmat*`, `smat`, `mat-*`, `mutablemat*` |
| Free modules | [free-modules.md](M2/Macaulay2/e/free-modules.md) | `freemod`, `schorder` |
| Gröbner bases | [groebner-bases.md](M2/Macaulay2/e/groebner-bases.md) | `comp-gb*`, `gb-*`, `reducedgb*`, `gbring`, `gbweight`, `spair`, `mathicgb-interface` |
| Resolutions | [resolutions.md](M2/Macaulay2/e/resolutions.md) | `comp-res`, `res-a0*`, `res-a1*`, `res-a2*`, `Eschreyer`, `betti` |
| Other computations | [computations.md](M2/Macaulay2/e/computations.md) | `hilb`, `LLL`, `NAG`, `SLP*`, `assprime`, `monideal`, `comb` |
| Ring elements & maps | [ring-elements-and-maps.md](M2/Macaulay2/e/ring-elements-and-maps.md) | `relem`, `ringmap`, `M2FreeAlgebra*` |
| Utilities | [utilities.md](M2/Macaulay2/e/utilities.md) | `buffer`, `text-io`, `error`, `debug`, `overflow`, `MemoryBlock` |

Subdirectories (each with its own README): [`interface/`](M2/Macaulay2/e/interface/README.md), [`f4/`](M2/Macaulay2/e/f4/README.md), [`gb-f4/`](M2/Macaulay2/e/gb-f4/README.md), [`schreyer-resolution/`](M2/Macaulay2/e/schreyer-resolution/README.md), [`NCAlgebras/`](M2/Macaulay2/e/NCAlgebras/README.md), [`NCResolutions/`](M2/Macaulay2/e/NCResolutions/README.md), [`bibasis/`](M2/Macaulay2/e/bibasis/README.md), [`unit-tests/`](M2/Macaulay2/e/unit-tests/README.md), [`doxygen-settings/`](M2/Macaulay2/e/doxygen-settings/README.md).

### Architecture

The engine is best thought of as four concentric layers:

```
                ┌──────────────────────────────────────────────┐
                │   interface/    ←  public C entry points     │  ←—called from d/engine.dd
                ├──────────────────────────────────────────────┤
                │   Computation framework                       │
                │   (comp-gb, comp-res, hilb, LLL, NAG, …)      │
                ├──────────────────────────────────────────────┤
                │   Mathematical objects                        │
                │   (rings, monoids, matrices, free modules)    │
                ├──────────────────────────────────────────────┤
                │   Primitives                                  │
                │   (allocators, monomial encodings, GMP/FLINT) │
                └──────────────────────────────────────────────┘
```

- The **public C interface** in [`e/interface/`](M2/Macaulay2/e/interface/README.md)
  is the only surface the interpreter sees. It uses only plain C types and
  opaque pointers, so it can be called from `.d`/`.dd` code after translation
  by `scc1`. No file there is allowed to `#include "engine.h"`.

- The **Computation framework** lets long-running algorithms (Gröbner bases,
  resolutions, Hilbert series) be started, paused, resumed, and queried for
  partial results. Each algorithm subclasses an abstract Computation base; the
  interpreter holds onto an opaque pointer and drives it.

- The **mathematical-object layer** is the bulk of `e/`. Rings, monoids,
  matrices, and free modules each have a tower of files — abstract base, an
  `aring` polymorphic variant, and concrete specialisations per coefficient
  type or sparsity profile.

- The **primitives layer** is the boundary against external libraries
  (FLINT, GMP, MPFR, bdwgc) and the place where memory allocation, monomial
  encoding, and overflow-checked arithmetic live.

### Engine subdirectories

| Subdirectory | Purpose | README |
|---|---|---|
| `e/interface/` | Public C interface — every entry point reachable from the interpreter | [README](M2/Macaulay2/e/interface/README.md) |
| `e/f4/` | Original F4 Gröbner basis engine | [README](M2/Macaulay2/e/f4/README.md) |
| `e/gb-f4/` | Refactored F4 Gröbner basis engine | [README](M2/Macaulay2/e/gb-f4/README.md) |
| `e/schreyer-resolution/` | F4-style free resolution via Schreyer frames | [README](M2/Macaulay2/e/schreyer-resolution/README.md) |
| `e/NCAlgebras/` | Non-commutative free algebras & Gröbner bases | [README](M2/Macaulay2/e/NCAlgebras/README.md) |
| `e/NCResolutions/` | Non-commutative free resolutions | [README](M2/Macaulay2/e/NCResolutions/README.md) |
| `e/bibasis/` | Involutive (Janet) bases for Boolean rings | [README](M2/Macaulay2/e/bibasis/README.md) |
| `e/unit-tests/` | C++ gtest suite for the engine | [README](M2/Macaulay2/e/unit-tests/README.md) |
| `e/doxygen-settings/` | Doxygen config & styling for the developer API docs | [README](M2/Macaulay2/e/doxygen-settings/README.md) |

### Top-level file groups in `e/`

These are the files **at the top level** of `e/` (i.e. not in any
subdirectory). Click through to per-area READMEs as they are added; for now,
the grouping below is the navigation map.

#### Public interface

| Pattern | Description |
|---|---|
| `engine.h` | Legacy aggregating header used by older paths. New code should add narrower headers in [`interface/`](M2/Macaulay2/e/interface/README.md) |
| `x-*.cpp` | Older flat-layout entry points (e.g. `x-mat.cpp`, `x-gb.cpp`, `x-relem.cpp`). Being migrated into [`interface/`](M2/Macaulay2/e/interface/README.md) |

#### Coefficient rings (`aring-*`)

The abstract-ring (`aring`) framework gives every coefficient ring a uniform
template-friendly interface. One pair of files per coefficient type:

| File pair | Coefficient ring |
|---|---|
| `aring.{cpp,hpp}` | Abstract ring base + dispatcher |
| `aring-zz-gmp.{cpp,hpp}` | Integers via GMP |
| `aring-zz-flint.{cpp,hpp}` | Integers via FLINT |
| `aring-qq.{cpp,hpp}` (header only) | QQ abstract |
| `aring-qq-gmp.{cpp,hpp}` | Rationals via GMP |
| `aring-qq-flint.{cpp,hpp}` | Rationals via FLINT |
| `aring-zzp.{cpp,hpp}` | Z/p (generic) |
| `aring-zzp-flint.{cpp,hpp}` | Z/p via FLINT |
| `aring-zzp-ffpack.{cpp,hpp}` | Z/p via FFLAS-FFPACK |
| `aring-gf-flint.{cpp,hpp}` | Galois fields via FLINT (small) |
| `aring-gf-flint-big.{cpp,hpp}` | Galois fields via FLINT (big) |
| `aring-m2-gf.{cpp,hpp}` | Native M2 Galois field |
| `aring-RR.{cpp,hpp}` | RR (double) |
| `aring-RRR.{cpp,hpp}` | RR with arbitrary precision (MPFR) |
| `aring-RRi.{cpp,hpp}` | Real interval (Arb / MPFI) |
| `aring-CC.{cpp,hpp}` | Complex (double) |
| `aring-CCC.{cpp,hpp}` | Complex with arbitrary precision |
| `aring-CCi.{cpp,hpp}` | Complex interval |
| `aring-tower.{cpp,hpp}` | Iterated finite extensions |
| `aring-glue.hpp`, `aring-translate.hpp`, `aring-wrap.{cpp,hpp}` | Templates and adapters that connect `aring` to the legacy `Ring` API |
| `coeffrings.{cpp,hpp}` | Concrete coefficient-ring registry |

Concrete top-level ring files (predating `aring`, still used in many paths):
`ZZ.{cpp,hpp}`, `ZZp.{cpp,hpp}`, `GF.{cpp,hpp}`.

#### Polynomial rings and friends

| File | Purpose |
|---|---|
| `polyring.{cpp,hpp}` | Polynomial ring |
| `poly.{cpp,hpp}` | Polynomial value type |
| `qring.{cpp,hpp}` | Quotient ring |
| `frac.{cpp,hpp}` | Field of fractions |
| `weylalg.{cpp,hpp}` | Weyl algebra |
| `skewpoly.{cpp,hpp}` | Skew-commutative (exterior-like) polynomial ring |
| `solvable.{cpp,hpp}` | Solvable algebras |
| `localring.{cpp,hpp}` | Local rings |
| `schorder.{cpp,hpp}` | Schreyer orderings |
| `BasicPoly.{cpp,hpp}`, `BasicPolyList.{cpp,hpp}`, `BasicPolyListParser.{cpp,hpp}` | Lightweight polynomial value types used in newer GB code |
| `Polynomial.{cpp,hpp}`, `PolynomialStream.hpp` | Polynomial value / streaming abstraction |

#### Monoids and monomials

| File | Purpose |
|---|---|
| `monoid.{cpp,hpp}` | Monoid base |
| `monorder.{cpp,hpp}`, `imonorder.{cpp,hpp}` | Monomial orders, internal-monomial-order helpers |
| `montable.{cpp,hpp}`, `montableZZ.{cpp,hpp}` | Monomial lookup tables (over ZZ-coefficient case included) |
| `varpower.{cpp,hpp}`, `ntuple.{cpp,hpp}` | Two monomial encodings |
| `ExponentList.{cpp,hpp}`, `ExponentVector.hpp` | Variable-length and fixed-length exponent representations used in newer code |

#### Matrices

| File | Purpose |
|---|---|
| `matrix.{cpp,hpp}`, `matrix-con.{cpp,hpp}` | Standard immutable matrix |
| `matrix-kbasis.{cpp,hpp}`, `matrix-sort.{cpp,hpp}`, `matrix-symm.{cpp,hpp}`, `matrix-stream.{cpp,hpp}` | Various matrix operations (k-basis, sort, symmetrization, streaming) |
| `mat.hpp`, `mat-arith.hpp`, `mat-elem-ops.hpp`, `mat-linalg.hpp`, `mat-util.hpp`, `mat-jordan.hpp` | The generic dense-matrix template |
| `dmat.hpp`, `dmat-CCC-flint.{cpp,hpp}`, `dmat-LU.hpp`, `dmat-LU-template.hpp`, `dmat-lu-inplace.hpp`, … | Dense matrix specialisations |
| `smat.hpp` | Sparse matrix |
| `mutablecomplex.{cpp,hpp}`, `mutablemat.{cpp,hpp}` | Mutable variants |
| `VectorArithmetic.hpp` | Vector op helpers |

#### Free modules and resolutions

| File | Purpose |
|---|---|
| `freemod.{cpp,hpp}` | Free module |
| `comp.{cpp,hpp}` | Generic Computation base class |
| `comp-gb.{cpp,hpp}`, `comp-gb-declared.{cpp,hpp}`, `comp-gb-proxy.{cpp,hpp}` | Gröbner basis Computations |
| `comp-res.{cpp,hpp}` | Resolution Computations |
| `res-a0.{cpp,hpp}`, `res-a0-poly.{cpp,hpp}`, `res-a0-pair.hpp` | "Generation 0" resolution |
| `res-a1.{cpp,hpp}`, `res-a1-poly.{cpp,hpp}` | "Generation 1" resolution |
| `res-a2.{cpp,hpp}`, `res-a2-gb.cpp` | "Generation 2" resolution (drives GB internally) |
| `Eschreyer.{cpp,hpp}` | Schreyer-frame resolution (older sibling of `schreyer-resolution/`) |
| `betti.{cpp,hpp}` | Betti table |

#### Gröbner machinery (top-level)

| File | Purpose |
|---|---|
| `gbring.{cpp,hpp}` | Polynomial ring view tailored for GB arithmetic |
| `gbweight.{cpp,hpp}` | Weight orderings during GB |
| `spair.{cpp,hpp}` | S-pair data structure |
| `gb-default.{cpp,hpp}` | Default GB algorithm |
| `gb-homog2.{cpp,hpp}` | Homogeneous specialisation |
| `gb-sugarless.{cpp,hpp}` | "Sugarless" GB variant |
| `gb-toric.{cpp,hpp}` | Toric GB |
| `gb-walk.{cpp,hpp}` | Gröbner walk |
| `reducedgb.{cpp,hpp}` | Reduced GB base |
| `reducedgb-field.{cpp,hpp}`, `reducedgb-field-local.{cpp,hpp}` | Field-coefficient cases |
| `reducedgb-ZZ.{cpp,hpp}` | ZZ-coefficient case |
| `reducedgb-marked.{cpp,hpp}` | "Marked" GB (precomputed leading-term map) |
| `mathicgb-interface.{cpp,hpp}` | Bridge to the `mathicgb` library (submodule) |

#### Special computations

| File | Purpose |
|---|---|
| `hilb.{cpp,hpp}` | Hilbert function / series |
| `LLL.{cpp,hpp}` | LLL lattice reduction |
| `NAG.{cpp,hpp}`, `SLP.{cpp,hpp}`, `SLP-defs.hpp`, `SLP-imp.hpp` | Numerical algebraic geometry + straight-line programs |
| `assprime.{cpp,hpp}` | Associated primes |
| `monideal.{cpp,hpp}` (see also [`README-monideals.md`](M2/Macaulay2/e/README-monideals.md)) | Monomial ideal operations |
| `comb.{cpp,hpp}` | Combinatorial helpers |
| `cra.{cpp,hpp}` (in [`interface/`](M2/Macaulay2/e/interface/README.md)) | Chinese remainder algorithm |

#### Ring elements and ring maps

| File | Purpose |
|---|---|
| `relem.{cpp,hpp}` | RingElement |
| `ringmap.{cpp,hpp}` | Ring map / homomorphism |
| `M2FreeAlgebra.{cpp,hpp}`, `M2FreeAlgebraQuotient.{cpp,hpp}` | M2-facing wrappers over `NCAlgebras/` |

#### Utilities

| File | Purpose |
|---|---|
| `buffer.{cpp,hpp}` | Append-only byte buffer used for serialisation and pretty-printing |
| `text-io.{cpp,hpp}` | Text I/O helpers used by the buffer code |
| `error.{cpp,hpp}` | Engine error reporting |
| `debug.{cpp,hpp}` | Debug printing |
| `overflow.{cpp,hpp}` | Overflow-checked integer arithmetic used throughout monomial and degree code |
| `MemoryBlock.hpp` | Bump allocator for hot loops |
| `newdelete.hpp` (within subdirs) | GC-friendly `operator new`/`delete` |
| `random.{cpp,hpp}` (in [`interface/`](M2/Macaulay2/e/interface/README.md)) | RNG |

#### Style and notes

| File | Purpose |
|---|---|
| `STYLE.txt` | C++ formatting conventions for engine code |
| `README.md` | Engine navigation hub + historical notes |
| `README-monideals.md` | Monomial-ideal implementation notes |
| `TODO`, `TODO-numerics`, `TODO-rings-matrices`, `TODO-SLPs`, `TODO-reallocate-heap` | Long-running design TODOs |

### Cross-cutting flows

**Calling into the engine from M2 code:**

```
m2/foo.m2          calls a Core method
   ↓
d/foo.dd           interpreter binding in .dd
   ↓ engine.dd
e/interface/foo.{h,cpp}    public C entry point
   ↓
e/foo.{cpp,hpp}    internal C++ implementation
```

**Adding a new coefficient ring:**

1. New `aring-foo.{cpp,hpp}` modelled on an existing entry.
2. Register in `coeffrings.{cpp,hpp}`.
3. Add a concrete unit test in `unit-tests/ARingFooTest.cpp`.
4. Expose through `interface/aring.{h,cpp}`.

**Adding a new computation (GB variant, resolution algorithm, …):**

1. Subclass the appropriate Computation base in `e/`.
2. Wire it through `comp-gb.cpp` / `comp-res.cpp` / etc.
3. Add a unit test.
4. Expose via `interface/groebner.{h,cpp}` (or a new file).

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
