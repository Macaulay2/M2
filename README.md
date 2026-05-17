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

## Documentation map

Three levels of documentation are reachable from this file:

1. **Top-level navigation** — the
   [Repository Architecture Table of Contents](#repository-architecture-table-of-contents)
   below covers every directory in the source tree at one click of depth.
2. **Per-directory READMEs** — every subdirectory has its own `README.md`
   describing its contents, build glue, and how it fits into the larger
   pipeline. Linked from the TOC.
3. **Engine deep-dive** — the
   [Engine deep-dive](#engine-deep-dive-m2macaulay2e) section presents the
   `M2/Macaulay2/e/` source tree in architectural layers, links to the 10
   per-area markdown files inside `e/`, and gives per-file groupings for every
   top-level engine source file.

### Quick links

- Source root: [`M2/`](M2/README.md) → [`M2/Macaulay2/`](M2/Macaulay2/README.md)
- The four layers: [`c/`](M2/Macaulay2/c/README.md) · [`d/`](M2/Macaulay2/d/README.md) · [`e/`](M2/Macaulay2/e/README.md) · [`m2/`](M2/Macaulay2/m2/README.md)
- Engine areas: [coefficient rings](M2/Macaulay2/e/coefficient-rings.md) · [polynomial rings](M2/Macaulay2/e/polynomial-rings.md) · [monoids](M2/Macaulay2/e/monoids-and-monomials.md) · [matrices](M2/Macaulay2/e/matrices.md) · [free modules](M2/Macaulay2/e/free-modules.md) · [Gröbner bases](M2/Macaulay2/e/groebner-bases.md) · [resolutions](M2/Macaulay2/e/resolutions.md) · [other computations](M2/Macaulay2/e/computations.md) · [ring elements / maps](M2/Macaulay2/e/ring-elements-and-maps.md) · [utilities](M2/Macaulay2/e/utilities.md)
- Build & packaging: [`cmake/`](M2/cmake/README.md) · [`libraries/`](M2/libraries/README.md) · [`submodules/`](M2/submodules/README.md) · [`distributions/`](M2/distributions/README.md)
- Tests: [`tests/`](M2/Macaulay2/tests/README.md) · [`e/unit-tests/`](M2/Macaulay2/e/unit-tests/README.md)
- Packages: [`packages/`](M2/Macaulay2/packages/README.md)
- Build instructions: project [`CLAUDE.md`](CLAUDE.md) at the repo root

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

### Deeper subdirectories

Documented multi-level paths under the entries above:

**Under [`M2/distributions/`](M2/distributions/README.md)**

| Path | Purpose |
|---|---|
| [`M2/distributions/top/`](M2/distributions/top/README.md) | User-facing top-level distribution templates (`INSTALL.in`, post-install, pre-remove) |
| [`M2/distributions/dmg/`](M2/distributions/dmg/README.md) | macOS disk-image packaging |
| [`M2/distributions/freebsd/`](M2/distributions/freebsd/README.md) | FreeBSD port packaging |
| [`M2/distributions/install/`](M2/distributions/install/README.md) | Generic install-time helpers |
| [`M2/distributions/tar/`](M2/distributions/tar/README.md) | Portable tarball packaging |

**Under [`M2/include/`](M2/include/README.md)**

| Path | Purpose |
|---|---|
| [`M2/include/M2/`](M2/include/M2/README.md) | Public M2 C/C++ headers (`atomic-field.h`, `gc-include.h`, `math-include.h`, `synchronization.h.in`, `config.h.cmake`) |
| [`M2/include/valgrind/`](M2/include/valgrind/README.md) | Bundled Valgrind client headers |

**Under [`M2/Macaulay2/editors/`](M2/Macaulay2/editors/README.md)**

| Path | Purpose |
|---|---|
| [`M2/Macaulay2/editors/emacs/`](M2/Macaulay2/editors/emacs/README.md) | M2-emacs submodule pointer |
| [`M2/Macaulay2/editors/vim/`](M2/Macaulay2/editors/vim/README.md) | Vim syntax / dictionary templates |
| `M2/Macaulay2/editors/prism/` | Prism (JS) syntax templates — README inside |
| `M2/Macaulay2/editors/pygments/` | Pygments (Python) syntax templates — README inside |

**Under [`M2/BUILD/docker/`](M2/BUILD/docker/README.md)**

| Path | Purpose |
|---|---|
| [`M2/BUILD/docker/actions/`](M2/BUILD/docker/actions/README.md) | GitHub Actions Ubuntu container |
| [`M2/BUILD/docker/arch/`](M2/BUILD/docker/arch/README.md) | Arch Linux container (experimental) |
| [`M2/BUILD/docker/autotools/`](M2/BUILD/docker/autotools/README.md) | Reusable `build-autotools` target snippet |
| [`M2/BUILD/docker/brew/`](M2/BUILD/docker/brew/README.md) | Homebrew bottling container |
| [`M2/BUILD/docker/debian/`](M2/BUILD/docker/debian/README.md) | Debian / Ubuntu `.deb` packaging |
| [`M2/BUILD/docker/fedora/`](M2/BUILD/docker/fedora/README.md) | Fedora `.rpm` packaging |
| [`M2/BUILD/docker/gentoo/`](M2/BUILD/docker/gentoo/README.md) | Gentoo container (experimental) |
| [`M2/BUILD/docker/nightly/`](M2/BUILD/docker/nightly/README.md) | Nightly-build smoke test |
| [`M2/BUILD/docker/rhel/`](M2/BUILD/docker/rhel/README.md) | RHEL-compatible build container |
| [`M2/BUILD/docker/storage/`](M2/BUILD/docker/storage/README.md) | Container home-dir scratch assets |
| [`M2/BUILD/docker/testbot/`](M2/BUILD/docker/testbot/README.md) | Workshop testbot container |
| [`M2/BUILD/docker/ubuntu/`](M2/BUILD/docker/ubuntu/README.md) | CMake-based Ubuntu container |
| [`M2/BUILD/docker/valgrind/`](M2/BUILD/docker/valgrind/README.md) | Valgrind debugging container |

**Under [`M2/Macaulay2/e/`](M2/Macaulay2/e/README.md)** — full engine subdirectory and per-area listing is in the [Engine deep-dive](#engine-deep-dive-m2macaulay2e) section.

**Under [`M2/Macaulay2/tests/`](M2/Macaulay2/tests/README.md)**

| Path | Purpose |
|---|---|
| [`M2/Macaulay2/tests/normal/`](M2/Macaulay2/tests/normal/README.md) | Default-tier regression tests (377 `.m2` scripts) |
| [`M2/Macaulay2/tests/engine/`](M2/Macaulay2/tests/engine/README.md) | Engine integration tests (CI-skipped, see #1213) |
| [`M2/Macaulay2/tests/ComputationsBook/`](M2/Macaulay2/tests/ComputationsBook/README.md) | Book regression tests (per-chapter) |
| [`M2/Macaulay2/tests/slow/`](M2/Macaulay2/tests/slow/README.md) | Slower regression tests |
| [`M2/Macaulay2/tests/gigantic/`](M2/Macaulay2/tests/gigantic/README.md) | Extreme-scale stress tests |
| [`M2/Macaulay2/tests/goals/`](M2/Macaulay2/tests/goals/README.md) | Worked-example / "goal" tests |
| [`M2/Macaulay2/tests/rationality/`](M2/Macaulay2/tests/rationality/README.md) | Rationality-question tests |
| [`M2/Macaulay2/tests/threads/`](M2/Macaulay2/tests/threads/README.md) | Thread-supervisor tests |
| [`M2/Macaulay2/tests/quarantine/`](M2/Macaulay2/tests/quarantine/README.md) | Disabled tests awaiting fixes |

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

### Single-file deep dives

Dedicated walkthroughs for especially central engine classes
(convention: `file-<basename>.md` in `M2/Macaulay2/e/`):

**Foundations**

| File doc | Class | Area |
|---|---|---|
| [file-monoid.md](M2/Macaulay2/e/file-monoid.md) | `Monoid` | Monoids & monomials |
| [file-aring.md](M2/Macaulay2/e/file-aring.md) | `aring` framework / dispatcher | Coefficient rings |
| [file-polyring.md](M2/Macaulay2/e/file-polyring.md) | `PolynomialRing` | Polynomial rings |
| [file-freemod.md](M2/Macaulay2/e/file-freemod.md) | `FreeModule` | Free modules |
| [file-schorder.md](M2/Macaulay2/e/file-schorder.md) | `SchreyerOrder` | Free modules |
| [file-matrix.md](M2/Macaulay2/e/file-matrix.md) | `Matrix` (immutable) | Matrices |
| [file-mutablemat.md](M2/Macaulay2/e/file-mutablemat.md) | `MutableMatrix` | Matrices |

**Polynomial ring variants**

| File doc | Class | Area |
|---|---|---|
| [file-frac.md](M2/Macaulay2/e/file-frac.md) | `FractionField` | Polynomial rings |
| [file-qring.md](M2/Macaulay2/e/file-qring.md) | `QRingInfo` / `PolyQuotient` | Polynomial rings |
| [file-localring.md](M2/Macaulay2/e/file-localring.md) | `LocalRing` | Polynomial rings |
| [file-weylalg.md](M2/Macaulay2/e/file-weylalg.md) | `WeylAlgebra` | Polynomial rings |
| [file-skewpoly.md](M2/Macaulay2/e/file-skewpoly.md) | `SkewPolynomialRing` | Polynomial rings |
| [file-solvable.md](M2/Macaulay2/e/file-solvable.md) | `SolvableAlgebra` (PBW) | Polynomial rings |

**Monomial machinery**

| File doc | Class | Area |
|---|---|---|
| [file-imonorder.md](M2/Macaulay2/e/file-imonorder.md) | Internal monomial order | Monoids & monomials |
| [file-montable.md](M2/Macaulay2/e/file-montable.md) | `MonomialTable` | Monoids & monomials |

**Matrices (template internals)**

| File doc | Class | Area |
|---|---|---|
| [file-dmat.md](M2/Macaulay2/e/file-dmat.md) | `DMat<R>` (dense matrix template) | Matrices |

**Ring elements and maps**

| File doc | Class | Area |
|---|---|---|
| [file-relem.md](M2/Macaulay2/e/file-relem.md) | `RingElement` | Ring elements & maps |
| [file-ringmap.md](M2/Macaulay2/e/file-ringmap.md) | `RingMap` | Ring elements & maps |

**Computations**

| File doc | Class | Area |
|---|---|---|
| [file-computation-framework.md](M2/Macaulay2/e/file-computation-framework.md) | `Computation` (abstract base) | GB / res / other |
| [file-comp-gb.md](M2/Macaulay2/e/file-comp-gb.md) | `GBComputation` | Gröbner bases |
| [file-gb-default.md](M2/Macaulay2/e/file-gb-default.md) | `gbA` (default GB algorithm) | Gröbner bases |
| [file-gb-variants.md](M2/Macaulay2/e/file-gb-variants.md) | `gb-homog2`/`gb-sugarless`/`gb-toric`/`gb-walk` | Gröbner bases |
| [file-mathicgb-interface.md](M2/Macaulay2/e/file-mathicgb-interface.md) | mathicgb bridge | Gröbner bases |
| [file-reducedgb.md](M2/Macaulay2/e/file-reducedgb.md) | `ReducedGB` family | Gröbner bases |
| [file-spair.md](M2/Macaulay2/e/file-spair.md) | `s_pair`, `gb_elem` | Gröbner bases |
| [file-gbweight.md](M2/Macaulay2/e/file-gbweight.md) | `GBWeight` | Gröbner bases |
| [file-comp-res.md](M2/Macaulay2/e/file-comp-res.md) | `ResolutionComputation` | Resolutions |
| [file-gbring.md](M2/Macaulay2/e/file-gbring.md) | `GBRing` / `gbvector` | Gröbner bases |
| [file-Eschreyer.md](M2/Macaulay2/e/file-Eschreyer.md) | `GBKernelComputation` (older Schreyer) | Resolutions |
| [file-betti.md](M2/Macaulay2/e/file-betti.md) | `BettiDisplay` | Resolutions |
| [file-hilb.md](M2/Macaulay2/e/file-hilb.md) | Hilbert function (Bigatti) | Other computations |
| [file-LLL.md](M2/Macaulay2/e/file-LLL.md) | LLL lattice reduction | Other computations |
| [file-monideal.md](M2/Macaulay2/e/file-monideal.md) | `MonomialIdeal` | Other computations |
| [file-NAG.md](M2/Macaulay2/e/file-NAG.md) | Numerical AG | Other computations |
| [file-SLP.md](M2/Macaulay2/e/file-SLP.md) | Straight-line programs | Other computations |
| [file-assprime.md](M2/Macaulay2/e/file-assprime.md) | `AssociatedPrimes` | Other computations |
| [file-comb.md](M2/Macaulay2/e/file-comb.md) | `Subsets` (combinatorial helpers) | Other computations |
| [file-overflow.md](M2/Macaulay2/e/file-overflow.md) | Overflow-checked arithmetic | Utilities |
| [file-buffer.md](M2/Macaulay2/e/file-buffer.md) | `buffer` (append-only byte buffer) | Utilities |
| [file-text-io.md](M2/Macaulay2/e/file-text-io.md) | `text-io` (wrap / `bignum_text_out`) | Utilities |
| [file-MemoryBlock.md](M2/Macaulay2/e/file-MemoryBlock.md) | `MemoryBlock` (bump allocator) | Utilities |
| [file-ExponentList.md](M2/Macaulay2/e/file-ExponentList.md) | `ExponentList` (sparse monomial encoding) | Monoids & monomials |
| [file-Polynomial.md](M2/Macaulay2/e/file-Polynomial.md) | `Monom` / `Poly` (modern polynomial value type) | Polynomial rings |
| [file-VectorArithmetic.md](M2/Macaulay2/e/file-VectorArithmetic.md) | `VectorArithmetic` (templated arithmetic dispatcher) | Matrices |
| [file-error.md](M2/Macaulay2/e/file-error.md) | `error.{c,h}` (engine error reporting) | Utilities |
| [file-debug.md](M2/Macaulay2/e/file-debug.md) | `debug.{cpp,hpp}` (debugger-callable printers) | Utilities |
| [file-coeffrings.md](M2/Macaulay2/e/file-coeffrings.md) | `CoefficientRing*` (registry + `SimpleARing` example) | Coefficient rings |
| [file-aring-glue.md](M2/Macaulay2/e/file-aring-glue.md) | `ConcreteRing<R>` (bridge from `aring` to `Ring`) | Coefficient rings |
| [file-aring-zz-flint.md](M2/Macaulay2/e/file-aring-zz-flint.md) | `ARingZZ` (ZZ via FLINT) | Coefficient rings |
| [file-aring-zzp-flint.md](M2/Macaulay2/e/file-aring-zzp-flint.md) | `ARingZZpFlint` (Z/p via FLINT) | Coefficient rings |
| [file-aring-RR.md](M2/Macaulay2/e/file-aring-RR.md) | `ARingRR` (RR via hardware `double`) | Coefficient rings |
| [file-aring-CC.md](M2/Macaulay2/e/file-aring-CC.md) | `ARingCC` (CC via pair of `double`) | Coefficient rings |
| [file-aring-qq-flint.md](M2/Macaulay2/e/file-aring-qq-flint.md) | `ARingQQFlint` (QQ via FLINT) | Coefficient rings |
| [file-aring-gf-flint.md](M2/Macaulay2/e/file-aring-gf-flint.md) | `ARingGFFlint` (small GF via FLINT Zech) | Coefficient rings |
| [file-aring-zz-gmp.md](M2/Macaulay2/e/file-aring-zz-gmp.md) | `ARingZZGMP` (ZZ via GMP) | Coefficient rings |
| [file-aring-zzp.md](M2/Macaulay2/e/file-aring-zzp.md) | `ARingZZp` (portable Z/p via log tables) | Coefficient rings |
| [file-aring-zzp-ffpack.md](M2/Macaulay2/e/file-aring-zzp-ffpack.md) | `ARingZZpFFPACK` (Z/p via FFLAS-FFPACK) | Coefficient rings |
| [file-aring-RRR.md](M2/Macaulay2/e/file-aring-RRR.md) | `ARingRRR` (RR via MPFR) | Coefficient rings |
| [file-aring-CCC.md](M2/Macaulay2/e/file-aring-CCC.md) | `ARingCCC` (CC via MPFR pair) | Coefficient rings |
| [file-aring-tower.md](M2/Macaulay2/e/file-aring-tower.md) | `ARingTower` (iterated finite extension) | Coefficient rings |
| [file-aring-RRi.md](M2/Macaulay2/e/file-aring-RRi.md) | `ARingRRi` (real intervals via MPFI) | Coefficient rings |
| [file-aring-CCi.md](M2/Macaulay2/e/file-aring-CCi.md) | `ARingCCi` (complex intervals via MPFI) | Coefficient rings |
| [file-aring-m2-gf.md](M2/Macaulay2/e/file-aring-m2-gf.md) | `ARingGFM2` (native M2 GF, no external dep) | Coefficient rings |
| [file-aring-gf-flint-big.md](M2/Macaulay2/e/file-aring-gf-flint-big.md) | `ARingGFFlintBig` (large GF via FLINT `fq_nmod`) | Coefficient rings |
| [file-M2FreeAlgebra.md](M2/Macaulay2/e/file-M2FreeAlgebra.md) | `M2FreeAlgebra` (`Ring` wrapper for NC algebras) | Polynomial rings |
| [file-skew.md](M2/Macaulay2/e/file-skew.md) | `SkewMultiplication` (skew-commutative config) | Polynomial rings |
| [file-ringelem.md](M2/Macaulay2/e/file-ringelem.md) | `ring_elem` (universal value type) | Ring elements & maps |
| [file-hash.md](M2/Macaulay2/e/file-hash.md) | `EngineObject` / `MutableEngineObject` (GC bases) | Utilities |
| [file-exceptions.md](M2/Macaulay2/e/file-exceptions.md) | Engine C++ exception hierarchy | Utilities |
| [file-engine-h.md](M2/Macaulay2/e/file-engine-h.md) | `engine.h` aggregating header | Public interface |
| [file-style.md](M2/Macaulay2/e/file-style.md) | `style.hpp` (comparison codes, `GEOHEAP_SIZE`) | Utilities |
| [file-newdelete.md](M2/Macaulay2/e/file-newdelete.md) | `our_new_delete` GC allocation hook | Utilities |
| [file-M2FreeAlgebraQuotient.md](M2/Macaulay2/e/file-M2FreeAlgebraQuotient.md) | `M2FreeAlgebraQuotient` (`Ring` wrapper for NC quotients) | Polynomial rings |
| [file-BasicPoly.md](M2/Macaulay2/e/file-BasicPoly.md) | `BasicPoly` / `BasicPolyList` (portable polynomial type) | Polynomial rings |
| [file-ExponentVector.md](M2/Macaulay2/e/file-ExponentVector.md) | `ExponentVector` (dense monomial encoding template) | Monoids & monomials |
| [file-det.md](M2/Macaulay2/e/file-det.md) | Determinants and minors | Matrices |
| [file-mutablecomplex.md](M2/Macaulay2/e/file-mutablecomplex.md) | `MutableComplex` (in-place chain complex) | Matrices |
| [file-dpoly.md](M2/Macaulay2/e/file-dpoly.md) | Univariate polys over QQ ext. / finite fields | Other computations |
| [file-schur.md](M2/Macaulay2/e/file-schur.md) | `SchurRing` (Schur function ring) | Polynomial rings |
| [file-tower.md](M2/Macaulay2/e/file-tower.md) | `Tower` (legacy tower-of-extensions) | Coefficient rings |
| [file-ntl-interface.md](M2/Macaulay2/e/file-ntl-interface.md) | Bridge to the NTL library | Coefficient rings |
| [file-gauss.md](M2/Macaulay2/e/file-gauss.md) | `GaussElimComputation` | Gröbner bases |
| [file-hermite.md](M2/Macaulay2/e/file-hermite.md) | `HermiteComputation` (ZZ Hermite normal form) | Gröbner bases |
| [file-lapack.md](M2/Macaulay2/e/file-lapack.md) | LAPACK bridge for `RR` / `CC` matrices | Matrices |
| [file-eigen.md](M2/Macaulay2/e/file-eigen.md) | Eigenvalues / SVD | Matrices |
| [file-pfaff.md](M2/Macaulay2/e/file-pfaff.md) | `PfaffianComputation` | Matrices |
| [file-matrix-con.md](M2/Macaulay2/e/file-matrix-con.md) | `MatrixConstructor` (immutable-matrix builder) | Matrices |
| [file-matrix-stream.md](M2/Macaulay2/e/file-matrix-stream.md) | `MatrixStream` (streaming matrix construction) | Matrices |
| [file-mat-linalg.md](M2/Macaulay2/e/file-mat-linalg.md) | Templated linear algebra for `DMat<R>` | Matrices |
| [file-mat-arith.md](M2/Macaulay2/e/file-mat-arith.md) | Templated matrix arithmetic + `MatrixWindow` | Matrices |
| [file-mat-elem-ops.md](M2/Macaulay2/e/file-mat-elem-ops.md) | `MatElementaryOps<MT>` | Matrices |
| [file-monomial-sets.md](M2/Macaulay2/e/file-monomial-sets.md) | Fixed/variable-size monomial sets | Monoids & monomials |
| [file-mat-util.md](M2/Macaulay2/e/file-mat-util.md) | Generic matrix helpers (`displayMat`) | Matrices |
| [file-poly.md](M2/Macaulay2/e/file-poly.md) | `PolyRing` (standard commutative polynomial ring) | Polynomial rings |
| [file-polyquotient.md](M2/Macaulay2/e/file-polyquotient.md) | `PolyQuotient` (concrete quotient subclass) | Polynomial rings |
| [file-sagbi.md](M2/Macaulay2/e/file-sagbi.md) | SAGBI helpers (legacy) | Gröbner bases |
| [file-points.md](M2/Macaulay2/e/file-points.md) | `PointsComputation<CoeffRing>` (ideal of points) | Other computations |
| [file-interreduce.md](M2/Macaulay2/e/file-interreduce.md) | `Interreducer` | Gröbner bases |
| [file-fractionfreeLU.md](M2/Macaulay2/e/file-fractionfreeLU.md) | `FF_LUComputation` (Bareiss LU over a domain) | Matrices |
| [file-franzi.md](M2/Macaulay2/e/file-franzi.md) | `franzi-*` (Boolean-ring GB family) | Gröbner bases |
| [file-mutablemat-defs.md](M2/Macaulay2/e/file-mutablemat-defs.md) | `MutableMat<Mat>` template internals | Matrices |
| [file-util.md](M2/Macaulay2/e/file-util.md) | `util.hpp` (M2-side string/array conversions) | Utilities |
| [file-cra-impl.md](M2/Macaulay2/e/file-cra-impl.md) | `ChineseRemainder` internals | Other computations |
| [file-monordering.md](M2/Macaulay2/e/file-monordering.md) | `MonomialOrdering` constructors (impl) | Monoids & monomials |
| [file-montableZZ.md](M2/Macaulay2/e/file-montableZZ.md) | `MonomialTableZZ` (ZZ-coeff monomial table) | Monoids & monomials |
| [file-monomial-collection.md](M2/Macaulay2/e/file-monomial-collection.md) | `IntsSet` / `ModuleMonomSet` | Monoids & monomials |
| [file-monsort.md](M2/Macaulay2/e/file-monsort.md) | Generic monomial sorter template | Monoids & monomials |
| [file-mem.md](M2/Macaulay2/e/file-mem.md) | `stash` (size-class slab allocator) | Utilities |
| [file-myalloc.md](M2/Macaulay2/e/file-myalloc.md) | `StatsAllocator` (debug allocator) | Utilities |
| [file-finalize.md](M2/Macaulay2/e/file-finalize.md) | Engine-object finalisation hooks | Utilities |
| [file-ring-vecs.md](M2/Macaulay2/e/file-ring-vecs.md) | `Ring`'s `vec` operations | Ring elements & maps |
| [file-monideal-minprimes.md](M2/Macaulay2/e/file-monideal-minprimes.md) | `MinimalPrimes` of a monomial ideal | Other computations |
| [file-interrupted.md](M2/Macaulay2/e/file-interrupted.md) | `system_interrupted()` (Ctrl+C polling) | Utilities |
| [file-int-bag.md](M2/Macaulay2/e/file-int-bag.md) | `int_bag` (small value + varpower monomial) | Monoids & monomials |
| [file-dmat-zz-flint.md](M2/Macaulay2/e/file-dmat-zz-flint.md) | `DMat<ARingZZ>` FLINT specialisation | Matrices |
| [file-dmat-zzp-flint.md](M2/Macaulay2/e/file-dmat-zzp-flint.md) | `DMat<ARingZZpFlint>` FLINT specialisation | Matrices |
| [file-memory-status.md](M2/Macaulay2/e/file-memory-status.md) | Placeholder memory-stats hooks | Utilities |
| [file-dmat-qq-flint.md](M2/Macaulay2/e/file-dmat-qq-flint.md) | `DMat<ARingQQFlint>` FLINT specialisation | Matrices |
| [file-dmat-gf-flint.md](M2/Macaulay2/e/file-dmat-gf-flint.md) | `DMat<ARingGFFlint*>` FLINT specialisations | Matrices |
| [file-dmat-lu.md](M2/Macaulay2/e/file-dmat-lu.md) | LU decomposition specialisations | Matrices |
| [file-dmat-ffpack.md](M2/Macaulay2/e/file-dmat-ffpack.md) | Historical FFLAS-FFPACK dispatcher (legacy) | Matrices |
| [file-geovec.md](M2/Macaulay2/e/file-geovec.md) | Geometric heap for `vec` accumulation | Ring elements & maps |
| [file-matrix-kbasis.md](M2/Macaulay2/e/file-matrix-kbasis.md) | k-basis of a graded module | Matrices |
| [file-matrix-symm.md](M2/Macaulay2/e/file-matrix-symm.md) | `SymmMatrix` (symmetric power) | Matrices |
| [file-matrix-sort.md](M2/Macaulay2/e/file-matrix-sort.md) | `MatrixSorter` (column sort) | Matrices |
| [file-ZZ.md](M2/Macaulay2/e/file-ZZ.md) | Legacy `ZZ` (`Ring`-based) | Coefficient rings |
| [file-ZZp.md](M2/Macaulay2/e/file-ZZp.md) | Legacy `Z_mod` (`Ring`-based) | Coefficient rings |
| [file-GF.md](M2/Macaulay2/e/file-GF.md) | Legacy `GF` (`Ring`-based) | Coefficient rings |
| [file-aring-translate.md](M2/Macaulay2/e/file-aring-translate.md) | Cross-ring coercion templates | Coefficient rings |
| [file-aring-wrap.md](M2/Macaulay2/e/file-aring-wrap.md) | `RElementWrap<RingType>` | Coefficient rings |
| [file-aring-qq.md](M2/Macaulay2/e/file-aring-qq.md) | `ARingQQ` typedef + dispatcher | Coefficient rings |
| [file-BasicPolyListParser.md](M2/Macaulay2/e/file-BasicPolyListParser.md) | `BasicPolyList` text-format parsers | Polynomial rings |
| [file-PolynomialStream.md](M2/Macaulay2/e/file-PolynomialStream.md) | Streaming polynomial-input concept (newf4) | Polynomial rings |
| [file-polyroots.md](M2/Macaulay2/e/file-polyroots.md) | Univariate polynomial root finder | Other computations |
| [file-schur2.md](M2/Macaulay2/e/file-schur2.md) | `SchurRing2` (refactored Schur ring) | Polynomial rings |
| [file-schurSn.md](M2/Macaulay2/e/file-schurSn.md) | `SchurSnRing` (symmetric-group ring) | Polynomial rings |
| [file-schur-poly-heap.md](M2/Macaulay2/e/file-schur-poly-heap.md) | `schur_poly_heap` accumulator | Polynomial rings |
| [file-matrix-ncbasis.md](M2/Macaulay2/e/file-matrix-ncbasis.md) | Non-commutative `basis` | Matrices |
| [file-SLP-defs.md](M2/Macaulay2/e/file-SLP-defs.md) | `SLProgram` / `M2SLProgram` declarations | Other computations |
| [file-SLP-imp.md](M2/Macaulay2/e/file-SLP-imp.md) | `SLEvaluatorConcrete<RT>` | Other computations |
| [file-monomial.md](M2/Macaulay2/e/file-monomial.md) | `EngineMonomial` (boundary monomial type) | Monoids & monomials |
| [file-godboltTest.md](M2/Macaulay2/e/file-godboltTest.md) | Standalone Z/p log-table sandbox | (sandbox) |
| [file-timing.md](M2/Macaulay2/e/file-timing.md) | `timing.hpp` (engine-side timestamps) | Utilities |
| [file-dmat-qq-interface-flint.md](M2/Macaulay2/e/file-dmat-qq-interface-flint.md) | FLINT-mat translation for GMP-based `DMat<ARingQQ>` | Matrices |
| [file-Eschreyer-cpp.md](M2/Macaulay2/e/file-Eschreyer-cpp.md) | `Eschreyer.cpp` implementation notes | Resolutions |
| [file-aring-ZZ-comparison.md](M2/Macaulay2/e/file-aring-ZZ-comparison.md) | Cross-reference: the three `ZZ` paths | Coefficient rings |
| [file-m2tbb.md](M2/Macaulay2/e/file-m2tbb.md) | `m2tbb.hpp` (TBB wrapper) | Utilities |

**Subdirectory file deep dives** (per-file docs alongside their source):

| Subdir | File doc | Class |
|---|---|---|
| `NCAlgebras/` | [NCAlgebras/file-FreeMonoid.md](M2/Macaulay2/e/NCAlgebras/file-FreeMonoid.md) | `FreeMonoid` |
| `NCAlgebras/` | [NCAlgebras/file-FreeAlgebra.md](M2/Macaulay2/e/NCAlgebras/file-FreeAlgebra.md) | `FreeAlgebra` |
| `NCAlgebras/` | [NCAlgebras/file-NCGroebner.md](M2/Macaulay2/e/NCAlgebras/file-NCGroebner.md) | `NCGroebner` |
| `NCAlgebras/` | [NCAlgebras/file-NCF4.md](M2/Macaulay2/e/NCAlgebras/file-NCF4.md) | `NCF4` |
| `NCAlgebras/` | [NCAlgebras/file-WordTable.md](M2/Macaulay2/e/NCAlgebras/file-WordTable.md) | `WordTable` |
| `NCAlgebras/` | [NCAlgebras/file-OverlapTable.md](M2/Macaulay2/e/NCAlgebras/file-OverlapTable.md) | `OverlapTable` |
| `NCAlgebras/` | [NCAlgebras/file-NCReduction.md](M2/Macaulay2/e/NCAlgebras/file-NCReduction.md) | `PolynomialHeap` (NC reduction) |
| `NCAlgebras/` | [NCAlgebras/file-SuffixTree.md](M2/Macaulay2/e/NCAlgebras/file-SuffixTree.md) | `SuffixTree` |
| `NCAlgebras/` | [NCAlgebras/file-FreeAlgebraQuotient.md](M2/Macaulay2/e/NCAlgebras/file-FreeAlgebraQuotient.md) | `FreeAlgebraQuotient` |
| `NCAlgebras/` | [NCAlgebras/file-Word.md](M2/Macaulay2/e/NCAlgebras/file-Word.md) | `Word` (non-owning word view) |
| `NCAlgebras/` | [NCAlgebras/file-Range.md](M2/Macaulay2/e/NCAlgebras/file-Range.md) | `Range<T>` (iterator-pair view) |
| `f4/` | [f4/file-f4-computation.md](M2/Macaulay2/e/f4/file-f4-computation.md) | `F4Computation` |
| `f4/` | [f4/file-f4-spairs.md](M2/Macaulay2/e/f4/file-f4-spairs.md) | `F4SPairSet` |
| `f4/` | [f4/file-f4-m2-interface.md](M2/Macaulay2/e/f4/file-f4-m2-interface.md) | `F4toM2Interface` |
| `f4/` | [f4/file-monhashtable.md](M2/Macaulay2/e/f4/file-monhashtable.md) | Monomial hash-table traits |
| `f4/` | [f4/file-varpower-monomial.md](M2/Macaulay2/e/f4/file-varpower-monomial.md) | F4-internal sparse monomial encoding |
| `f4/` | [f4/file-ntuple-monomial.md](M2/Macaulay2/e/f4/file-ntuple-monomial.md) | F4-internal dense monomial encoding |
| `f4/` | [f4/file-moninfo.md](M2/Macaulay2/e/f4/file-moninfo.md) | `MonomialInfo` (F4 monomial layout) |
| `f4/` | [f4/file-f4.md](M2/Macaulay2/e/f4/file-f4.md) | `F4GB` (the F4 algorithm) |
| `f4/` | [f4/file-hilb-fcn.md](M2/Macaulay2/e/f4/file-hilb-fcn.md) | `HilbertController` |
| `f4/` | [f4/file-memblock.md](M2/Macaulay2/e/f4/file-memblock.md) | `F4MemoryBlock<T>` |
| `f4/` | [f4/file-f4-monlookup.md](M2/Macaulay2/e/f4/file-f4-monlookup.md) | `F4MonomialLookupTableT<Key>` |
| `f4/` | [f4/file-f4-types.md](M2/Macaulay2/e/f4/file-f4-types.md) | F4 type vocabulary |
| `bibasis/` | [bibasis/file-bibasis.md](M2/Macaulay2/e/bibasis/file-bibasis.md) | `BIBasis` driver |
| `bibasis/` | [bibasis/file-monom.md](M2/Macaulay2/e/bibasis/file-monom.md) | `Monom` + ordering specialisations |
| `bibasis/` | [bibasis/file-janettree.md](M2/Macaulay2/e/bibasis/file-janettree.md) | `JanetTree<MonomType>` |
| `bibasis/` | [bibasis/file-polynom.md](M2/Macaulay2/e/bibasis/file-polynom.md) | `Polynom<MonomType>` |
| `NCResolutions/` | [NCResolutions/file-nc-res-computation.md](M2/Macaulay2/e/NCResolutions/file-nc-res-computation.md) | `NCResComputation` |
| `interface/` | [interface/file-aring-interface.md](M2/Macaulay2/e/interface/file-aring-interface.md) | aring C entry points |
| `interface/` | [interface/file-groebner-interface.md](M2/Macaulay2/e/interface/file-groebner-interface.md) | GB / resolution C entry points |
| `interface/` | [interface/file-ring-interface.md](M2/Macaulay2/e/interface/file-ring-interface.md) | Legacy `Ring` C entry points |
| `interface/` | [interface/file-matrix-interface.md](M2/Macaulay2/e/interface/file-matrix-interface.md) | `Matrix` C entry points |
| `interface/` | [interface/file-freemodule-interface.md](M2/Macaulay2/e/interface/file-freemodule-interface.md) | `FreeModule` C entry points |
| `interface/` | [interface/file-monoid-interface.md](M2/Macaulay2/e/interface/file-monoid-interface.md) | `Monoid` C entry points |
| `interface/` | [interface/file-computation-interface.md](M2/Macaulay2/e/interface/file-computation-interface.md) | Computation status / stop-condition enums |
| `interface/` | [interface/file-ringelement-interface.md](M2/Macaulay2/e/interface/file-ringelement-interface.md) | `RingElement` C entry points |
| `interface/` | [interface/file-ringmap-interface.md](M2/Macaulay2/e/interface/file-ringmap-interface.md) | `RingMap` C entry points |
| `interface/` | [interface/file-monomial-ideal-interface.md](M2/Macaulay2/e/interface/file-monomial-ideal-interface.md) | `MonomialIdeal` C entry points |
| `interface/` | [interface/file-mutable-matrix-interface.md](M2/Macaulay2/e/interface/file-mutable-matrix-interface.md) | `MutableMatrix` C entry points |
| `interface/` | [interface/file-monomial-ordering-interface.md](M2/Macaulay2/e/interface/file-monomial-ordering-interface.md) | `MonomialOrdering` enum + constructors |
| `interface/` | [interface/file-flint-interface.md](M2/Macaulay2/e/interface/file-flint-interface.md) | FLINT primality / factorisation |
| `interface/` | [interface/file-cone-interface.md](M2/Macaulay2/e/interface/file-cone-interface.md) | Cone operations |
| `interface/` | [interface/file-factory-interface.md](M2/Macaulay2/e/interface/file-factory-interface.md) | Polynomial GCD / factorisation |
| `interface/` | [interface/file-cra-interface.md](M2/Macaulay2/e/interface/file-cra-interface.md) | CRT / rational reconstruction |
| `interface/` | [interface/file-NAG-interface.md](M2/Macaulay2/e/interface/file-NAG-interface.md) | Numerical Algebraic Geometry C API |
| `interface/` | [interface/file-random-interface.md](M2/Macaulay2/e/interface/file-random-interface.md) | Engine RNG entry points |
| `interface/` | [interface/file-gmp-util-interface.md](M2/Macaulay2/e/interface/file-gmp-util-interface.md) | GMP/MPFR allocation helpers |
| `interface/` | [interface/file-m2-mem-interface.md](M2/Macaulay2/e/interface/file-m2-mem-interface.md) | Engine memory hooks + debug traps |
| `interface/` | [interface/file-m2-types-interface.md](M2/Macaulay2/e/interface/file-m2-types-interface.md) | Base type aliases |
| `gb-f4/` | [gb-f4/file-GBF4Computation.md](M2/Macaulay2/e/gb-f4/file-GBF4Computation.md) | `GBF4Computation` |
| `gb-f4/` | [gb-f4/file-MacaulayMatrix.md](M2/Macaulay2/e/gb-f4/file-MacaulayMatrix.md) | `MacaulayMatrix` |
| `gb-f4/` | [gb-f4/file-Basis.md](M2/Macaulay2/e/gb-f4/file-Basis.md) | `Basis` |
| `gb-f4/` | [gb-f4/file-SPairs.md](M2/Macaulay2/e/gb-f4/file-SPairs.md) | `SPairs` (refactored F4) |
| `gb-f4/` | [gb-f4/file-MonomialHashTable.md](M2/Macaulay2/e/gb-f4/file-MonomialHashTable.md) | `MonomialHashFunction` + table |
| `gb-f4/` | [gb-f4/file-MonomialLookupTable.md](M2/Macaulay2/e/gb-f4/file-MonomialLookupTable.md) | `MonomialLookupTable` (divisibility) |
| `gb-f4/` | [gb-f4/file-PolynomialList.md](M2/Macaulay2/e/gb-f4/file-PolynomialList.md) | `PolynomialList` |
| `gb-f4/` | [gb-f4/file-MonomialView.md](M2/Macaulay2/e/gb-f4/file-MonomialView.md) | `MonomialView` |
| `gb-f4/` | [gb-f4/file-MonomialTypes.md](M2/Macaulay2/e/gb-f4/file-MonomialTypes.md) | Typed integers (`newf4` vocabulary) |
| `schreyer-resolution/` | [schreyer-resolution/file-res-f4-computation.md](M2/Macaulay2/e/schreyer-resolution/file-res-f4-computation.md) | `F4ResComputation` |
| `schreyer-resolution/` | [schreyer-resolution/file-res-schreyer-frame.md](M2/Macaulay2/e/schreyer-resolution/file-res-schreyer-frame.md) | `SchreyerFrame` |
| `schreyer-resolution/` | [schreyer-resolution/file-res-poly-ring.md](M2/Macaulay2/e/schreyer-resolution/file-res-poly-ring.md) | `ResPolyRing` / `ResPolynomial` |
| `schreyer-resolution/` | [schreyer-resolution/file-res-monomial-sorter.md](M2/Macaulay2/e/schreyer-resolution/file-res-monomial-sorter.md) | `MonomialSorterObject` |
| `schreyer-resolution/` | [schreyer-resolution/file-res-dep-graph.md](M2/Macaulay2/e/schreyer-resolution/file-res-dep-graph.md) | TBB dependency graph |
| `schreyer-resolution/` | [schreyer-resolution/file-res-moninfo.md](M2/Macaulay2/e/schreyer-resolution/file-res-moninfo.md) | `ResMonoid` dispatcher |
| `schreyer-resolution/` | [schreyer-resolution/file-res-schreyer-order.md](M2/Macaulay2/e/schreyer-resolution/file-res-schreyer-order.md) | `ResSchreyerOrder` |
| `schreyer-resolution/` | [schreyer-resolution/file-res-f4.md](M2/Macaulay2/e/schreyer-resolution/file-res-f4.md) | `F4Res` (F4 reduction loop) |
| `schreyer-resolution/` | [schreyer-resolution/file-res-monomial-types.md](M2/Macaulay2/e/schreyer-resolution/file-res-monomial-types.md) | Type vocabulary + encoding typedefs |
| `schreyer-resolution/` | [schreyer-resolution/file-res-f4-monlookup.md](M2/Macaulay2/e/schreyer-resolution/file-res-f4-monlookup.md) | `ResF4MonomialLookupTableT<Key>` |
| `schreyer-resolution/` | [schreyer-resolution/file-res-f4-m2-interface.md](M2/Macaulay2/e/schreyer-resolution/file-res-f4-m2-interface.md) | `ResF4toM2Interface` |
| `schreyer-resolution/` | [schreyer-resolution/file-res-memblock.md](M2/Macaulay2/e/schreyer-resolution/file-res-memblock.md) | `ResMemoryBlock<T>` |

### Per-area docs (quick navigation)

For each top-level area of the engine, there is a dedicated markdown file
alongside the source. Use these as your entry point when you know which area
you care about.

| Area | Doc | Covers |
|---|---|---|
| Coefficient rings | [coefficient-rings.md](M2/Macaulay2/e/coefficient-rings.md) | `aring-*`, `ZZ`, `ZZp`, `GF`, `coeffrings` |
| Polynomial rings | [polynomial-rings.md](M2/Macaulay2/e/polynomial-rings.md) | `polyring`, `poly`, `qring`, `frac`, `weylalg`, `skewpoly`, `solvable`, `localring`, `BasicPoly*` |
| Monoids & monomials | [monoids-and-monomials.md](M2/Macaulay2/e/monoids-and-monomials.md) | `monoid`, `monorder`, `imonorder`, `montable*`, `ExponentList`, `ExponentVector`; `f4/varpower-monomial`, `f4/ntuple-monomial` |
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
| `ExponentList.{cpp,hpp}`, `ExponentVector.hpp` | Top-level monomial encodings (sparse list + dense vector view); `f4/varpower-monomial.hpp`, `f4/ntuple-monomial.hpp` are F4-internal variants |
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
