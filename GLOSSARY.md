# Glossary

A cross-cutting reference for **terminology** used across the M2
documentation tree. Each entry points at the deep-dive doc where
the term is treated in detail.

[← repository TOC](README.md)

## Build system

**autotools** — the GNU `configure` / `make` build system. M2's
autotools entry points are
[`configure.ac`](M2/file-configure-ac.md),
[`autogen.sh`](M2/file-autogen-sh.md), and
[`Makefile.in`](M2/file-Makefile-in.md). Counterpart to CMake.

**CMake** — the modern preferred build system. M2's CMake entry
point is [`CMakeLists.txt`](M2/file-CMakeLists-txt.md), with
modules in [`M2/cmake/`](M2/cmake/README.md). CI on macOS uses
CMake.

**`scc1`** — M2's custom compiler-compiler that translates `.d` /
`.dd` source into C / C++. Lives in
[`M2/Macaulay2/c/`](M2/Macaulay2/c/README.md). See
[c/architecture.md](M2/Macaulay2/c/architecture.md).

**`.d`** / **`.dd`** — M2's home-grown safer-than-C language for
the interpreter. `.d` compiles to C; `.dd` to C++. Both go through
`scc1`. The language spec lives in
[`M2/Macaulay2/c/README`](M2/Macaulay2/c/README) (plain text).

**`Ccode(t, "...", args)`** — `.d` escape hatch for embedding
inline C. The `t` argument is the return type. Used pervasively in
FFI bindings.

**`VERSION`** — single-source-of-truth file at
[`M2/VERSION`](M2/file-VERSION.md). Both build systems read it.

**`startup.m2`** — Core M2 code embedded into the M2 binary at
build time. The CMake template is
[`bin/startup.c.cmake`](M2/Macaulay2/bin/file-startup.md); the M2
source is `m2/startup.m2.in`.

## Architecture: layers

**The four-language stack** —
`.d`/`.dd` → C/C++ → M2-interpreter → M2. See
[`README.md#the-four-language-stack`](README.md#the-four-language-stack).

**Translator** — [`Macaulay2/c/`](M2/Macaulay2/c/architecture.md).

**Interpreter** — [`Macaulay2/d/`](M2/Macaulay2/d/architecture.md).
Becomes `M2-interpreter` after `scc1` translation.

**Engine** — [`Macaulay2/e/`](M2/Macaulay2/e/architecture.md).
C++ mathematical kernel; ~340 source files.

**Core M2** — [`Macaulay2/m2/`](M2/Macaulay2/m2/architecture.md).
The ~100 `.m2` files loaded at startup defining the `Core` package.

## Engine: ring framework

**`Ring`** — the legacy abstract ring base class. Virtual-dispatch.
See [`file-ring.md`](M2/Macaulay2/e/file-ring.md).

**`aring`** — the modern templated ring framework. Compile-time
dispatch. See [`file-aring.md`](M2/Macaulay2/e/file-aring.md) and
[`ring-elements-and-maps.md`](M2/Macaulay2/e/ring-elements-and-maps.md).

**`ARingZZpFlint`**, **`ARingZZpFFPACK`**, **`ARingQQGMP`**,
**`ARingGFFlint`**, etc. — specific `aring` instances per
coefficient ring. See
[coefficient-rings.md](M2/Macaulay2/e/coefficient-rings.md).

**`ConcreteRing<RingType>`** — bridge wrapping an `aring`-style
ring so it presents as legacy `Ring`. See
[`file-aring-glue.md`](M2/Macaulay2/e/file-aring-glue.md).

**`ring_elem`** — legacy universal value type. Tagged union. See
[`file-ringelem.md`](M2/Macaulay2/e/file-ringelem.md).

**`ElementType`** — `aring` framework typed value. Per-ring
typedef.

**`promote`** / **`lift`** — legacy ring-element conversion methods
on `Ring`. Modern code uses `ConversionMap<From,To>` templates
([`file-aring-translate.md`](M2/Macaulay2/e/file-aring-translate.md)).

## Engine: matrices

**`Matrix`** — immutable matrix. Source/target are `FreeModule`s.
See [matrices.md](M2/Macaulay2/e/matrices.md).

**`MutableMatrix`** — mutable matrix abstract base. See
[`file-mat.md`](M2/Macaulay2/e/file-mat.md).

**`DMat<R>`** — dense matrix template, parameterised on ring. See
[`file-dmat.md`](M2/Macaulay2/e/file-dmat.md).

**`SMat<R>`** — sparse matrix template. See
[`file-smat.md`](M2/Macaulay2/e/file-smat.md).

**`MutableMat<MatT>`** — virtual wrapper over a templated `DMat`
or `SMat`. Bridge between the templated and virtual worlds. See
[`file-mutablemat-imp.md`](M2/Macaulay2/e/file-mutablemat-imp.md).

**FFPACK** — finite-field BLAS/LAPACK library, used heavily for
`DMat<Z/p>` linear algebra.

**FLINT** — fast number-theory library. Used for `DMat<Z/p>`,
`DMat<QQ>`, `DMat<GF(p^n)>` operations. See submodule.

## Engine: free modules and resolutions

**`FreeModule`** — a free `R`-module `R^n`. Carries rank and
degrees-of-generators vectors. See
[free-modules.md](M2/Macaulay2/e/free-modules.md).

**Schreyer order** — a monomial order on a free module induced
from "leading monomials of a presentation matrix's columns." Makes
syzygy computation efficient. See
[`file-schorder.md`](M2/Macaulay2/e/file-schorder.md).

**Schreyer frame** — the carried-along Schreyer-order data through
successive resolution levels. Enables F4-style resolution. See
[schreyer-resolution/architecture.md](M2/Macaulay2/e/schreyer-resolution/architecture.md).

**Free resolution** — exact complex
`… → F_2 → F_1 → F_0 → M → 0` of free modules. See
[resolutions.md](M2/Macaulay2/e/resolutions.md).

**Betti table** — the human-readable summary of a resolution
(ranks per homological+internal-degree cell). See
[`file-betti.md`](M2/Macaulay2/e/file-betti.md).

## Engine: Gröbner bases

**Gröbner basis (GB)** — a generating set for an ideal with the
property that leading terms generate the ideal of leading terms.
See [groebner-bases.md](M2/Macaulay2/e/groebner-bases.md).

**Reduced GB** — the canonical form. See
[`file-reducedgb.md`](M2/Macaulay2/e/file-reducedgb.md).

**S-polynomial** / **S-pair** — Buchberger's primary algorithmic
unit. `S(f, g) = lcm(lt f, lt g)/lt(f) · f - lcm(...)/lt(g) · g`.

**F4** — Faugère's algorithm replacing per-S-pair reduction with
Macaulay-matrix sweeps. M2 has two implementations:
[`f4/`](M2/Macaulay2/e/f4/architecture.md) (original) and
[`gb-f4/`](M2/Macaulay2/e/gb-f4/architecture.md) (refactored).

**Macaulay matrix** — the matrix F4 builds containing all
S-polynomials and reducer-monomials in a given degree.

**Involutive basis / Janet basis** — a stronger structure than GB
with unique reduction. M2's BIBasis engine
([`bibasis/architecture.md`](M2/Macaulay2/e/bibasis/architecture.md))
computes Boolean involutive bases.

**mathicgb** — submodule providing a Stillman/Roune
signature-based GB engine. See
[`submodules/`](M2/submodules/README.md).

## Engine: computations

**`Computation`** — abstract base for resumable long-running
computations (GB, resolution, Hilbert, LLL, NAG). See
[`file-comp.md`](M2/Macaulay2/e/file-comp.md).

**Stop conditions** — degree limit, basis-element limit, syzygy
limit, pair limit, etc. Set via `set_stop_conditions`. See
[`file-comp.md`](M2/Macaulay2/e/file-comp.md).

**`GBComputation`** — Computation subclass for Gröbner bases.

**`ResolutionComputation`** — Computation subclass for resolutions.

**Hilbert function / Hilbert series** — dimension counts per
internal degree, summarised as a generating function. See
[`file-hilb.md`](M2/Macaulay2/e/file-hilb.md).

**LLL** — Lenstra-Lenstra-Lovász lattice reduction. See
[`file-LLL.md`](M2/Macaulay2/e/file-LLL.md).

**NAG** — Numerical Algebraic Geometry. See
[`file-NAG.md`](M2/Macaulay2/e/file-NAG.md).

**SLP** — straight-line program. Used by NAG for cheap repeated
evaluation. See [`file-SLP.md`](M2/Macaulay2/e/file-SLP.md).

**CRT** — Chinese Remainder Theorem. Used for modular methods over
QQ. See [`file-cra.md`](M2/Macaulay2/e/file-cra.md).

## Engine: memory and primitives

**Boehm GC** / **bdwgc** — Boehm-Demers-Weiser conservative
garbage collector. M2's GC throughout. Vendored as
[submodule `bdwgc`](M2/submodules/README.md).

**`our_new_delete`** — GC-aware `operator new` / `delete`
overrides. Inherited by most engine classes. See
[`file-newdelete.md`](M2/Macaulay2/e/file-newdelete.md).

**`MutableEngineObject`** — GC base class for mutable + tracked
objects. See [`file-hash.md`](M2/Macaulay2/e/file-hash.md).

**`MemoryBlock<T>`** — bump-pointer pool allocator. Used in F4
inner loops. See
[`file-MemoryBlock.md`](M2/Macaulay2/e/file-MemoryBlock.md).

**`FastAllocator`** — BIBasis's custom slab allocator. See
[`bibasis/file-allocator.md`](M2/Macaulay2/e/bibasis/file-allocator.md).

**`safe::add` / `safe::mul` / `safe::pow`** — overflow-checked
arithmetic. Pervasive in monomial-degree code. See
[`file-overflow.md`](M2/Macaulay2/e/file-overflow.md).

**Finaliser** — callback invoked by GC when wrapping an external-
library type (GMP, MPFR, FLINT). See
[`file-finalize.md`](M2/Macaulay2/e/file-finalize.md).

**TBB** — Intel Threading Building Blocks. Used by the modern
resolution engine. See [`file-m2tbb.md`](M2/Macaulay2/e/file-m2tbb.md).

## Interpreter

**`Expr`** — discriminated union of every M2 runtime value type
(ZZcell, Sequence, HashTable, ...). See
[`file-expr.md`](M2/Macaulay2/d/file-expr.md).

**`Symbol`** / **`SymbolClosure`** — names and their values in
scopes. See [`file-tokens.md`](M2/Macaulay2/d/file-tokens.md).

**`HashTable`** / **`MutableHashTable`** — pervasive associative
type. See
[`file-hashtables.md`](M2/Macaulay2/d/file-hashtables.md).

**`Net`** — 2D character grid for pretty-printing matrices,
polynomials, etc. See [`file-nets.md`](M2/Macaulay2/d/file-nets.md).

**`varstring`** / **`varnet`** — mutable builder versions of
string / net. See
[`file-strings.md`](M2/Macaulay2/d/file-strings.md).

**Error flag** — thread-local "an error happened" indicator. M2's
error-propagation mechanism (replaces C++ exceptions across the C
ABI). See [`file-err.md`](M2/Macaulay2/d/file-err.md).

**Interrupt flag** — thread-local "user pressed Ctrl-C"
indicator. Polled by long-running loops. See
[`file-interrupts.md`](M2/Macaulay2/d/file-interrupts.md).

**Supervisor** — thread-pool manager for M2 `Task` /
`schedule`. See
[`../system/file-supervisor.md`](M2/Macaulay2/system/file-supervisor.md).

## Core M2

**Core package** — the package every other package implicitly
imports. Defined by the ~100 m2 files in
[`m2/`](M2/Macaulay2/m2/architecture.md).

**`loadsequence`** — script defining the load order of m2 files.
See [`file-loadsequence.md`](M2/Macaulay2/m2/file-loadsequence.md).

**`newPackage`** — M2-level package declaration. See
[`file-packages.md`](M2/Macaulay2/m2/file-packages.md).

**`loadPackage`** vs **`needsPackage`** — load by-name (force
reload) vs only-if-needed.

**`installPackage`** — generate HTML docs, run examples, build
info database. Slow. See
[`file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md).

**Documentation DSL** — `doc ///...///` syntax for typed M2
documentation. See
[`file-document.md`](M2/Macaulay2/m2/file-document.md) and
[`packages/file-Macaulay2Doc.md`](M2/Macaulay2/packages/file-Macaulay2Doc.md).

**`TEST ///...///`** — package-level test block. Run with
`check "PackageName"`.

**`Method`** — M2's typed function-dispatch. See
[`file-methods.md`](M2/Macaulay2/m2/file-methods.md).

**`Type`** — M2's class system. Every value has a type; types
form an inheritance tree rooted at `Thing`. See
[`file-classes.md`](M2/Macaulay2/m2/file-classes.md).

**`Expression`** — M2's expression-AST type used by `format`. See
[`file-expressions.md`](M2/Macaulay2/m2/file-expressions.md).

**`OptionTable`** — keyword arguments. See
[`file-option.md`](M2/Macaulay2/m2/file-option.md).

## External libraries

**GMP** — GNU Multi-Precision arithmetic. Integers (`mpz_t`) and
rationals (`mpq_t`).

**MPFR** — multi-precision floating-point with correct rounding.

**MPFI** — interval arithmetic on top of MPFR.

**FLINT** — fast number-theory library. Includes Arb (interval
arithmetic, formerly separate).

**FFPACK / fflas-ffpack** — finite-field BLAS-style linear algebra.

**Factory** — polynomial GCD / factoring library.

**NTL** — Victor Shoup's number theory library. M2 uses it for
some LLL and polynomial-factoring paths.

**Boost** — used for Stacktrace (crash reporting),
Multiprecision (in `boostmath.dd`), and Regex (M2's `match` and
friends).

**libffi** — generic FFI library. M2's
[`file-ffi.md`](M2/Macaulay2/d/file-ffi.md) wraps it.

**Jansson** — JSON parser used by
[`file-json.md`](M2/Macaulay2/d/file-json.md).

**libxml2** — XML parser used by
[`file-xml.md`](M2/Macaulay2/d/file-xml.md).

**CPython** — Python C API. M2 embeds Python via
[`file-python.md`](M2/Macaulay2/d/file-python.md).

**MPSolve** — polynomial root-finder used by
[`interface/file-polyroots.md`](M2/Macaulay2/e/interface/file-polyroots.md).

**TBB** — Intel Threading Building Blocks (concurrent algorithms).

**Sphinx + Doxygen** — documentation toolchain for the engine's
C++ API. See [`docs/`](M2/Macaulay2/docs/README.md).

## Common abbreviations

| Acronym | Meaning |
|---|---|
| GB | Gröbner basis |
| NC | Non-commutative |
| F4 | Faugère's GB algorithm using Macaulay matrices |
| BIBasis | Boolean Involutive basis engine |
| SLP | Straight-line program |
| CRT | Chinese Remainder Theorem |
| LLL | Lenstra-Lenstra-Lovász lattice reduction |
| NAG | Numerical Algebraic Geometry |
| GC | Garbage collector (Boehm GC) |
| FFI | Foreign Function Interface |
| TBB | Threading Building Blocks (Intel) |
| ABI | Application Binary Interface |
| CI | Continuous Integration |
| DSL | Domain-Specific Language |

## Conventions

**`file-<basename>.md`** — per-file deep-dive doc. Lives in the
same directory as the source file.

**`README.md`** — per-directory navigation hub.

**`architecture.md`** — per-layer / per-subdirectory architectural
reference. Sits beside `README.md` when the directory has enough
architectural complexity to merit one.

**`IM2_<Type>_<verb>`** — C-ABI engine entry point naming
convention. See
[`interface/architecture.md`](M2/Macaulay2/e/interface/architecture.md).

**`raw<Verb>`** — interpreter-side names for engine boundary
calls. E.g. `rawGB`, `rawMatrix`. Maps to `IM2_*` on the engine
side.

**`@VAR@`** — autoconf/CMake substitution placeholder. Filled in
at configure time.

## Related

- [`README.md`](README.md) — top-level repository TOC.
- [`INDEX.md`](INDEX.md) — flat alphabetical doc catalogue.
- [`CHEATSHEET.md`](CHEATSHEET.md) — one-page command card.
- [`TOUR.md`](TOUR.md) — audience-specific reading orders.
- Project [Wiki](https://github.com/Macaulay2/M2/wiki) and
  `.github/workflows/test_build.yml` — build instructions.
- All architecture references —
  [`c/`](M2/Macaulay2/c/architecture.md) ·
  [`d/`](M2/Macaulay2/d/architecture.md) ·
  [`e/`](M2/Macaulay2/e/architecture.md) ·
  [`m2/`](M2/Macaulay2/m2/architecture.md) ·
  6 engine-subdir architecture docs.
