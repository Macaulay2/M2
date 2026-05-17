# Package ecosystem

M2 ships ~400 user-contributed **packages** in addition to the
Core. This document is the **end-to-end reference** for the
package ecosystem: how packages work, how they relate to Core,
their lifecycle, conventions, and how to author one.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md) · [Build](BUILD.md) · [Testing](TESTING.md)

## What a package is

A **package** in M2 is a collection of types, methods, functions,
documentation, and tests bundled as one or more `.m2` files. The
canonical structure:

```
Foo.m2                          ← package entry point
Foo/                            ← auxiliary files (optional)
├── tests.m2
├── examples.m2
├── doc/
│   └── Foo-docs.m2
└── data/
```

Every distributed package follows this layout. See the full
conventions in
[`packages/file-package-conventions.md`](M2/Macaulay2/packages/file-package-conventions.md).

## Core vs distributed

```
┌──────────────────────────────────────────────────────────┐
│   Core package                                             │
│   (defined by ~100 .m2 files in M2/Macaulay2/m2/)         │
│   Always loaded at startup; every package implicitly       │
│   inherits.                                                 │
├──────────────────────────────────────────────────────────┤
│   Distributed packages                                     │
│   (~400 in M2/Macaulay2/packages/)                         │
│   Some auto-load on startup; others via `needsPackage`.   │
├──────────────────────────────────────────────────────────┤
│   User packages                                            │
│   (in ~/.Macaulay2/code/ or anywhere on the load path)    │
│   Loaded explicitly by the user.                          │
└──────────────────────────────────────────────────────────┘
```

The **Core** boundary is firm: types and methods defined in Core
m2 files (e.g., `Module`, `Matrix`, `Ring`, `gb`, `resolution`)
are always available. Distributed packages add user-facing
functionality on top.

## The `=distributed-packages` file

**Source**: `M2/Macaulay2/packages/=distributed-packages`.

A whitespace-sensitive list of packages M2 ships:

```
A1BrouwerDegrees
AbstractSimplicialComplexes
AbstractToricVarieties
AdjointIdeal
...
```

Rules:

- One package name per line.
- No trailing blank lines.
- Alphabetical convention (not enforced; for human navigation).

**To add a new package to the distribution**: append its name
here. To remove: delete the line. The build picks this up at
configure time.

## `newPackage` declaration

Every package starts with:

```m2
newPackage("Foo",
    Version => "1.0",
    Headline => "one-line description",
    AuxiliaryFiles => true,            -- if Foo/ subdir exists
    Authors => {
        {Name => "...", Email => "...", HomePage => "..."}
    },
    Keywords => {"Algebra"},
    PackageExports => {"Bar"},         -- packages we re-export
    PackageImports => {"Baz"},         -- packages we need
    HomePage => "https://...",
    DebuggingMode => false,
)
```

Fields are documented in
[`packages/file-package-conventions.md`](M2/Macaulay2/packages/file-package-conventions.md).

## Three sections of a package

```m2
newPackage("Foo", ...)

-- Section 1: Setup
export { ... }
importFrom(Core, ...)

-- Section 2: Code
foo = method(...)
foo PolynomialRing := R -> ...

-- Section 3: Documentation and tests
beginDocumentation()
doc ///
Key
   foo
Headline
   compute the foo
Description
  Text
    ...
  Example
    R = QQ[x, y]
    foo R
///

TEST ///
R = QQ[x, y]
assert(foo R == expected)
///
```

The **`beginDocumentation()`** marker is structural: M2 only
loads code before it during `loadPackage`; documentation and
tests load during `installPackage` or `check`.

## `PackageExports` vs `PackageImports`

| Directive | What it does |
|---|---|
| `PackageExports => {"Bar"}` | Loading Foo also re-exports Bar's symbols. Users of Foo see Bar's API. |
| `PackageImports => {"Baz"}` | Foo uses Baz internally but doesn't re-export. Foo's user doesn't see Baz directly. |

Example:

- `Polyhedra` `PackageImports {"FourierMotzkin"}` — uses
  Fourier-Motzkin internally but the API isn't part of `Polyhedra`'s
  public surface.
- `Complexes` `PackageExports {"Truncations"}` — automatically
  imports `Truncations` because the API is logically combined.

## The four foundational packages

Three packages are "structural" — every distribution includes them:

| Package | Role | Deep dive |
|---|---|---|
| `Macaulay2Doc` | The Core documentation. Every built-in type and function gets its `help` content here. | [`packages/file-Macaulay2Doc.md`](M2/Macaulay2/packages/file-Macaulay2Doc.md) |
| `Style` | HTML/CSS styling for generated docs + `generateGrammar` (used by `editors/`). | [`packages/file-Style.md`](M2/Macaulay2/packages/file-Style.md) |
| `EngineTests` | Engine-test M2-level suite (in `PACKAGES_DEVEL`, not shipped to users but run by CI). | [`packages/file-EngineTests.md`](M2/Macaulay2/packages/file-EngineTests.md) |
| `Macaulay2Doc/` | Auxiliary doc files for `Macaulay2Doc`. | (linked above) |

`Macaulay2Doc` and `Style` always ship; `EngineTests` is
developer-only.

## Documentation DSL

**Source**:
[`m2/file-document.md`](M2/Macaulay2/m2/file-document.md).

Every package documents its API with the `doc ///...///` DSL:

```m2
doc ///
Key
   (foo, Ring, ZZ)
Headline
   compute the foo of a ring at a level
Usage
   y = foo(R, n)
Inputs
   R:Ring
      the base ring
   n:ZZ
      the level
Outputs
   :Sequence
      a pair of (Matrix, Module)
Description
   Text
      The foo of a ring computes ...
   Example
      R = QQ[x, y]
      foo(R, 3)
   CannedExample
      i1 : foo R
      o1 = ...
SeeAlso
   bar
   baz
///
```

**Key invariants**:

- **Cross-references** (`SeeAlso => bar`) are **validated** —
  unknown references fail at install time.
- **Example blocks** are **executed during `installPackage`** —
  outputs are captured. Typos become test failures.
- **The `Key` is type-checked**: `(foo, Ring, ZZ)` means "the
  documentation for `foo` with arguments of types `Ring`,
  `ZZ`."

## TEST blocks

```m2
TEST ///
R = QQ[x, y];
assert(foo(R, 3) == expected)
assert(rank target M == 5)
///
```

Each `TEST ///...///` block registers a test the `check
"PackageName"` runs. Multiple per package are typical.

See [`TESTING.md`](TESTING.md) section 3 for the test machinery.

## The package lifecycle

### 1. Write

Drop `Foo.m2` (and optional `Foo/`) into
`M2/Macaulay2/packages/`. Use the
[conventions](M2/Macaulay2/packages/file-package-conventions.md)
above.

### 2. Load (iteration)

From inside an M2 session:

```m2
loadPackage("Foo", Reload => true)
```

`Reload => true` re-reads the file even if previously loaded —
the right thing for development iteration.

For code-only changes, this is fast (seconds). It only re-loads
*code*; docs/tests aren't re-processed.

### 3. Install (slow)

```m2
installPackage "Foo"
```

This runs every example, captures every output, builds the HTML
docs, creates the GDBM info database, and writes everything to
the install prefix. Slow — minutes for a non-trivial package.

See [`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md).

### 4. Test

```m2
check "Foo"
```

Runs every `TEST ///...///` block. Typically seconds to minutes.

### 5. Distribute

To make a new package ship with M2:

1. Add the name to
   `M2/Macaulay2/packages/=distributed-packages`.
2. If the package needs an external library, add a
   [`Find<Lib>.cmake`](M2/cmake/file-find-cmakes.md) and a
   [`libraries/<lib>/`](M2/libraries/file-per-library-subdirs.md)
   wrapper.
3. Open a PR.
4. CI builds and tests the package on every supported platform.
5. Once merged, the package ships in the next release.

### 6. Update

Edit the package. Re-test. Bump the `Version` field. CI on the
PR catches regressions. After merge the new version ships in
the next release.

## Package categories (rough)

```
~ 60   Computational algebra      (Complexes, Cremona, Groebner, ...)
~ 50   Algebraic geometry         (NormalToricVarieties, NumericalAG, ...)
~ 80   Commutative algebra        (LocalRings, Depth, Posets, ...)
~ 40   Combinatorics              (Polyhedra, Graphs, ...)
~ 30   Numerical                  (Bertini, NumericalLinearAlgebra, NAGtypes, ...)
~ ~    Misc utility               (PackageTemplate, Style, ...)
```

The full set lives in
[`M2/Macaulay2/packages/`](M2/Macaulay2/packages/README.md).

## External-library dependencies

Some packages need an **external program or library** to function:

| Package | External | Wrapper |
|---|---|---|
| `FourTiTwo` | 4ti2 | [`libraries/4ti2/`](M2/libraries/file-per-library-subdirs.md) |
| `Polyhedra` | lrslib | [`libraries/lrslib/`](M2/libraries/file-per-library-subdirs.md) |
| `Normaliz` | Normaliz | [`libraries/normaliz/`](M2/libraries/file-per-library-subdirs.md) |
| `Bertini` | Bertini | [`libraries/bertini/`](M2/libraries/file-per-library-subdirs.md) |
| `gfanInterface` | gfan | [`libraries/gfan/`](M2/libraries/file-per-library-subdirs.md) |
| `CohomCalg` | cohomCalg | [`libraries/cohomcalg/`](M2/libraries/file-per-library-subdirs.md) |
| `Topcom` | TOPCOM | [`libraries/topcom/`](M2/libraries/file-per-library-subdirs.md) |

The CMake build conditionally enables each package based on the
external availability — see
[`packages/CMakeLists.txt`](M2/Macaulay2/packages/) for the
wiring.

## Discovering packages

**From within M2**:

```m2
help "available packages"   -- list distributed packages
help PackageName            -- per-package docs
viewHelp PackageName        -- open HTML docs in browser
```

**In the source tree**:

- [`M2/Macaulay2/packages/`](M2/Macaulay2/packages/README.md)
  with one `.m2` file (or `.m2`+subdir) per package.
- [`=distributed-packages`](M2/Macaulay2/packages/) lists which
  ones ship.

**Online**:

- [Macaulay2 doc site](https://macaulay2.com/doc/Macaulay2-1.26.05/share/doc/Macaulay2/) — every package's HTML docs.
- The [GitHub Macaulay2 organisation](https://github.com/Macaulay2)
  for related repositories.

## Auto-loaded packages

The M2 banner lists packages "with packages: ..." — these are
**auto-loaded at startup**. Currently:

```
ConwayPolynomials, Elimination, IntegralClosure, InverseSystems,
Isomorphism, LLLBases, MinimalPrimes, OnlineLookup,
PrimaryDecomposition, ReesAlgebra, Saturation, TangentCone,
Truncations, Varieties
```

These are foundational enough that they're useful without
`needsPackage` and small enough that auto-load doesn't bloat
startup. Adding to this set requires careful consideration of
startup time impact.

See [`STARTUP.md`](STARTUP.md) phase 8 (Core load) — auto-loaded
packages are loaded there.

## CMake-side package wiring

**Source**: `M2/Macaulay2/packages/CMakeLists.txt`.

For each distributed package:

```cmake
# Conditionally enable based on library detection
if(FFLAS_FFPACK_FOUND OR BUILD_FFLAS_FFPACK)
  add_package(Schubert2)
endif()
```

Packages that need an external library are gated on the
library's detection. If the library isn't available, the
package doesn't get installed (but still gets built).

## Special directives

```m2
newPackage("Foo",
    DebuggingMode => false,
    OptionalComponentsPresent => boolean,
    InfoDirSection => "Macaulay2 and its packages",
    ...
)
```

| Directive | What it does |
|---|---|
| `DebuggingMode` | If true, errors drop into the M2 debugger. False for production. |
| `OptionalComponentsPresent` | Run-time gate: if false, package errors out informatively. |
| `InfoDirSection` | Where the info database registers the package. |
| `Reload` (passed to `loadPackage`) | Force re-read even if already loaded. |
| `Configuration` | Per-user config defaults. |

## How users interact with packages

```m2
-- Once-per-session: load
needsPackage "Foo"

-- Always available after that
foo R

-- Documentation
help foo

-- See SeeAlso links
viewHelp foo

-- Per-user package configuration
options Foo

-- Reload after editing
loadPackage("Foo", Reload => true)
```

User-level packages (in `~/.Macaulay2/code/` or wherever the
user's load path points) work the same way as distributed
packages. The only difference is whether they ship with M2's
distribution.

## Common pitfalls

### Forgetting `=distributed-packages`

A new package can be in `packages/Foo.m2` but **not appear in
`M2`** unless you add `Foo` to the `=distributed-packages`
list. This is the most common new-package mistake.

### `beginDocumentation()` order

Code before `beginDocumentation()` runs at `loadPackage` time.
Code after only runs at `installPackage` / `check` time.
Putting a doc-generation helper before the marker → it runs but
no docs are produced. Putting algorithm code after the marker →
the algorithm only exists during install/check.

### `PackageImports` vs `needsPackage`

`PackageImports` is **declarative**: it gets loaded as part of
your package's setup. `needsPackage` inside your package body is
**imperative**: it gets loaded when execution reaches that line.

Prefer `PackageImports`.

### Example failures

`Example ...` blocks must succeed. A failing example fails the
install. Either:

- Fix the example.
- Use `CannedExample ...` (output is taken literally — useful
  for examples whose outputs are reproducible-but-tedious).

### Documentation node not found

If you write `SeeAlso => myFunction` and there's no doc node
for `myFunction`, install fails. Either remove the reference or
write the missing node.

## Why so many packages?

M2's design is **package-centric**: Core stays small (basics),
and specialised functionality lives in packages. Benefits:

- Users opt into what they need (`needsPackage`).
- Different research domains evolve their packages
  independently.
- Documentation stays focused (each package has its own
  manual).
- Maintenance scales — package authors are responsible for
  their own packages, not Core maintainers.

The cost: discoverability. The
[Macaulay2 doc site](https://macaulay2.com/doc/Macaulay2-1.26.05/share/doc/Macaulay2/)
provides a global index.

## Related

- [`README.md`](README.md) — repository TOC.
- [`TOUR.md`](TOUR.md) — Path C (package author) covers the
  full author workflow.
- [`BUILD.md`](BUILD.md) — phase 7 (`install-packages`,
  `check-packages`).
- [`TESTING.md`](TESTING.md) — section 3 (per-package tests).
- [`STARTUP.md`](STARTUP.md) — phase 8 (Core + auto-loaded
  packages).
- [`M2/Macaulay2/packages/README.md`](M2/Macaulay2/packages/README.md)
  — directory index.
- [`M2/Macaulay2/packages/file-package-conventions.md`](M2/Macaulay2/packages/file-package-conventions.md)
  — full conventions reference.
- [`M2/Macaulay2/packages/file-Macaulay2Doc.md`](M2/Macaulay2/packages/file-Macaulay2Doc.md)
  · [`file-Style.md`](M2/Macaulay2/packages/file-Style.md)
  · [`file-EngineTests.md`](M2/Macaulay2/packages/file-EngineTests.md)
  — foundational-package deep dives.
- [`M2/Macaulay2/m2/file-document.md`](M2/Macaulay2/m2/file-document.md)
  — documentation DSL implementation.
- [`M2/Macaulay2/m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md)
  — `installPackage` machinery.
- Project [Wiki](https://github.com/Macaulay2/M2/wiki) and
  [doc site](https://macaulay2.com/doc/) for end-user docs.
