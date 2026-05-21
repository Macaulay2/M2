# Package conventions — file layout, dependencies, distribution

This doc catalogues the **conventions** every M2 package follows.
With ~400 packages in this directory, the conventions matter:
they're what makes M2's package ecosystem coherent.

Part of [`packages/`](README.md).

[← packages/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Two file layouts

```
Single-file package:
  Foo.m2                    (entire package)

Multi-file package:
  Foo.m2                    (entry point, declares package, exports)
  Foo/
  ├── loads.m2              (or similar — manifest)
  ├── doc/                  (documentation .m2 files)
  ├── tests.m2              (or test-*.m2)
  ├── examples/             (example data)
  └── ...
```

`Foo.m2` is **always present** — even multi-file packages have
one entry point. M2's package loader looks for `Foo.m2` first.

## `newPackage` declaration

```m2
newPackage("Foo",
    Version => "1.0",
    Headline => "one-line description",
    AuxiliaryFiles => true,         -- if multi-file
    Authors => {
        {Name => "...", Email => "...", HomePage => "..."}
    },
    Keywords => {"Algebra", "..."},
    PackageExports => {"Bar", "..."},   -- packages we re-export
    PackageImports => {"Baz", "..."},   -- packages we need but don't re-export
    HomePage => "https://...",
    DebuggingMode => false,
    OptionalComponentsPresent => boolean,   -- if package depends on optional libs
)
```

Every field except `Version` and `Headline` is optional. Common
patterns:

- **`AuxiliaryFiles => true`** — declares the `Foo/` subdir
  exists.
- **`PackageExports`** vs **`PackageImports`** — re-export the
  package's API too, or just use it internally.

## Three sections of a package

```m2
newPackage("Foo", ...)

-- Section 1: top-level setup
export { ... }
importFrom(Core, ...)

-- Section 2: code
foo = method(...)
foo PolynomialRing := R -> ...
...

-- Section 3: documentation and tests
beginDocumentation()
doc ///
Key
   Foo
Headline
   ...
///

TEST ///
R = QQ[x, y]
assert(foo R == ...)
///
```

The `beginDocumentation()` marker is **structurally important**:
M2 only loads code before it during `loadPackage`; doc/test load
during `installPackage` or `check`.

## Documentation DSL

Inside `doc ///...///`:

```m2
doc ///
Node
   Key
      myFunction
   Headline
      compute the foo of a bar
   Usage
      myFunction(x, y)
   Inputs
      x:Ring
         the base ring
      y:ZZ
         degree limit
   Outputs
      :Sequence
         a pair of matrices
   Description
      Text
         This function computes the @TO myFunction@ of the input ...
      Example
         R = QQ[x, y];
         myFunction(R, 3)
   SeeAlso
      relatedFunction
///
```

Documented in [`../m2/file-document.md`](../m2/file-document.md).
Key invariants:

- Examples are **executed** during `installPackage` and their
  output captured.
- Cross-references (`@TO ...@`, `SeeAlso`) are **validated**.
- Failure during example execution **fails the build**.

## Tests

```m2
TEST ///
R = QQ[x, y];
assert(myFunction R == expectedValue)
///
```

The `TEST` macro registers a test the `check` function will run.
Multiple `TEST` blocks per package are typical.

## `=distributed-packages`

The whitespace-sensitive file that controls which packages ship.
The Core README explicitly notes:

> The file is whitespace-sensitive: one name per line, no
> trailing blank lines.

Adding a new package without appending its name here means it
won't be installed even though it builds.

## Categories of packages

Rough taxonomy of the ~400 packages:

| Category | Examples | Count |
|---|---|---|
| Doc/Style/Test | Macaulay2Doc, Style, EngineTests | 3-4 |
| Computational algebra | Complexes, Cremona, Groebner, ... | ~60 |
| Algebraic geometry | NumericalAlgebraicGeometry, NormalToricVarieties, ... | ~50 |
| Commutative algebra | LocalRings, Depth, Posets, ... | ~80 |
| Combinatorics | Polytopes, Polyhedra, Graphs, ... | ~40 |
| Numerical | NumericalLinearAlgebra, Bertini, NAGtypes, ... | ~30 |
| Misc / utility | StringTorics, Tropical, PackageTemplate, ... | many |

Each category has its own conventions, but all share the
overall file layout, documentation DSL, and test pattern.

## Per-package CMakeLists

Some packages need external libraries to install:

```cmake
# in packages/CMakeLists.txt
if(NORMALIZ_FOUND)
  list(APPEND PACKAGES Normaliz)
endif()
```

The `Find*.cmake` modules in [`../../cmake/`](../../cmake/README.md)
support these per-package conditionals.

## Used by

- The CMake/autotools build, when assembling the distribution.
- Package authors writing new packages.

## Related

- [`README.md`](README.md) — packages/ overview.
- [`file-Macaulay2Doc.md`](file-Macaulay2Doc.md) — exemplar package.
- [`file-Style.md`](file-Style.md) — sister doc-support package.
- [`file-EngineTests.md`](file-EngineTests.md) — exemplar test-only
  package.
- [`../m2/file-packages.md`](../m2/file-packages.md) — M2-side
  package machinery.
- [`../m2/file-document.md`](../m2/file-document.md) — doc DSL.
- [`../m2/file-installPackage.md`](../m2/file-installPackage.md) —
  what runs at install time.
