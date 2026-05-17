# `EngineTests.m2` + `EngineTests/` — engine-level test suite

`EngineTests` is a **test-only package** that exercises the
engine's correctness across rings, matrices, linear algebra, and
specialised algorithms. Not installed in user-facing builds —
listed in `PACKAGES_DEVEL`, not `PACKAGES` — but run by CI.

Part of [`packages/`](README.md).

[← packages/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
newPackage(
        "EngineTests",
    	AuxiliaryFiles => true,
        Version => "0.1", 
        Date => "29 Aug 2011",
	    Authors => {
            {Name => "Michael E. Stillman", 
		        Email => "mike@math.cornell.edu", ...},
	        {Name => "Jakob Kröker", 
		        Email => "kroeker@math.uni-hannover.de", ...}
            },
        Headline => "a test suite for the Macaulay2 engine",
        PackageExports => {"Complexes"},
	Keywords => {"Miscellaneous"},
        AuxiliaryFiles=> true
        )

export { 
   "runTests",
   "testNorm",
   "testClean",
   "hasFFPACK",
   "fields",
   "finitefields",
   "fieldsFFPACK",
   "fieldsFLINT",
   "fieldsGF",
   "ringsPID",
```

Authored by Mike Stillman + Jakob Kröker (2011). The exports
reveal the test taxonomy:

- **Test categories** — `testNorm`, `testClean`, etc.
- **Ring catalogues** — `fields`, `finitefields`, `fieldsFFPACK`,
  `fieldsFLINT`, `fieldsGF`, `ringsPID`.
- **Capability checks** — `hasFFPACK` (whether the engine has
  FFPACK linked in).
- **Driver** — `runTests` to run them all.

## Why this exists alongside `e/unit-tests/`

Two parallel test suites for the engine:

| Suite | Language | Tests via |
|---|---|---|
| [`Macaulay2/e/unit-tests/`](../e/unit-tests/README.md) | C++ | gtest, direct C++ engine API |
| `EngineTests` (this package) | M2 | M2 user-level API + observable behaviour |

`unit-tests/` is fast and verifies engine internals work. But
they can't catch:

- **Boundary bugs** — where engine results cross the C ABI into
  M2 and get misinterpreted.
- **High-level correctness** — does `gb I` actually return the
  Gröbner basis users expect?
- **Cross-feature interaction** — does `det (X * Y)` give the
  right answer when X, Y are inverse-Hilbert-function-driven?

`EngineTests` runs from M2's perspective. End-to-end. Catches
bugs `unit-tests/` misses.

## Why not in `PACKAGES`?

The README at the top of `packages/` notes:

> `EngineTests` lives in `PACKAGES_DEVEL` and is checked but not
> installed.

That is:

- **Checked** — `make check` runs its tests on every build.
- **Not installed** — end users don't get `EngineTests` in their
  shipped M2. It's developer-only.

## `PackageExports => {"Complexes"}`

```m2
PackageExports => {"Complexes"},
```

EngineTests depends on the `Complexes` package because some of
its resolution / homology tests need that DSL. The
`PackageExports` directive tells M2's load system "if you load
EngineTests, also export Complexes."

## What runs

Inside `EngineTests/`, file structure:

```
EngineTests/
├── Engine.m2          (master loader)
├── Ring.m2            (ring-level tests)
├── Matrix.m2          (matrix tests)
├── DMat.m2            (dense matrix)
├── SMat.m2            (sparse matrix)
├── LinearAlgebra.m2   (linalg over various rings)
├── Resolution.m2      (resolution tests)
├── FreeResolution.m2  (newer res engine)
├── GBTests.m2         (Gröbner basis correctness)
├── NCAlgebra.m2       (NC algebra tests)
└── ...
```

The master `Engine.m2` loads them all. `runTests` iterates and
reports.

## Used by

- CI's `make check` / `ctest -R EngineTests`.
- Engine developers verifying changes don't regress.

## Related

- [`README.md`](README.md) — packages/ overview.
- [`../e/unit-tests/README.md`](../e/unit-tests/README.md) —
  sister C++ test suite.
- [`../tests/README.md`](../tests/README.md) — broader integration
  tests.
- `PACKAGES_DEVEL` line in
  [`../packages/CMakeLists.txt`](CMakeLists.txt).
