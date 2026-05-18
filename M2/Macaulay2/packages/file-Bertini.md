# `Bertini.m2` — interface to the Bertini numerical solver

The `Bertini` package is M2's **interface to the external Bertini
homotopy-continuation solver**. Bertini is the de facto industry-
standard polynomial-system solver in numerical algebraic geometry;
this package exposes its capabilities to M2 users through a
standard M2 API.

Used **directly** (via `bertiniZeroDimSolve`, `bertiniPosDimSolve`,
`bertiniParameterHomotopy`, etc.) **or indirectly** as a backend
to [`NumericalAlgebraicGeometry`](file-NumericalAlgebraicGeometry.md)
when you pass `Software => BERTINI`.

- Main file: `Bertini.m2` (3 910 lines)
- Auxiliary directory: `Bertini/` (doc, tests, and `examples/`
  containing 30+ example-output files — a `CacheExampleOutput =>
  true` package that caches example output for fast doc rebuilds)
- Authors: Elizabeth Gross, Jose Israel Rodriguez, Dan Bates, Anton Leykin
- Version: 2.1.2.3 (February 2024)
- Re-exports / Imports: `NAGtypes`
- Configuration: `BERTINIexecutable => "bertini"` — the binary name
- **Requires Bertini installed externally** (checked at load with `bertiniPresent`)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Top-level Bertini operations

```m2
bertiniZeroDimSolve F                -- solve a zero-dimensional polynomial system
bertiniPosDimSolve F                 -- positive-dimensional decomposition
                                      -- (returns witness sets for each component)
bertiniParameterHomotopy(F, params, paramValues)
                                      -- track a parameter homotopy
bertiniTrackHomotopy(H, start, end)  -- track a user-supplied homotopy
bertiniSample(W, n)                  -- sample n points from a witness set W
bertiniComponentMemberTest(p, W)     -- is point p on the component W?
bertiniRefineSols(F, sols)           -- Newton-refine approximate solutions
bertiniUserHomotopy(...)             -- general-purpose homotopy interface
```

### Configuration / option keys (Bertini-specific)

```m2
MPType => 0|1|2                       -- machine / fixed / adaptive multi-precision
PrecisionType
RandomGamma                            -- the gamma in cheater's homotopy
ParameterHomotopy
Verbose
TopDirectory                           -- where Bertini stages temp files
StorageFolder, SubFolder
StartPoints, StartParameters
MultiplicityTol, ConditionNumTol      -- numerical tolerances
SetParameterGroup
ReturnPoints
PrintMidStatus
OrderPaths
AdditionalFiles, PathList
MainDataDirectory
OutputStyle                            -- legacy; TODO removed
```

### Internal helpers (exported for advanced users)

```m2
subPoint(...)                          -- substitute a numerical point into a polynomial
makeB'InputFile, makeB'Section,
makeB'Slice, importMainDataFile,
moveB'File, writeStartFile             -- low-level file-marshalling
storeBM2Files                          -- session state for caching
```

These let advanced users **build Bertini input files manually**,
invoke Bertini through them, and re-import the output. Most users
work at the high level via `bertiniZeroDimSolve` / etc.

## How the interface works

Bertini is a **standalone command-line solver**: you write a
"Bertini input file" describing the polynomial system + the
computation, run `bertini` on it, parse the output files. This
package automates the entire round-trip:

```
M2:  bertiniZeroDimSolve F
   ↓
write a temp directory with the Bertini input file describing F
   (via makeB'InputFile + helpers)
   ↓
shell out to the Bertini binary (configured via "BERTINIexecutable")
   ↓ Bertini writes output files in the temp dir
parse the output files (via importMainDataFile + helpers)
   ↓
return a list of Point objects (from NAGtypes)
```

The `Configuration => { "BERTINIexecutable" => "bertini" }` package
option lets users override the binary name if it's installed under
a different name on their system.

## Architecture: a 3 910-line single file

Like several other external-library wrappers, `Bertini.m2` is one
big file. Internal structure (by section comment):

| Section (approx) | Topic |
|---|---|
| Header | newPackage, exports, configuration, `bertiniPresent` check |
| File marshalling | `makeB'InputFile`, `makeB'Section`, `makeB'Slice`, `writeStartFile`, etc. |
| Output parsing | `importMainDataFile`, `moveB'File`, `subPoint`, the file-format parsers |
| Zero-dim solving | `bertiniZeroDimSolve` and its option processing |
| Pos-dim solving | `bertiniPosDimSolve` + witness-set construction |
| Parameter / user homotopies | `bertiniParameterHomotopy`, `bertiniTrackHomotopy`, `bertiniUserHomotopy` |
| Refinement / sampling | `bertiniRefineSols`, `bertiniSample`, `bertiniComponentMemberTest` |
| Documentation `doc ///…///` | trailing |
| Test suite `TEST ///…///` | trailing |

## What's in `examples/`?

The aux directory contains an `examples/` folder with **30+
pre-computed example-output files** (`.out` files with auto-mangled
names like `_bertini__Pos__Dim__Solve.out`). These are M2's **doc-
example output cache**: every `doc ///…///` block has `Example`
sub-blocks that run M2 code; the package's
`CacheExampleOutput => true` declaration tells `installPackage` to
cache the output of those examples so that:

1. Documentation rebuilds don't have to re-run Bertini every time
   (which would require Bertini installed on the doc build machine).
2. Users without Bertini can still build the docs and view example output (cached from a previous build).

The example output is also useful as a **reference** for what Bertini's output looks like in normal operation.

## Optional-component handling

The package's `OptionalComponentsPresent => bertiniPresent` declaration tells M2 that this package depends on an external binary. When Bertini isn't installed:

- The package still loads.
- All `bertini*` functions error out with a clear message saying Bertini isn't installed.
- `installPackage "Bertini"` still works using cached example output.

This is the **standard pattern** for M2's external-library-wrapper packages — see also `PHCpack`, `gfanInterface`, `FourTiTwo`, `Normaliz`.

## When `Bertini` vs `NumericalAlgebraicGeometry`?

**Use `Bertini` directly** when:
- You're a Bertini expert and want to use Bertini's full option set.
- You want to inspect the Bertini input/output files directly (the package leaves them in the temp dir after the run).
- You need very specific Bertini features (specific homotopy types, adaptive multi-precision settings, etc.).

**Use `NumericalAlgebraicGeometry`** when:
- You want a portable interface that works across multiple solver backends.
- You want consistent return types across backends (M2's `Point` type from `NAGtypes`).
- You're not sure which solver fits your problem best.

`NumericalAlgebraicGeometry`'s `Software => BERTINI` option routes through this package internally — so even if you use the high-level interface, this package is what's doing the work behind the scenes.

## Multi-precision modes

Bertini's headline feature is **adaptive multi-precision tracking**: when a path approaches a singular target, Bertini automatically increases working precision (via MPFR) to maintain accuracy. The `MPType` option controls the strategy:

| `MPType` | Behaviour |
|---|---|
| `0` | Fixed machine precision (`double`) — fastest, may fail near singularities |
| `1` | Fixed user-chosen precision — controllable but no adaptivity |
| `2` (default) | Adaptive multi-precision — slower in general but correct near singularities |

For most problems `MPType => 2` is the right default; drop to `0` only if you've verified your paths stay away from singularities.

## When this is slow

| Symptom | Try |
|---|---|
| `bertiniZeroDimSolve F` slow for moderate system | Pass `MPType => 0` if you trust the precision; the Bertini start-up overhead is fixed per call |
| Bertini errors with "Bertini not found" | Set `Configuration => { "BERTINIexecutable" => "/full/path/to/bertini" }` or add to `PATH` |
| Many calls to small systems | The shell-out overhead is per-call; consider running one parameter homotopy across all the systems instead |
| Output parsing slow | Cached automatically after first parse; subsequent same-input calls reuse the parse |

## See also

- [`file-NumericalAlgebraicGeometry.md`](file-NumericalAlgebraicGeometry.md) — `Software => BERTINI` routes through this package
- `NAGtypes` — re-exported; defines the `Point`, `Homotopy`, `WitnessSet`, `PolySystem` types
- `PHCpack`, `gfanInterface`, `FourTiTwo`, `Normaliz` — sibling external-library wrappers following the same pattern
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Bertini](https://bertini.nd.edu/) — the upstream solver
- [Repo `DEPENDENCIES.md`](../../../DEPENDENCIES.md) — external-library catalogue
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — numerical AG section
