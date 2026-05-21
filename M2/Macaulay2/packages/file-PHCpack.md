# `PHCpack.m2` — interface to the PHCpack numerical solver

The `PHCpack` package is M2's **interface to the external PHCpack
homotopy-continuation solver** (Jan Verschelde's polyhedral-homotopy
toolkit). Sibling to [`Bertini`](file-Bertini.md); both implement
the same external-library-wrapper pattern, differing in which
upstream solver they call. PHCpack's headline algorithmic feature
is **polyhedral homotopy** — a method that exploits Newton polytope
structure to dramatically reduce path counts for sparse / mixed
systems.

JSAG-certified (vol. 5, 2013).

- Main file: `PHCpack.m2` (2 133 lines)
- Auxiliary directory: `PHCpack/` (doc + tests + example-output cache)
- Authors: Elizabeth Gross, Sonja Petrovic, Jan Verschelde
- Version: 1.8 (May 2016)
- Re-exports: `NAGtypes`
- **Requires PHCpack v2.4.77+** installed externally (checked at load
  with `phcPresentAndModern`)
- `CacheExampleOutput => true` — doc-example output cached for fast
  doc rebuilds without PHCpack installed

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Top-level solving

```m2
solveSystem F                          -- solve a polynomial system
solveRationalSystem F                  -- solve a rational system (numerators + denominators)
trackPaths(start, end, …)              -- track a parameter homotopy
refineSolutions(F, sols)                -- Newton-refine
numericalIrreducibleDecomposition F    -- positive-dim decomposition
mixedVolume F                          -- the BKK / mixed-volume bound (root count by polyhedral methods)
```

### Polyhedral / cascade machinery

```m2
cascade(F, n)                          -- cascade homotopy for positive-dim systems (level n)
constructEmbedding(F, k)               -- embedding for cascade methods
factorWitnessSet W                     -- factor a witness set into irreducible components
topWitnessSet F                        -- witness set at top level
toLaurentPolynomial f                  -- coerce to Laurent for polyhedral methods
```

### Witness-set and component-membership operations

```m2
isWitnessSetMember(p, W)               -- is p on the variety witnessed by W?
intersectSlice(W, L)                   -- intersect a witness set with a linear slice
realSlice1D(W), realSlice2D(W)         -- restrict to real slices
searchDelta, searchNpoints, searchTolerance
                                        -- numerical search parameters
isCoordinateZero(p, i)                 -- is coordinate i of p numerically zero?
zeroFilter(L), nonZeroFilter(L)        -- partition a list by zero / non-zero predicate
```

### Tunable parameters

```m2
computingPrecision => N                -- working precision in bits
randomSeed => N                         -- reproducibility
numThreads => N                         -- parallel paths (PHCpack supports thread pools)
seeProgress => true|false               -- live progress reporting
gamma                                   -- the gamma in cheater's homotopy
tDegree                                 -- t-degree for special-case homotopies
intermediateSolutions => true|false     -- save intermediate path data
StartDimension, StartSystem             -- start data for parameter homotopies
StableMixedVolume                        -- option for `mixedVolume`
loadSettingsPath, saveSettingsPath      -- restore / save PHCpack settings between runs
versionNumber                            -- the PHCpack version detected at load
```

## How it differs from `Bertini`

Both packages wrap external numerical solvers and follow the same architectural pattern:

| Feature | `Bertini` (this package's sibling) | `PHCpack` (this package) |
|---|---|---|
| Algorithm focus | Adaptive multi-precision tracking, robust on singular targets | **Polyhedral homotopy** — exploits Newton polytope structure |
| Mixed-volume / BKK bound | No direct exposure | **Yes**, via `mixedVolume F` |
| Sparse-system path counts | Sometimes high | Often dramatically lower via polyhedral methods |
| Multi-precision | Adaptive multi-precision (`MPType => 0/1/2`) | Working precision in bits (`computingPrecision => N`) |
| Multi-threading | No M2-side option | **Yes**, via `numThreads => N` |
| Native irreducible decomposition | `bertiniPosDimSolve` | `numericalIrreducibleDecomposition` + `cascade` (more granular) |
| JSAG certification | No | **Yes** (vol. 5, 2013) |
| Best for | Dense systems with near-singular targets | Sparse / structured / mixed systems |

For most workflows, the high-level [`NumericalAlgebraicGeometry`](file-NumericalAlgebraicGeometry.md) interface gives you both: pass `Software => BERTINI` or `Software => PHCPACK` to switch backends.

## The cascade method

PHCpack's headline algorithm for positive-dimensional systems:

1. **Start at level 0** — find isolated solutions.
2. **Level k**: add `k` random linear forms, find solutions to the augmented system on a generic slice. The "extra" solutions (not coming from level `k-1`) correspond to `k`-dimensional irreducible components.
3. **Stop** when no new solutions appear.

The result is a stratified witness-set representation of the variety. `cascade(F, n)` exposes this directly.

## When PHCpack runs

```
M2:  solveSystem F
   ↓
build a PHCpack input file (text format describing F)
   ↓
shell out to `phc` binary (configured via PATH)
   ↓ PHCpack writes solutions to output files
parse them into M2 Points (from NAGtypes)
   ↓ return
List of Point objects
```

The temp-file location can be controlled via the same staging
options as `Bertini` (different option names; consult `viewHelp` of
the specific functions).

## Architecture

```
PHCpack.m2 (2 133 lines)                ← all production code
   │
   ├─→ PHCpack/                          ← auxiliary directory
   │     │
   │     ├─→ doc + tests + examples/      ← M2-level doc + test suite + cached
   │     │                                   example output (same pattern as Bertini)
   │     │
   │     └─→ (PHCpack-specific .m2 helpers)
```

Like `Bertini`, this package keeps the implementation in one big `.m2` file (2 133 lines). Internal structure (by section comment):

- File marshalling (write PHCpack input files)
- Output parsing
- `solveSystem`, `solveRationalSystem`, `trackPaths`, `refineSolutions` — the headline solvers
- `cascade`, `numericalIrreducibleDecomposition`, `topWitnessSet`, `factorWitnessSet` — the positive-dim suite
- `mixedVolume` (BKK bound; the polyhedral-homotopy entry point)
- `isWitnessSetMember`, `intersectSlice`, `realSlice*` — the witness-set search and slicing tools
- Documentation and tests trailing

## When this is slow

| Symptom | Try |
|---|---|
| `solveSystem F` slow on a dense quadratic system | PHCpack's polyhedral homotopy doesn't help much for dense inputs; try `Bertini` (adaptive precision) or pass `Software => M2engine` in `NumericalAlgebraicGeometry` |
| `mixedVolume F` slow | The mixed-volume computation itself can be expensive for high-dim sparse systems; `StableMixedVolume => true` sometimes helps |
| Many small systems | The shell-out overhead is per-call; consider one parameter homotopy across the family |
| `numericalIrreducibleDecomposition F` returns wrong components | PHCpack's positive-dim path needs careful precision; try larger `computingPrecision` and re-run |

## See also

- [`file-Bertini.md`](file-Bertini.md) — sibling external-library wrapper following the same pattern
- [`file-NumericalAlgebraicGeometry.md`](file-NumericalAlgebraicGeometry.md) — uses this package as backend when `Software => PHCPACK`
- `NAGtypes` — re-exported; defines `Point`, `Homotopy`, `WitnessSet`, `PolySystem`
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [PHCpack](https://homepages.math.uic.edu/~jan/PHCpack/phcpack.html) — the upstream solver
- [JSAG 2013 article](https://msp.org/jsag/2013/5-1/p04.xhtml) — Gross-Petrovic-Verschelde: *Interfacing with PHCpack*
- [Repo `DEPENDENCIES.md`](../../../DEPENDENCIES.md) — external-library catalogue
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — numerical AG section
