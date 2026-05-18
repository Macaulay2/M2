# `NumericalAlgebraicGeometry.m2` — homotopy continuation, witness sets, irreducible decomposition

The `NumericalAlgebraicGeometry` (NAG) package implements **numerical
algebraic geometry**: solving polynomial systems by **homotopy
continuation**, building **witness sets** for positive-dimensional
solution components, performing **numerical irreducible
decomposition**, and the supporting infrastructure (path trackers,
straight-line programs for fast evaluation, certified tracking,
deflation for multiple solutions).

The package is the **M2 frontend** for several upstream
homotopy-tracking engines: M2's own internal SLP-based tracker
(`M2engine` / `M2enginePrecookedSLPs`), the external libraries
**Bertini**, **PHCpack**, and **HOM4PS2**, and an experimental
Julia bridge. Choose a backend with `Software => …`.

- Main file: `NumericalAlgebraicGeometry.m2` (567 lines)
- Auxiliary directory: `NumericalAlgebraicGeometry/` (20 files,
  **7 071 lines**)
- Authors: Anton Leykin, Robert Krone
- Version: 1.24 (May 2024)
- Certification: published in [JSAG vol. 3, 2011](https://msp.org/jsag/2011/3-1/p02.xhtml) (article describes v1.4; package has evolved substantially since)
- Re-exports: `NAGtypes`, `NumericalLinearAlgebra`, `SLPexpressions`
- Imports: `PHCpack`, `Bertini`

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API (highlights)

### Zero-dimensional solving

```m2
solveSystem F             -- solve a square polynomial system
parameterHomotopy(F, ...) -- track parameter homotopy
totalDegreeStartSystem F  -- the standard total-degree start system
```

### Positive-dimensional decomposition

```m2
numericalIrreducibleDecomposition I
                          -- numerical version of decompose I
                          -- returns a list of witness sets, one per irreducible component
regeneration(F, …)        -- iterative regeneration method for witness sets
isSolution(p, F)          -- is p a solution of F?
```

### Path tracking & refinement

```m2
track(start, target, …)   -- track a homotopy from start to target solutions
refine(F, sols)           -- Newton-refine approximate solutions
squareUp(F)               -- reduce an overdetermined system to a square one
```

### Defaults & tuning

```m2
setDefault(Software => BERTINI, gamma => …)  -- session-wide defaults
getDefault Software       -- query defaults
```

## Software backends

Pass `Software => X` to most operations to pick a tracker:

| Backend | What it is | When to use |
|---|---|---|
| `M2` | High-level M2-only tracker (slow, transparent) | Educational / debugging |
| `M2engine` | M2's engine-compiled SLP tracker | **Default for serious use**; fast and always available |
| `M2enginePrecookedSLPs` | Like `M2engine` but with pre-compiled SLPs cached | When tracking many homotopies with the same shape |
| `BERTINI` | Calls out to [Bertini](https://bertini.nd.edu/) | Industry-standard for very large systems; needs Bertini installed |
| `PHCPACK` | Calls out to [PHCpack](https://homepages.math.uic.edu/~jan/) | Strong for sparse / mixed-volume systems; needs PHCpack installed |
| `HOM4PS2` | Calls out to HOM4PS-2 | Polyhedral homotopy specialist; needs HOM4PS-2 installed |

Defaults set via `setDefault`. External-backend availability is
checked at first use; if the binary isn't on `PATH`, you get a
clear error pointing at the configuration option.

The `Configuration => { "PHCPACK" => "phc", "BERTINI" => "bertini", "HOM4PS2" => "hom4ps2" }` in the package header lets the
user override the binary names without editing the package.

## Tunable parameters (~25 of them)

NAG path-tracking has many knobs. The most-tweaked:

| Option | Default | What it controls |
|---|---|---|
| `gamma` | random unit | The "random complex number" in the standard cheater's homotopy |
| `tStep`, `tStepMin` | varying | Initial / minimum step sizes for path advance |
| `stepIncreaseFactor` | 2.0 | Step-size growth factor on success |
| `numberSuccessesBeforeIncrease` | 5 | How many successful steps before growing |
| `Predictor` | `RungeKutta4` | Predictor type: `RungeKutta4`, `Multistep`, `Tangent`, `Euler`, `Secant`, `Certified` |
| `MultistepDegree` | 3 | Order for the `Multistep` predictor |
| `maxCorrSteps` | 3 | Max Newton-corrector iterations per step |
| `CorrectorTolerance` | tight | Newton residual tolerance |
| `EndZoneFactor` | 0.05 | Where the "end zone" of the homotopy begins (smaller steps near `t=1`) |
| `InfinityThreshold` | large | Bound for detecting paths going to infinity |
| `Projectivize` | false | Track in projective coordinates? |
| `AffinePatches`, `DynamicPatch` | … | Patch-selection options when projective |
| `SingularConditionNumber` | large | Threshold for declaring a solution singular |
| `Bits`, `Iterations`, `ErrorTolerance` | varied | Adaptive-precision controls |

A solution status enum is tracked through each path:

```
Undetermined, Processing, Regular, Singular, Infinity,
MinStepFailure, Origin, IncreasePrecision, DecreasePrecision,
RefinementFailure
```

## Architecture

```
NumericalAlgebraicGeometry.m2 (567 lines)        ← exports, defaults, dispatch
   │
   ├─→ track.m2 (1 133)                          ← THE path tracker — largest file
   ├─→ SLP.m2 (907)                              ← straight-line programs for fast evaluation
   ├─→ extraNAGtypes.m2 (598)                    ← additional types beyond NAGtypes
   ├─→ refine.m2 (469)                           ← Newton refinement
   ├─→ certifiedNAG.m2 (399)                     ← certified tracking (rigorous bounds)
   ├─→ witness-set.m2 (289)                      ← WitnessSet construction & manipulation
   ├─→ deflation.m2 (254)                        ← deflation for multiple solutions
   ├─→ benchmarks.m2 (248)                       ← benchmark harness
   ├─→ JuliaInterface.m2 (224)                   ← experimental Julia bridge
   ├─→ intersection.m2 (178)                     ← witness-set intersection
   ├─→ decomposition.m2 (174)                    ← irreducible decomposition algorithm
   ├─→ 0-dim-methods.m2 (157)                    ← zero-dim solving entry points
   ├─→ julia.m2 (145)                            ← more Julia integration
   ├─→ systems.m2 (135)                          ← polynomial system manipulation
   ├─→ BSS-certified.m2 (113)                    ← Beltran-Shub-Smale certified tracking
   ├─→ WSet-deflation.m2 (71)                    ← witness sets + deflation
   ├─→ showcase.m2 (66)                          ← demo examples
   ├─→ WSet-NumericalAlgebraicGeometry.m2 (34)   ← witness-set glue
   ├─→ positive-dim-methods.m2 (21)              ← positive-dim solving entry points
   │
   └─→ doc.m2 (1 456)                            ← M2-doc DSL for all of the above
```

**`track.m2` (1 133 lines) is the algorithmic heart** — it implements
the predictor-corrector loop with all the adaptive step-size,
precision, and refinement logic. The dispatch between M2-internal
tracking and external backends happens early; once a backend is
chosen, the rest of `track.m2` is bypassed for `PHCPACK`/`BERTINI`/`HOM4PS2`.

**`SLP.m2` (907 lines)** implements **Straight-Line Programs** —
the evaluation model used by `M2engine`. An SLP is a sequence of
arithmetic operations representing a polynomial system; once
"pre-cooked," it can be evaluated at any point in `O(L)` time
where `L` is the SLP length. The engine-side counterpart is
`e/SLP.{cpp,hpp}` (see [`e/file-SLP.md`](../e/file-SLP.md)).

## How witness sets work

For a positive-dimensional variety `V ⊂ C^n` of dimension `d`:

1. Cut `V` with a generic codimension-`d` linear space `L`.
2. The intersection `V ∩ L` is **zero-dimensional** — finitely
   many points.
3. The collection of those points is a **witness set** for `V`.

`numericalIrreducibleDecomposition I`:

1. Computes the projective closure.
2. Repeatedly slices with hyperplanes.
3. Uses **monodromy** to group points by which component of `V`
   they come from.
4. Returns the witness sets, one per component.

The implementation in `decomposition.m2` (174 lines) is the
algorithmic core; `witness-set.m2` (289 lines) is the data
structure and per-witness-set operations.

## Deflation: handling multiple solutions

Multiple solutions (paths converging to the same point with
multiplicity) lose Newton convergence. **Deflation**
(`deflation.m2`, 254 lines) builds an enlarged system whose roots
include the original singular roots as **simple** roots — fixing the
Newton convergence at the cost of evaluating a bigger system.

The package's `deflation` method handles this automatically when
`PostProcess => true`. Manual deflation is exposed for users who
want to inspect the deflated system directly.

## Engine integration

NAG-specific operations bottom out at engine code:

| Operation | Engine entry |
|---|---|
| SLP evaluation | `rawSLP*` → [`e/file-SLP.md`](../e/file-SLP.md) |
| Path tracking (M2engine backend) | `rawHomotopy` and the NAG family in `e/file-NAG.md` |
| Square-up / random matrices | `rawRandomMatrix` over `CC`, `RR` |
| Precision control | Multi-precision MPFR/MPC via [`e/coefficient-rings.md`](../e/coefficient-rings.md) |

See `e/computations.md` for the full numerical-algebra section of
the engine.

## When this is slow

| Symptom | Try |
|---|---|
| Single tracking step takes seconds | Switch to `Software => M2engine` (Java/Python-style high-level M2 is much slower than the engine SLP tracker) |
| Solution count near correct but not exact | Raise `Tries`; lower `CorrectorTolerance`; check for paths going to infinity (`Projectivize => true`) |
| `numericalIrreducibleDecomposition` runs forever | Reduce expected component count via `Software => BERTINI` (which has its own decomposition) |
| Multiple solutions detected as singular | Use `PostProcess => true` to engage deflation; or pass `MaxNumberOfSolutions` to bound the search |
| Loss of precision during long tracking | Enable `Bits => 100` (adaptive multi-precision) |

## See also

- [`NAGtypes.m2`](NAGtypes.m2) — re-exported; defines `Point`, `Homotopy`, `WitnessSet`, `PolySystem`
- [`NumericalLinearAlgebra.m2`](NumericalLinearAlgebra.m2) — re-exported; SVD, condition numbers, Newton
- [`SLPexpressions.m2`](SLPexpressions.m2) — re-exported; the SLP-expression DSL
- [`PHCpack.m2`](PHCpack.m2), [`Bertini.m2`](Bertini.m2) — imported; external-backend wrappers
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- Engine: [`e/file-NAG.md`](../e/file-NAG.md), [`e/file-SLP.md`](../e/file-SLP.md), [`e/computations.md`](../e/computations.md)
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — `CC` and `RR` rings (numerical AG operates over these)
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — numerical AG strategies entry
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [JSAG 2011 article](https://msp.org/jsag/2011/3-1/p02.xhtml) — Leykin: *Numerical Algebraic Geometry*
