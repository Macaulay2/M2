# `BernsteinSato.m2` — Bernstein-Sato polynomials & D-module algorithms

The `BernsteinSato` package implements the **Bernstein-Sato
polynomial** `b_f(s)` (the b-function of a polynomial) and a battery
of D-module-theoretic algorithms built on it: **multiplier ideals**,
**D-module restriction / localisation / De Rham cohomology / local
cohomology**, **Weyl closure**, **annihilator of `f^s`**.

The package's headline operation `globalBFunction f` produces the
Bernstein-Sato polynomial `b_f(s) ∈ QQ[s]` — the monic polynomial of
smallest degree such that `b_f(s) f^s ∈ D[s] f^{s+1}` (where `D` is
the Weyl algebra). Its roots determine many invariants: log-canonical
threshold, multiplier ideals at each rational threshold, jumping
numbers.

- Main file: `BernsteinSato.m2` (311 lines — load orchestration + a few exports)
- Auxiliary directory: `BernsteinSato/` (45 files, **6 547 lines**)
- Authors: Anton Leykin, Harrison Tsai
- Version: 1.0 (February 2023)
- Re-exports: `WeylAlgebras`, `HolonomicSystems`, `Complexes`
- Imports: `PrimaryDecomposition`, `ReesAlgebra`, `Elimination`, `FourTiTwo`

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API (~80+ symbols)

### Bernstein-Sato (b-) functions

```m2
globalBFunction f            -- the b-function b_f(s) of a polynomial
globalB(I, f)                -- generalised: b-function of an ideal with respect to f
generalB I                   -- general b-function of an ideal
localBFunction(f, P)         -- b-function at a prime ideal P
bFunction(I)                 -- D-module b-function of an Ideal
bFunctionRoots(b)            -- rational roots of a b-function
factorBFunction(b)           -- factor a b-function into linear factors
getIntRoots(b)               -- the integer roots
ReducedB                     -- option: ask for the reduced (squarefree) form
NonGeneric                   -- option: switch from generic to non-generic algorithm
TryGeneric                   -- option: attempt generic first, fall back
StarIdeal, InitialIdeal      -- intermediate constructs exposed for debugging
Exponent, Boperator,
Bpolynomial, globalBoperator -- additional operators/coefficients
```

### Multiplier ideals & jumping numbers

```m2
multiplierIdeal(f, c)        -- the multiplier ideal J(f^c) at c
jumpingCoefficients(f)       -- the c-values where J(f^c) jumps
```

### D-module operations

```m2
Dresolution M                -- free resolution as a D-module
Drestriction(M, w)           -- restriction to a weight w
Dlocalize(M, f)              -- localisation at f
Dintegration(M, w)           -- integration along w
DHom(M, N)                   -- Hom in the D-module category
DeRham M                     -- de Rham cohomology
localCohom(I, M)             -- local cohomology
intersectionCohom(M)         -- intersection cohomology
characteristicCycle M        -- the characteristic cycle of a holonomic module
isHolonomic M
WeylClosure I                -- the Weyl closure of an ideal
annFs f                      -- the annihilator of f^s in D[s]
```

### Operations on the Weyl algebra (via `WeylAlgebras` re-export)

```m2
makeWeylAlgebra R            -- construct the Weyl algebra W_R
makeWA R                     -- alias
```

## Architecture

The package orchestrates 15 algorithmic modules plus a sizable
documentation tree:

```
BernsteinSato.m2 (311 lines)              ← exports + load sequence
   │
   ├─→ globalBFunction.m2        ← Anton Leykin's globalBFunction algorithm
   ├─→ localBFunction.m2         ← b-functions at a prime ideal
   ├─→ bFunction.ideal.m2        ← b-functions of ideals
   ├─→ bFunction.module.m2       ← b-functions of modules
   ├─→ multiplierIdeals.m2       ← J(f^c) at rational c
   ├─→ annFs.m2                  ← annihilator of f^s
   ├─→ paramBpoly.m2             ← parameterised b-polynomials (deprecated path)
   ├─→ WeylClosure.m2            ← Weyl closure of an ideal in W
   ├─→ Dresolution.m2            ← free resolution as a D-module
   ├─→ Drestriction.m2           ← D-module restriction to a weight
   ├─→ Dlocalize.m2              ← D-module localisation at f
   ├─→ DHom.m2                   ← Hom in the D-module category
   ├─→ DeRham.m2                 ← de Rham cohomology
   ├─→ localCohom.m2             ← local cohomology
   ├─→ CC.m2                     ← characteristic cycle
   └─→ intersectionCohom.m2      ← intersection cohomology
   │
   ├─→ TST/                      ← test suite (3 files)
   └─→ DOC/                      ← documentation (24 files matching algorithm modules)
```

**Documentation files mirror algorithm files**: `DOC/Dlocalize.m2`
documents `Dlocalize.m2`, etc. This makes incremental documentation
edits straightforward — open the matching `DOC/*.m2` and the
algorithm `*.m2` in parallel.

## The b-function algorithm chain

`globalBFunction f` is the most-called entry; the algorithm:

1. Construct the Weyl algebra `W = k[x_1, …, x_n, ∂_1, …, ∂_n]`.
2. Set up the **`annFs f`** ideal in `W[s]` — the annihilator of `f^s` as a `D[s]`-module.
3. Compute the **initial ideal w.r.t. a weight order** that promotes `(s)` over `x`s and `∂`s.
4. Eliminate everything but `s` from the initial ideal — what's left is `b_f(s) D[s]`.
5. Return the monic generator: `b_f(s)`.

The implementation has two algorithmic flavours selected by
`Strategy => …`:

| Strategy | When picked | Notes |
|---|---|---|
| `TryGeneric` (default) | First | Fast for generic inputs |
| `NonGeneric` | If generic fails | Catches all cases; slower |
| `GeneralBernsteinSato` | Explicit user request | For `globalB(I, f)` over an ideal |

The package imports `Elimination` (for the elimination step), `ReesAlgebra` (for some auxiliary GB computations), `FourTiTwo` (for toric inputs), and `PrimaryDecomposition` (for some inner intermediate factorisations).

## D-module landscape — how this package fits

D-modules in M2 are the workhorse for algorithmic algebraic analysis:

| Package | Role | Auto-loaded? |
|---|---|---|
| `WeylAlgebras` | Defines the Weyl algebra type `WeylAlgebra` itself | No — required for D-module work |
| `HolonomicSystems` | Holonomic D-modules + their algorithms (`isHolonomic`, etc.) | No — re-exported by `BernsteinSato` |
| **`BernsteinSato`** | b-functions + multiplier ideals + D-restriction/localisation/etc. | No |
| `Dmodules` (older) | Predecessor package; now largely superseded | No |

For commutative-side D-module wrappers (e.g. PBW orders), see
[`SolvableAlgebra`](../e/file-solvable.md) in the engine.

## Multiplier ideals & jumping coefficients

`multiplierIdeal(f, c)` returns the multiplier ideal `J(f^c)` for a
rational threshold `c`. Its key property: there's a finite list of
**jumping coefficients** `0 < c_1 < c_2 < … < 1` where `J(f^c)`
strictly decreases. These are computed via:

```m2
jumpingCoefficients f
```

The first one, `c_1`, is the **log-canonical threshold** `lct(f)`. The full set is determined by the roots of the b-function `b_f(s)`:

> Jumping coefficients of `f` are exactly the (negatives of the rational) roots of `b_f(s)` that fall in `(0, 1]`.

So `globalBFunction` is the central computational primitive that everything else builds on.

## When this is slow

| Symptom | Try |
|---|---|
| `globalBFunction f` hangs on a degree-4 polynomial in 4 variables | The Weyl-algebra GB grows fast; pass `Strategy => NonGeneric` if `TryGeneric` is taking too long |
| `multiplierIdeal(f, c)` slow | Compute `globalBFunction f` once, factor it, then `multiplierIdeal(f, c)` reuses the cache |
| `Dlocalize(M, f)` slow | Localisation is computed via the resolution of `M[f^{-1}]`; consider `Dresolution M` first and reuse |
| `DeRham M` very slow | De Rham requires the full `Drestriction` chain; bound it with degree limits if possible |

## See also

- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [`WeylAlgebras.m2`](WeylAlgebras.m2) — re-exported by this package; defines the Weyl-algebra type
- [`HolonomicSystems.m2`](HolonomicSystems.m2) — re-exported; holonomic D-modules
- [`file-Complexes.md`](file-Complexes.md) — re-exported; D-module resolutions return `Complex` objects
- [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) — imported (auto-loaded; used internally)
- [`file-ReesAlgebra.md`](file-ReesAlgebra.md) — imported (auto-loaded; used internally)
- [`file-Elimination.md`](file-Elimination.md) — imported (auto-loaded; used for the elimination step)
- `FourTiTwo` package — imported for toric-style inputs
- Engine Weyl algebra: [`e/file-weylalg.md`](../e/file-weylalg.md)
- Engine solvable algebra: [`e/file-solvable.md`](../e/file-solvable.md) — alternative D-like rings with PBW order
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — Weyl algebra in the ring catalogue
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
