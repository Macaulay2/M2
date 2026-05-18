# `LLLBases.m2` — Lenstra-Lenstra-Lovász basis reduction

The `LLLBases` package implements **LLL basis reduction** for
integer lattices, plus **Hermite normal form** computation,
**kernel computation via LLL**, **gcd via LLL**, and the
**Gram-Schmidt orthogonalisation** that LLL uses internally.
**Auto-loaded** — every M2 session has these operations available
without `needsPackage`.

LLL is the workhorse for **short-vector problems in lattices**:
finding a basis where every vector is "almost" as short as the
shortest. Used by `Complexes` for `freeResolution` over `ZZ` and by
many algorithmic number-theory routines.

- File: `LLLBases.m2` (1744 lines — single file, no aux directory)
- Author: Mike Stillman
- Version: 1.1 (July 2005)
- Imported by: [`Complexes`](file-Complexes.md)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Core operations

```m2
LLL M                       -- LLL-reduce the columns of M (a ZZ-matrix)
LLL(M, ChangeMatrix => true) -- also return the basis-change matrix
isLLL M                     -- is M already LLL-reduced?
gramm M                     -- Gram-Schmidt orthogonalisation
kernelLLL M                 -- the kernel of an integer linear map, via LLL
hermite M                   -- Hermite normal form
gcdLLL L                    -- gcd of a list of integers (via LLL on the row)
```

### Options & strategies

```
Threshold => 3/4        -- LLL reduction parameter δ (default depends on backend)
ChangeMatrix => false   -- also return the basis-change matrix?
Limit => infinity       -- bound on iterations (or kernel basis size)
Strategy => NTL         -- which LLL backend
```

### Strategy symbols (the rich part)

| Symbol | Backend |
|---|---|
| `CohenEngine` (default = 0) | The engine's pure-integer LLL (Cohen's textbook version) |
| `CohenTopLevel` (= 1) | Top-level M2 reimplementation of Cohen's algorithm; slow but readable |
| `NTL` (= 2, default for `LLL`) | NTL's `LLL` — fast, integer arithmetic |
| `NTL` + `RealFP` (= 2+16) | NTL with floating-point Gram-Schmidt (`double`) |
| `NTL` + `RealQP` (= 2+48) | NTL with quad-precision FP |
| `NTL` + `RealXD` (= 2+64) | NTL with extended-precision FP |
| `NTL` + `RealRR` (= 2+80) | NTL with MPFR (arbitrary-precision) FP |
| `Givens` + `Real*` (= 6+…) | NTL with Givens rotations instead of Householder reflections |
| `BKZ` + `Real*` (= 10+…) | NTL's BKZ (Block Korkine-Zolotarev) — stronger than LLL |
| `BKZ` + `Givens` + `Real*` (= 14+…) | BKZ + Givens combinations |
| `fpLLL` (= 4) | Damien Stehlé's fpLLL library |

Combinations are passed as **lists**: `Strategy => {BKZ, RealXD}`
selects "BKZ with NTL's extended-precision FP Gram-Schmidt." The
package's `LLLstrategies` HashTable maps every supported
combination to an integer flag that the engine call understands.

The bit-pattern encoding is **intentional**: the engine's
`rawLLL` takes a single integer where individual bits select
features (NTL vs Cohen vs fpLLL, FP precision, Givens, BKZ). The
top-level uses symbolic names so users don't have to remember the
encoding.

## What LLL does

Given an integer matrix `M` whose columns span a lattice `L ⊂
ZZ^n`, LLL produces a new basis `M'` of `L` where:

1. Each `m'_i` is **almost orthogonal** to the previous ones
   (`|μ_{i,j}| ≤ 1/2` for the Gram-Schmidt coefficients).
2. The **size-reduced** Lovász condition holds at threshold `δ`
   (default `3/4` for integer LLL, `99/100` for floating-point):
   ```
   ||π_i(m'_{i+1})||² ≥ (δ - μ²) · ||π_i(m'_i)||²
   ```
3. The resulting `m'_1` is at most `2^{(n-1)/2}` longer than the
   shortest vector in `L` (Hermite's bound).

This makes LLL useful for:

- Finding small relations between numbers (`gcdLLL`).
- Computing kernels of integer linear maps (`kernelLLL`).
- Building minimal resolutions over `ZZ` (`Complexes`'s use).
- Cryptanalysis of knapsack / lattice-based systems (academic).

## Engine integration

LLL is one of the few operations that crosses **deep into the
engine**:

| Function | Engine entry |
|---|---|
| `LLL M` | `rawLLL(rawMutableMatrix, change, threshold, strategy_flags)` |
| `kernelLLL` | `rawLLL` followed by extracting null columns |
| `hermite` | `rawHermite` |
| `gcdLLL` | `LLL` applied to a single-row matrix |

The engine side lives in [`e/file-LLL.md`](../e/file-LLL.md). The
package converts user-level arguments into the bit-encoded flag
and dispatches; engine code handles the heavy arithmetic.

## fpLLL — Damien Stehlé's library

`Strategy => fpLLL` switches to a different upstream library
(fpLLL) entirely. Use when:

- NTL's LLL is too slow for your input (fpLLL is generally
  faster on very-high-dimensional lattices).
- You need **BKZ with custom block size** — fpLLL exposes more
  knobs.

The fpLLL binding lives behind a compile-time `HAVE_FPLLL` check.
On platforms where fpLLL isn't installed, requesting this strategy
yields an error suggesting you install it.

## `hermite` — Hermite normal form

Different from LLL but in the same package because both compute
canonical forms of integer matrices:

```m2
hermite M    -- returns (H, U) with H = M*U, H upper-triangular
              -- and entries below the diagonal smaller than the
              -- corresponding diagonal element
```

Used by `kernelLLL` internally. The implementation uses an
LLL-flavoured size-reduction step that runs in polynomial time
(unlike the naïve Hermite reduction which can have entries grow
exponentially).

## `gcdLLL` — find small linear combinations

```m2
gcdLLL {a_1, …, a_n}
  -- returns (g, U) where g = gcd(a_1, …, a_n) and U is a unimodular
  -- matrix with `gcd` in its top-left corner and small entries
```

The "small entries" guarantee is what makes this useful: a Bezout
coefficient computation via the extended Euclidean algorithm can
produce huge intermediate values; `gcdLLL` keeps them small.

## When this is slow

| Symptom | Try |
|---|---|
| LLL hangs on a small input | Switch to `Strategy => NTL` (it's almost always best for input sizes M2 typically sees) |
| Very high dimension (>200) | `Strategy => {BKZ, RealXD}` or `Strategy => fpLLL` |
| Numerical instability in answer | `Strategy => NTL` (integer-only, no FP) or `Strategy => {NTL, RealRR}` for MPFR precision |
| Want exact bound on Hermite-form entries | The default `hermite` uses LLL internally; consider `Strategy => CohenTopLevel` (slower but exact arithmetic, more transparent) |

## Single-file architecture

Like [`ReesAlgebra`](file-ReesAlgebra.md), `LLLBases` is a single
1 744-line file with no auxiliary directory. The structure is
roughly:

| Section | Topic |
|---|---|
| Lines 1-100 | Exports, helper functions, strategy table |
| Lines 107-275 | `LLL` method dispatching to the engine |
| Lines 285-325 | `gramm` (Gram-Schmidt) and `isLLL` (validation) |
| Lines 488-630 | `kernelLLL` and `hermite` |
| Lines 657-750 | `gcdLLL` |
| Lines 750+ | Documentation `doc ///…///` and `TEST ///…///` blocks |

## Dependencies

LLLBases is **self-contained** — depends only on Core. It's
imported by `Complexes` (for ZZ-module reductions in the
resolution path) and used standalone by many lattice / number-
theory packages.

## See also

- [`file-Complexes.md`](file-Complexes.md) — primary downstream consumer
- [`file-IntegralClosure.md`](file-IntegralClosure.md) — uses LLL transitively via `Complexes`
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- Engine LLL: [`e/file-LLL.md`](../e/file-LLL.md) — bottom-half engine implementation
- Engine matrix arithmetic: [`e/matrices.md`](../e/matrices.md)
- [Repo `DEPENDENCIES.md`](../../../DEPENDENCIES.md) — NTL and fpLLL library entries
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — `ZZ` ring (the only base LLL operates on)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — `LLL` strategy entry
