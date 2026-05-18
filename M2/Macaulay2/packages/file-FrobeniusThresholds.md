# `FrobeniusThresholds.m2` — F-thresholds & jumping exponents in char `p`

The `FrobeniusThresholds` package computes **F-thresholds** of ring
elements and ideals in **prime characteristic `p > 0`** — invariants
that play the same role in positive characteristic that
log-canonical thresholds play in characteristic zero (and are
conjecturally equal for many classes of singularities).

The headline operations:
- **`fpt f`** — the F-pure threshold of an element `f` (or ideal `I`).
- **`isFPT(t, f)`** / **`compareFPT(t, f)`** — test whether a rational
  number `t` equals `fpt(f)`.
- **`isFJumpingExponent(t, f)`** — test whether `t` is an F-jumping
  exponent (the characteristic-p analogue of jumping coefficients
  from `BernsteinSato`'s multiplier ideals).
- **`frobeniusNu(e, f)`** — the *nu* invariants whose limit defines `fpt`.

JSAG-certified (vol. 11, 2021).

- Main file: `FrobeniusThresholds.m2` (94 lines — orchestration + exports)
- Auxiliary directory: `FrobeniusThresholds/` (9 files, **3 538 lines**)
- Authors: Juliette Bruce, Daniel Hernández, Karl Schwede, Dan Smolkin, Pedro Teixeira, Emily Witt
- Version: 2.1 (June 2020)
- Imports: [`MinimalPrimes`](file-MinimalPrimes.md) (auto-loaded)
- Re-exports: `TestIdeals`

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### F-pure threshold (the headline operation)

```m2
fpt f                       -- F-pure threshold of an element f ∈ R (R of char p)
fpt I                       -- F-pure threshold of an ideal I
fpt(L, f)                   -- with custom search parameters in option L
isFPT(t, f)                 -- is t = fpt(f)?
compareFPT(t, f)            -- 0 if t = fpt(f), -1 if t < fpt(f), 1 if t > fpt(f)
isFJumpingExponent(t, f)    -- is t an F-jumping exponent?
```

### Frobenius `nu` invariants

```m2
frobeniusNu(e, f)           -- the eth nu invariant of f
                            -- (fpt(f) = lim_e nu_e(f) / p^e)
ReturnList                  -- option: return a list of (e, nu_e) pairs
Search                      -- option: search strategy (linear/binary)
Bounds                      -- option: known a priori bounds on the answer
GuessStrategy               -- option: initial-guess heuristic
StandardPower               -- option: use standard p^e powers
UseSpecialAlgorithms        -- option: fast paths for known polynomial families
FinalAttempt                -- option: last-resort exhaustive search
```

### Frobenius powers and roots

```m2
FrobeniusPower              -- the [p^e]-th Frobenius power of an ideal
FrobeniusRoot               -- the [p^e]-th Frobenius root (preimage)
GlobalFrobeniusRoot         -- global version
isSimpleNormalCrossing      -- precondition checker for the SNC case
ContainmentTest             -- option: how to test ideal containment
```

## What an F-threshold is

In **characteristic 0**, given a polynomial `f ∈ R = k[x_1, …, x_n]`,
the log-canonical threshold `lct(f)` measures the worst singularity
of `V(f)` at the origin. It's rational, between 0 and 1, and
computable via the Bernstein-Sato polynomial: `lct(f) = -max(roots of
b_f(s) in (-1, 0))`.

In **positive characteristic** `p > 0`, there's an analogous
invariant called the **F-pure threshold `fpt(f)`**. Definitions:

> `fpt(f) = sup { t ∈ ℚ : (f^t)_R is F-pure }`

where the F-pure condition involves the Frobenius endomorphism of
`R`. Many singularity-theory results conjecture (and sometimes prove)
that as `p → ∞`, `fpt(f) → lct(f)`. So computing `fpt` in many primes
gives access to the characteristic-0 invariant.

`fpt(f)` is computed as a **limit**:

```
fpt(f) = lim_{e→∞} ν_e(f) / p^e
```

where `ν_e(f)` is the largest integer `N` such that `f^N` is not in
the Frobenius power `I^{[p^e]}` of the irrelevant maximal ideal `I`.
The package's `frobeniusNu(e, f)` computes these `ν_e` directly;
`fpt(f)` runs the limit far enough to identify the rational answer
unambiguously.

## Architecture

```
FrobeniusThresholds.m2 (94 lines)         ← orchestration + exports
   │
   ├─→ DivisorPatch.m2 (308)              — divisor helpers patching gaps in the
                                              Divisor package
   ├─→ BasicFunctions.m2 (354)            — combinatorial primitives:
                                              Frobenius-power / root computation,
                                              ideal-containment tests
   ├─→ MainFunctions.m2 (1 070)           — fpt / isFPT / compareFPT /
                                              isFJumpingExponent / frobeniusNu
   ├─→ SpecialFThresholds.m2 (558)        — fast paths for **known polynomial families**:
                                              diagonal hypersurfaces, binomials,
                                              homogeneous polynomials, etc.
   │
   ├─→ MainFunctionsDoc.m2 (720)          — M2 doc DSL for the main API
   ├─→ SpecialFThresholdsDoc.m2 (50)       — doc for the special-case algorithms
   ├─→ FThresholdsDoc.m2 (36)              — package-level overview doc
   ├─→ MainFunctionsTest.m2 (404)          — main test suite
   └─→ SpecialFThresholdsTest.m2 (38)      — test for the special-case algorithms
```

The split between `MainFunctions.m2` (generic algorithms) and `SpecialFThresholds.m2` (fast paths for known polynomial families) is the package's key performance lever. For inputs that match one of the recognised patterns (diagonal `x_1^a + … + x_n^a`, simple-normal-crossings divisors, binomial hypersurfaces), the special-case algorithm returns the exact `fpt` from a closed formula in constant time; otherwise the generic path runs the `ν_e` limit.

## The TestIdeals connection

This package **re-exports `TestIdeals`** — the foundational package
that defines:

- The **test ideal** `τ(R)` of a singularity (the char-p analogue of
  the multiplier ideal).
- **F-singularity types**: F-regular, F-pure, F-rational, F-injective.
- The Frobenius pullback `F^!` and related operators.

`fpt(f)` is closely related to the test ideal of the principal
divisor `div(f)`: the F-jumping exponents are exactly the points where
the test ideal jumps. So this package and `TestIdeals` are sibling
tools — most users load both.

## When this is fast vs slow

| Input shape | Speed |
|---|---|
| Diagonal hypersurface `x_1^a + … + x_n^a` | **Instant** — closed formula in `SpecialFThresholds` |
| Simple normal crossings divisor | **Instant** — closed formula |
| Binomial `x_1^a − x_2^b` | **Instant** — closed formula |
| Homogeneous polynomial of small degree | Fast — bounds from regularity short-circuit the limit |
| Generic polynomial | **Slow** — runs the `ν_e` limit, expensive in many variables |
| Ideal with many generators | Slowest — multiple `ν_e` runs |

The `Bounds` option lets the user pre-supply lower/upper bounds to
short-circuit the search; `GuessStrategy` lets them seed the iteration
with a good initial estimate.

## Tunable options

```m2
fpt(f, Bounds => (0, 1))             -- search only in this range
fpt(f, Search => "binary")            -- use binary search (default linear)
fpt(f, UseSpecialAlgorithms => true) -- prefer special-case paths
fpt(f, FinalAttempt => true)          -- last-resort exhaustive
fpt(f, GuessStrategy => …)            -- custom guess
fpt(f, StandardPower => true)         -- use p^e exactly (not approximation)
fpt(f, ContainmentTest => "syzygy")   -- alternative ideal-containment algorithm
```

## Relationship to `BernsteinSato` (the char-0 analogue)

| Question | Char-0 tool | Char-p tool |
|---|---|---|
| What's the threshold? | `lct(f) = -max(roots of bFunction f in (-1, 0))` ([`BernsteinSato`](file-BernsteinSato.md)) | `fpt(f)` (this package) |
| What's the multiplier / test ideal at threshold? | `multiplierIdeal(f, c)` ([`BernsteinSato`](file-BernsteinSato.md)) | `testIdeal(f, c)` via `TestIdeals` |
| What are the jumping points? | `jumpingCoefficients f` | `isFJumpingExponent(t, f)` (this package) |

Both follow the same algorithmic pattern: build the relevant primary
ideals, test containment iteratively, recover the rational threshold.

## When this is slow — debug recipes

| Symptom | Try |
|---|---|
| `fpt f` runs forever on a 5-variable polynomial | Pass `UseSpecialAlgorithms => true`; if the polynomial fits a known family this returns immediately |
| `isFPT(t, f)` faster than `fpt(f)` | Yes — testing a specific `t` is `O(1)` in `e`, but `fpt(f)` runs the full limit; if you have a candidate, test it first |
| Cohen-Macaulay base ring needed for some algorithms | Some special-case paths assume CM; check the input first |
| `MainFunctions` running but no closed-form answer | The limit hasn't converged; raise `MaxAttempts` or fall back to `compareFPT` for a guess |

## See also

- [`file-MinimalPrimes.md`](file-MinimalPrimes.md) — imported (auto-loaded); used internally
- [`file-BernsteinSato.md`](file-BernsteinSato.md) — the characteristic-0 counterpart
- `TestIdeals` (re-exported) — test ideals, F-singularity types — not yet deep-dived
- `Divisor` package — `DivisorPatch.m2` patches gaps in this package
- Engine `aring-zz-flint`: [`e/file-aring-zz-flint.md`](../e/file-aring-zz-flint.md) — the FLINT `nmod` rings used for char-p inputs
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [JSAG 2021 article](https://msp.org/jsag/2021/11-1/p04.xhtml) — Bruce-Hernández-Schwede-Smolkin-Teixeira-Witt: *The FrobeniusThresholds package for Macaulay2*
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — `ZZ/p` rings (the only inputs this package operates on)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
