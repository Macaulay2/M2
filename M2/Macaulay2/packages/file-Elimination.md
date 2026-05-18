# `Elimination.m2` — eliminate variables, Sylvester resultant

The `Elimination` package implements `eliminate`, `sylvesterMatrix`,
`resultant`, and `discriminant`. **Auto-loaded** — every M2 session
has it available without `needsPackage`. Small (314 lines, no
auxiliary directory) but **imported by every package that does
algebraic decomposition**: [`MinimalPrimes`](file-MinimalPrimes.md),
[`PrimaryDecomposition`](file-PrimaryDecomposition.md),
[`Saturation`](file-Saturation.md) all pull it in.

- File: `Elimination.m2` (314 lines, single file — no auxiliary dir)
- Author: Mike Stillman
- Date: January 5, 2005 (one of the oldest auto-loaded packages still in active use)
- Imports: `monoidIndices` from `Core`

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

```m2
eliminate(v, I)              -- elim variables v from ideal I
eliminate(I, v)              -- same, args reversed
sylvesterMatrix(f, g, x)     -- the Sylvester matrix of f, g w.r.t. x
resultant(f, g, x)           -- res(f, g; x) = det(sylvesterMatrix)
discriminant(f, x)           -- disc(f) = resultant(f, df/dx, x)
```

`eliminate` accepts the variable list in either position; ditto a
single `RingElement` for the variable.

## How `eliminate` works

The implementation is short — one core insight, well-executed:

1. **Reorder variables**: `eliminate(v, I)` constructs a new
   polynomial ring `R1` with the elimination variables placed first
   and a **`MonomialOrder => Eliminate(k)`** monomial order, where
   `k = |v|`. This order has the property that the leading term of
   a polynomial depends on the elimination variables iff the
   polynomial uses them at all.

2. **Compute a Gröbner basis** of `I` in `R1`.

3. **Select sub-ring generators**: `selectInSubring(1, gens gb J)`
   returns exactly the elements of the GB whose leading term lives
   in the non-eliminated variables. By the elimination property of
   the monomial order, these generate the elimination ideal
   `I ∩ R[x_remaining]`.

4. **Map back** to a ring with the original variable order via
   the inverse permutation `toR`.

Roughly:

```m2
eliminate(v, I) ≡
  ideal mingens ideal toR selectInSubring(1, gens gb (toR1 I))
```

The clever bit is in the **`eliminationRing` helper** (lines 35-71):
it preserves all the original ring's structure (degrees,
WeylAlgebra, SkewCommutative, MonomialSize) while just reordering
variables. This means `eliminate` works on Weyl algebras and
skew-commutative rings, not just plain polynomial rings.

The current implementation requires `R` be a **flat polynomial
ring** (`isFlatPolynomialRing R`); for quotient rings, the caller
must lift to the polynomial cover first.

## Why this is the bottleneck

Step 2 — `gens gb J` — is the entire cost. Elimination is a thin
wrapper around the engine's GB computation under an elimination
order. Practical performance of `eliminate` is the practical
performance of GB on that input shape, which is often **the most
expensive case for GB** because elimination orders tend to be
unstable.

When this is slow, options to try:

| Symptom | Fix |
|---|---|
| Hangs forever | Use the alternative `eliminateH` (defined in this file but not exported) — adds a homogenising variable, sometimes faster |
| Wants degree bound | Pass `DegreeLimit => N` through; bottoms out at `gb(..., DegreeLimit => N)` |
| Eliminating too many variables | Eliminate in stages; smaller individual GBs combine well |
| Ring is a quotient | Lift to `ambient R` first, eliminate, map back |

## `eliminateH` — the alternative

Lines 22-33 hold `eliminateH`, an alternative implementation Mike
Stillman wrote for Sottile's group in 2009. It homogenises with a
new variable `h`, eliminates in the homogenised ring, then
dehomogenises. Sometimes faster. The header comment notes:

> "I still want to work this into the eliminate command…"

— a TODO that hasn't been finished. For now, users wanting that
behaviour invoke `eliminateH` directly from the package's private
state, or copy the few lines into their own code.

## Sylvester resultants

The second half of the file implements the **classical resultant of
two univariate polynomials** with respect to a chosen variable.

```m2
sylvesterMatrix(f, g, x)
  -- = the standard Sylvester matrix of f, g, viewed as polynomials in x
resultant(f, g, x)
  -- = det(sylvesterMatrix(f, g, x))
discriminant(f, x)
  -- = resultant(f, df/dx, x)
```

`f` and `g` need not be univariate **globally**; they only need to
be univariate **in `x`** (with the other variables treated as
constants in the base ring). This is what makes the resultant
useful in algebraic geometry — it eliminates one variable from a
two-polynomial system.

The package docstring notes:

> "It would be nice to implement multivariate resultants,
>  Bezoutians, and sparse resultants. Laurent Buse has written
>  code for this in the past."

— a long-standing aspirational feature.

The `Algorithm => ` option on `resultant` / `discriminant` is
currently unused (defaults to `null`); the slot is reserved for
future strategy selection if multivariate / sparse variants ever
arrive.

## Why this small package is auto-loaded

`eliminate` is one of the most-used user-facing operations in
algebraic geometry workflows — every "find me the implicit equation
of a parametrised variety" recipe ends in `eliminate`. Auto-loading
spares users a `needsPackage "Elimination"` they would invariably
forget.

The downstream impact: the three algebraic-decomposition packages
above call `eliminate` repeatedly. Improving `eliminate`'s
performance would benefit all of them.

## Companions

This package is independent of any other; it bottoms out at engine
primitives (`gb`, `selectInSubring`, `det`) without taking on
package dependencies. The reverse is not true — `MinimalPrimes`,
`PrimaryDecomposition`, `Saturation`, `IntegralClosure`,
`ReesAlgebra` all import it.

## See also

- [`file-MinimalPrimes.md`](file-MinimalPrimes.md) — primary downstream consumer
- [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) — downstream consumer
- [`file-Saturation.md`](file-Saturation.md) — uses elimination internally for the `Eliminate` strategy
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Engine monomial orderings: `e/file-monordering.md`](../e/file-monordering.md) — defines the `Eliminate(k)` order this package depends on
- [Engine GB: `e/groebner-bases.md`](../e/groebner-bases.md) — does the actual elimination work
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — the rings on which this package's preconditions hold
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
