# Polynomial rings and their cousins

The polynomial-ring layer sits on top of the
[coefficient rings](coefficient-rings.md) and below the
[matrix layer](matrices.md). It defines `Polynomial`-valued ring elements and
the various wrapping constructions M2 understands: quotient rings, fields of
fractions, Weyl algebras, skew (exterior) algebras, solvable algebras, local
rings.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Core polynomial value types

| File pair | Purpose |
|---|---|
| `polyring.{cpp,hpp}` | The standard commutative polynomial ring `R[x_1,…,x_n]`. **Deep dive:** [`file-polyring.md`](file-polyring.md) |
| `poly.{cpp,hpp}` | The polynomial value type stored in a `polyring` |
| `Polynomial.{cpp,hpp}` | Modernised polynomial value type used in newer GB code |
| `BasicPoly.{cpp,hpp}` | Lightweight polynomial value (just a vector of `(coeff, monom)`) |
| `BasicPolyList.{cpp,hpp}` | List of `BasicPoly` |
| `BasicPolyListParser.{cpp,hpp}` | Parser for the textual `BasicPolyList` form used by tests |
| `PolynomialStream.hpp` | Streaming interface: feed monomial+coeff pairs without building the whole polynomial first |

## Wrapping constructions

| File pair | What it builds |
|---|---|
| `qring.{cpp,hpp}` | Quotient ring `R/I`. **Deep dive:** [`file-qring.md`](file-qring.md) |
| `frac.{cpp,hpp}` | Field of fractions of an integral domain. **Deep dive:** [`file-frac.md`](file-frac.md) |
| `localring.{cpp,hpp}` | Local ring (localised at a prime ideal). **Deep dive:** [`file-localring.md`](file-localring.md) |

## Non-commutative algebras

| File pair | What it builds |
|---|---|
| `weylalg.{cpp,hpp}` | Weyl algebra (rings of differential operators). **Deep dive:** [`file-weylalg.md`](file-weylalg.md) |
| `skewpoly.{cpp,hpp}` | Skew-commutative polynomial ring (exterior-algebra-like). **Deep dive:** [`file-skewpoly.md`](file-skewpoly.md) |
| `solvable.{cpp,hpp}` | Solvable algebras (PBW-style ordered algebras). **Deep dive:** [`file-solvable.md`](file-solvable.md) |

The free-algebra side of non-commutative algebra lives in
[`NCAlgebras/`](NCAlgebras/README.md); the files here are the
*finite-presentation* non-commutative case that still has a commutative-style
monomial basis.

## Engine-facing wrappers

| File pair | Purpose |
|---|---|
| `M2FreeAlgebra.{cpp,hpp}` | M2-facing wrapper around [`NCAlgebras/FreeAlgebra`](NCAlgebras/README.md) |
| `M2FreeAlgebraQuotient.{cpp,hpp}` | Wrapper around [`NCAlgebras/FreeAlgebraQuotient`](NCAlgebras/README.md) |

## How polynomial arithmetic dispatches

A `polyring` is parameterised by:

- a **base ring** (often a coefficient ring from
  [`aring-*`](coefficient-rings.md), but it can be another ring)
- a **monoid** (see [`monoids-and-monomials.md`](monoids-and-monomials.md))
- optional flags (skew, weyl, solvable, quotient)

Addition / multiplication of polynomial values dispatches first through the
ring's flag set, then through the base ring's `aring` (or `Ring*`) for
coefficient arithmetic, then through the monoid for monomial arithmetic.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — base rings used as
  coefficients.
- [`monoids-and-monomials.md`](monoids-and-monomials.md) — the monomial layer.
- [`groebner-bases.md`](groebner-bases.md) — primary consumer of polynomial
  rings.
- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — top-level
  `RingElement` and `RingMap` types.
