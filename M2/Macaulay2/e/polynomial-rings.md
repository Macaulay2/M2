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
| `M2FreeAlgebra.{cpp,hpp}` | M2-facing wrapper around [`NCAlgebras/FreeAlgebra`](NCAlgebras/README.md). **Deep dive:** [`file-M2FreeAlgebra.md`](file-M2FreeAlgebra.md) |
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

## M2 ring constructor → engine class

The mapping from what an M2 user types when building a polynomial-style ring to which engine class actually holds it:

| M2 expression | Engine class | Source file | Notes |
|---|---|---|---|
| `R[x, y, z]` (commutative poly) | `PolynomialRing` | `polyring.{cpp,hpp}` | The default; entries via `Nterm *` |
| `R[x, y, z]/I` | `PolynomialRing` (with `quotient_ideal` flag) | `polyring.{cpp,hpp}` + `qring.{cpp,hpp}` | Quotient ring; relations stored alongside the ambient `PolynomialRing` |
| `frac R` (fraction field of integral domain) | `FractionField` | `frac.{cpp,hpp}` | Each element is a pair `(numerator, denominator)`; normalisation lazy |
| `R[x, y, z, Weights => {1,1,1}]` | `PolynomialRing` with weighted monoid | `polyring.{cpp,hpp}` + custom `Monoid` | Grading is in the monoid, not the ring |
| `R[x, y, z, SkewCommutative => {x,y}]` | `SkewPolynomialRing` | `skewpoly.{cpp,hpp}` | `x*y = -y*x` for skew vars; tracks skew-pair bitmask |
| `R[x, y, ∂_x, ∂_y, WeylAlgebra => {x => ∂_x, …}]` | `WeylAlgebra` | `weylalg.{cpp,hpp}` | Differential-operator ring; tracks commutator data |
| `R[x, y, z, SkewCommutative => …, …]` (generic ordered NC) | `SolvableAlgebra` | `solvable.{cpp,hpp}` | PBW-style; user supplies the `<` ordering |
| `freeAlgebra(R, vars)` (no commutativity) | `FreeAlgebra` / `M2FreeAlgebra` | `M2FreeAlgebra.{cpp,hpp}` + [`NCAlgebras/FreeAlgebra.{cpp,hpp}`](NCAlgebras/file-FreeAlgebra.md) | True free associative algebra; monomials are words, not exponent vectors |
| `freeAlgebra(R, vars)/I` | `FreeAlgebraQuotient` / `M2FreeAlgebraQuotient` | `M2FreeAlgebraQuotient.{cpp,hpp}` + [`NCAlgebras/FreeAlgebraQuotient.{cpp,hpp}`](NCAlgebras/file-FreeAlgebraQuotient.md) | NC quotient; requires a Gröbner basis of the two-sided ideal |
| `R = localRing(P, P_maxIdeal)` | `LocalRing` | `localring.{cpp,hpp}` | Localisation at a maximal ideal; element = `(num, denom)` with denom outside the ideal |
| `schurRing(QQ, "s", n)` | `SchurRing` | `schur.{cpp,hpp}` | Ring of symmetric functions in the Schur basis |
| `Tower` (legacy nested extensions) | `Tower` | `tower.{cpp,hpp}` | Older nested extension representation; mostly superseded by aring-tower |

Construction routes through:

```
M2: R = QQ[x, y, z]
   ↓
m2/setup.m2  →  R = polynomialRing(QQ, getSymbol \ {"x","y","z"}, ...)
   ↓
d/monoid.dd / d/interface.dd  →  rawMonoid(...) then rawPolynomialRing(...)
   ↓
e/interface/ring.h  →  IM2_Ring_polyring(QQ, monoid, ...)
   ↓
e/polyring.cpp  →  new PolynomialRing(base, monoid, ...)
   ↓
wrapped via ConcreteRing if needed, returned as Ring* to interpreter
```

The flag set (`isWeyl`, `isSkew`, `isQuotient`, `isLocal`) is checked in every arithmetic operation — that's the first dispatch layer. The second is the base ring's `mult`/`add`/etc.; the third is the monoid's `multmon`/`compare`.

## Choosing a polynomial-ring backend

When implementing a new operation that needs to work over multiple polynomial-ring shapes:

| Want | Pick |
|---|---|
| Standard commutative + GB workflow | `PolynomialRing` (the default) |
| Quotient by an ideal | `PolynomialRing` + `quotient_ideal`; the ambient ring carries the relations |
| Differential operators (D-modules) | `WeylAlgebra`; commutator data is in the ring |
| Skew/exterior algebra | `SkewPolynomialRing` (very fast; skew bitmask is checked inline) |
| True non-commutative free algebra | `M2FreeAlgebra` / `FreeAlgebra` (deeper subsystem; see [`NCAlgebras/`](NCAlgebras/README.md)) |
| Local ring at a prime | `LocalRing`; localisation is lazy until needed |
| Symmetric-function manipulation | `SchurRing`; the only ring whose elements are stored in a non-monomial basis |
| Fraction field | `frac R` (only valid when `R` is an integral domain) |

The decision is driven by **which arithmetic flags you need** more than by which type symbol. The `PolynomialRing` class with various flag combinations covers most workflows; the dedicated `SkewPolynomialRing` / `WeylAlgebra` / `SolvableAlgebra` classes exist because their inner loops benefit from specialised code paths beyond what the generic flag-based dispatch can inline.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — base rings used as
  coefficients.
- [`monoids-and-monomials.md`](monoids-and-monomials.md) — the monomial layer.
- [`groebner-bases.md`](groebner-bases.md) — primary consumer of polynomial
  rings.
- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — top-level
  `RingElement` and `RingMap` types.
