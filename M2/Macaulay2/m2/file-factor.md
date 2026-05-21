# `factor.m2` — polynomial / integer factorisation

`factor.m2` is the M2-side wrapper for **factorisation** —
factor(`f`), `roots(f)`, `irreducibleCharacteristicSeries`, and
related operations. It routes through the engine's
[Factory](../e/interface/file-factory-interface.md) bridge plus
specialised paths for integers and univariate polynomials.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "integers.m2"
needs "lists.m2"
needs "matrix1.m2"

monic := t -> (
    c := leadCoefficient t;
    ...
)
```

A local `monic` helper normalises a polynomial to monic form
(divide by leading coefficient). Used as a normalisation step
before / after factorisation so the output is canonical.

## User-facing API

- **`factor n`** — for `n` an integer, return its prime factorisation
  as a `Product` of `Power(prime, exponent)` expressions.
- **`factor f`** — for `f` a polynomial, return its irreducible
  factorisation. The coefficient ring must be one Factory supports
  (`QQ`, `Z/p`, `GF`, …).
- **`isPrime n`** — primality test via FLINT
  ([`../e/interface/file-flint-interface.md`](../e/interface/file-flint-interface.md)).
- **`roots f`** — numerical roots of a univariate polynomial
  ([`../e/file-polyroots.md`](../e/file-polyroots.md)).
- **`irreducibleCharacteristicSeries`** — for systems of polynomials.

## Output format

`factor` returns an `Expression`-typed value
([`file-expressions.md`](file-expressions.md)) — specifically a
`Product` whose factors are `Power` nodes. This lets the result
display naturally:

```text
factor(12)         -- (2)^2 * (3)
factor(x^4 - 1)    -- (x - 1) * (x + 1) * (x^2 + 1)
```

Users can extract the underlying numeric form via `value`.

## Engine path

Most of the heavy lifting happens in the engine:

- **Integers**: FLINT's `fmpz_factor` via
  [`../e/interface/file-flint-interface.md`](../e/interface/file-flint-interface.md).
- **Polynomials**: Factory via
  [`../e/interface/file-factory-interface.md`](../e/interface/file-factory-interface.md).
- **Roots**: MPSolve or FLINT-Arb via
  [`../e/file-polyroots.md`](../e/file-polyroots.md).

This file is the M2-side glue that picks the right path.

## Used by

- Every M2 user calling `factor`.
- Number-theory packages.
- Algebraic-geometry packages computing primary decomposition.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/interface/file-factory-interface.md`](../e/interface/file-factory-interface.md)
  — Factory bridge.
- [`../e/interface/file-flint-interface.md`](../e/interface/file-flint-interface.md)
  — FLINT bridge.
- [`../e/file-polyroots.md`](../e/file-polyroots.md) — root finder.
- [`../e/file-dpoly.md`](../e/file-dpoly.md) — engine-side dpoly.
