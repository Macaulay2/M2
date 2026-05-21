# `polyrings.m2` — the M2-side `PolynomialRing` type

`polyrings.m2` defines the **M2-side `PolynomialRing` type** — the
top-level wrapper for the engine's
[`PolyRing`](../e/file-polyring.md) class. It is the file that
implements the syntax `R = QQ[x, y, z]`.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## The `PolynomialRing` type

```m2
needs "methods.m2"
needs "enginering.m2"
needs "monoids.m2"
needs "indeterminates.m2"    -- runLengthEncode

PolynomialRing = new Type of EngineRing
PolynomialRing.synonym = "polynomial ring"

PolynomialRing#AfterPrint = R -> (
    class R,
    if #R.monoid.Options.SkewCommutative > 0
    then (", ", #R.monoid.Options.SkewCommutative, " skew commutative variable(s)"),
    if #R.monoid.Options.WeylAlgebra > 0
    then (", ", #R.monoid.Options.WeylAlgebra, " differential variable(s)"),
    -- ...
)
```

Three things to note:

1. `PolynomialRing` is a **subclass of `EngineRing`** — i.e., a ring
   whose underlying representation lives in the engine. The pattern
   `new Type of <Parent>` is M2's standard way of declaring new
   types.
2. **`PolynomialRing#AfterPrint`** — what M2 displays after printing
   a `PolynomialRing` value. Reports skew-commutative variables,
   Weyl-differential variables, etc., so the user can tell flavoured
   variants apart at a glance.
3. Heavy use of `R.monoid.Options` — most metadata about a
   polynomial ring lives in its underlying monoid's options table.

## How `R = QQ[x, y, z]` flows

```text
1. M2 parser sees `QQ[x, y, z]`
2. `Ring [Symbol, Symbol, Symbol]` method (declared here) is called
3. It builds a Monoid from the symbol list (via monoids.m2)
4. It calls engineRing(R, M) (via enginering.m2)
5. enginering.m2 calls into the engine via rawPolynomialRing(...)
6. The engine returns a Ring* the user holds onto.
```

The `PolynomialRing` type wraps that engine `Ring*`.

## Sibling types

`polyrings.m2` declares the standard polynomial-ring family:

- **`PolynomialRing`** (this file) — base.
- **`PolyRingSkew`** — skew-commutative (declared elsewhere but
  registers here).
- **`QuotientRing`** — quotient. (Declared in `quotring.m2`.)

## Used by

- Every M2 user that constructs a polynomial ring.
- Downstream packages that subclass `PolynomialRing` for specialised
  rings.
- Engine code paths that interrogate `R.monoid.Options` to decide
  on a strategy.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-polyring.md`](../e/file-polyring.md) — engine class.
- [`../e/file-poly.md`](../e/file-poly.md) — concrete `PolyRing`.
- `enginering.m2` — bridge to the engine.
- `monoids.m2` — `Monoid` construction.
- `indeterminates.m2` — variable-name handling.
- `quotring.m2` — `QuotientRing` (`R/I`).
- `freealgebras.m2` — non-commutative variant.
