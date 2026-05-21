# `monoids.m2` — the `Monoid` type

`monoids.m2` defines **`Monoid`** — the M2-side type for the
multiplicative side of a polynomial ring (variables + monomial
ordering + degree map). It is the bridge to the engine's
[`Monoid`](../e/file-monoid.md) class.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
needs "engine.m2"
needs "expressions.m2"
needs "indeterminates.m2"
needs "methods.m2"
needs "remember.m2"
needs "shared.m2"     -- for tensor
needs "variables.m2"

-- TODO:
-- 1. implement a free monoid M whose degrees have torsion [done]
-- 2. implement the degrees ring of M as a quotient ring (optional?)
-- 3. implement the degrees monoid of M as a monoid with relations (optional?)
```

The TODO block documents future work toward more flexible degree
groups. Item 1 (torsion degree groups) was completed; items 2-3
remain.

## What's declared

The `Monoid` type and its construction:

```m2
Monoid = new Type of GradedMonoid
Monoid.synonym = "monoid"

monoid Array := opts -> args -> (
    -- parse the M2 syntax `R[x,y,z]`
    -- build degree vectors, monomial ordering, weight functions
    -- call into the engine via rawMonoid(...)
)
```

The `monoid Array` method is what runs when the user writes
`R[x, y, z, MonomialOrder => Lex]`. It:

1. Parses the array of variable names and options.
2. Resolves the monomial-order specification.
3. Computes the degree map (with default `Degrees => {1, 1, …}`).
4. Calls the engine's
   [`interface/file-monoid-interface.md`](../e/interface/file-monoid-interface.md)
   to build the engine-side `Monoid`.
5. Wraps the result.

## Monomial ordering construction

The file also parses the various `MonomialOrder => …` syntaxes the
user can write:

- `MonomialOrder => Lex`
- `MonomialOrder => GRevLex`
- `MonomialOrder => {GRevLex => 3, Lex => 2}` (block order)
- `MonomialOrder => Weights => {1, 0, 1, 0}` (weight block)
- ... and combinations.

The output is a list of `MonomialOrdering` blocks the engine
understands.

## Degrees

By default each variable has degree 1. The `Degrees => …` option
lets the user customise. The degrees can be multi-graded:

```m2
R = QQ[x, y, z, Degrees => {{1, 0}, {0, 1}, {1, 1}}]
```

`monoids.m2` handles all the multi-graded plumbing, including the
"degree monoid" (a monoid whose elements are the degree vectors of
this monoid's variables).

## Used by

- Every M2 user constructing a polynomial ring.
- [`file-polyrings.md`](file-polyrings.md) — uses `Monoid` as the
  monomial half of `PolynomialRing`.
- `indeterminates.m2`, `variables.m2` — sibling files for
  variable-name management.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-monoid.md`](../e/file-monoid.md) — engine class.
- [`../e/interface/file-monoid-interface.md`](../e/interface/file-monoid-interface.md)
  — public C entry points.
- [`../e/interface/file-monomial-ordering-interface.md`](../e/interface/file-monomial-ordering-interface.md)
  — ordering constructors.
- [`file-polyrings.md`](file-polyrings.md) — primary consumer.
