# `monomial-ordering.{h,cpp}` (in `interface/`) — public C entry points for `MonomialOrdering`

`interface/monomial-ordering.h` declares the **public C functions and
the `MonomialOrdering_type` enum** the interpreter uses to construct
monomial orderings before handing them off to
[`Monoid`](file-monoid-interface.md) construction.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## The `MonomialOrdering_type` enum

```c
enum MonomialOrdering_type {
    MO_LEX            = 1,
    MO_LEX2           = 2,
    MO_LEX4           = 3,
    MO_GREVLEX        = 4,
    MO_GREVLEX2       = 5,
    MO_GREVLEX4       = 6,
    MO_GREVLEX_WTS    = 7,
    MO_GREVLEX2_WTS   = 8,
    // ...
};
```

The enum is the **public vocabulary** of monomial orderings: lex,
graded reverse lex, with packed (`2`/`4`) or weighted variants, plus
revlex, weight blocks, component-up/down, position blocks. This vocabulary
flows from the M2 user (`Lex => …`) through the interpreter into here,
and from here into the encoded form built by
[`imonorder.cpp`](../file-imonorder.md).

The packed variants (`MO_LEX2`, `MO_LEX4`) pack two or four exponents per
`int` to halve / quarter the monomial-comparison memory footprint when
exponents are small.

## Entry points

The header declares `raw…` constructors for each block type:

- `rawLexMonomialOrdering(n)` — `Lex` over `n` variables.
- `rawGRevLexMonomialOrdering(n, wts)` — graded reverse lex.
- `rawWeightsMonomialOrdering(wts)` — a pure weight block.
- `rawProductMonomialOrdering(blocks)` — concatenation of orderings.
- `rawPositionMonomialOrdering(up_or_down)` — position-up / position-down
  for free-module components.

Composition of orderings is by `rawProductMonomialOrdering`, which
takes an array of `MonomialOrdering*` and produces a new one.

## How it connects to `Monoid`

A `Monoid` ([`../file-monoid.md`](../file-monoid.md)) is constructed by
passing a `MonomialOrdering*` to
[`rawMonoid`](file-monoid-interface.md). Internally the `Monoid`
constructor calls `imonorder.cpp` to compile the declarative
`MonomialOrdering` into an operational
[`MonomialOrder`](../file-imonorder.md).

## Related

- [`README.md`](README.md) — interface overview.
- [`file-monoid-interface.md`](file-monoid-interface.md) — consumes the
  output of this header.
- [`../monoids-and-monomials.md`](../monoids-and-monomials.md) — area
  overview.
- [`../file-imonorder.md`](../file-imonorder.md) — compiled form.
- [`../../m2/monoids.m2`](../../m2/README.md) — M2-side wrapper.
