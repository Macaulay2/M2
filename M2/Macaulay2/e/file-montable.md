# `montable.{cpp,hpp}` — `MonomialTable`

`MonomialTable` is the engine's **leading-monomial index** — a data structure
that maps each monomial in a Gröbner basis to its owning polynomial, with
fast divisibility queries. It is the central reduction-lookup structure in
[`gbA`](file-gb-default.md) and its peers.

Part of the [Monoids & monomials](monoids-and-monomials.md) area. The
ZZ-coefficient analogue is `montableZZ.{cpp,hpp}`.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## The reduction-lookup problem

A GB step does this thousands of times:

> Given a monomial `m`, is there a basis element whose leading monomial
> divides `m`? If yes, return it.

`MonomialTable` is optimised for that single query.

## Internal representation

The header comment (lightly paraphrased):

> Elements are kept in **lex-increasing order**. Exponent vectors are passed
> by pointer; this code never allocates or frees them — they may have more
> entries than `nvars` (e.g. sugar homogenisation), and those extra entries
> are ignored.

The exponent vector is the same `[e_1, …, e_n]` layout used throughout the
engine ([`monoids-and-monomials.md`](monoids-and-monomials.md)). Lex
sorting lets divisibility queries prune quickly: once you walk past a
candidate whose first variable's exponent exceeds yours, you can stop.

## Operations

- **`insert(exp, value)`** — add `(monomial, basis-element-index)` pair.
- **`find_divisor(exp)`** — return the first inserted monomial that
  divides `exp`, or none.
- **`find_divisors(exp, out)`** — collect all divisors.
- **`remove(exp)`** — drop a monomial.

## Memory

`MonomialTable` doesn't own the exponent vectors — it stores pointers. This
matches how `gbA` keeps its basis: the polynomial owns its leading monomial,
and the table merely points at it. When a polynomial is removed, the table
entry is removed first.

## The ZZ variant

[`montableZZ.{cpp,hpp}`](monoids-and-monomials.md) is the same idea
specialised to ZZ coefficients. The difference: over ZZ, divisibility tests
must also handle the **coefficient** comparison (a basis element's
leading coefficient must divide the reducee's). The class signature is
mostly the same, with extra `ring_elem` parameters.

## Use sites

- [`gbA`](file-gb-default.md) — primary user.
- `gb-homog2.cpp`, `gb-sugarless.cpp` — same role in homogeneous /
  sugarless GB variants.
- [`file-monideal.md`](file-monideal.md) — uses a `MonomialTable`-like
  layout internally.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-gb-default.md`](file-gb-default.md) — primary consumer.
- [`file-monideal.md`](file-monideal.md) — adjacent structure.
- `ExponentVector.hpp` / `ExponentList.hpp` — exponent encodings.
