# `imonorder.{cpp,hpp}` — internal monomial ordering

`imonorder.cpp` defines the **internal form** of a monomial ordering — the
encoded, runtime-friendly representation walked by the inner loop when two
monomials are compared. It is the operational counterpart to the declarative
[`MonomialOrdering`](monoids-and-monomials.md) the user wrote.

Part of the [Monoids & monomials](monoids-and-monomials.md) area. The
high-level walkthrough is in [`file-monoid.md`](file-monoid.md).

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## The `mo_block` building block

```cpp
struct mo_block {
    enum MonomialOrdering_type typ;
    int nvars;
    // ...
};
```

The internal form is a **list of blocks**. Each block has:

- A **type** — one of `MO_LEX`, `MO_GREVLEX`, `MO_WEIGHTS`,
  `MO_REVLEX`, `MO_POSITION_UP`, `MO_POSITION_DOWN`, etc. (the enum lives
  in [`interface/monomial-ordering.h`](interface/README.md)).
- A **variable count** `nvars` — how many variables the block governs.
- Block-specific data — weight vectors, position offsets, etc.

A comparison of two encoded monomials walks the block list and returns at
the first block that breaks the tie.

## Encoded monomial layout

```
[ slot_0, slot_1, …, slot_{m-1} ]
```

where:

- The first slot is often a **degree** or **weight sum** (chosen so that
  the most common comparison terminates after reading just one int).
- Subsequent slots hold per-block packed exponent vectors in the form that
  block's compare routine expects.

The "compare = `memcmp`" trick works for many orderings: encode so that the
lexicographic comparison of the packed bytes produces the correct
monomial-order result.

## Why a distinct form

The declarative `MonomialOrdering` is good for:
- M2-side serialisation (round-trip through `toString`),
- algebraic introspection ("is this lex?"),
- presenting orderings to the user.

The internal `MonomialOrder` is good for:
- inner loop speed — a tight pointer walk,
- monomial multiplication's per-slot fixed structure.

`imonorder.cpp` is the translator. A `Monoid` constructor calls it once at
ring-construction time; the resulting `MonomialOrder*` is then frozen.

## `deg_t`

```cpp
typedef int32_t deg_t;
```

Degrees and weights are 32-bit signed. Larger values force `overflow.hpp`
([`utilities.md`](utilities.md)) checks; the engine refuses computations
whose intermediate degrees would overflow.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-monoid.md`](file-monoid.md) — `Monoid` consumer.
- [`monorder.{cpp,hpp}`](monoids-and-monomials.md) — user-facing description.
- [`interface/monomial-ordering.{h,cpp}`](interface/README.md) — public API.
- [`utilities.md`](utilities.md) — overflow.
