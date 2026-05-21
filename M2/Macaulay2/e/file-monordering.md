# `monordering.cpp` — `MonomialOrdering` constructors (implementation)

`monordering.cpp` is the **implementation** of the user-facing
[`MonomialOrdering`](interface/file-monomial-ordering-interface.md)
constructors. It is the C++ engine code behind the `rawLex…`,
`rawGRevLex…`, `rawWeights…`, `rawProductMonomialOrdering` C entry
points.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## What lives in this file

The file implements:

- **One function per block type** — `lex_block(n)`,
  `grevlex_block(n, wts)`, `weights_block(wts)`, `revlex_block(n)`, …
  Each returns a freshly allocated `MonomialOrdering*` representing a
  single block.
- **The composition operator** — `product_block(orderings)` walks an
  array of `MonomialOrdering*` and produces their concatenation as a
  new `MonomialOrdering*`.
- **Validation** — each constructor checks variable counts, weight
  vector lengths, and the consistency of any heft data.

## Why the implementation is here, not in `imonorder.cpp`

The engine carefully separates the user-facing **declarative** form
(`MonomialOrdering`, this file) from the inner-loop **operational**
form (`MonomialOrder`, [`file-imonorder.md`](file-imonorder.md)):

| File | Form | Purpose |
|---|---|---|
| `monordering.cpp` (this file) | Declarative | `MonomialOrdering` construction + textual rendering |
| `imonorder.cpp` ([`file-imonorder.md`](file-imonorder.md)) | Operational | Encoded comparator the inner loop walks |

The two forms have to coexist because:

- The declarative form is what the user wrote and what gets stored on
  disk.
- The operational form is what the engine actually uses to compare
  encoded monomials byte-by-byte.

`monordering.cpp` builds the declarative form. The
`Monoid` constructor ([`file-monoid.md`](file-monoid.md)) calls into
`imonorder.cpp` to compile it once, and from then on uses the encoded
form.

## Textual rendering

The file also implements `MonomialOrdering::text_out(buffer&)` —
producing the textual rendering of an ordering that goes back to the
M2 user. The format matches what the user types: `Lex => …`,
`GRevLex => …`, etc.

## Used by

- [`interface/file-monomial-ordering-interface.md`](interface/file-monomial-ordering-interface.md)
  — public C entry points dispatch into here.
- [`file-monoid.md`](file-monoid.md) — compiles the result into a
  `MonomialOrder` via `imonorder.cpp`.
- M2-side `monomialOrdering` wrapper in
  [`m2/monoids.m2`](../m2/README.md).

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-imonorder.md`](file-imonorder.md) — encoded operational form.
- [`interface/file-monomial-ordering-interface.md`](interface/file-monomial-ordering-interface.md)
  — public C interface.
