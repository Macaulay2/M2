# `reducedgb*.{cpp,hpp}` — the reduced-GB family

After a Gröbner basis is computed, the engine canonicalises it to a **reduced
GB**: each leading monomial is unique, no element's tail is divisible by any
other element's leading monomial, and (over a field) leading coefficients
are 1. This page covers the family of classes that does that pass.

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## Class hierarchy

```
GBComputation                          ← comp-gb.hpp
 └── ReducedGB                         ← reducedgb.{cpp,hpp}        (base)
      ├── ReducedGB_Field              ← reducedgb-field.{cpp,hpp}
      │    └── ReducedGB_FieldLocal    ← reducedgb-field-local.{cpp,hpp}
      ├── ReducedGB_ZZ                 ← reducedgb-ZZ.{cpp,hpp}
      └── MarkedGB                     ← reducedgb-marked.{cpp,hpp}
```

`ReducedGB::create(...)` is the factory that picks the right subclass based
on the coefficient ring (field vs. ZZ vs. local) and whether the user
supplied a pre-marked leading-term map.

## What "reduced" means

For a GB `G = {g_1, …, g_r}` of an ideal `I`:

| Condition | Field case | ZZ case |
|---|---|---|
| Leading monomials distinct | ✓ | ✓ |
| No tail divisible by another LT | ✓ | ✓ |
| Leading coefficients | normalised to 1 | normalised to positive |
| Coefficient minimality | n/a | gcd-reduced |

`ReducedGB_Field` enforces the field case; `ReducedGB_ZZ` enforces the ZZ
case (where you can have multiple basis elements with the same monomial
support, distinguished only by leading coefficient).

## `ReducedGB_FieldLocal`

The local-ring case is subtler — divisibility over a local ring depends not
just on leading monomials but on whether the "extra" factor lies outside
the maximal ideal. `ReducedGB_FieldLocal` walks the basis with an extra
unit-check per reduction step. The matching ring code is in
[`file-localring.md`](file-localring.md).

## `MarkedGB`

`MarkedGB` is the case where the user supplied a Gröbner basis *along with*
a separate list of leading monomials (e.g. via `forceGB`). It does no
computation — it just bundles the data into the engine's `ReducedGB` shape so
downstream code can reach it.

## Shared state

`ReducedGB` (the base) carries:

- `GBRing *R;` — the GB-tuned ring view ([`file-gbring.md`](file-gbring.md))
- a `MonomialTable*` and (over ZZ) a `MonomialTableZZ*`
- a `GBWeight*` for degree tracking
- the `PolynomialRing*` and the underlying free module

Subclasses add their reduction loop and a small amount of state.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-gb-default.md`](file-gb-default.md) — produces the *non-reduced* GB
  that this family reduces.
- [`file-comp-gb.md`](file-comp-gb.md) — `GBComputation` base.
- [`file-gbring.md`](file-gbring.md), [`file-montable.md`](file-montable.md) —
  shared infrastructure.
