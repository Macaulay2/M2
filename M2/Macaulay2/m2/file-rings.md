# `rings.m2` — the `Ring` base type

`rings.m2` declares the **`Ring`** type — the abstract M2 base class
for every ring the user manipulates. Polynomial rings, finite fields,
fraction fields, and quotients all inherit from this.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared here

```m2
needs "methods.m2"
needs "expressions.m2"  -- for Constant

Ring.synonym = "ring"

Ring == ZZ := (R, i) -> (
    if i === 0 then 1_R == 0
    else error "comparison of ring with nonzero integer encountered"
)
```

The basics:

- **`Ring.synonym = "ring"`** — the noun M2 uses in error messages.
- **`Ring == ZZ`** — comparing a ring to an integer is only defined
  for `0` (testing whether the ring is the zero ring). Other
  comparisons are explicit errors.

Beyond these, `rings.m2` declares the general `Ring` method set:

- `+`, `*`, `==`, `==>` on rings (mostly raising appropriate errors;
  arithmetic gets defined in subclasses).
- Default `coefficientRing R`, `characteristic R`, `dim R`, `degree R`,
  `degreeLength R` accessors.
- `isField R`, `isDomain R` predicates.
- `ring x` — given any value, return the ring it belongs to.

## Class hierarchy

`Ring` is the parent of:

| Subclass | File |
|---|---|
| `EngineRing` | `enginering.m2` ([`file-enginering.md`](file-enginering.md)) |
| `InexactField` | `reals.m2` |
| `FractionField` | `fractions.m2` |

`EngineRing` is in turn the parent of:

- [`PolynomialRing`](file-polyrings.md)
- `QuotientRing` (`quotring.m2`)
- `LocalRing` (`localring.m2`)
- `GaloisField` (`galois.m2`)
- `FreeAlgebra`, `FreeAlgebraQuotient` (`freealgebras.m2`)
- ... and others.

Every ring the user encounters is a `Ring` somewhere in this tree.

## Comparing rings

`R === S` is identity equality (same Ring object). `R == S` is
mathematically-equivalence (defined per subclass). The two are
mostly the same but can differ when the same ring is reconstructed
twice — `==` then says "yes," `===` says "no."

## Used by

Essentially every other `.m2` file — `Ring` is fundamental.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-polyrings.md`](file-polyrings.md) — primary subclass.
- [`file-enginering.md`](file-enginering.md) — engine-backed
  subclass.
- [`../e/file-ring-interface.md`](../e/interface/file-ring-interface.md)
  — engine-side `Ring`.
- `quotring.m2`, `localring.m2`, `galois.m2`, `fractions.m2` — other
  flavours.
