# `frac.{cpp,hpp}` — `FractionField`

`FractionField` is the engine's representation of the **field of fractions**
of an integral domain. Internally it stores fractions as a `(numerator,
denominator)` pair with on-the-fly simplification.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## State

```cpp
struct frac_elem {
    ring_elem numer;
    ring_elem denom;
};

class FractionField : public Ring {
    const PolyRingFlat *R_;     // base ring (an integral domain)
    bool use_gcd_simplify;      // true iff R_ is frac(ZZ[xs]) or frac(ZZ/p[xs])
    // ...
};
```

Two invariants matter:

1. The **base ring `R_`** must be an integral domain. The engine does **not**
   verify this — passing a non-domain produces silent garbage. The
   [M2-side](../m2/README.md) wrapper is responsible for the check.
2. The **denominator is normalised** (positive sign / leading-coefficient
   chosen) so that `(2/3) == (4/6)` after simplification.

When the base ring is `ZZ[x_1, …, x_n]` or `(ZZ/p)[x_1, …, x_n]`, `simplify`
uses an explicit GCD — the resulting fraction is in canonical form. For
other domains the engine falls back to a weaker normalisation that still
preserves equality but may leave a common factor in.

## Arithmetic

The standard cross-multiplication formulas are used:

```
(a/b) + (c/d)  =  (ad + bc) / bd
(a/b) * (c/d)  =  (ac) / (bd)
```

A `simplify(...)` pass after each operation keeps fractions reduced.

## Allocation

`new_frac_elem()` allocates a `frac_elem` from the engine's pool. As with
every other ring-element-internal type, allocation is GC-managed and the
returned pointer is stored inside a `ring_elem`.

## When is `FractionField` used

- M2 expression `frac R` (e.g. `frac(QQ[x,y])` to get `QQ(x,y)`).
- Internally as the coefficient ring of certain GB strategies that work over
  the function field.
- Local rings ([`file-localring.md`](file-localring.md)) reuse this fraction
  representation as their stored form.

## Limitations

The base ring must be `PolyRingFlat` — a flat polynomial ring. Iterated
constructions (e.g. `frac(frac(R))`) work because the engine flattens before
constructing the fraction field. The header notes this is an artefact of a
historical design and "when we change fractions to be flat" the restriction
will go away.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-localring.md`](file-localring.md) — local-ring case reuses
  `frac_elem`.
- [`file-qring.md`](file-qring.md) — quotient-ring construction.
- [`interface/ring.{h,cpp}`](interface/README.md) — public API.
