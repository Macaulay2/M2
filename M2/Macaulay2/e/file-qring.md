# `qring.{cpp,hpp}` — quotient ring information

`qring.cpp` provides the **bookkeeping for a polynomial-ring quotient**
`R/I`. It is *not* a separate `Ring` subclass — `PolyQuotient` (declared in
[`polyring.hpp`](file-polyring.md)) is the ring class, while
`QRingInfo` here is the data attached to it.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## What `QRingInfo` stores

```cpp
class QRingInfo : public our_new_delete {
    VECTOR(Nterm *)    quotient_ideal;       // generators as polynomial terms
    VECTOR(gbvector *) quotient_gbvectors;   // same, in gbvector form
    // ...
};
```

The defining ideal is kept in **two redundant forms**:

- **`Nterm *`** — the standard polynomial representation (used when the
  quotient ring is asked to reduce a `ring_elem`).
- **`gbvector *`** — the GB-tuned form (used when a GB computation inside
  the quotient needs fast reduction).

Keeping both avoids per-reduction conversion. They are kept in sync by the
construction code in `qring.cpp`.

## Multiplication in a quotient

When you multiply two elements of `R/I`:

1. Multiply as you would in `R`.
2. Reduce modulo the stored GB of `I`.

The reduction uses the `quotient_gbvectors` representation via
[`gbring`](file-gbring.md).

## Quotient detection

Some operations distinguish whether a ring is a "true" quotient or a thin
wrapper:

- `is_quotient_ring()` on the `PolyQuotient` returns true.
- `getQRingInfo()` returns the populated `QRingInfo*`.
- The flag-based dispatch in [`file-polyring.md`](file-polyring.md) chooses
  the quotient code path when this flag is set.

## How a `PolyQuotient` is built

From M2: `R = QQ[x,y]; I = ideal(x^2 - y); Q = R/I` calls into
`QuotientPolyRing::create` (or equivalent), which:

1. Computes a Gröbner basis of `I`.
2. Stores both `Nterm` and `gbvector` forms in a fresh `QRingInfo`.
3. Returns a `PolyQuotient` referencing the original `R` plus this
   `QRingInfo`.

## Limitations

- `R` must be a flat polynomial ring (`PolyRingFlat`). Compositions like
  `R/I/J` are flattened first.
- The quotient ideal must be homogeneous **or** the user must accept that
  certain operations (e.g. `Hilbert`) may behave unexpectedly.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-polyring.md`](file-polyring.md) — `PolyQuotient` lives there.
- [`file-gbring.md`](file-gbring.md) — GB-vector form used internally.
- [`file-frac.md`](file-frac.md) — sibling construction (fractions).
- [`interface/ring.{h,cpp}`](interface/README.md) — public API.
