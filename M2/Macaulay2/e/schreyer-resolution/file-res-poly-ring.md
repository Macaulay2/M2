# `res-poly-ring.{cpp,hpp}` — `ResPolyRing` and `ResPolynomial`

`ResPolyRing` is the **resolution-tuned polynomial ring view**, and
`ResPolynomial` is the matching polynomial value type. Together they are
the resolution-side analogue of [`GBRing`](../file-gbring.md) / `gbvector`
on the GB side.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Why a separate ring view

A free resolution does **enormous** amounts of monomial arithmetic across
multiple homological levels. The standard
[`PolynomialRing`](../file-polyring.md) is more flexible than required:

- It supports arbitrary coefficient operations through virtuals.
- It carries the full degree-monoid recursion.
- It is not designed around the Schreyer-order encoding that resolution
  code wants for cheap leading-term factoring.

`ResPolyRing` is a stripped-down, encoded view tuned for the resolution
hot path:

- Templated on `VectorArithmetic` (so coefficient arithmetic inlines).
- Uses [`ResMonoid`](../README.md) (a resolution-specific monomial layout
  declared in [`res-moninfo.hpp`](README.md)).
- Supports both dense and sparse monomial encodings — see
  [`res-moninfo-dense.cpp`](README.md) and
  [`res-moninfo-sparse.cpp`](README.md).

## `ResPolynomial`

```cpp
class ResPolynomial {
    friend class ResPolyRing;
    friend class ResPolynomialConstructor;
    friend class ResPolynomialIterator;
    // ...
};
```

The friend triple — `ResPolyRing`, the constructor helper, and the
iterator — is the only thing allowed to look inside. Outside code must go
through `ResPolynomialIterator`; this guarantees that the internal storage
can be reorganised without breaking callers.

`ResPolynomialConstructor` is the moral analogue of `MatrixConstructor` on
the [`Matrix`](../file-matrix.md) side: it builds a `ResPolynomial`
piece-by-piece and yields an immutable result.

## Skew-multiplication

The header forward-declares `SkewMultiplication`; `ResPolyRing` supports
skew (exterior) variables for resolutions over skew-commutative rings.
The configuration matches what
[`file-skewpoly.md`](../file-skewpoly.md) stores on its host `PolyRing`.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-f4-computation.md`](file-res-f4-computation.md), [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — primary consumers.
- [`../file-gbring.md`](../file-gbring.md) — GB-side analogue.
- [`../file-polyring.md`](../file-polyring.md) — base polynomial ring.
- `res-moninfo*.{cpp,hpp}` — monomial encoding details.
