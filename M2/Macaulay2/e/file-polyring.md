# `polyring.{cpp,hpp}` — the `PolynomialRing` class

`PolynomialRing` is the engine's polynomial ring `R[x_1,…,x_n]`. It is the
single most heavily reused class in the engine — almost every algorithm in
`e/` is parameterised by a `PolynomialRing*`.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Inheritance

```
Ring                 ← base class (ring.hpp)
 └── PolynomialRing  ← this file
      ├── PolyRing             commutative
      ├── PolyRingSkew         skew-commutative (exterior)
      ├── PolyRingWeyl         Weyl algebra
      ├── PolyRingNC           non-commutative
      └── PolyQuotient         quotient by an ideal
```

The `is_skew_`, `is_weyl_`, `is_solvable_` flags on the base class let one
pointer represent any of the flavours; ring-specific logic dispatches on the
flag in hot paths and through virtuals where the divergence is large.

## Key state

- `is_graded_` — whether the ring has a grading respected by all operations.
- `is_skew_` + a `SkewMultiplication skew_` configuration — controls how
  same-variable multiplication wraps to zero in exterior-like rings.
- `is_weyl_`, `is_solvable_` — flags selecting alternative multiplication
  implementations in [`weylalg.cpp`](weylalg.cpp) / [`solvable.cpp`](solvable.cpp).
- A `Monoid*` for the monomial side (see [`file-monoid.md`](file-monoid.md)).
- A coefficient `Ring*` (often an `aring` wrapper — see
  [`coefficient-rings.md`](coefficient-rings.md)).

## Friend classes

`polyring.hpp` forward-declares a number of friends:

- `GBRing`, `GBRingSkew` — GB-tuned views over the same monomial / coefficient
  layout (see [`gbring.cpp`](gbring.cpp)).
- `GBComputation` — the Gröbner Computation operating on this ring.

The friend graph lets these classes reach into the polynomial ring's encoded
monomial representation without going through virtual calls — important for
inner-loop performance.

## Construction

`PolynomialRing` instances are built through a small set of factory functions
in `polyring.cpp` (and in [`enginering.m2`](../m2/README.md) on the M2 side).
Direct construction is unusual; users normally see `R = ZZ/101[x,y,z]`.

## Quotient case

A `PolyQuotient` carries an extra Gröbner basis of the defining ideal so that
multiplication followed by reduction is always available. Construction goes
through [`qring.cpp`](qring.cpp).

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-monoid.md`](file-monoid.md) — `Monoid` half of a polynomial ring.
- [`gbring.cpp`](gbring.cpp), [`groebner-bases.md`](groebner-bases.md) —
  GB-tuned view.
- [`qring.cpp`](qring.cpp) — quotient rings.
- [`interface/ring.{h,cpp}`](interface/README.md) — public C interface.
