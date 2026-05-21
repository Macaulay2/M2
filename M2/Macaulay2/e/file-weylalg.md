# `weylalg.{cpp,hpp}` — `WeylAlgebra`

`WeylAlgebra` implements the **Weyl algebra**: the ring of polynomial
differential operators. Each "variable" comes in two flavours — a coordinate
`x_i` and its derivative `∂_i = ∂/∂x_i` — with the relation `∂_i x_i =
x_i ∂_i + 1`.

Subclasses [`PolyRing`](file-polyring.md). Part of the
[Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## State

```cpp
class WeylAlgebra : public PolyRing {
    int   _nderivatives;
    bool  _homogeneous_weyl_algebra;
    int   _homog_var;
    int  *_derivative;     // _derivative[i] >= 0 means x_i = diff(_derivative[i])
    int  *_commutative;    // inverse of _derivative
    // static caches for binomial coefficients
};
```

The user specifies which variables are derivatives of which:

- `_derivative[i] = j` means variable `i` is `∂_{j}`.
- `_derivative[i] = -1` means variable `i` is an ordinary commuting variable
  (constants of integration / parameters that don't act on anyone).

`_commutative` is the inverse map (`_commutative[j] = i` iff `∂_i = ∂/∂x_j`).

## Multiplication

The non-trivial part is multiplying `∂` past `x`:

```
∂^a · x^b  =  Σ_{k} C(a,k) C(b,k) k! · x^{b-k} ∂^{a-k}
```

Two cached tables make this fast:

- **`binomtable`** — precomputed binomial coefficients `C(a, k)`.
- **`diffcoeffstable`** — precomputed Leibniz coefficients `C(a,k) C(b,k) k!`.

Both tables grow on demand up to a configurable `binomtop` /
`diffcoeffstop`.

## Homogeneous variant

When `_homogeneous_weyl_algebra` is true, `∂_i` carries the same degree as
`x_i` (rather than its natural degree of `-deg(x_i)`). This is the form
typically used when computing characteristic varieties / D-module
intersections; the homogeneity makes everything graded and lets standard GB
machinery apply unchanged.

The homogeneity variable `_homog_var` is an extra variable that absorbs the
extra degree.

## What you get over a base `PolyRing`

`WeylAlgebra` overrides:

- `mult` (multiplication of two terms / two polynomials)
- `is_commutative_ring()` → false
- the constructor parses an array of `(derivative-of, of-which-variable)`
  pairs

Everything else (basis, addition, GB infrastructure) is inherited from
`PolyRing` and dispatches through the `is_weyl_` flag in
[`file-polyring.md`](file-polyring.md).

## Use sites

- [`Dmodules`](../packages/Dmodules.m2) package — D-modules computations.
- [`BernsteinSato`](../packages/BernsteinSato.m2) — *b*-functions.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-polyring.md`](file-polyring.md) — base class.
- [`file-skewpoly.md`](file-skewpoly.md) — sibling non-commutative variant.
- [`solvable.{cpp,hpp}`](polynomial-rings.md) — generalisation (PBW
  solvable algebras).
