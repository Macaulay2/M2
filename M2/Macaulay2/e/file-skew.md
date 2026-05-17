# `skew.hpp` — `SkewMultiplication`

`SkewMultiplication` is the **configuration object** for skew-commutative
multiplication. It is attached to a [`PolyRing`](file-polyring.md) (and
to F4's [`MonomialInfo`](f4/file-moninfo.md)) when the user has declared
some variables skew.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
class SkewMultiplication {
public:
    int   _n_vars;
    int   _n_skew;
    int  *_skew_list;   // indices of the skew-commuting variables
    bool *_skew_exp;    // bitmap: is variable i a skew variable?

    unsigned long skew_byte_size;

    SkewMultiplication();
    SkewMultiplication(int nvars, int nskew, int *skew_list);
    ~SkewMultiplication() {}

    int  n_skew_vars() const            { return _n_skew; }
    bool is_skew_var(int i) const       { return _skew_exp[i]; }
    int  skew_variable(int i) const     { return _skew_list[i]; }
    int  skew_degree(const int *exp) const;
    // ...
};
```

The class is **pure configuration**. It doesn't perform multiplication
itself — it tells the polynomial-ring class how to compute the sign
that arises when permuting skew variables.

Two parallel representations of the skew set:

- **`_skew_list`** — array of skew-variable indices in order. O(`_n_skew`)
  per lookup.
- **`_skew_exp`** — `_n_vars`-sized boolean array. O(1) per lookup.

Both exist for performance: in some hot loops we iterate over only the
skew variables (use `_skew_list`); in others we test a specific variable
(use `_skew_exp`).

## How it's used

For each multiplication `x_i · x_j` in a skew-commutative polynomial
ring:

1. Look up `is_skew_var(i)` and `is_skew_var(j)`.
2. If both are skew with `i > j`, emit `-x_j · x_i`.
3. If both are skew with `i == j`, emit `0`.
4. Otherwise commute freely.

For multiplying two arbitrary monomials, the implementation in
[`file-skewpoly.md`](file-skewpoly.md) walks both monomials and uses
`SkewMultiplication`'s queries to compute the sign of the product.

## `skew_degree`

```cpp
int skew_degree(const int *exp) const;
```

For an exponent vector `exp`, return the number of skew variables that
appear with positive exponent. Used by the engine's parity-check
logic — for example, the Koszul sign of a multi-index product.

## `skew_byte_size`

The cached byte size of an encoded monomial in this ring. Stored here
rather than in `Monoid` because the skew machinery affects the
required slot layout.

## Used by

- [`file-polyring.md`](file-polyring.md) and the `is_skew_` flag —
  primary consumer.
- [`file-skewpoly.md`](file-skewpoly.md) — `SkewPolynomialRing`.
- [`f4/file-moninfo.md`](f4/file-moninfo.md) — F4's `MonomialInfo`
  carries one of these.
- Resolutions over exterior algebras.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-skewpoly.md`](file-skewpoly.md) — primary consumer.
- [`file-polyring.md`](file-polyring.md) — base polynomial ring with
  the skew flag.
