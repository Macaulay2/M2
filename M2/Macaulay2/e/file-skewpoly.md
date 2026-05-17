# `skewpoly.{cpp,hpp}` — `SkewPolynomialRing`

`SkewPolynomialRing` implements **skew-commutative** polynomial rings — the
generalisation of exterior algebras where a designated subset of variables
anticommutes (and squares to zero) while the rest commute normally.

Subclasses [`PolyRing`](file-polyring.md). Part of the
[Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## State

```cpp
class SkewPolynomialRing : public PolyRing {
    bool initialize_skew(M2_arrayint skewvars);
    // configuration done at construction; subsequent state is on PolyRing
};
```

The constructor takes the base coefficient ring `K`, the monoid `M`, and
`M2_arrayint skewvars` — the indices of variables that anticommute. The skew
data ends up on `PolyRing`'s `SkewMultiplication skew_` field; instances of
`SkewPolynomialRing` simply switch on the `is_skew_` flag during arithmetic.

## Multiplication

For skew variables `x_i, x_j` (i ≠ j):

```
x_i · x_j   =  −x_j · x_i
x_i · x_i   =  0
```

Ordinary variables commute with everyone (including skew ones) — this is
"super-commutative" if you treat skew variables as odd-degree.

A multiplication implementation walks both factors term-by-term and resolves
the skew permutation by counting transpositions. The sign is the parity of
the permutation that moves skew variables of the second factor past skew
variables of the first.

## What you get over a base `PolyRing`

`SkewPolynomialRing` overrides:

- `mult` for term-by-term skew multiplication
- `is_skew_commutative_ring()` → `true`
- `has_gcd()` → `false` (GCD makes no sense over a non-domain)

Everything else (addition, GB, basis) flows through `PolyRing`'s
`is_skew_` dispatch.

## Exterior algebras

The pure exterior algebra `Λ(x_1, …, x_n)` is the special case where *every*
variable is skew. The engine doesn't have a separate type for this — it is
just a `SkewPolynomialRing` with `skewvars = {0, 1, …, n-1}`.

## Use sites

- [`m2/polyrings.m2`](../m2/README.md) — exposes `R[x_1, …, x_n, SkewCommutative => ...]`.
- [Sheaf cohomology computations via BGG](../packages/BGG.m2) — the BGG
  correspondence depends on exterior-algebra GBs.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-polyring.md`](file-polyring.md) — base class.
- [`file-weylalg.md`](file-weylalg.md) — sibling non-commutative variant.
- [`solvable.{cpp,hpp}`](polynomial-rings.md) — broader generalisation.
