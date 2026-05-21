# `solvable.{cpp,hpp}` — `SolvableAlgebra`

`SolvableAlgebra` implements **PBW-style solvable algebras** — algebras that
look like polynomial rings except that the commutation `x_j x_i` (for `i < j`)
can equal a fixed lower-order polynomial `q_{ij}` instead of just `x_i x_j`.

Subclasses [`PolyRing`](file-polyring.md). Part of the
[Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## State

```cpp
class SolvableAlgebra : public PolyRing {
    const Matrix *Q_;          // matrix of commutation polynomials
    bool initialize_solvable(const Matrix *Q);
    // ...
public:
    static SolvableAlgebra *create(const PolynomialRing *R, const Matrix *Q);
};
```

The `Q_` matrix encodes the commutation relations. Specifically, for `i < j`
the `(i, j)` entry of `Q` is the polynomial `q_{ij}` such that

```
x_j · x_i  =  x_i · x_j  +  q_{ij}
```

When `Q` is the zero matrix the algebra is just the underlying polynomial
ring; non-zero entries introduce the solvable structure.

## Examples

| Algebra | Commutation rule | Form of `Q` |
|---|---|---|
| Polynomial ring | `x_j x_i = x_i x_j` | Zero matrix |
| Weyl algebra `(∂_i, x_i)` | `∂_i x_i = x_i ∂_i + 1` | `1` in the relevant slot |
| Quantum plane `K_q[x, y]` | `y x = q · x y` | `(q - 1) x y` |
| Universal enveloping algebra `U(g)` | `[x_i, x_j] = Σ c^k_{ij} x_k` | Linear forms |

In each case the commutation relation expresses `x_j x_i` as `x_i x_j +
(lower-order)`. PBW order ensures this lower-order term keeps lower
"sugar" so reduction always terminates.

## Multiplication

The override of `mult` walks both factors. When it would emit `x_j x_i` for
`i < j`, it substitutes `x_i x_j + q_{ij}` and recurses on `q_{ij}` (which
may itself contain `x_k x_l` pairs).

Termination requires `q_{ij}` to be "strictly smaller" than `x_i x_j` in
some well-founded ordering — this is the **PBW condition**, and the
constructor checks it (lazily — full verification can be expensive). It is
the user's responsibility to provide a `Q` that yields an associative
algebra.

## Generalisation of `WeylAlgebra` and `SkewPolynomialRing`

Both [`WeylAlgebra`](file-weylalg.md) and
[`SkewPolynomialRing`](file-skewpoly.md) are special cases of solvable
algebras. They exist as separate classes for performance (their
commutation rule has a known shape and can be implemented more directly)
and ergonomics (the API takes shape-specific arguments).

## Use sites

- Quantum-algebra packages.
- Universal enveloping algebra computations.
- Generic non-commutative ring construction from M2 (`R = solvable(...)`).

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-polyring.md`](file-polyring.md) — base class.
- [`file-weylalg.md`](file-weylalg.md) — Weyl-algebra specialisation.
- [`file-skewpoly.md`](file-skewpoly.md) — skew-commutative specialisation.
