# `FreeAlgebra.{cpp,hpp}` — `FreeAlgebra`

`FreeAlgebra` is the **free associative algebra** `k⟨x_1, …, x_n⟩` over a
coefficient ring `k`. It is the non-commutative analogue of
[`PolyRing`](../file-polyring.md): polynomials are linear combinations of
non-commutative words, multiplication is concatenation lifted to
polynomials.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class FreeAlgebra : public our_new_delete {
private:
    const Ring& mCoefficientRing;
    std::shared_ptr<FreeMonoid> mMonoid;
    // ...
};
```

A `FreeAlgebra` carries:

- A reference to its **coefficient ring** (any engine `Ring`, but typically
  a field).
- A shared pointer to its [`FreeMonoid`](file-FreeMonoid.md) (the word
  side).

Note: unlike [`PolyRing`](../file-polyring.md), `FreeAlgebra` does **not**
inherit from `Ring`. It is wrapped by [`M2FreeAlgebra`](../file-polyring.md)
(declared in `M2FreeAlgebra.hpp` at the top of `e/`) which provides the
`Ring`-shaped façade the rest of the engine expects.

## Polynomials

A `Poly` is a vector of `(coefficient, word)` pairs sorted by word order.
The relevant type is declared in [`Polynomial.hpp`](../polynomial-rings.md)
at the engine top level — it is shared with newer commutative GB code.

## Multiplication

For two polynomials `p = Σ c_i m_i` and `q = Σ d_j n_j`:

```
p · q = Σ_{i,j} (c_i · d_j) · (m_i ⌣ n_j)
```

where `⌣` is word concatenation. No reordering — non-commutative.

The implementation accumulates products into a `SumCollector` (declared in
`ring.hpp`) for efficient term combining.

## Use sites

- [`M2FreeAlgebra` / `M2FreeAlgebraQuotient`](../file-polyring.md) — M2-side
  wrappers.
- [`NCGroebner`](file-NCGroebner.md), [`NCF4`](file-NCF4.md) — GB algorithms
  over the free algebra.
- [`FreeAlgebraQuotient.{cpp,hpp}`](README.md) — quotient by a two-sided
  ideal.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-FreeMonoid.md`](file-FreeMonoid.md) — word side.
- [`file-NCGroebner.md`](file-NCGroebner.md) — primary GB consumer.
- [`../file-polyring.md`](../file-polyring.md) — commutative analogue.
- `Polynomial.hpp` at the engine top level — shared polynomial value type.
