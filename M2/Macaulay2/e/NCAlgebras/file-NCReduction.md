# `NCReduction.{cpp,hpp}` — `PolynomialHeap` (NC polynomial reduction)

`NCReduction` provides the **`PolynomialHeap`** abstraction used during
non-commutative Gröbner basis reduction. It is the NC analogue of
`gbvectorHeap` in [`../file-gbring.md`](../file-gbring.md) — a heap that
lets the reduction step accumulate many polynomial contributions in
`O(n log k)` time.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## Abstract interface

```cpp
class PolynomialHeap : public our_new_delete {
public:
    virtual ~PolynomialHeap() {}

    virtual PolynomialHeap &addPolynomial(const Poly &poly) = 0;
    // additional pure-virtual operations:
    //   - subtract a multiple of a polynomial,
    //   - extract the current leading term,
    //   - test for zero,
    //   - render to a Poly.
};
```

`PolynomialHeap` is pure-virtual; concrete implementations live in
`NCReduction.cpp`. The factory used by [`NCGroebner`](file-NCGroebner.md)
constructs the heap matched to the algorithm's monomial layout.

## What a polynomial heap does

Reduction in a Buchberger-style GB looks like:

```text
res = s_polynomial
while res != 0 and there's a basis element f with lt(f) | lt(res):
    multiplier = lt(res) / lt(f)
    res -= multiplier * f
```

A naïve representation walks two polynomials and merges term-by-term per
iteration. The heap pattern instead **schedules** all the subtractions
together: each pending subtraction becomes a heap entry. The leading term
of `res` at any time is the min of the heap; pulling it amounts to a
`pop` (`O(log k)` where `k` is the number of active heap entries).

Across a full reduction the total work drops from `O(n²)` to `O(n log k)`.

## Implementation choices

The header mentions multiple concrete `PolynomialHeap` implementations
under development. The choice impacts:

- Whether terms are stored by encoded word vs. raw word.
- Whether heap entries carry a precomputed multiplier or recompute it.
- Whether the heap uses a binary heap vs. a tournament tree vs. a
  pairing heap.

[`NCGroebner`](file-NCGroebner.md) selects one at construction; the
others remain in the codebase for benchmarking.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-NCGroebner.md`](file-NCGroebner.md) — primary consumer.
- [`file-FreeAlgebra.md`](file-FreeAlgebra.md), `Polynomial.hpp` —
  underlying polynomial type.
- [`../file-gbring.md`](../file-gbring.md) — commutative analogue
  (`gbvectorHeap`).
