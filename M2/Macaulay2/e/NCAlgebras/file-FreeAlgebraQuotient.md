# `FreeAlgebraQuotient.{cpp,hpp}` — `FreeAlgebraQuotient`

`FreeAlgebraQuotient` is a **quotient of a free algebra by a two-sided
ideal** — the non-commutative analogue of
[`PolyQuotient`](../file-qring.md). It holds the ambient
[`FreeAlgebra`](file-FreeAlgebra.md) plus a stored Gröbner basis of the
defining ideal.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class FreeAlgebraQuotient : public our_new_delete {
private:
    const FreeAlgebra &mFreeAlgebra;
    NCGroebner         mGroebner;
    // word tables now placed inside the NCGroebner object
    // WordTable mWordTable;
    // ...
};
```

The quotient owns:

- A reference to its **ambient `FreeAlgebra`** (the free non-commutative
  algebra `k⟨x_1, …, x_n⟩`).
- An [`NCGroebner`](file-NCGroebner.md) — both a representation of the
  defining ideal and the engine that runs reduction modulo it.

The commented-out `WordTable` shows a refactor in progress: word tables
used to live on the quotient and are being consolidated into the
`NCGroebner` instance.

## Multiplication in a quotient

When two elements of `R = FreeAlgebra/I` are multiplied:

1. Multiply in the free algebra (concatenation lifted to polynomials).
2. Reduce modulo the stored GB of `I` via `mGroebner`.

The reduction uses `NCGroebner`'s reduction loop (`NCReduction`'s
`PolynomialHeap`).

## Construction

Built from M2 via `R = freeAlgebra(...); R / ideal(...)`. The constructor
runs a Gröbner basis computation on the defining ideal up to a chosen
degree limit before storing the result.

## M2-side wrapper

The class doesn't inherit from `Ring`. The
[`M2FreeAlgebraQuotient`](../file-polyring.md) wrapper (declared in
`M2FreeAlgebraQuotient.hpp` at the top of `e/`) provides the `Ring`-shaped
façade so the rest of the engine — matrices, modules, resolutions —
treats it as any other ring.

## Termination caveat

The Gröbner basis of a two-sided ideal in a free algebra is not always
finite. `FreeAlgebraQuotient` accepts a degree limit and operates within
that limit. Reductions of inputs whose computation requires words of
higher degree than the GB covers will silently fail to fully simplify.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-FreeAlgebra.md`](file-FreeAlgebra.md) — ambient algebra.
- [`file-NCGroebner.md`](file-NCGroebner.md) — embedded GB.
- [`../file-qring.md`](../file-qring.md) — commutative analogue.
- [`../M2FreeAlgebraQuotient.{cpp,hpp}`](../README.md) — top-level
  `Ring`-shaped wrapper.
