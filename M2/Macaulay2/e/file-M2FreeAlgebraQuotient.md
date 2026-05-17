# `M2FreeAlgebraQuotient.{cpp,hpp}` — M2-facing `Ring` wrapper for NC quotients

`M2FreeAlgebraQuotient` is the **`Ring`-shaped façade** around a
non-commutative quotient algebra. It packages a
[`NCAlgebras/FreeAlgebraQuotient`](NCAlgebras/file-FreeAlgebraQuotient.md)
in a `Ring` subclass so the rest of the engine can treat the quotient
like any other ring. It is the quotient counterpart of
[`file-M2FreeAlgebra.md`](file-M2FreeAlgebra.md).

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include "engine-includes.hpp"
#include <memory>

#include "M2FreeAlgebra.hpp"
#include "NCAlgebras/FreeAlgebraQuotient.hpp"
#include "NCAlgebras/FreeMonoid.hpp"
#include "Polynomial.hpp"
#include "ringelem.hpp"

class FreeAlgebra;
class Matrix;
// ...
```

The wrapper inherits the engine boundary (`engine-includes.hpp`) and
the non-commutative ring implementations from
[`NCAlgebras/`](NCAlgebras/README.md), plus the shared
[`Polynomial`](file-Polynomial.md) value type.

## Class structure (paraphrased)

```cpp
class M2FreeAlgebraQuotient : public M2FreeAlgebra {
    std::unique_ptr<FreeAlgebraQuotient> mQuotient;
    // overrides: mult, normal_form, is_unit, ...
};
```

Two design choices to notice:

1. It **inherits from [`M2FreeAlgebra`](file-M2FreeAlgebra.md)**, not
   from `Ring` directly. The ambient free algebra is already a
   `Ring`; the quotient layer adds reduction-modulo-the-defining-ideal
   on top of every operation.
2. The quotient state (the GB of the two-sided defining ideal) lives in
   the wrapped `FreeAlgebraQuotient`.

## Multiplication in the quotient

When two `RingElement`s in an `M2FreeAlgebraQuotient` are multiplied:

1. Multiply as elements of the ambient `M2FreeAlgebra` (word
   concatenation lifted to polynomials).
2. Reduce the result modulo the stored GB via the wrapped
   `FreeAlgebraQuotient`'s `NCGroebner`
   ([`NCAlgebras/file-NCGroebner.md`](NCAlgebras/file-NCGroebner.md)).

The reduction step is what distinguishes quotient operations from
ambient free-algebra ones — and what makes the quotient finite over
many fields where the ambient algebra is infinite-dimensional.

## Construction

From M2:

```m2
R = freeAlgebra(QQ, {x, y, z});
I = ideal(x*y - y*x);     -- a two-sided ideal
Q = R / I;
```

The slash invokes `M2FreeAlgebraQuotient`'s factory, which computes a
non-commutative Gröbner basis up to a chosen degree and packages the
result as a `FreeAlgebraQuotient` + the `M2FreeAlgebra` wrapper.

## Caveats

Non-commutative GBs of two-sided ideals are not always finite. The
quotient is **partial up to the chosen degree limit**: operations on
words longer than that limit may not fully simplify. The interpreter
surfaces this via the `Strategy => …` option.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-M2FreeAlgebra.md`](file-M2FreeAlgebra.md) — ambient wrapper.
- [`NCAlgebras/file-FreeAlgebraQuotient.md`](NCAlgebras/file-FreeAlgebraQuotient.md)
  — wrapped implementation.
- [`file-qring.md`](file-qring.md) — commutative analogue.
- [`NCAlgebras/file-NCGroebner.md`](NCAlgebras/file-NCGroebner.md) —
  reduction engine used during quotient multiplication.
