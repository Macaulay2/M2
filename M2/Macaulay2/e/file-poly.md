# `poly.{cpp,hpp}` — `PolyRing` (the standard polynomial ring class)

`poly.cpp` defines **`PolyRing`** — the engine's standard polynomial
ring class. It subclasses `PolyRingFlat` (a flatness intermediate;
"flat" means coefficients live in a non-polynomial base ring) and is
the immediate parent of the various flavoured polynomial rings
([`PolyRingSkew`](file-skewpoly.md), [`WeylAlgebra`](file-weylalg.md),
etc.).

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "ring.hpp"
#include "ringelem.hpp"
#include "skew.hpp"

class TermIdeal;
class Matrix;
class GBRing;
class GBRingSkew;
class GBComputation;
class ChineseRemainder;

#include "polyring.hpp"

class PolyRing : public PolyRingFlat {
    // generators, monoid, coefficient ring, GB-ring view, ...
};
```

`PolyRing` is what most engine code actually instantiates when it
talks about "a polynomial ring." The chain of bases is:

```
Ring
 └── PolyRing (abstract; from file-polyring.md)
      └── PolyRingFlat (flat = coefficients are non-polynomial)
           └── PolyRing (this file; the standard commutative case)
                ├── PolyRingSkew (skewpoly.cpp)
                ├── PolyRingWeyl (weylalg.cpp)
                ├── PolyRingNC  (M2FreeAlgebra)
                └── PolyQuotient (polyquotient.cpp)
```

Compare with [`file-polyring.md`](file-polyring.md) which documents the
abstract base. This file is the concrete commutative `PolyRing`.

## Forward declarations

Six friend classes:

- **`TermIdeal`** — a monomial ideal augmented with term-by-term ring
  context.
- **`Matrix`** — for matrix-shaped operations on `PolyRing` elements.
- **`GBRing`, `GBRingSkew`** — the GB-tuned view
  ([`file-gbring.md`](file-gbring.md)).
- **`GBComputation`** — drives GB computations on `PolyRing` elements.
- **`ChineseRemainder`** — for modular reconstruction of `QQ`-valued
  computations.

The friend declarations let these classes touch `PolyRing`'s private
state without going through virtuals — important for the inner-loop
performance of GB algorithms.

## What's defined in `poly.cpp`

The `.cpp` implements:

- Multiplication and tail multiplication of polynomial values.
- Ring-element construction from `(coefficient, monomial)` pairs.
- Coercion between flavours (e.g. recover a commutative
  multiplication from a `PolyRingSkew` instance).
- Pretty-printing (via `text_out(buffer&)`).

## Used by

Essentially every engine path that touches polynomials over a flat
base ring. The dispatcher in [`file-aring.md`](file-aring.md) and the
constructors in [`interface/file-ring-interface.md`](interface/file-ring-interface.md)
return `PolyRing*` for the standard commutative case.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-polyring.md`](file-polyring.md) — abstract base.
- [`file-skewpoly.md`](file-skewpoly.md), [`file-weylalg.md`](file-weylalg.md),
  [`file-solvable.md`](file-solvable.md) — flavoured siblings.
- [`file-polyquotient.md`](file-polyquotient.md) — quotient subclass.
- [`file-gbring.md`](file-gbring.md) — GB-tuned view.
