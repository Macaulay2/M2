# `polyquotient.{cpp,hpp}` — `PolyQuotient` (polynomial ring modulo an ideal)

`polyquotient.cpp` defines **`PolyQuotient`** — the concrete `Ring`
subclass for `R/I` where `R` is a [`PolyRing`](file-poly.md) (or a
flavoured variant) and `I` is a polynomial ideal with a known
Gröbner basis. It uses [`QRingInfo`](file-qring.md) for the bookkeeping
state.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Header includes

```cpp
#include "engine-includes.hpp"

#include "poly.hpp"
#include "polyring.hpp"
#include "qring.hpp"
#include "ringelem.hpp"

class FreeModule;
class GBComputation;
class Ring;
class buffer;
class gbvector;
struct RingMap;
```

`PolyQuotient` inherits from a flavour of [`PolyRing`](file-poly.md)
(the ambient ring), augmented with a stored `QRingInfo`
([`file-qring.md`](file-qring.md)) holding both:

- The defining ideal's generators as `Nterm*` (used by ring-element
  reduction).
- The same generators as `gbvector*` (used by GB-internal reduction).

Multiplication and inversion within the quotient both reduce against
the stored GB.

## Class shape (paraphrased)

```cpp
class PolyQuotient : public PolyRing {
    QRingInfo *mQRingInfo;
    // ...
};
```

The class doesn't add much beyond `QRingInfo` — almost every operation
inherited from `PolyRing` is augmented by a final reduction step.
That reduction step lives in `PolyQuotient::multiply` and friends.

## Construction

Built from M2 via `R = QQ[x, y, z]; I = ideal(x*y - z); Q = R/I`. The
factory:

1. Validates that `I` is a non-trivial ideal (else returns the
   ambient `R`).
2. Computes a Gröbner basis of `I` w.r.t. `R`'s monomial order.
3. Constructs a fresh `QRingInfo` with both forms of the GB.
4. Returns a `PolyQuotient*` pointing at the original `R` plus the
   new `QRingInfo`.

The resulting ring is **graded if and only if** the defining ideal is
homogeneous. Non-graded quotients are supported but lose the Hilbert-
series shortcut.

## Why a separate class from `PolyRing`

`PolyQuotient` could in principle be a flag on `PolyRing`, but:

- It has substantially different `is_unit`, `invert`, and `gcd` logic.
- The `QRingInfo` data is large; embedding it in every `PolyRing`
  would bloat ordinary polynomial rings.
- Several engine paths short-circuit on `dynamic_cast<PolyQuotient*>`.

Keeping it as a subclass keeps the hot non-quotient path fast.

## Limitations

- The ambient ring must be a `PolyRingFlat` (no nested polynomial
  rings). Quotients are flattened first.
- The defining ideal must have a finite GB up to the chosen degree
  limit.
- Some operations require homogeneity; the engine errors clearly when
  non-homogeneous inputs hit those paths.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-qring.md`](file-qring.md) — the `QRingInfo` bookkeeping.
- [`file-poly.md`](file-poly.md), [`file-polyring.md`](file-polyring.md)
  — base classes.
- [`file-comp-gb.md`](file-comp-gb.md) — produces the input GB.
- [`file-localring.md`](file-localring.md) — sibling construction.
