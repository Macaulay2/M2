# `aring-CC.{cpp,hpp}` — `M2::ARingCC` (complex numbers, `double` precision)

`aring-CC.cpp` implements the engine's **machine-precision complex
numbers** — a pair of `double`s wrapped in a [`SimpleARing`](file-coeffrings.md)
specialisation. It is the complex sibling of
[`file-aring-RR.md`](file-aring-RR.md).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "aring-RR.hpp"

namespace M2 {

class ARingCC : public SimpleARing<ARingCC> {
    // approximate real numbers, implemented as doubles.
public:
    static const RingID ringID = ring_CC;
    // typedef <pair-of-double> elem;
    // arithmetic, conjugation, modulus, RNG
};

}
```

The class includes [`aring-RR.hpp`](file-aring-RR.md) for the
`ARingRR::elem` type and reuses it for both the real and imaginary
parts of a complex value.

The header comment says "approximate real numbers" — slightly
misleading; the class is for complex numbers, and the comment is a
leftover from the file that was used as a template.

## Element type

```cpp
struct CC_struct { double re; double im; };
typedef CC_struct elem;
```

A complex value is a struct of two `double`s. Arithmetic uses the
standard formulas:

- `(a + bi) + (c + di) = (a + c) + (b + d)i`
- `(a + bi) · (c + di) = (ac - bd) + (ad + bc)i`
- `1 / (a + bi) = (a - bi) / (a² + b²)`
- `|a + bi| = √(a² + b²)`

The implementation uses hardware operations throughout — no MPFR, no
allocation.

## Operations specific to CC

Beyond what `ARingRR` provides, `ARingCC` adds:

- **`conjugate(z)`** — `(a + bi) ↦ (a - bi)`.
- **`real(z)`**, **`imag(z)`** — projections.
- **`abs(z)`** — modulus.
- **`is_zero(z)`** — both components zero.

## Used by

- NAG path tracking ([`file-NAG.md`](file-NAG.md)) — most homotopy
  continuation happens in `CC`.
- Numerical linear algebra over `CC`-valued matrices.
- Roots of univariate polynomials via [`file-LLL.md`](file-LLL.md)'s
  cousins.

## Random numbers

```cpp
#include "interface/random.h"  // for randomDouble
```

`randomDouble()` provides uniformly distributed `double` values in
`[0, 1)`. `ARingCC` builds random complex numbers as `(randomDouble(),
randomDouble())`.

## Arbitrary-precision siblings

For higher precision the engine has:

- `ARingCCC` — complex numbers with MPFR precision.
- `ARingCCi` — complex intervals (Arb-based).

The dispatcher in [`file-aring.md`](file-aring.md) picks the right back
end based on the user's M2-level precision option.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-RR.md`](file-aring-RR.md) — real sibling.
- [`file-aring.md`](file-aring.md), [`file-coeffrings.md`](file-coeffrings.md)
  — framework / registry.
- `aring-CCC.{cpp,hpp}`, `aring-CCi.{cpp,hpp}` — higher-precision siblings.
- [`file-NAG.md`](file-NAG.md) — primary consumer.
