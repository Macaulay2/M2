# `res-f4-m2-interface.{cpp,hpp}` — `ResF4toM2Interface`

`ResF4toM2Interface` is the **translation layer** between the engine's
M2-side polynomial and matrix types (`vec`, `Matrix`,
`PolynomialRing`-shaped) and the F4-resolution internal types
(`ResPolynomial`, `ResPolyRing`-shaped). It mirrors
[`f4/file-f4-m2-interface.md`](../f4/file-f4-m2-interface.md) on the
resolution side.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
#include "ringelem.hpp"
#include <utility>

class FreeModule;
class Matrix;
class MutableMatrix;
class PolynomialRing;
class ResPolyRing;
class Ring;
class SchreyerFrame;
class ResPolynomial;
template <typename ACoeffRing> class DMat;

class ResF4toM2Interface {
public:
    static void from_M2_vec(const ResPolyRing &R,
                            const FreeModule  *F,
                            vec                v,
                            ResPolynomial     &result);

    static vec to_M2_vec(const ResPolyRing    &R,
                         const ResPolynomial  &f,
                         /* and so on */);
    // ... matrix-shaped variants, mutable-matrix variants, etc.
};
```

All methods are static — `ResF4toM2Interface` is essentially a
namespace.

## What it translates

- **`from_M2_vec(R, F, v, result)`** — convert an engine `vec`
  (sparse polynomial-coefficient vector with `(component, coeff)`
  entries over a `PolynomialRing`) into a `ResPolynomial` in the
  resolution-tuned ring `R`.
- **`to_M2_vec(R, f, …)`** — the inverse direction.
- Matrix variants — bulk translation of column collections.
- `MutableMatrix` variants — used when the resolution is asked to
  return a `MutableMatrix` directly (rare).

## Use cycle

`F4ResComputation` ([`file-res-f4-computation.md`](file-res-f4-computation.md))
calls `from_M2_vec` once at startup to translate the input GB matrix
into resolution-internal form. The resolution loop runs entirely on
`ResPolynomial`s. When the user asks for a level's differential,
`F4ResComputation` calls `to_M2_vec` to translate back.

This means the translation cost is paid once at the boundaries and
nowhere inside the loop.

## Templated `DMat`

The forward declaration `template <typename ACoeffRing> class DMat`
hints that some of the static methods convert between `ResPolynomial`
and a `DMat<R>` row. The matrix-shaped translation paths in
`ResF4toM2Interface` handle dense conversion for users who want a
straight matrix output instead of `vec` columns.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-f4-computation.md`](file-res-f4-computation.md) — primary
  caller.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — `ResPolynomial`
  value type.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — uses
  these helpers when extracting per-level matrices.
- [`../f4/file-f4-m2-interface.md`](../f4/file-f4-m2-interface.md) —
  GB-side analogue.
