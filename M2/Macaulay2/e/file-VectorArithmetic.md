# `VectorArithmetic.hpp` — templated arithmetic over `aring` rings

`VectorArithmetic.hpp` is the engine's **abstract arithmetic dispatcher**
used by the F4 and resolution code paths. It hides the choice of
coefficient ring behind a uniform interface so a single algorithm
implementation can run against any
[`aring`-backed ring](coefficient-rings.md).

Part of the [Matrices](matrices.md) area (it backs `DMat` and the
matrix-reduction routines in F4 / Schreyer-resolution).

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## What problem it solves

The F4 inner loop does this billions of times:

```
for each entry (coeff, monomial) in row:
    accumulator[col(monomial)] += coeff * scaling_factor
```

The arithmetic has to dispatch by **coefficient ring**: `Z/p` via FFLAS,
`Z/p` via FLINT, `Q` via FLINT, `GF` via FLINT, `RR` via MPFR, etc. Each
back end uses a different value type and a different native add / multiply.

`VectorArithmetic` provides one templated `add_scaled(...)` method that
the F4 code calls; the implementation specialised on the coefficient ring
inlines the native arithmetic.

## How it dispatches

The header's commented-out includes hint at the dispatch shape:

```cpp
// #include <variant>             // for visit, variant
// #include "ARingElem.hpp"        // for ARingElem
// #include "ZZp.hpp"              // for Z_mod
// #include "aring-glue.hpp"       // for ConcreteRing
// #include "aring.hpp"            // for DummyRing, ring_GFFlintBig, …
```

The actual implementation uses a `std::variant` (or equivalent) keyed by
ring type, with `std::visit` to dispatch. Once dispatched, the call sees
a concrete `ConcreteRing<R>` and arithmetic inlines.

## Used by

- [`f4/`](f4/README.md) — `F4Computation` constructs a `VectorArithmetic*`
  matched to the coefficient ring.
- [`gb-f4/`](gb-f4/README.md) — same pattern.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — F4-style
  reductions over `ResPolyRing`.
- [`NCAlgebras/file-NCF4.md`](NCAlgebras/file-NCF4.md) — non-commutative F4.

## Cross-ring reuse

Because every F4 and resolution path goes through `VectorArithmetic`,
adding support for a new coefficient ring is mostly a matter of:

1. Implementing the new ring as an [`aring`](file-aring.md).
2. Registering it in `VectorArithmetic`'s variant.
3. Exposing the construction through [`interface/aring.h`](interface/README.md).

No changes to the F4 or resolution code itself.

## Related

- [`matrices.md`](matrices.md), [`file-dmat.md`](file-dmat.md) — `DMat<R>`
  also uses templated arithmetic.
- [`coefficient-rings.md`](coefficient-rings.md), [`file-aring.md`](file-aring.md)
  — underlying ring framework.
- [`f4/file-f4-m2-interface.md`](f4/file-f4-m2-interface.md) — passes a
  `VectorArithmetic*` to all F4 entry points.
