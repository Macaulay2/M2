# `localring.{cpp,hpp}` — `LocalRing`

`LocalRing` implements the engine's representation of a **localisation** of a
polynomial ring at a prime ideal `P`. Elements are stored as fractions
`(numer, denom)` where `denom` is required to lie outside `P`.

Originally written by Mahrud Sayrafi; that author's contribution is in the
public domain. Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## State

```cpp
struct local_elem {
    ring_elem numer;
    ring_elem denom;
};

class LocalRing : public Ring {
    const PolyRing      *mRing;     // R, the underlying polynomial ring
    GBComputation       *mPrime;    // GB of the prime ideal P
    // ...
};
```

Notice the parallel with [`file-frac.md`](file-frac.md): the value type is
the same `(numer, denom)` pair. The difference is the **denominator
constraint** — for `LocalRing` we require `denom ∉ P`; for `FractionField`
we require `denom ≠ 0`.

The constraint is enforced by checking each new denominator against the
**stored Gröbner basis of `P`** (`mPrime`). If a denominator's normal form
modulo `P` is zero, the operation fails with an engine error.

## Why a separate type from `FractionField`

When `P = (0)` is the zero ideal, `LocalRing` *is* the fraction field —
there is no constraint to check. For non-zero `P`, however, the constraint
fundamentally changes:

- `LocalRing` operations need access to a GB of `P`.
- The unit group is different — `LocalRing` has nilpotent elements modulo
  `P` once non-trivial division-by-non-unit fails.
- Standard simplification of `(numer, denom)` is restricted: we can only
  cancel common factors that lie in `R`, not in `R_P`.

These differences justify a distinct class.

## Operations

- `add`, `mult`, `negate` — standard fraction arithmetic, with a denominator
  check after each.
- `is_unit(f)` — true iff `numer` and `denom` are both outside `P` modulo
  the GB.
- `mod` (M2's `f % P` operator) — reduce a `local_elem` modulo `P`.

## Use sites

- [`m2/localring.m2`](../m2/README.md) — exposes `localize(R, P)` to users.
- Local cohomology computations.
- Tangent / normal cone computations that operate at a single point of a
  variety.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-frac.md`](file-frac.md) — sibling fraction construction.
- [`file-comp-gb.md`](file-comp-gb.md) — GB of the prime ideal is held here.
- [`reducedgb-field-local.{cpp,hpp}`](groebner-bases.md) — GB algorithm
  specialised for local rings.
