# `f4-computation.{cpp,hpp}` — `F4Computation`

`F4Computation` is the top-level glue that adapts the internal F4 engine
(class `F4GB` in [`f4.{cpp,hpp}`](README.md)) to the engine's general
[`GBComputation`](../file-comp-gb.md) interface. The interpreter sees only
this class; the actual F4 algorithm runs underneath.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class F4Computation : public GBComputation {
    const PolynomialRing *mOriginalRing;
    F4GB                 *mF4;           // the actual algorithm
    MonomialInfo         *mMonomialInfo; // F4-specific monomial layout
    VectorArithmetic     *mVectorArith;  // coefficient-ring-specific arithmetic
    // ...
};
```

Two pieces of translation happen here:

1. **Polynomials in / out** — the user supplies polynomials as engine `vec`
   objects with [`PolynomialRing`](../file-polyring.md) coefficients; the
   F4 inner loop uses `GBF4Polynomial` over a `MonomialInfo` layout with a
   templated `VectorArithmetic`.
2. **Template instantiations** — F4's inner loop is parameterised on the
   coefficient ring (FFLAS-FFPACK Z/p, FLINT Z/p, generic, …). This class
   picks the right instantiation at construction time and pins it.

## Public surface

The same as the base [`GBComputation`](../file-comp-gb.md):

- `start_computation()` runs F4 until a stop condition triggers.
- `get_gb()`, `get_mingens()`, `get_change()`, `get_syzygies()`,
  `get_initial(nparts)`, `complete_thru_degree()`.

Internally each of these reads results out of the running `F4GB` and
re-wraps them as engine `Matrix` objects.

## Translation entry points

The actual translation is in
[`f4-m2-interface.{cpp,hpp}`](file-f4-m2-interface.md). `F4Computation`
delegates to that helper.

## Related

- [`README.md`](README.md) — F4 overview.
- [`file-f4-spairs.md`](file-f4-spairs.md), [`file-f4-m2-interface.md`](file-f4-m2-interface.md), [`file-monhashtable.md`](file-monhashtable.md) — internals.
- [`../file-comp-gb.md`](../file-comp-gb.md) — `GBComputation` base.
- [`../gb-f4/README.md`](../gb-f4/README.md) — newer F4 implementation.
