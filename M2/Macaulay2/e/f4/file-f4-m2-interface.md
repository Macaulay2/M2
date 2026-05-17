# `f4-m2-interface.{cpp,hpp}` — `F4toM2Interface`

`F4toM2Interface` is a collection of **static translation routines**
between the engine's M2-side polynomial representation (`vec`,
`PolynomialRing`-based) and the F4 internal representation
(`GBF4Polynomial`, `MonomialInfo`-encoded, `VectorArithmetic`-driven).

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
class F4toM2Interface {
public:
    static void poly_set_degrees(const VectorArithmetic *VA,
                                 const MonomialInfo *MI,
                                 M2_arrayint wts,
                                 const GBF4Polynomial &f,
                                 int &deg, int &alpha);

    static void from_M2_vec(const VectorArithmetic *VA,
                            const MonomialInfo *MI,
                            ...);
    // and the inverse `to_M2_vec(...)`, plus matrix variants
};
```

All methods are static — `F4toM2Interface` is essentially a namespace.

## What it translates

- **Input matrix → F4 polynomial array** — convert each column of an engine
  `Matrix` into a `GBF4Polynomial` for the F4 inner loop.
- **F4 polynomial array → output matrix** — once F4 finishes, pack the basis
  back into a `Matrix` for the interpreter to consume.
- **Degree extraction** — compute polynomial degrees in the engine's
  weight system (for sugar / `DegreeLimit =>` accounting).

## Why a static helper

The translation needs two pieces of context that change per coefficient
ring:

- A `VectorArithmetic*` — encapsulates the arithmetic operations
  (`add`, `mult`, etc.) over whichever back-end ring is in use.
- A `MonomialInfo*` — the F4-specific encoded monomial layout for the
  current monoid.

Rather than passing both around as object state, `F4toM2Interface` takes
them as parameters to each static method. This makes the interface trivial
to call from any setup code.

## Used by

[`file-f4-computation.md`](file-f4-computation.md) — every entry point to
F4 from the engine goes through these helpers on the way in *and* on the
way out.

## Related

- [`README.md`](README.md) — F4 overview.
- [`file-f4-computation.md`](file-f4-computation.md) — primary caller.
- [`../matrices.md`](../matrices.md) — `Matrix` / `vec` types.
- `VectorArithmetic.hpp` at the engine top level — back-end arithmetic.
