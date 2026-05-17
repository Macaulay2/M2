# `M2/Macaulay2/e/interface/` — public C interface of the engine

This is the modern, organised home for **engine entry points** — the C-callable
functions the [`d/` interpreter](../../d/README.md) reaches across the
engine/front-end boundary to invoke.

It is the **public surface** of the engine. Internal C++ types are *not*
exposed here; everything is reduced to plain C, opaque pointers, and primitive
types so it can be called from `.dd`/`.d` after translation by
[`scc1`](../../c/README.md).

## Design rules (from [`../README.md`](../README.md))

- Each `<area>.h` / `<area>.cpp` pair is self-contained: minimal includes,
  **never** depends on `engine.h`.
- Internal headers go in subdirectories of `e/` (e.g. `e/matrix/`), not here.
- Every interface routine should have a matching gtest in
  [`../unit-tests/`](../unit-tests/README.md).
- The corresponding interpreter binding goes in [`d/<area>.dd`](../../d/README.md);
  the M2-level wrapper goes in [`m2/<area>.m2`](../../m2/README.md).

This directory replaces the older flat `x-*.cpp` layout that still exists at
the top of `e/`. New code goes here; old code is being migrated.

## Areas

| Area | Files | Exposes |
|---|---|---|
| Rings (abstract) | `aring.{h,cpp}` | The `aring` polymorphic interface |
| Concrete coefficients | `flint.{h,cpp}` | FLINT-backed rings (ZZ, QQ, ZZ/p) |
| Rings (legacy) | `ring.{h,cpp}` | Older ring API still in use |
| Ring elements | `ringelement.{h,cpp}` | Ring element operations |
| Ring maps | `ringmap.{h,cpp}` | Homomorphisms between rings |
| Monoids | `monoid.{h,cpp}`, `monomial-ordering.{h,cpp}` | Monoid + monomial-order construction |
| Matrices | `matrix.{h,cpp}`, `mutable-matrix.{h,cpp}` | Matrix and mutable-matrix ops |
| Free modules | `freemodule.{h,cpp}` | Free module construction / arithmetic |
| Monomial ideals | `monomial-ideal.{h,cpp}` | Monomial ideal ops |
| Groebner | `groebner.{h,cpp}` | Gröbner basis entry points |
| Computations | `computation.h` | Generic computation framework |
| Cones / polyhedra | `cone.{h,cpp}` | Cone operations |
| Factoring | `factory.{h,cpp}` | Bridge to the Factory library |
| CRT | `cra.{h,cpp}` | Chinese remainder algorithm |
| Numerics | `random.{h,cpp}`, `polyroots.cpp` | RNG, root finding |
| Memory | `m2-mem.{h,cpp}` | Allocation hooks exposed to interpreter |
| Engine types | `m2-types.{h,cpp}` | The opaque type tags interpreter sees |
| Utilities | `gmp-util.h` | GMP helpers |
| Numerical AG | `NAG.h` | Numerical algebraic geometry entry points |

## How to add a new engine function

1. Implement it in C++ in [`../`](../README.md) (internal headers in subdirs
   like `e/matrix/`).
2. Add a `.h` / `.cpp` pair here. Keep includes minimal; do **not** include
   `engine.h`.
3. Add the interpreter binding in [`d/<area>.dd`](../../d/README.md).
4. Add the M2-level wrapper in [`m2/<area>.m2`](../../m2/README.md).
5. Add a gtest in [`../unit-tests/<area>.cpp`](../unit-tests/README.md).

## Related

- [`../README.md`](../README.md) — engine overview.
- [`../../d/engine.dd`](../../d/README.md) — the interpreter-side bridge.
- [`../../../../README.md#engine-deep-dive-m2macaulay2e`](../../../../README.md#engine-deep-dive-m2macaulay2e) — engine deep-dive in the top-level TOC.

[← back to engine overview](../README.md)
