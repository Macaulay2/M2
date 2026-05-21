# `polyroots.cpp` — univariate polynomial root finder (FLINT-backed)

`polyroots.cpp` implements the engine's **univariate polynomial root
finder** for `RR` and `CC` polynomials. It is the implementation
behind `interface/factory.h`'s `rawRoots(...)` entry point — the
M2-level `roots(f)` and `roots(f, Precision => …)` functions.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## What it computes

Given a univariate polynomial `f ∈ R[x]` where `R` is `RR_n` or `CC_n`:

- For `CC`: find all `deg(f)` complex roots, counted with
  multiplicity.
- For `RR`: find all real roots; complex roots come back as conjugate
  pairs (if requested).

The output is a `Matrix` whose columns are root vectors.

## Algorithm

For low-degree polynomials the engine uses **closed-form formulas**
(degrees 1, 2, 3, 4). For higher degrees it dispatches to either:

- **MPSolve** — when the user requests arbitrary precision. MPSolve is
  a vendored library specialised for high-precision complex roots.
- **FLINT's `arb` complex roots** — when standard MPFR precision is
  enough.
- **LAPACK eigenvalue routine** for the companion matrix — when
  hardware precision suffices and the polynomial is small.

The dispatcher in `polyroots.cpp` picks based on:

- The coefficient ring's precision.
- Polynomial degree.
- User options.

## Why `rawRoots` is in `interface/factory.h`

Historically root finding was bundled with factorisation under the
Factory library. The bundling persists in the interface header
naming, even though MPSolve and FLINT now do the heavy lifting. See
[`interface/file-factory-interface.md`](interface/file-factory-interface.md)
for the public side.

## Used by

- M2-level `roots(f)` and `polyRoots`.
- NAG path tracking ([`file-NAG.md`](file-NAG.md)) — when given a
  univariate system, it dispatches to the root finder rather than
  running full continuation.
- Eigenvalue computation through the
  [`file-eigen.md`](file-eigen.md) characteristic-polynomial path.

## Related

- [`computations.md`](computations.md) — area overview.
- [`interface/file-factory-interface.md`](interface/file-factory-interface.md)
  — public C entry point.
- [`file-aring-CCC.md`](file-aring-CCC.md), [`file-aring-CC.md`](file-aring-CC.md)
  — complex element types.
- MPSolve — vendored under [`../../libraries/mpsolve/`](../../libraries/README.md).
- FLINT submodule under [`../../submodules/README.md`](../../submodules/README.md).
