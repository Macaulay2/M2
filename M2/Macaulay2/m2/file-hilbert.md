# `hilbert.m2` — M2-side Hilbert function / series / polynomial

`hilbert.m2` is the **M2-side wrapper** for the engine's
[Hilbert-function machinery](../e/file-hilb.md). It exposes
`hilbertFunction`, `hilbertSeries`, `hilbertPolynomial`, plus utility
helpers like `reciprocal` for inverting Hilbert series.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
needs "max.m2"        -- infinity
needs "modules2.m2"

protect symbol Order

recipN := (n, wts, f) -> (
    -- n is a positive integer
    -- wts is a weight vector
    -- f is a polynomial of the form 1 plus terms of positive weight, which we verify
```

The `protect symbol Order` line freezes the `Order` symbol against
accidental redefinition — `Order` is the option name used to set
truncation order in `hilbertSeries`, and freezing it prevents user
code from shadowing the option.

## User-facing functions

- **`hilbertFunction(d, M)`** — dimension of `M_d` as a `k`-vector
  space (where `k` is the coefficient ring).
- **`hilbertFunction(D, M)`** — for a multi-degree `D` and a
  multigraded module.
- **`hilbertSeries M`** — generating function `Σ_d (dim M_d) t^d`,
  returned as a rational function.
- **`hilbertSeries(M, Order => N)`** — truncated expansion to order `N`.
- **`hilbertPolynomial M`** — the eventually-polynomial of
  `hilbertFunction(d, M)` for large `d`.

The implementation calls into the engine's
[`hilb.cpp`](../e/file-hilb.md) for the heavy work and post-processes
the output.

## `recipN`

The local helper `recipN(n, wts, f)`:

- Takes a polynomial `f = 1 + (higher-order stuff in weight-graded
  order)`.
- Returns the first `n` terms of `1/f` (a formal power-series
  inverse).
- Used to expand `hilbertSeries M` as a series rather than as a
  rational function.

The `wts` weight vector lets the routine work over weighted gradings.

## Used by

- Every M2 user calling `hilbertFunction`, `hilbertSeries`, or
  `hilbertPolynomial`.
- Algebraic-geometry packages that compute degrees, dimensions,
  Hilbert polynomials of varieties.
- [`file-betti.md`](file-betti.md) — Betti tables are sometimes
  derived from Hilbert series.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-hilb.md`](../e/file-hilb.md) — engine implementation.
- [`file-betti.md`](file-betti.md) — Betti tables.
- [`../e/interface/file-groebner-interface.md`](../e/interface/file-groebner-interface.md)
  — engine boundary (Hilbert is bundled with GB / resolution).
