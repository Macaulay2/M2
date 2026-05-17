# `integrate.m2` — numerical integration (Simpson's rule)

`integrate.m2` provides M2's **numerical-integration** primitive:
Simpson's rule for definite integrals of an M2 function over a
bounded interval.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
--		Copyright 1993-1999 by Daniel R. Grayson

simpson := (f, a, b, k) -> (
    count := 0;
    oldf := f;
    f = x -> (
        count = count + 1;
        oldf x);
    h := (b - a) / (2 * k);
    ...
)
```

A classic Simpson's-rule implementation with a counter to track how
many function evaluations were performed.

## User-facing API

- **`integrate(f, a, b)`** — `∫_a^b f(x) dx` via Simpson's rule.
- **`integrate(f, a, b, MaxIntervals => N)`** — cap the subdivision.

Internally `integrate` adaptively refines the partition until the
Simpson estimate converges (or hits the max-interval cap).

## Simpson's rule

For a partition `a = x_0 < x_1 < … < x_{2k} = b` with uniform
spacing `h = (b - a) / (2k)`:

```
∫ ≈ (h/3) · (f(x_0) + 4·f(x_1) + 2·f(x_2) + 4·f(x_3) + … + f(x_{2k}))
```

The error is `O(h^4)` — much better than the trapezoidal rule for
smooth integrands. For non-smooth integrands convergence may be
slow.

## Why this is in Core

`integrate` is needed by enough downstream code (numerical packages
mainly) to justify being in Core. It is too small to deserve its own
package.

## Limitations

The implementation is **not** adaptive in the Romberg or
Gauss-Legendre sense — it uses uniform Simpson with refinement
doubling. Sophisticated users wanting Romberg or Clenshaw-Curtis
would write their own (the function is small enough that it's
easier to copy than to extend the engine).

## Used by

- Numerical packages that need quick integration.
- Tutorial examples illustrating M2's numeric capabilities.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-reals.md`](file-reals.md) — `RR` type used for results.
- `NumericalAlgebraicGeometry` package — has its own more
  specialised integration paths.
