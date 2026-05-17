# `intervals.m2` — `interval` constructor

`intervals.m2` defines the **`interval`** constructor — the M2-level
entry point for building values in `RRi` (real interval) and `CCi`
(complex interval).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "methods.m2"
needs "shared.m2"
needs "reals.m2"

interval = method(Options => {Precision => -1})

interval Number := opts -> N -> (
    p := if opts.Precision < 0 then defaultPrecision else opts.Precision;
    interval(numeric(p, N), opts))
interval RR := opts -> N -> ...
```

`interval` accepts:

- A `Number` (any numeric type).
- An `RR` (real number).
- A `(lo, hi)` pair for an explicit interval.

With `Precision => p`, the result has `p` mantissa bits per
endpoint. Default precision is engine-configurable.

## Why intervals matter

Interval arithmetic gives certified bounds: every interval operation
returns an enclosure of the true result. This lets numerical
computations be checked against rigorous error bounds.

- `RRi` — real intervals via MPFI
  ([`../e/file-aring-RRi.md`](../e/file-aring-RRi.md)).
- `CCi` — complex intervals (rectangle representation)
  ([`../e/file-aring-CCi.md`](../e/file-aring-CCi.md)).

## Used by

- Numerical packages requiring certified bounds.
- Root-isolation algorithms.
- The `NumericalAlgebraicGeometry` package's certification routines.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-reals.md`](file-reals.md) — `RR` / `CC` / `RRi` / `CCi`
  types.
- [`../e/file-aring-RRi.md`](../e/file-aring-RRi.md),
  [`../e/file-aring-CCi.md`](../e/file-aring-CCi.md) — engine
  implementations.
- MPFI library — external dependency.
