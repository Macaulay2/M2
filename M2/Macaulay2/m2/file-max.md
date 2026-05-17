# `max.m2` — `InfiniteNumber`, `infinity`, `max`/`min`

`max.m2` defines the **`InfiniteNumber`** type — Macaulay2's
representation of positive and negative infinity — plus the
fundamental `max` / `min` operations.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "fold.m2"
needs "methods.m2"

InfiniteNumber          = new Type of Number
InfiniteNumber.synonym  = "infinite number"
infinity                = new InfiniteNumber from {1}
neginfinity             = new InfiniteNumber from {-1}
- InfiniteNumber := x -> if x === infinity then neginfinity else infinity
```

Two distinguished `InfiniteNumber` values:

- **`infinity`** — positive infinity.
- **`neginfinity`** — negative infinity (also accessible as
  `-infinity`).

Arithmetic with infinities follows the standard rules:

- `infinity + x = infinity` (for finite `x`).
- `infinity * x = infinity` for `x > 0`, `neginfinity` for `x < 0`.
- `infinity - infinity` is an error (`NaN`-like).

## `max` and `min`

Defined via `fold` on the comparison:

```m2
max{1, 2, 3, infinity}      -- infinity
min{1, 2, 3, -infinity}     -- -infinity
max(L, key => f)            -- via projection
```

The `key =>` option lets `max` / `min` find the argmax / argmin via
a user-supplied function.

## Why a dedicated `InfiniteNumber`

Many M2 operations take a bound:

- `DegreeLimit => infinity` — compute to all degrees.
- `gb(I, BasisElementLimit => infinity)` — no element-count cap.

Without `InfiniteNumber`, these would need ad-hoc sentinel encodings
(`-1`, `null`, …) — error-prone and ugly. The explicit
`InfiniteNumber` type lets the user write `infinity` and have it
work everywhere uniformly.

## Used by

- Every M2 option that accepts a bound.
- `dim`, `degree`, `codim` — return `InfiniteNumber` for degenerate
  cases.
- `min` / `max` everywhere.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-integers.md`](file-integers.md), [`file-rationals.md`](file-rationals.md),
  [`file-reals.md`](file-reals.md) — finite-number types.
- [`file-fold.md`](file-fold.md) — `max`/`min` are folds.
- [`file-option.md`](file-option.md) — options that accept
  `infinity`.
