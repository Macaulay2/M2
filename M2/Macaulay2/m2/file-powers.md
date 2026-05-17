# `powers.m2` — `binomial` and power-related operations

`powers.m2` defines **`binomial`** (the binomial coefficient) and
other power / exponent operations.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "methods.m2"
needs "remember.m2"

binomial(ZZ, ZZ) := ZZ => binomial0

binomial(Number, Number) := ZZ => memoize (
    (n, i) -> (
        if instance(numeric n, CC) or instance(numeric i, CC)
        ...
    )
)
```

The `binomial0` entry point (defined in the engine) handles the
integer case. The `Number, Number` case is a memoised M2-side
function dispatching to fractional / complex arguments via the
`numeric` coercion.

## What's implemented

- **`binomial(n, k)`** — `n choose k`. Works for `ZZ`, `QQ`, `RR`,
  `CC` inputs.
- **`product L`**, **`product(n, f)`** — products / repeated
  multiplication.
- **`power(R, n)`** — `n`-fold tensor of a ring.

## Memoisation

The `memoize` wrapper on `binomial(Number, Number)` is essential —
recursive evaluation is exponential without it. With memoisation,
repeated calls to the same `(n, k)` pair return instantly.

## Engine partnership

For pure-integer `binomial`, the engine has direct FLINT support;
the M2-side `binomial(ZZ, ZZ) := binomial0` reflects this. For
non-integer arguments, the M2-side function does the work.

## Used by

- [`file-combinatorics.md`](file-combinatorics.md) — uses
  `binomial` heavily.
- Hilbert-polynomial computations.
- Schubert calculus.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-combinatorics.md`](file-combinatorics.md) — primary user.
- [`file-remember.md`](file-remember.md) — `memoize` machinery.
- [`file-integers.md`](file-integers.md) — integer arithmetic.
