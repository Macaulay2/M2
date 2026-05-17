# `rationals.m2` — `QQ` operations

`rationals.m2` defines the M2-side operations on the rational ring
**`QQ`**: arithmetic, comparison, denominator/numerator extraction,
display formatting.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "expressions.m2"
needs "integers.m2"

QQ.synonym = "rational number"
```

`QQ` is treated as a `Number` subclass with its own methods:

- **Arithmetic** — `+`, `−`, `*`, `/`, `==` between QQ values and
  between QQ and ZZ (with appropriate promotion).
- **Components** — `numerator q`, `denominator q`. Returned as `ZZ`
  values.
- **Display** — `QQ` values print as `p/q` (simplified). The
  display logic uses `Expression` ([`file-expressions.md`](file-expressions.md))
  for TeX / HTML rendering.

`QQ` builds on [`file-integers.md`](file-integers.md) — its
arithmetic is component-wise on numerator and denominator (with GCD
simplification at the end), and the components themselves are
integers.

## Engine backing

Like `ZZ`, `QQ` is engine-backed. Operations on QQ values go through
the engine via [`../e/file-aring-qq.md`](../e/file-aring-qq.md)'s
dispatcher; today that means FLINT (`fmpq_t`) or GMP (`mpq_t`)
depending on the build.

## Field properties

The file declares:

- **`isField QQ`** — returns `true`. `QQ` is the only built-in field
  with a single-symbol name.
- **`coefficientRing QQ`** — `QQ` is a field, so its coefficient
  ring is itself.
- **`characteristic QQ`** — returns `0`.

These properties cascade into other M2 logic — `isField` controls
whether quotient rings are constructed, whether characteristic-zero
algorithms are usable, etc.

## Used by

- Every M2 expression containing a rational number.
- Polynomial rings over `QQ` — by far the most common ground field.
- Every algorithm that needs to choose between characteristic-0 and
  positive-characteristic strategies.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-integers.md`](file-integers.md) — `ZZ` building block.
- [`file-reals.md`](file-reals.md) — `RR`/`CC` siblings.
- [`../e/file-aring-qq.md`](../e/file-aring-qq.md) — engine QQ.
- [`../e/file-aring-qq-flint.md`](../e/file-aring-qq-flint.md),
  [`../e/file-aring-qq-gmp.md`](../e/file-aring-qq-gmp.md) — engine
  implementations.
