# `ballarith.d` — FLINT/Arb ball arithmetic

`ballarith.d` provides M2's interpreter bindings to **Arb** —
arbitrary-precision interval arithmetic with rigorously certified
error bounds. (Arb merged into FLINT in 2023; the file's header
notes "formerly Arb.")

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
------------------------------------------
-- FLINT ball arithmetic (formerly Arb) --
------------------------------------------

use gmp;

declarations "
    #ifdef HAVE_ARB_H
```

Builds on [`file-gmp.md`](file-gmp.md). The `#ifdef HAVE_ARB_H`
preamble means **the file's bindings are only enabled when Arb is
available** — M2 builds without Arb get only the stubs.

## What's exposed

"Ball arithmetic" represents a real number as a centre value plus a
non-negative radius:

```
x = center ± radius
```

Every arithmetic operation produces a new ball whose centre and
radius are guaranteed to enclose the true result. The radius grows
as operations chain — that's the rigorous error-tracking story.

`ballarith.d` exposes:

- **`RR`** ball constructors at user-controlled precision.
- **`CC`** complex ball constructors (pair of reals plus combined
  radius).
- **Arithmetic** — every standard operation lifted to balls.
- **Special functions** — sin, cos, exp, log, gamma, etc.

## Why use balls over fixed-precision floats

Three reasons:

1. **Certification** — every ball *provably* contains the true
   answer. You can't get this from `double` or even MPFR.
2. **Adaptive precision** — if the answer needs more precision, the
   user just asks for more bits.
3. **Numerical stability** — the radius reflects how unstable the
   computation is. A computation that produces a huge radius is a
   warning sign.

## Used by

- M2-level `RRi` and `CCi` types
  ([`../m2/file-intervals.md`](../m2/file-intervals.md)).
- Engine's [`aring-RRi`](../e/file-aring-RRi.md) and
  [`aring-CCi`](../e/file-aring-CCi.md) implementations
  (when interval values cross the engine boundary).
- Some `NumericalAlgebraicGeometry` paths that need certified bounds.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-gmp.md`](file-gmp.md) — building blocks (Arb is on top of
  GMP).
- [`../e/file-aring-RRi.md`](../e/file-aring-RRi.md), [`../e/file-aring-CCi.md`](../e/file-aring-CCi.md)
  — engine peers.
- [`../m2/file-intervals.md`](../m2/file-intervals.md) — M2-side
  consumer.
- FLINT (which absorbed Arb) — external linked library.
