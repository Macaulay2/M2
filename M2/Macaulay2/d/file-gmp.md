# `gmp.d` / `gmp1.d` — GMP integer / rational bindings

`gmp.d` and `gmp1.d` provide the M2 interpreter's **GMP bindings** —
the wrappers that expose GMP's `mpz_t` (integer) and `mpq_t`
(rational) arithmetic to `.d`-level code.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Two-file split

```d
--This file contains gmp declarations and elementary functions.
--Functions in this file should not need to make calls to stdio.

use arithmetic;
use stdiop0;

declarations "
  #include <M2/math-include.h>
```

Same `0` vs. non-`0` split pattern as `stdio*.d`:

- **`gmp.d`** — declarations + functions that **don't** need stdio.
- **`gmp1.d`** — functions that **do** need stdio (printing,
  reading numbers from text).

The split lets parser-level files use the basic GMP types without
pulling in stdio.

## What's exposed

The bindings expose:

- **`ZZ`** — the `.d`-level integer type, aliased to GMP `mpz_t`.
- **`QQ`** — rational type, aliased to GMP `mpq_t`.
- **Arithmetic** — `+`, `-`, `*`, `/`, `%`, comparison.
- **Conversions** — `toZZ(int)`, `toZZ(string)`, `toString(ZZ)`,
  etc.
- **GMP-specific** — `gcd`, `pow`, `sqrt`, factorial helpers.

All of these come through the `declarations "..."` block at the top
that pulls in `<M2/math-include.h>` (which aggregates GMP, MPFR,
MPFI, Arb).

## Why GMP bindings live in the interpreter

The interpreter needs GMP arithmetic for:

- M2-level integer literals (`123456789...`).
- Hash computations (which can produce large integers).
- M2-level `gcd`, `mod`, `factorial`, etc. before any engine ring
  is constructed.

The engine has its own GMP / FLINT paths
([`../e/file-aring-zz-gmp.md`](../e/file-aring-zz-gmp.md),
[`../e/file-aring-zz-flint.md`](../e/file-aring-zz-flint.md)), but
those are only available *after* the engine has been initialised.
The interpreter's `gmp.d` runs before that.

## Used by

- [`file-parse.md`](file-parse.md), [`file-parser.md`](file-parser.md)
  — parse integer / rational literals.
- [`file-evaluate.md`](file-evaluate.md) — evaluate integer
  arithmetic.
- `actors4.d` — registers integer operators
  ([`file-actors.md`](file-actors.md)).
- Engine path via [`file-engine-dd.md`](file-engine-dd.md) — engine
  also wants GMP integers at its boundary.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-ballarith.md`](file-ballarith.md) — interval arithmetic
  builds on this.
- `boostmath.dd` — additional math primitives.
- GMP — external linked library.
