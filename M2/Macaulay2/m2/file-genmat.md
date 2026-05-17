# `genmat.m2` — `genericMatrix`, `genericSymmetricMatrix`

`genmat.m2` defines **`genericMatrix`**, **`genericSymmetricMatrix`**,
**`genericSkewMatrix`** — operations that construct matrices whose
entries are freshly named variables in a polynomial ring. Useful for
symbolic linear-algebra computations.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's exposed

- **`genericMatrix(R, m, n)`** — `m × n` matrix whose entries are
  the first `m·n` variables of `R`.
- **`genericMatrix(R, x, m, n)`** — same but using `x_0, …,
  x_(m·n-1)` (or whatever index style the variable supports).
- **`genericSymmetricMatrix(R, n)`** — symmetric `n × n` matrix
  with `n(n+1)/2` distinct variables.
- **`genericSkewMatrix(R, n)`** — skew-symmetric `n × n` matrix
  with `n(n-1)/2` distinct variables and zeros on the diagonal.

## Example

```m2
R = QQ[x_0..x_5]
M = genericMatrix(R, 2, 3)
-- |  x_0  x_2  x_4 |
-- |  x_1  x_3  x_5 |
```

The order convention: entries are listed column-by-column starting
from the top-left.

## Why "generic"

In symbolic computation, "generic" means "no special algebraic
relations." `genericMatrix(R, 2, 2)` has *no* algebraic relations
between its entries beyond what `R` already imposes — it's the
universal `2 × 2` matrix over `R`.

This is useful for:

- Computing what an algebraic operation produces "in general."
- Generating worked-out specific examples by specialising the
  entries.
- Defining moduli spaces.

## Used by

- Algebraic-geometry packages that work with generic varieties.
- M2 users computing characteristic polynomials, generic invariants.
- Schubert calculus packages.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-matrix.md`](file-matrix.md) — `Matrix` type produced.
- [`file-variables.md`](file-variables.md) — indexed variables
  used in some constructors.
- [`../e/file-matrix-symm.md`](../e/file-matrix-symm.md) —
  engine-side symmetric-matrix helper.
- [`../e/file-pfaff.md`](../e/file-pfaff.md) — Pfaffian operations
  on generic skew matrices.
