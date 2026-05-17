# `nets.m2` — `Net` (2-D character grids for pretty-printing)

`nets.m2` defines the **`Net`** type — Macaulay2's 2-D character
grid used by the pretty-printer to render multi-line objects
(matrices, polynomials, Betti tables, …) at the M2 prompt. Nets are
what makes:

```
  +-          -+
  | x   y   z  |
  |            |
  | x^2 y^2 z^2|
  +-          -+
```

possible in a terminal.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "set.m2"        -- for demark
needs "methods.m2"

-- nets

Net.synonym = "net"

toString MutableHashTable := s -> (
    ...
)
```

A `Net` is structurally:

- A **list of strings**, each being one row.
- A **depth** — how many rows are above the baseline (the rest are
  below).

The "baseline" lets nets stack vertically while keeping their
mathematical typesetting aligned: a fraction net has its baseline
between numerator and denominator; a matrix net has its baseline at
the centre row.

## Net algebra

Nets compose by:

- **`|`** — horizontal concatenation: `a | b` places `b` to the
  right of `a`, aligning baselines.
- **`||`** — vertical concatenation: `a || b` places `b` below `a`,
  with `b`'s top row immediately below `a`'s bottom row.
- **`^n`** — raise / lower by `n` rows (positive = raise, negative
  = lower).

These three operators compose to produce arbitrarily complex 2-D
layouts.

## Why a custom 2-D grid

Plain `\n`-separated strings would print but couldn't be combined
algebraically — you can't put one multi-line expression next to
another without losing alignment. The `Net` type carries the
alignment information explicitly.

## Used by

- M2's `print` / `<<` operators when given multi-line values.
- [`file-expressions.md`](file-expressions.md)'s formatters — nets
  are the bottom layer of the printing pipeline.
- [`file-betti.md`](file-betti.md) — Betti tables are nets.
- Matrix display, polynomial display, etc.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-expressions.md`](file-expressions.md) — `Expression` AST.
- [`file-printing.md`](file-printing.md) — printing pipeline.
- `pretty.m2`, `format.m2` — sibling formatters.
- [`../d/nets.d`](../d/README.md) — engine-side net primitives.
