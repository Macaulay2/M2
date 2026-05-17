# `nets.d` and `varnets.d` — 2D character grids

`nets.d` defines the **`Net`** type — M2's two-dimensional
string-of-strings used for laying out matrices, multi-line output,
and pretty-printing. `varnets.d` is the mutable builder
counterpart, analogous to `varstring` vs `string`.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What is a `Net`?

```d
--		Copyright 1996 by Daniel R. Grayson

-- nets are 2 dimensional strings of characters

use varstrin;
use ctype;

export Net := {+
     height:int,    -- number of strings above the baseline
     width:int,     -- width of body (strings may be shorter)
     body:array(string)   -- one string for each row, read-only
     };
```

A `Net` is:

- An array of strings (rows of text).
- A **height** field: how many rows are above the baseline.
- A **width** field: the visual width of the widest row.

The baseline lets you align nets sensibly (subscripts vs.
superscripts, matrix rows vs. headings). Two nets of different
heights stacked horizontally line up by *baseline*, not by *top*.

## Why nets

M2 produces a lot of multi-line output:

- Matrices with row/column labels.
- Polynomials with fractional exponents.
- Boxed help-page formatting.

Plain strings can't represent these — concatenating fragments
horizontally requires aligning each fragment vertically. Nets are
designed for exactly that: every operation (`||`, `|`, `^`, `_`)
preserves baseline alignment.

## Core operations

- **`a || b`** — vertical concatenation.
- **`a | b`** — horizontal, both at same baseline.
- **`a ^ n`** — raise baseline (superscript).
- **`a _ n`** — lower baseline (subscript).
- **`stack {a, b, c}`** — stack many nets.
- **`box(net)`** — wrap in box characters.

## `varnets.d`

```d
--		Copyright 1994-2006 by Daniel R. Grayson
use nets;
-- varnets:
-- a varnet is a net whose strings are varstrings
-- pushing N spaces is quick - our routine accepts an integer
```

The mutable builder. Same relationship to `Net` as `varstring` is
to `string`: build it up cheaply, snapshot at the end.

## Used by

- The M2 printer (`toString`, `print` paths).
- Pretty-printing matrices, modules, polynomials.
- The help / documentation renderer.
- M2 user code constructing custom output (`stack`, `|`, `||`).

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-strings.md`](file-strings.md) — building blocks
  (`varstring`).
- [`../m2/file-nets.md`](../m2/file-nets.md) — M2-side `Net` API
  built on this.
- [`../m2/file-printing.md`](../m2/file-printing.md) (if added) —
  the printer that uses nets pervasively.
