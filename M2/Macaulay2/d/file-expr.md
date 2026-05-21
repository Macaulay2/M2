# `expr.d` — operations on `Expr` (without parsing)

`expr.d` is the **operations side** of the `Expr` story: where
[`parse.d`](file-parse.md) declares the `Expr` type, `expr.d`
provides everything you'd want to do *with* an `Expr` (without
producing one from text).

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--This file contains functions that operate on Exprs.
--It also contains misc dummy declarations.
--Finally classes are declared at the end of this file.
--Functions in this file should not use stdio so that expr can be used by stdio.

use pthread0;
use gmp;
use xml;
use engine;
use varnets;
use parse;
use stdio0;
use strings;
--Note: scclib.c uses interrupts, so use it here so that it gets included in expr-exports.h
use interrupts;
```

The header explains the layering:

- **No stdio** — like [`parse.d`](file-parse.md), this file is
  upstream of stdio.
- **Class declarations at the end** — the engine's class types
  (`Ring`, `Matrix`, etc.) are declared near the end of the file
  so they have access to the entire `Expr` vocabulary.

## What's in `expr.d`

Operations on `Expr`:

- **Equality / comparison** — `==`, `<`, `>` between `Expr`s.
- **Coercions** — `toInteger`, `toString`, `toExpr` for various
  source types.
- **Inspection** — `class Expr`, `parent Expr`, `hash Expr`.
- **Hash-table operations** — get / put on `Expr`-keyed hash tables.
- **Dummy declarations** — placeholders for things that get filled
  in later by `actors*.d`.

Plus **class declarations**: the M2 type tags `Ring`, `Matrix`,
`Module`, etc. are declared here so that pattern-matching on
`Expr` can recognise them.

## "Misc dummy declarations"

The header note "It also contains misc dummy declarations" reflects
the `.d` language's forward-reference handling: where a symbol
needs to be visible before its real definition exists, the file
declares a dummy that's replaced later. The pattern is used heavily
in `expr.d` because of `Expr`'s many cross-cutting variants.

## Used by

- [`file-parse.md`](file-parse.md) — sister declarations file.
- [`file-evaluate.md`](file-evaluate.md) — works with `Expr`s.
- Every other `.d` file using `Expr` operations.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-parse.md`](file-parse.md) — `Expr` declarations.
- [`file-evaluate.md`](file-evaluate.md) — primary consumer of
  `Expr` operations.
- [`file-tokens.md`](file-tokens.md) — token types.
- `engine.dd` — engine-call hooks that produce `Expr` results.
