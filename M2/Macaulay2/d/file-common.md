# `common.d` — shared helpers across `.d` files

`common.d` is the **junction box** of the interpreter — utility
functions used everywhere: `codePosition(c)`, `Sequence` helpers,
`Frame` / `Code` traversal, common error-creation helpers.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header and a representative function

```d
--		Copyright 1994-2003 by Daniel R. Grayson
use basic;
use binding;
use stdiop0;

export codePosition(c:Code):Position := ( -- TODO retire
    when c
    is f:nullCode                  do dummyPosition
    is f:realCode                  do f.position
    is f:stringCode                do f.position
    is f:integerCode               do f.position
    ...);
```

The `codePosition` function (with its long `when ... is ... do`
chain) is illustrative: takes any `Code` variant and returns its
source position. The TODO marks it for retirement — newer code
should access `.position` directly because every `Code` variant
carries one — but lots of older code calls `codePosition`.

## What `common.d` contains

- **`codePosition(Code)`** — universal source-position extractor.
- **`Sequence` helpers** — `seq()`, `seq(a, b)`, `seq(a, b, c)`.
- **`Frame` manipulation** — building activation records.
- **Common error helpers** — `WrongArg`, `WrongArgZZ`,
  `WrongNumArgs`, etc.
- **`toExpr(...)`** — promoting native values into `Expr`.

## Why a "common" file

Without `common.d`, every other `.d` file would re-implement
`seq(a, b)`, `WrongArg`, `codePosition`, etc. Centralising them
here cuts the dependency graph and avoids triplicate definitions.

Most other `.d` files end up with `use common;` near the top.

## Used by

- [`file-evaluate.md`](file-evaluate.md), `actors*.d` — call
  `WrongArg`, `seq`, etc.
- [`file-debugging.md`](file-debugging.md) — uses `codePosition` to
  format breakpoints.
- Essentially every `.d` file above the lexer / parser layer.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-util.md`](file-util.md) — sister file with more
  type-specific argument-checking helpers.
- [`file-tokens.md`](file-tokens.md) — defines the `Code` /
  `Frame` types `common.d` operates on.
- [`file-binding.md`](file-binding.md) — `common.d` uses
  `binding.d`'s symbol-resolution helpers.
