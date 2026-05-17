# `util.d` — argument-checking helpers

`util.d` provides the **argument-checking helpers** that
`actors*.d` builtins use to validate their inputs: `isBoolean`,
`toBoolean`, `isSmallInt`, `toInt`, etc.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 2002,2010 by Daniel R. Grayson
-- helper routines for checking arguments
use basic;
export isBoolean(e:Expr):bool := e == True || e == False;
export toBoolean(e:Expr):bool := e == True;

export isSmallInt(e:Expr):bool := (
     when e is i:ZZcell do ...);
```

Every builtin operator in `actors*.d` follows the same dance:

1. Pattern-match the `Expr` argument.
2. Check via `isBoolean(e)`, `isSmallInt(e)`, etc.
3. If wrong, return `WrongArg(...)`.
4. Otherwise, convert with `toBoolean(e)`, `toInt(e)`, etc.

`util.d` is where all the `isXxx` / `toXxx` helpers live.

## The catalog

The most-used helpers:

| Helper | Returns | Use |
|---|---|---|
| `isBoolean(e)` | `bool` | Test for `True`/`False` |
| `toBoolean(e)` | `bool` | Extract bool (assumes `isBoolean`) |
| `isSmallInt(e)` | `bool` | Test for `ZZcell` ≤ INT_MAX |
| `toInt(e)` | `int` | Extract C `int` |
| `isString(e)` | `bool` | Test for `stringCell` |
| `toString(e)` | `string` | Extract string |
| `isSequenceOfStrings(e)` | `bool` | Heterogeneous-list check |

The pattern is consistent enough that `actors*.d` files can stay
quite readable despite handling dozens of edge cases.

## Why a dedicated file

`common.d` is already loaded; why a separate `util.d`?

- **Layering** — `util.d` depends on `basic.d` only; it can be
  used in low-level files where `common.d` is too heavy.
- **Focus** — `util.d` is entirely about argument-checking, while
  `common.d` is more of a junction box.

In practice, files generally `use common` *and* `use util`.

## Used by

- All `actors*.d` files
  ([`file-actors.md`](file-actors.md)) — primary consumer.
- [`file-evaluate.md`](file-evaluate.md) — uses argument-checking
  for built-in evaluator paths.
- Many FFI files — [`file-ffi.md`](file-ffi.md),
  [`file-python.md`](file-python.md), etc.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-common.md`](file-common.md) — sister helpers file.
- [`file-actors.md`](file-actors.md) — primary consumer.
- [`file-basic.md`](file-basic.md) (if added) — even-lower-level
  hash / sequence helpers.
