# `strings.d`, `strings1.d`, `varstrin.d` — string handling

`strings.d`, `strings1.d`, and `varstrin.d` together implement M2's
**string-handling primitives** at the interpreter layer — fixed
strings, integer-to-string conversion, and the mutable
`varstring` builder type used by lexer / parser / formatter.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## The three files

| File | Contains |
|---|---|
| `strings.d` | core operations on (immutable) `string` |
| `strings1.d` | integer → string conversion (`tostring(long)`) |
| `varstrin.d` | mutable `varstring` builder |

The split keeps the `tostring` family — which depends on
`varstrin.d` for its scratch buffer — separated from raw string
ops that need no buffer machinery.

## `strings.d`

Header:

```d
--		Copyright 1994 by Daniel R. Grayson

use arithmetic;
use system;

export concatenate(x:array(string)):string := ( ... );
```

Exposes `concatenate`, slicing helpers, character-search loops,
hash-of-string. These are the primitives M2's `String` type
methods are built on top of.

## `varstrin.d`

```d
export varstring := {
     str:string,
     width:int   -- the number of bytes in str that are used
     };

export newvarstring(n:int):varstring := ( ... );
```

A `varstring` is a `string` plus a `width` field saying how much
of the backing buffer is used. Operations like `appendc`,
`appendstring`, `addint` grow the buffer (doubling) and update
`width`. `tostring(varstring)` snapshots out the used prefix.

Used heavily in:

- The lexer's token-building loop ([`file-lex.md`](file-lex.md)).
- The parser's error-message accumulation.
- The formatter that produces `toString(Expr)`.
- Net-rendering (via `varnets.d`).

## `strings1.d`

Provides `tostring(long)` (and friends for `int`, `short`, ...).
Builds digits in a `varstring` then snapshots. Why a separate file:
keeping the integer-formatting path out of the foundational
`strings.d` cuts a dependency cycle.

## Used by

- Essentially every `.d` file that builds or examines strings.
- [`file-lex.md`](file-lex.md), [`file-parser.md`](file-parser.md),
  [`file-evaluate.md`](file-evaluate.md), `actors*.d`.
- The net-printing system in [`file-nets.md`](file-nets.md).

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-nets.md`](file-nets.md) — 2D `Net` type that uses
  `varstring` underneath.
- [`file-lex.md`](file-lex.md) — primary consumer.
- `vararray.d` — sister "varying-length array of int" type.
