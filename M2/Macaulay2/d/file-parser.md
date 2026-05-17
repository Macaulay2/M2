# `parser.d` — the actual parser (with stdio)

`parser.d` is the **actual parser** — the bit of the interpreter
that reads tokens and produces `Expr` ASTs. The split between
`parser.d` and [`parse.d`](file-parse.md) lets `stdio.d` use
parser-level declarations without depending on the full parser
machinery.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994 by Daniel R. Grayson
--- This file contains functions for parsing and expressions that need to use stdio.

use pthread0;
use stdiop;
use gmp1;
use xml;
use engine;
use varnets;
use expr;


-- misc

export flushToken(f:TokenFile):void := (f.nexttoken = NULL; flushInput(f.posFile););
```

The wide `use` list (stdio, GMP, XML, engine, ...) is why this file
can't live alongside `stdio.d`'s declarations — it depends on
basically everything.

`flushToken` is one of many small parser utilities — clear the
pending token and flush the input file.

## How parsing flows

The M2 parser is a **recursive descent / operator-precedence**
parser. The top-level entry point reads a token, dispatches on its
type to the right specialised parser routine, and emits `Expr`
nodes.

Operator precedence is encoded in a precedence table — the same
information that [`file-expressions.md`](../m2/file-expressions.md)
on the M2 side surfaces for pretty-printing. The parser uses it to
decide where parenthesis nesting goes.

## What `Expr` nodes get produced

Concretely the parser produces:

- **`localAssignExpr`** — `x = …`.
- **`tryExpr`** — `try ... else ...`.
- **`forExpr`** — `for x in ... do ...`.
- **`whileExpr`** — `while ... do ...`.
- **`functionCallExpr`** — `f(...)`.
- **`adjacentExpr`** — `M_3`, `R_(0,1)` etc. (adjacent juxtaposition).
- ... many more variants.

Each variant is a sum-type alternative the
[`file-evaluate.md`](file-evaluate.md) walker pattern-matches on.

## `TokenFile`

The parser reads from a `TokenFile` — a wrapped `posFile` (file with
position tracking) plus a one-token lookahead buffer. The
`flushToken` shown above clears the lookahead, used by interactive-
prompt error recovery.

## Used by

- [`file-evaluate.md`](file-evaluate.md) — consumes the `Expr`s.
- [`file-interp.md`](file-interp.md) — calls the parser inside the
  main loop.
- File-loading paths — `load`, `needs`, `loadPackage` all parse via
  `parser.d`.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-parse.md`](file-parse.md) — parser-level declarations
  without stdio.
- [`file-lex.md`](file-lex.md), [`file-tokens.md`](file-tokens.md)
  — token stream input.
- [`file-evaluate.md`](file-evaluate.md) — `Expr` consumer.
- [`file-interp.md`](file-interp.md) — top-level loop driver.
