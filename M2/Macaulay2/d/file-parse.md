# `parse.d` — parser-level declarations (without stdio)

`parse.d` contains **parser-level declarations** — the `Expr` type
plus everything the parser needs to consume tokens and produce
expression ASTs. It deliberately does not depend on stdio, so it can
be used by `stdio.d` itself (chicken-and-egg avoidance).

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--This file contains declarations for the parser
--It also contains declarations for expressions that are strictly necessary for the parser
--This is necessary because Expr must be declared in this file for functions that return Expr to be declared properly
--Functions that merely operate on Exprs should go in expr.d
--Functions in this file should not use stdio so that parse can be used by stdio.
```

The header explains the design constraint:

- **`Expr`** (the AST type) must be declared here, because
  parser-producing functions return `Expr`.
- **No stdio** — `stdio.d` itself uses `parse.d`, so a circular
  dependency would otherwise form.
- **Operations on `Expr`** (without producing them from text) live
  in [`file-expr.md`](file-expr.md).

The discipline keeps the layering clean: parse.d for "produces
Expr", expr.d for "manipulates Expr".

## `Expr` declaration

The `Expr` type itself is declared in this file as an opaque sum
type — concretely, a union over the various M2 value types
(integers, rationals, polynomials, matrices, sequences, errors,
functions, hash tables, etc.). The detailed variant declarations
live further down in the file.

Plus error-handling declarations:

```d
--Error function predeclaration
declarations "
#ifdef __cplusplus
extern \"C\" {
#endif
struct M2_string_struct;
extern void err_abort(struct M2_string_struct*);
extern void err_fatal(struct M2_string_struct*);
```

`err_abort` and `err_fatal` are C-side error reporters the parser
calls when it hits unrecoverable situations.

## What the parser produces

For the M2 source:

```m2
R = QQ[x, y]
```

The parser walks the token stream and produces an `Expr` representing
the assignment expression:

- An `Assign` node.
- Left side: identifier `R`.
- Right side: subscript expression `QQ[x, y]`.

The resulting `Expr` is then handed to the evaluator
([`file-evaluate.md`](file-evaluate.md)) which actually runs it.

## Used by

- [`file-parser.md`](file-parser.md) — uses `parse.d` declarations.
- [`file-evaluate.md`](file-evaluate.md) — consumes `Expr`s.
- [`file-expr.md`](file-expr.md) — provides operations on `Expr`.
- Everything that produces M2 ASTs.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-expr.md`](file-expr.md) — `Expr` operations.
- [`file-parser.md`](file-parser.md) — parser implementation with
  stdio.
- [`file-lex.md`](file-lex.md) — tokens this consumes.
- [`file-evaluate.md`](file-evaluate.md) — evaluates the result.
