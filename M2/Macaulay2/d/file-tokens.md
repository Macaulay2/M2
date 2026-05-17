# `tokens.d` — token type definitions

`tokens.d` defines the **token types** the lexer produces and the
parser consumes: `Token`, `Position`, `Word`, `Symbol`, plus the
sentinel values that mark special positions in the token stream.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Core types

The file declares:

- **`Position`** — a `(file, line, column)` triple. Every token
  carries one.
- **`Word`** — the textual form of an identifier or literal. Each
  `Word` is interned (single representative per textual form).
- **`Symbol`** — a binding: `Word` + value + scope.
- **`Token`** — a `Word` plus its `Position` plus any
  literal-value-specific data.

## How tokens flow

Lifecycle of a token:

1. The lexer ([`file-lex.md`](file-lex.md)) reads characters and
   produces `Token`s.
2. The parser ([`file-parser.md`](file-parser.md)) consumes them,
   producing `Expr` ASTs.
3. The evaluator ([`file-evaluate.md`](file-evaluate.md)) walks
   `Expr`s and dispatches via `Symbol`-table lookups.

Tokens are intermediate; their lifetime is brief.

## Dummies and sentinels

`tokens.d` declares many sentinel values:

- **`dummyPosition`** — a `Position` used as a placeholder.
- **`dummyWord`** — for "this slot will be filled in later."
- **`dummySymbol`** — same for `Symbol`.

The pattern is forced by the `.d` language: types must be declared
before they're used, but some types reference each other in a way
that requires sentinels to break the cycle.

## Used by

- [`file-lex.md`](file-lex.md) — produces tokens.
- [`file-parser.md`](file-parser.md), [`file-parse.md`](file-parse.md)
  — consumes tokens.
- [`file-binding.md`](file-binding.md) — uses `Symbol` and `Word`.
- Every error-reporting path — uses `Position`.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-lex.md`](file-lex.md), [`file-parser.md`](file-parser.md)
  — producer / consumer.
- [`file-binding.md`](file-binding.md) — `Symbol` table management.
- `types.h` — C-side counterpart declarations.
