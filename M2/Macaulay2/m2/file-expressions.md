# `expressions.m2` — `Expression` type hierarchy and operator precedence

`expressions.m2` defines the **`Expression`** type — M2's
internal AST for mathematical expressions — plus the operator-
precedence machinery the pretty-printer uses to decide where to
place parentheses. It is the core of M2's hypertext / TeX / unicode
output pipelines.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What `Expression` is for

When M2 prints a `RingElement` like `x^2 + 3*y - 5`, it doesn't
print directly. Instead it:

1. Builds an **`Expression`** AST representing the mathematical
   structure.
2. Hands the AST to an output formatter (HTML, TeX, plain text, …)
   that walks it.
3. The formatter uses **precedence** information attached to each
   node to decide where parentheses are needed.

This decouples "what mathematics" from "how it looks." A
`SUM(x^2, -3*y, -5)` expression renders to:

| Formatter | Output |
|---|---|
| Plain text | `x^2 + 3*y - 5` |
| TeX | `x^{2} + 3\,y - 5` |
| HTML | `x<sup>2</sup> + 3<i>y</i> &minus; 5` |
| Unicode pretty-printer | `x² + 3·y − 5` |

All four come from the same `Expression`.

## Precedence machinery

```m2
needs "max.m2"
needs "methods.m2"
needs "nets.m2"

Constant = new Type of Number
globalAssignment Constant

precedence       = method(Dispatch => Thing)
rightPrecedence  = method(Dispatch => Thing)
lprec = prec     = x -> (getParsing x)#0
rprec = strength2 = x -> (getParsing x)#1
uprec = strength1 = x -> (getParsing x)#2
```

Three precedence functions:

- **`prec`** / `lprec` — left binding strength of an operator.
- **`strength2`** / `rprec` — right binding strength.
- **`strength1`** / `uprec` — unary binding strength.

These match the engine's grammar (`Macaulay2/c/grammar.y`) and the
M2 parser's operator table. Keeping them in sync is a maintenance
burden but unavoidable: the printer and the parser must agree on
what binds tighter.

## Author note

The file's `Copyright 1993-2002` header credits Daniel Grayson; the
inline comment `-- rewritten by P. Zinn-Justin 2018` records a major
rewrite by Paul Zinn-Justin. The rewrite was driven by the modern
HTML/TeX output paths needing finer-grained precedence than the
original code supported.

## Expression type hierarchy

`Expression` is the parent of:

- `Holder` — wraps a Thing (for atomic values).
- `Sum`, `Product`, `Power`, `Minus`, `Divide`, `Negate` — arithmetic.
- `Subscript`, `Superscript` — structural.
- `Matrix`, `MutableMatrix`, `Adjacent`, `Equation` — domain-specific.
- ... and many more.

Each node carries its own precedence value via `getParsing`.

## Used by

- Every M2 output formatter: `printing.m2`, `nets.m2`, `html.m2`,
  `latex.m2`, `mathml.m2`, `markdown.m2`, `texmacs.m2`.
- M2-level `expression x` returns an `Expression` for any `x`.
- The output of M2's interactive prompt walks `Expression` ASTs.

## Related

- [`README.md`](README.md) — m2/ overview.
- `printing.m2`, `nets.m2`, `pretty.m2`, `format.m2` — output
  pipeline (Expressions → text).
- `html.m2`, `latex.m2`, `mathml.m2`, `markdown.m2`, `texmacs.m2`
  — alternative output formats.
- `../c/grammar.y` — parser side that must agree on precedences.
