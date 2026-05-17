# `html.m2` — HTML rendering of `Expression`s and hypertext

`html.m2` is the M2 documentation system's **HTML rendering**
backend. Given an `Expression` ([`file-expressions.md`](file-expressions.md))
or a `Hypertext` AST ([`file-hypertext.md`](file-hypertext.md)),
it produces HTML strings the `installPackage` pipeline writes to
disk.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-*- coding: utf-8 -*-
-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

needs "format.m2"
needs "system.m2"     -- for getViewer
needs "monoids.m2"    -- for Monoid

getStyleFile := fn -> locateCorePackageFileRelative("Style",
    ...
)
```

The author intentionally declares `coding: utf-8` to make sure the
M2 source itself can contain non-ASCII characters that flow through
to the generated HTML.

## What it produces

For every `Hypertext` node type — `TT`, `EM`, `STRONG`, `UL`, `OL`,
`LI`, `H1` through `H6`, `TABLE`, `TR`, `TD`, `CODE`, `PRE`, `IMG`,
`TO`, … — `html.m2` has a corresponding method that turns it into
the equivalent HTML tag.

For mathematical expressions, the file routes through
[`file-expressions.md`](file-expressions.md)'s precedence machinery
to decide where parentheses are needed, then renders the result
with `<sup>` / `<sub>` / `<mfrac>` / etc. as appropriate.

## `getStyleFile`

```m2
getStyleFile := fn -> locateCorePackageFileRelative("Style", ...)
```

CSS for M2's generated documentation comes from the
[`Style`](../packages/) package. `getStyleFile` finds the right
stylesheet (`m2style.css` or similar) for the current package's
HTML output. The same mechanism is used for KaTeX / MathJax assets
on pages that need rendered math.

## Companion formatters

| Formatter | File | Output |
|---|---|---|
| HTML | `html.m2` (this file) | Standalone `.html` pages |
| LaTeX | `latex.m2` | `.tex` files |
| MathML | `mathml.m2` | MathML XML |
| TeXmacs | `texmacs.m2` | TeXmacs scheme blocks |
| Markdown | `markdown.m2` | GitHub-flavoured `.md` |
| Plain text | `nets.m2` | `Net` 2-D grid |

All five consume the same input (`Expression` AST + `Hypertext` AST)
and differ only in output syntax.

## Used by

- [`file-installPackage.md`](file-installPackage.md) — primary
  consumer. Produces the per-package `share/M2/<pkg>/html/` trees.
- [`file-help.md`](file-help.md) — `viewHelp` opens generated HTML in
  the user's browser.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-expressions.md`](file-expressions.md) — AST input.
- [`file-document.md`](file-document.md) — doc-node DSL whose
  output flows here.
- [`file-installPackage.md`](file-installPackage.md) — primary
  consumer.
- [`Style` package](../packages/README.md) — supplies CSS.
