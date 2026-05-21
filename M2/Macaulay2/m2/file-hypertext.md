# `hypertext.m2` — `Hypertext` AST for documentation

`hypertext.m2` defines the **`Hypertext`** type hierarchy — the AST
that documentation nodes are parsed into. Every doc-string format
(HTML, LaTeX, MathML, Markdown, info, terminal text) consumes
`Hypertext` ASTs and emits its own syntax.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
--  Copyright 1993-2003 by Daniel R. Grayson
-- Revamped by P. Zinn-Justin and Mahrud Sayrafi 2020

needs "debugging.m2"    -- for Descent, FilePosition
needs "regex.m2"        -- for toLower
needs "lists.m2"        -- for all
needs "max.m2"          -- for IndeterminateNumber
```

The "Revamped 2020" comment marks a major rewrite by Zinn-Justin
and Sayrafi that cleaned up the hypertext model substantially. The
file works today in essentially the post-revamp form.

## The `Hypertext` type tree

```text
Hypertext (abstract)
 ├── HypertextContainer (block-level)
 │    ├── HTML, BODY, DIV, P, ...
 │    ├── UL, OL, LI, DL, DT, DD
 │    ├── TABLE, TR, TD, TH
 │    ├── PRE, BLOCKQUOTE
 │    └── ...
 ├── HypertextSpan (inline)
 │    ├── EM, STRONG, TT, CODE, KBD
 │    ├── TO, TO2, HREF, ANCHOR
 │    └── ...
 └── HypertextLeaf (text-like)
      ├── String literals
      ├── BR, HR
      └── ...
```

Each subclass is a `Type of HashTable` carrying its content
plus optional formatting attributes.

## Inline vs. block

The container/span/leaf distinction is structural. It controls how
the various output formatters lay out the result:

- **`HypertextContainer`** nodes are rendered as block elements
  (separate lines, indentation).
- **`HypertextSpan`** nodes are rendered inline (no line break).
- **`HypertextLeaf`** nodes are atomic — no children.

The distinction is enforced by validation: `validate.m2` rejects
nesting block elements inside inline ones.

## Building a `Hypertext`

User code typically builds a `Hypertext` via the named constructors:

```m2
DIV { "Some text.", EM "Emphasized.", BR, "More text." }
```

The named constructors (`DIV`, `EM`, `BR`, etc.) are functions that
return a node of the appropriate type carrying the supplied
children.

## Output formatters

Every formatter walks the same `Hypertext` AST:

- [`file-html.md`](file-html.md) — HTML output.
- `latex.m2` ([`file-latex.md`](file-latex.md)) — LaTeX.
- `mathml.m2` — MathML.
- `markdown.m2` — Markdown.
- `texmacs.m2` — TeXmacs.
- `format.m2` ([`file-format.md`](file-format.md)) — terminal nets +
  info-page text.

## Used by

- [`file-document.md`](file-document.md) — `document{...}` blocks
  become `Hypertext` ASTs.
- Every output formatter.
- M2 packages writing custom documentation.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-document.md`](file-document.md) — DSL producing this AST.
- [`file-format.md`](file-format.md) — terminal / info formatter.
- [`file-html.md`](file-html.md), [`file-latex.md`](file-latex.md)
  — output backends.
- [`file-expressions.md`](file-expressions.md) — sibling AST for
  mathematical expressions.
