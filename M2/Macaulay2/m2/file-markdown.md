# `markdown.m2` — Markdown rendering of `Hypertext`

`markdown.m2` is a Markdown output formatter for M2's documentation
system. It walks a [`Hypertext`](file-hypertext.md) AST and emits
GitHub-flavoured Markdown.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
--  Copyright 2020 by Mahrud Sayrafi
-----------------------------------------------------------------------------
-- markdown output
-- See https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
-- TODO: task lists
-----------------------------------------------------------------------------

-* Note: users can modify the behavior of markdown on individual types in
-- order to adjust its output to different markdown processors.
-- For example, to specify the layout and excerpt:
```

Authored by Mahrud Sayrafi in 2020. The TODO captures planned
support for GitHub task-list syntax (`- [ ]` / `- [x]`).

## What it emits

For each `Hypertext` node type, `markdown.m2` produces the
corresponding Markdown construct:

| Hypertext | Markdown |
|---|---|
| `EM "x"` | `*x*` |
| `STRONG "x"` | `**x**` |
| `CODE "x"` | `` `x` `` |
| `PRE "x"` | ` ```x``` ` |
| `UL { LI a, LI b }` | `- a` / `- b` |
| `H1 "x"` | `# x` |
| `HREF{url, text}` | `[text](url)` |
| `TABLE { ... }` | GitHub Markdown table |

Math expressions get rendered as MathJax / KaTeX-compatible LaTeX
(via [`file-latex.md`](file-latex.md)) wrapped in `$$...$$`.

## Customisability

Per the author's note, **users can override per-type behaviour**:

```m2
markdown DocumentTag := tag -> (
    -- custom rendering for DocumentTag
    ...
)
```

This is the standard M2 extension mechanism — define a method
override and the formatter picks it up automatically. The use case
is targeting a specific Markdown processor (CommonMark vs.
GitHub-flavoured vs. Pandoc) that handles edge cases differently.

## Used by

- M2's `markdown` built-in for converting docs to Markdown.
- The website-building infrastructure when generating documentation
  for sites that prefer Markdown over HTML.
- Users sharing M2 examples in chat / issue trackers.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-hypertext.md`](file-hypertext.md) — AST input.
- [`file-html.md`](file-html.md), [`file-latex.md`](file-latex.md),
  `mathml.m2` — sibling output formatters.
- [`file-document.md`](file-document.md) — produces the AST.
