# `format.m2` — `Hypertext` formatter dispatch

`format.m2` provides the **shared output-formatter dispatch
machinery** that all the per-format files
([`file-html.md`](file-html.md), [`file-latex.md`](file-latex.md),
[`file-mathml.md`](file-mathml.md), [`file-markdown.md`](file-markdown.md),
[`file-texmacs.md`](file-texmacs.md)) build on.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
---             Copyright 1993-2004 by Daniel R. Grayson
-*
  The help command returns its output as a Hypertext object.
  Three steps are necessary in order to generate documentation:
  - parse Hypertext nodes into subnodes
  - render subnodes in the appropriate format
  - join the result

  info and net are parsed and rendered in this file.
  When possible, write a new core script that performs the steps above.
*-
```

The header explains the **three-step pipeline** every M2 output
formatter follows:

1. **Parse** the `Hypertext` node into its sub-nodes.
2. **Render** each sub-node into the target syntax (HTML, LaTeX, …).
3. **Join** the rendered pieces into the final output.

`format.m2` implements two formatters directly:

- **`info`** — flat text suitable for emitting to `texinfo`.
- **`net`** — 2-D character grids
  ([`file-nets.md`](file-nets.md)) for terminal output.

The other formatters (HTML, LaTeX, MathML, Markdown, TeXmacs) live
in their own files but share the dispatch helpers `format.m2`
provides.

## Dispatch helpers

The file exports the `setupRenderer(name, joiner, parent)`
construction:

```m2
setupRenderer(html, concatenate, Hypertext)
setupRenderer(mathML, concatenate, Hypertext)
```

Each `setupRenderer(...)` call wires a new formatter (`html`,
`mathML`, etc.) to dispatch over `Hypertext` nodes via `concatenate`
as the joining function. Per-node behaviour gets overridden in each
formatter's own file.

## `info` and `net` direct implementations

The `info` and `net` formatters are implemented directly in this
file because they share the most code:

- Both produce text output.
- Both need careful handling of line breaks, indentation.
- Both consume the same `Hypertext` AST.

Putting them here avoids duplication.

## Used by

- Every output formatter (`html`, `latex`, `mathml`, `markdown`,
  `texmacs`) — sets up dispatch via `setupRenderer`.
- The `texinfo`-based documentation pipeline.
- `print` and `<<` at the M2 prompt — produce nets.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-hypertext.md`](file-hypertext.md) — AST input.
- [`file-html.md`](file-html.md), [`file-latex.md`](file-latex.md),
  [`file-mathml.md`](file-mathml.md), [`file-markdown.md`](file-markdown.md),
  [`file-texmacs.md`](file-texmacs.md) — derivative formatters.
- [`file-nets.md`](file-nets.md) — `net` formatter target type.
