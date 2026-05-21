# `mathml.m2` — MathML rendering of mathematical expressions

`mathml.m2` is the **MathML output formatter** — it converts
[`Expression`](file-expressions.md) ASTs and
[`Hypertext`](file-hypertext.md) nodes into MathML XML for embedding
in MathML-aware viewers (notably the TeXmacs frontend and some web
viewers).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "format.m2"
needs "html.m2"        -- for htmlLiteral

-- Default rendering is by concatenation of rendered inputs
setupRenderer(mathML, concatenate, Hypertext)

moen := name -> concatenate("<mo>&", name, ";</mo>")
nest := (tag, s) -> concatenate("<", tag, ">", s, "</", tag, ">")
```

Two helper functions used pervasively:

- **`moen(name)`** — wrap an HTML entity as a MathML `<mo>` operator
  element. Used for `<mo>&plus;</mo>`, `<mo>&times;</mo>`, etc.
- **`nest(tag, s)`** — basic XML element nesting.

`setupRenderer(mathML, concatenate, Hypertext)` registers the default
rendering: walk a `Hypertext` node and concatenate the per-child
renderings.

## What it emits

MathML is the W3C standard for mathematical markup. A polynomial
`x^2 + 1` becomes:

```xml
<mrow>
  <msup><mi>x</mi><mn>2</mn></msup>
  <mo>&plus;</mo>
  <mn>1</mn>
</mrow>
```

Every `Expression` node maps to a `<m…>` MathML element. The
output is one big XML fragment.

## Why MathML

- **TeXmacs** frontend renders MathML natively.
- **MathJax**-equipped web viewers accept MathML as an alternative
  to LaTeX.
- **Accessibility** — screen readers understand MathML.

## Sibling formatters

| Formatter | Output |
|---|---|
| [`file-html.md`](file-html.md) | HTML |
| [`file-latex.md`](file-latex.md) | LaTeX `.tex` |
| `mathml.m2` (this file) | MathML XML |
| `texmacs.m2` ([`file-texmacs.md`](file-texmacs.md)) | TeXmacs scheme blocks (includes MathML) |
| [`file-markdown.md`](file-markdown.md) | Markdown |
| `nets.m2` ([`file-nets.md`](file-nets.md)) | Terminal 2-D grids |

All six consume the same AST.

## Used by

- The TeXmacs interactive frontend.
- HTML documentation that includes MathJax / KaTeX with MathML
  support.
- Accessibility-focused output paths.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-expressions.md`](file-expressions.md) — AST input.
- [`file-hypertext.md`](file-hypertext.md) — alternative AST input.
- [`file-texmacs.md`](file-texmacs.md) — primary user.
- [`file-html.md`](file-html.md) — `htmlLiteral` shared helper.
