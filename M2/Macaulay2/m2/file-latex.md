# `latex.m2` — `tex` and `texMath` LaTeX rendering

`latex.m2` is the M2 documentation system's **LaTeX rendering**
backend. It implements `tex`, `texMath`, and the LaTeX output paths
for the documentation pipeline.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-----------------------------------------------------------------------------
-- tex and texMath output
-- See https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
-- TODO: eye-friendly block indentation
-----------------------------------------------------------------------------

needs "format.m2"
needs "html.m2"
```

The header references a markdown cheatsheet (a leftover comment
from a side comparison) and lists "eye-friendly block indentation"
as outstanding work.

## What `tex` and `texMath` produce

- **`tex x`** — produces a LaTeX document fragment for `x`. The
  result includes display-math wrappers when appropriate.
- **`texMath x`** — produces inline LaTeX math for `x`. The result
  is meant to be embedded inside `$...$` or `\(\,\)`.

The two differ in the framing: `tex` wraps in display math, `texMath`
does not.

## What it consumes

Same as every other formatter: an `Expression`
([`file-expressions.md`](file-expressions.md)) or a `Hypertext`
([`file-hypertext.md`](file-hypertext.md)) value. The file
includes [`file-html.md`](file-html.md) for shared logic.

## Output examples

```text
texMath x^2 + 1        →  "x^{2}+1"
texMath matrix{{1,2},{3,4}}  →  "\\begin{pmatrix} 1 & 2 \\\\ 3 & 4 \\end{pmatrix}"
texMath 1/2            →  "\\frac{1}{2}"
texMath sqrt 2         →  "\\sqrt{2}"
```

Operators flow through precedence
([`file-expressions.md`](file-expressions.md)) so parentheses appear
where needed.

## Consumed by

- **`installPackage`** — when generating PDF documentation via
  `pdflatex`.
- **Jupyter notebooks** — `display` of M2 values in Jupyter renders
  via `texMath`.
- **TeXmacs frontend** — uses LaTeX as an intermediate.
- **Direct user calls** — `tex polynomial` for embedding in papers.

## Sibling files

| File | Output |
|---|---|
| [`file-html.md`](file-html.md) | HTML |
| `latex.m2` (this file) | LaTeX (`.tex`) |
| `mathml.m2` | MathML XML |
| `markdown.m2` | Markdown |
| `texmacs.m2` | TeXmacs scheme |
| `book.m2` | LaTeX with book-format styling |

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-expressions.md`](file-expressions.md) — AST input.
- [`file-hypertext.md`](file-hypertext.md) — hypertext AST input.
- [`file-html.md`](file-html.md) — closest sibling formatter.
- [`file-document.md`](file-document.md), [`file-installPackage.md`](file-installPackage.md)
  — pipeline.
