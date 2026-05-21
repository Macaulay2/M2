# `texmacs.m2` — TeXmacs frontend protocol

`texmacs.m2` implements the **TeXmacs frontend protocol** —
the M2-side communication layer used when M2 is running as a
back end for the [TeXmacs](https://www.texmacs.org/) interactive
typesetting system. The file is what makes "M2 inside TeXmacs"
work.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "reals.m2"

TeXmacsBegin = "\2"
TeXmacsEnd   = "\5"
fix  := s -> replace("\2|\5", "\33\\0", s)
fixn := s -> concatenate between("\0", apply(separate("\\0", s), fix))
red  := p -> concatenate("<mstyle color=\"red\">", concatenate p, "</mstyle>")
mathMode := s -> concatenate("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
                              concatenate s, "</math>")
```

The two special byte values are part of TeXmacs's protocol:

- **`\2`** (`STX`) — "begin TeXmacs block."
- **`\5`** (`ENQ`) — "end TeXmacs block."

M2 wraps its output in these markers so TeXmacs can tell what's
typeset output vs. plain text.

The `fix` / `fixn` helpers escape any `\2` or `\5` inside output so
content with those bytes doesn't break the protocol.

## The pipeline

When M2 runs under TeXmacs:

1. M2 detects it's running in TeXmacs mode (env variable or flag).
2. Every result is rendered via this file's helpers:
   - Mathematical output → MathML via
     [`file-mathml.md`](file-mathml.md), wrapped in
     `mathMode(...)`.
   - Text output → plain text, escaped via `fix`.
   - Errors → red MathML via `red(...)`.
3. Output is bracketed with `\2 ... \5` markers.
4. TeXmacs reads the bytes and typesets accordingly.

## `red` for errors

The `red(p)` helper wraps a MathML fragment in a red `<mstyle>`. M2
uses it for error messages and warnings so they stand out
visually in TeXmacs.

## Used by

- M2 users running M2 from inside TeXmacs.
- The `Style` package's TeXmacs-mode test suite.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-mathml.md`](file-mathml.md) — MathML formatter used here.
- [`../d/texmacs.d`](../d/README.md) — engine-side TeXmacs protocol
  primitives.
- TeXmacs — external editor.
