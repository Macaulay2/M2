# `webapp.m2` — M2 web-app frontend protocol

`webapp.m2` implements the **web-app frontend protocol** — the
M2-side communication layer used when M2 runs as a back end for the
[Macaulay2Web](https://github.com/pzinn/Macaulay2Web) browser-based
interface.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Paul Zinn-Justin 2018-2022

needs "expressions.m2"
needs "matrix1.m2"
needs "monideal.m2"

-- topLevelMode=WebApp definitions
-- tags are required to help the browser app distinguish html from text
webAppTags = apply((17, 18, 19, 20, 28, 29, 30, 14, 21), ascii);
    ( webAppHtmlTag,    -- indicates what follows is HTML ~ <span class='M2Html'>
      ...
    )
```

Authored by Paul Zinn-Justin (2018-2022). Most of the work on
`Macaulay2Web` lives in a separate repository; this file is the
M2-side bridge.

## What `WebApp` mode does

When the user runs M2 with `topLevelMode = WebApp`:

- Output is **framed with special bytes** (control characters 17,
  18, 19, …) so the JavaScript frontend can demarcate HTML, math,
  errors, etc. from plain text.
- Some output paths are **redirected to HTML** rather than plain
  nets — the browser can render math beautifully with KaTeX.
- The interactive prompt's lifecycle (input, output, error) is
  exposed for the frontend to capture.

## Special framing bytes

The control bytes (`STX`, `ETX`, `EOT`, etc.) are the protocol
tokens. Each one marks the start of a specific content type the
frontend should treat differently:

- `webAppHtmlTag` (`\17`) — what follows is HTML.
- Variants for math, error messages, code blocks, etc.

The frontend reads the bytes and dispatches each tagged chunk to the
right renderer.

## Comparison with TeXmacs

| | TeXmacs | WebApp |
|---|---|---|
| Framing | `\2 ... \5` | Multiple control bytes |
| Math format | MathML | HTML+KaTeX |
| Author | Grayson | Zinn-Justin |
| File | [`file-texmacs.md`](file-texmacs.md) | `webapp.m2` (this) |

The two protocols address the same problem (frame M2 output for an
external frontend) with different design choices.

## Used by

- M2 users running M2 inside Macaulay2Web.
- The Macaulay2Web JavaScript frontend.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-texmacs.md`](file-texmacs.md) — sister protocol for TeXmacs.
- [`file-expressions.md`](file-expressions.md), [`file-html.md`](file-html.md)
  — used by the renderer.
- Macaulay2Web — external frontend.
