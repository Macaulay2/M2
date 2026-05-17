# `texmacs.d` — TeXmacs frontend bindings

`texmacs.d` implements the **interpreter side of the
[TeXmacs](https://www.texmacs.org/) frontend protocol** — the
DATA-BEGIN / DATA-END escape sequences that the TeXmacs editor
uses to talk to M2 as a backend.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 2000 by Daniel R. Grayson

use getline;
use util;
use evaluate;

TeXmacsEvaluate := makeProtectedSymbolClosure("TeXmacsEvaluate");
```

The single `TeXmacsEvaluate` symbol closure is the **named hook**
that M2 user code can override to customise TeXmacs evaluation.

## The TeXmacs protocol (brief)

TeXmacs talks to its backends over stdin/stdout with framing
characters:

```
\002verbatim:\005   ← prompt (DATA-BEGIN ... DATA-END)
M2 sends output:    verbatim:foo\005
TeXmacs sends:      print "hello"\n\005
M2 responds:        verbatim:hello\005
```

The framing characters (`\002`, `\005`) bracket each "data
message" so TeXmacs can distinguish M2 output from terminal
control sequences.

## What `texmacs.d` does

- Sets up `--texmacs` startup mode (detects the flag).
- Wraps the M2 main read-eval-print loop with TeXmacs framing.
- Translates M2 nets / pretty-printed output into TeXmacs's
  schemeXml-flavored markup.
- Routes prompts, errors, and side-output through the appropriate
  framed channel.

## How users invoke it

```sh
M2 --texmacs    # used by the TeXmacs M2 plugin
```

The TeXmacs plugin (a small Scheme file shipped with TeXmacs)
spawns `M2 --texmacs` and pipes user input / displays output.

## Used by

- The TeXmacs M2 plugin (lives in the TeXmacs distribution).
- Less common today than `--script` or Jupyter-based
  frontends, but still maintained.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-xml.md`](file-xml.md) — sister XML protocol bindings.
- [`../m2/file-texmacs.md`](../m2/file-texmacs.md) — M2-side
  protocol helpers (encoding M2 values as TeXmacs trees).
- [`../m2/file-mathml.md`](../m2/file-mathml.md) — MathML output
  used by TeXmacs's HTML mode.
