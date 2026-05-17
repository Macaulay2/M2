# `printing.m2` — `pad` and printing-pipeline primitives

`printing.m2` provides **printing primitives** the rest of the M2
output pipeline builds on — most notably `pad`, the
fixed-width-string formatter. It is the bottom layer that
[`file-nets.md`](file-nets.md), `pretty.m2`, and `format.m2` sit on.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "max.m2"
needs "methods.m2"

pad = method()

pad(String, ZZ) := String => (s, n) -> concatenate(s, n - #s)
pad(ZZ, String) := String => (n, s) -> concatenate(n - #s, s)
pad(Net, ZZ)    := (S, n) -> S | (concatenate(n - width S))^(-depth S)
```

The **`pad`** method's three overloads:

- **`pad(s, n)`** — right-pad string `s` to width `n` (left-align).
- **`pad(n, s)`** — left-pad string `s` to width `n` (right-align).
- **`pad(net, n)`** — pad a net to width `n` while preserving its
  baseline.

`pad` is the workhorse of tabular output:

```m2
"alpha" | pad("beta", 10) | "gamma"
-- "alphabeta      gamma"
```

## The printing pipeline

M2's output for a value `v` goes through several layers:

1. **`expression v`** — produces a [`file-expressions.md`](file-expressions.md) AST.
2. **Formatter** (`net`, `html`, `tex`, …) — walks the AST to
   produce a `Net`, an HTML string, etc.
3. **`pad`** — applies width / alignment to the formatted output.
4. **Output** — writes the result to stdout or a file.

`printing.m2` sits at level 3. The companion files
[`file-nets.md`](file-nets.md) (level 2 for terminal output) and
`format.m2` (level 1-2 for arithmetic-aware formatting) make up the
rest of the pipeline.

## `unbag`

The file also defines `unbag` — used to flatten the wrapped values
that some computations produce. It is referenced by
[`file-gb.md`](file-gb.md) and similar files that hand back results
in opaque "bag" wrappers.

## Used by

- Every formatter — `net`, `texMath`, `html`, `markdown`, `tex`.
- M2's `<<` operator at the prompt.
- Status-line / progress-bar code in long-running operations.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-nets.md`](file-nets.md) — terminal-output 2-D grid.
- [`file-expressions.md`](file-expressions.md) — `Expression` AST.
- `pretty.m2`, `format.m2` — adjacent formatters.
