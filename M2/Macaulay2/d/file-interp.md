# `interp.dd` — the top-level interpreter loop

`interp.dd` is the **top-level interpreter loop** — the read-eval-
print loop (REPL) that drives an M2 session. It wires together the
parser, the evaluator, and the printer.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994-2000 by Daniel R. Grayson

newStartupMethod := true;            -- for testing purposes
use evaluate;
use parser;
use texmacs;
use actors5;
use profiler;

import dirname(s:string):string;

dummyError := Error(dummyPosition, "dummy error message", nullE, false, dummyFrame);

export setupargv():void := (
    setupconst("commandLine", toExpr(argv));
    ...
)
```

The flag `newStartupMethod := true` tracks an older code path
toggle that defaults to the new approach.

`setupargv()` exports the command-line arguments to M2-level code as
the global `commandLine`.

## The `.dd` file extension

`interp.dd` is a `.dd` file rather than `.d` — it compiles to C++
rather than plain C. The `.dd` flavour is for parts of the
interpreter that need C++ features (`std::vector`, templates, RAII)
in their generated code; `.d` is for the parts that stay in plain C.

`interp.dd` uses C++ because:

- It interacts with the C++ engine.
- It calls into TeXmacs / WebApp protocol code that uses C++
  STL.
- It uses Boost.Stacktrace for crash reports.

## REPL flow

The classic loop:

```text
loop {
    prompt
    line = read_line()
    if line is end-of-input: break
    expr = parse(line)
    if parse error: report and continue
    value = evaluate(expr)
    if eval error: report and continue
    if not silent: print(value)
}
```

The reality is more nuanced (handling Ctrl+C, abort signals,
multi-line input, debugger entry, ...) but the shape is the same.

## Hand-off from `M2lib.c`

[`M2lib.c`](file-M2lib.md) does the system-level setup and then
calls into `interp.dd`'s startup function. From that point on, M2
is in interactive mode.

## Used by

- The M2 binary's main entry point.
- Every interactive M2 session.
- `load`, `needs` — invoke the parser + evaluator with a file as
  input, bypassing the prompt.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-M2lib.md`](file-M2lib.md) — system startup.
- [`file-parser.md`](file-parser.md), [`file-evaluate.md`](file-evaluate.md)
  — building blocks.
- `texmacs.d` ([`file-texmacs-d.md`](README.md)) — TeXmacs protocol
  alternative loop.
- `actors5.d` — operator implementations.
- `profiler.dd` — profiling support.
