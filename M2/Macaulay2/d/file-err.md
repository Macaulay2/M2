# `err.d` and `errio.d` — error reporting

`err.d` and `errio.d` together implement the interpreter's
**error-reporting machinery** — `error`, `warning`, deduplication,
formatting, and the link between caught errors and the interactive
debugger.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Two-file split

The same `0`-suffix / non-suffix pattern that other low-level
files use:

- **`err.d`** — error-reporting primitives that don't need stdio.
- **`errio.d`** — error-reporting primitives that do need stdio
  (writing to stderr, formatting positions, etc.).

The split lets parser-level code call `error()` without pulling in
stdio.

## What's in `err.d`

```d
--		Copyright 1994 by Daniel R. Grayson
use stdio;

warnings := 0;
export warning(msg:string):void := (
    warnings = warnings + 1;
    stdIO << flush;
    endLine(stdError);
    ...
)
```

(The "`use stdio`" line is somewhat misleading — `err.d` actually
sits below stdio in some configurations; this is one of the
historical bits of layering.)

The `warning(msg)` function:

1. Increments a global warning counter.
2. Flushes stdout (so warnings appear in chronological order).
3. Ends any current line on stderr.
4. Writes the warning message.

## What `errio.d` adds

The stdio-dependent extras:

- **Position-aware error formatting** — "file.m2:42:7: error: …".
- **Backtrace generation** — for the interactive debugger.
- **Color-coded output** when stderr is a terminal.

## How errors flow

An error in `.d` code:

1. Calls `error()` or `WrongArg()` or similar.
2. These set a thread-local "error pending" flag.
3. Higher-level callers check the flag after every operation.
4. The interpreter's main loop sees the flag, formats and prints,
   resets, returns to prompt.

The pattern avoids C++ exception unwinding across the `.d`-generated
C boundary.

## Used by

- Every `.d` file that reports errors — i.e., basically all of them.
- [`file-evaluate.md`](file-evaluate.md) — the evaluator checks for
  errors after every operation.
- [`file-parser.md`](file-parser.md) — parse errors.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-interrupts.md`](file-interrupts.md) — sister flag-based
  mechanism for Ctrl+C.
- [`../m2/file-debugging.md`](../m2/file-debugging.md) — M2-side
  error / warning routing.
- [`../e/file-error.md`](../e/file-error.md) — engine-side error
  reporting (counterpart on the other side of the boundary).
