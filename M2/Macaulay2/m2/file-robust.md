# `robust.m2` — `timelimit` and error-printing safety

`robust.m2` provides **`timelimit`** and related operations that
wrap unreliable M2 code with timeouts and crash-resistance. The
header announces the file's intended fate.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
-- all this code should go!!

needs "lists.m2"
needs "max.m2"
needs "nets.m2"

timelimit := (t, f) -> (alarm t; r := f(); alarm 0; r)

printingTimeLimit = 20
errorPrintingTimeLimit := 3
```

The "-- all this code should go!!" header is unambiguous: the
author wants to delete this file. The pieces survived because
removing them carefully would break edge cases. They're documented
here so the reader understands the intent.

## `timelimit`

```m2
timelimit := (t, f) -> (alarm t; r := f(); alarm 0; r)
```

Run `f` with a `t`-second SIGALRM. If `f` exceeds the time limit,
the engine raises an interrupt. The pattern works for arbitrary M2
functions but is fragile — `alarm`-based time-limits don't compose
well with nested calls.

## Output time limits

```m2
printingTimeLimit = 20
errorPrintingTimeLimit := 3
```

These cap how long M2 spends formatting output. If printing a value
exceeds the limit, M2 aborts the print and shows a truncated form.
This is what prevents `<< huge_matrix` from locking the prompt.

`printingTimeLimit` is **mutable** so the user can override it;
`errorPrintingTimeLimit` is private and used internally when
formatting error messages.

## Why the file should go

The functionality is real but the implementation is unsatisfactory:

- `alarm`-based time limits are fragile.
- The "robust" concept is too vague — different callers want
  different kinds of robustness.

The plan is to replace this with proper `Task`-based time-limit
machinery from [`file-threads.md`](file-threads.md) and to push
output-time-limit logic into [`file-format.md`](file-format.md).
Until then, `robust.m2` survives.

## Used by

- M2's error / output paths via `errorPrintingTimeLimit` and
  `printingTimeLimit`.
- A handful of packages that explicitly call `timelimit`.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-threads.md`](file-threads.md) — `Task` (the planned
  replacement).
- [`file-format.md`](file-format.md) — printing pipeline.
- [`file-debugging.md`](file-debugging.md) — adjacent error
  handling.
