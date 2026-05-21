# `debugging.m2` — debugging helpers and warning machinery

`debugging.m2` provides M2's **debugging helpers** — warning
messages, error formatting, `FilePosition`, the `debug` mode flag,
plus interactive-debugger entry points.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
needs "nets.m2"
needs "methods.m2"

warningMessage0 = (args, deb) -> (
    args = processErrorArgs args;
    h := hash args % 10000;
    if debugWarningHashcode === h
    then error args
    ...
)
```

The header shows the **opt-in error-on-warning** mechanism: setting
`debugWarningHashcode` to a specific hash makes M2 raise an error
the next time it sees a warning whose hash matches. This is how the
user debugs "I keep getting this warning but no stack trace tells me
where" issues.

## What's exposed

- **`FilePosition`** — type representing `(filename, start_line,
  end_line, start_col, end_col)`. Used by
  [`file-code.md`](file-code.md) for `locate` / `code`.
- **`error` / `warning`** — the entry points for raising errors and
  warnings.
- **`debug` flag** — when true, M2 drops into the interactive
  debugger on error.
- **`stackTrace`** — formatted stack-trace output.

## The interactive debugger

When `debug` is on and an error occurs, M2 stops and presents the
user with an interactive prompt scoped to the error's stack frame.
The user can inspect local variables, evaluate expressions in the
error's context, continue, abort, etc.

The debugger is what makes `errorDepth`, `debuggingMode`, and
similar interactive-debugging flags meaningful.

## Warning suppression

Warnings dedupe by content: the same warning hash within a single
session prints only once. This avoids spamming the user with
repeated warnings from inside loops. The mechanism is in
`warningMessage0`.

## Used by

- Every M2 file that raises an error or warning.
- [`file-code.md`](file-code.md) — `FilePosition` consumer.
- M2 users debugging packages — `debug = true`.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-code.md`](file-code.md) — uses `FilePosition`.
- [`file-nets.md`](file-nets.md) — formats error / warning output.
- `profile.m2`, `remember.m2` — sister utility files.
