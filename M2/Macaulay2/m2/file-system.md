# `system.m2` — system commands and external processes

`system.m2` provides M2's interface to the **operating system** —
shell command execution, environment variables, process spawning,
and similar OS-level operations.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "methods.m2"
needs "remember.m2"

-- This version of 'run' doesn't handle pipes or redirection, of course
-- but it's an advantage to have this facility without depending on an outside shell.
-- We comment it out because some systems don't have wordexp() in libc, upon which
-- expandWord is based.
-- run = cmd -> if (pid := fork()) == 0 then exec expandWord cmd else wait pid
```

The commented-out experimental `run` shows the file's evolution:
the maintainers tried a shell-free implementation, found portability
issues (`wordexp()` is missing on some systems), and reverted to a
shell-based path. The comment is preserved for future revisits.

## User-facing API

- **`run cmd`** — execute `cmd` via `/bin/sh -c`. Returns the
  process exit status.
- **`chkrun cmd`** — `run` plus error on non-zero exit.
- **`getenv name`**, **`setenv(name, value)`** — environment
  variables.
- **`get "!cmd"`** — read the stdout of `cmd` as a string (the `!`
  prefix on the filename triggers process execution).
- **`getViewer (var, default)`** — find an external viewer
  application (used by `viewHelp` in
  [`file-help.md`](file-help.md)).
- **`fork`, `exec`, `wait`** — direct POSIX wrappers.

## Why `run` lives here

`run` (and the related shell-pipeline helpers) is the single most
common point where M2 hands control to an external program. Bundling
the related helpers here gives one consistent error-handling story
across all of them.

## Cross-platform abstraction

The file defers a lot to the underlying engine / runtime
(`d/system.d`), which handles the platform differences. `system.m2`
adds M2-language-friendly wrappers:

- `run` returns the exit status as an `Int`, never raises.
- `chkrun` raises an `error` with the command on failure.
- `get "!cmd"` integrates with `<<` and the get-from-file path so
  the user doesn't need a separate API for "read from file" vs.
  "read from process."

## Used by

- Every M2 path that calls an external tool — `installPackage`
  (which calls `pdflatex`, `texi2dvi`, etc.), `viewHelp` (which calls
  the user's browser), package builds that invoke `make`, etc.
- [`file-examples.md`](file-examples.md) — spawns subprocesses to
  run examples.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../d/system.d`](../d/README.md) — engine-side primitives.
- [`file-files.md`](file-files.md) — file system I/O sibling.
- `regex.m2` — text manipulation often consumed alongside `run`.
- [`file-help.md`](file-help.md) — uses `getViewer`.
