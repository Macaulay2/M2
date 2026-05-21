# `debugging.dd` — interactive debugger backend

`debugging.dd` implements the **backend of M2's interactive
debugger** — the `debug ...` REPL the interpreter drops into when
an error occurs (with `debugError = true`).

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
use tokens;
use common;
use util;
use hashtables;

-- debugger is not defined until interp.dd
-- so we use a pointer and populate it later.
dummydebugger(f:Frame,c:Code):Expr := ( ... );
```

The "debugger is not defined until `interp.dd`" comment is the
key: `debugging.dd` defines the *infrastructure* (where to call
into, what state to capture), but the actual REPL is wired in
from [`file-interp.md`](file-interp.md). A function-pointer
mechanism lets the interpreter swap the dummy out for the real
debugger at startup.

## What the debugger captures

When an error fires and `debugError = true`:

1. The evaluator catches the error before unwinding.
2. The current `Frame` (activation record) is captured.
3. The current `Code` (source position) is captured.
4. Control transfers to the debugger.
5. User can inspect locals, step, evaluate sub-expressions, etc.
6. User types `continue` to bail out.

`debugging.dd` is where steps 1–4 are implemented; the REPL itself
is in [`file-interp.md`](file-interp.md).

## Locals introspection

The debugger walks the captured `Frame`:

- Frame contains `valuesUsedInFrame` — array of slot values.
- The corresponding `Symbol` for each slot is in the closure's
  `Dictionary`.
- The debugger zips these together to produce a `name → value`
  display.

## Used by

- [`file-interp.md`](file-interp.md) — provides the actual REPL.
- M2 users running `M2 --debug` or setting `debugError = true`.
- [`../m2/file-debugging.md`](../m2/file-debugging.md) — M2-side
  user-facing API.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-evaluate.md`](file-evaluate.md) — error-catch hook.
- [`file-interp.md`](file-interp.md) — REPL.
- [`../m2/file-debugging.md`](../m2/file-debugging.md) — M2-side
  wrapper.
- [`file-err.md`](file-err.md) — error-flag mechanism.
