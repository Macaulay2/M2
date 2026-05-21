# `interrupts.d` — Ctrl+C / signal handling

`interrupts.d` implements the interpreter's **interrupt handling** —
the thread-local flags and `interrupted()` check that let
long-running computations bail out cleanly when the user presses
Ctrl+C.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994-2006,2010 by Daniel R. Grayson

use atomic;
use arithmetic;

export threadLocal interruptShield := false;
export threadLocal interruptPending := false;
export threadLocal alarmedFlag := false;
```

Three thread-local flags:

- **`interruptShield`** — when true, the current thread refuses
  interruption (used during cleanup code that must complete).
- **`interruptPending`** — set by the signal handler when SIGINT
  arrives; checked by polling code.
- **`alarmedFlag`** — set when SIGALRM arrives (for `alarm`-based
  timeouts).

The `threadLocal` keyword guarantees each thread has its own copy —
critical for the supervisor's parallel workers.

## How interrupts flow

The full chain when the user presses Ctrl+C:

1. Kernel delivers `SIGINT` to the M2 process.
2. Signal handler (installed in
   [`file-M2lib.md`](file-M2lib.md)) sets `interruptPending = true`.
3. Every long-running inner loop polls `interrupted()` periodically.
4. When the poll returns `true`, the loop bails out with a
   `Computation interrupted` status.
5. The interpreter's main loop sees the bail-out, resets the flag,
   returns to the prompt.

The pattern depends on every inner loop polling regularly. The
engine side has its own version of this in
[`../e/file-interrupted.md`](../e/file-interrupted.md).

## `interruptShield`

Some operations must complete atomically — for example, freeing a
GB intermediate state, finalising an open file. Wrapping such code
in `interruptShield = true; ... ; interruptShield = false;` blocks
SIGINT delivery during the critical region. The signal isn't lost;
it just gets deferred until the shield drops.

## Atomic operations

The `use atomic` import is essential — the interrupt flags are
read concurrently from the main thread and the signal handler.
Atomic reads/writes ensure no torn-write race conditions.

## Used by

- Every long-running interpreter loop —
  [`file-evaluate.md`](file-evaluate.md), `actors*.d` all poll.
- The engine via the boundary in
  [`file-engine-dd.md`](file-engine-dd.md).
- File-loading paths.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-err.md`](file-err.md) — error-flag analogue.
- [`../e/file-interrupted.md`](../e/file-interrupted.md) — engine's
  parallel `system_interrupted()` mechanism.
- `atomic.d`, `atomic2.d` — atomic primitives used here.
