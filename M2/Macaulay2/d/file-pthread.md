# `pthread.d` and `pthread0.d` — POSIX threads bindings

`pthread.d` and `pthread0.d` provide M2's **POSIX threads (pthread)
bindings** — the low-level layer that
[`threads.dd`](file-threads.md) builds on.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Why two files

Same `0` / non-`0` split as `stdio*.d`, `gmp*.d`:

- **`pthread0.d`** — primitive declarations (no `evaluate` or
  high-level types).
- **`pthread.d`** — operations that need higher-level imports
  (`evaluate`, `expr`).

This lets even foundational modules use `pthread0.d` without
pulling in the evaluator.

## `pthread0.d`

```d
use M2;
declarations "
    #include <M2/gc-include.h>
    #include <../system/mutex.h>
";
export voidPointer := Pointer "void *";
export nullPointer() ::= Ccode(voidPointer,"((void *)0)");
export threadFunction := function(voidPointer):voidPointer;
```

Declares:

- **`voidPointer`** — the `.d` equivalent of `void *`.
- **`nullPointer()`** — constant `NULL`.
- **`threadFunction`** — function-pointer type for pthread workers.

Plus the underlying `pthread_*` declarations.

## `pthread.d`

```d
use M2;
use evaluate;
use expr;

header "#include \"../system/supervisorinterface.h\"";


taskCreatePush(f:function(TaskCellBody):null,tb:TaskCellBody) ::=  Ccode(taskPointer,
```

Adds the **supervisor interface** glue:

- `taskCreatePush(...)` — pushes a new task into the supervisor's
  queue.
- `taskWait(...)` — blocks until a task completes.
- `taskInterrupt(...)`, `taskCancel(...)` — signaling.
- Mutex / condvar wrappers used by the supervisor.

`supervisorinterface.h` lives in `Macaulay2/system/`
([README](../system/README.md)) — the supervisor implementation
proper. `pthread.d` is the interpreter-side ABI for it.

## Layering

```
threads.dd       (M2-level Task type, AtomicInt)
   │
   ▼
pthread.d        (supervisor glue, task push/wait)
   │
   ▼
pthread0.d       (raw pthread_* declarations)
   │
   ▼
system/         (C++ supervisor implementation)
```

## Used by

- [`file-threads.md`](file-threads.md) — primary consumer.
- [`file-system.md`](file-system.md) — system-level threading
  primitives.
- The engine occasionally for thread-safety locks (mostly the
  engine uses its own TBB / OpenMP layer).

## Related

- [`README.md`](README.md) — d/ overview.
- [`../system/README.md`](../system/README.md) — supervisor
  implementation.
- [`file-threads.md`](file-threads.md) — M2-level API on top.
- [`file-atomic.md`](file-atomic.md) — atomic primitives the
  threading layer uses.
