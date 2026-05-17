# `M2/Macaulay2/tests/threads/` — concurrency / thread-supervisor tests

This directory contains **thread-supervisor tests** that exercise
Macaulay2's threading machinery — the
[`Macaulay2/system/`](../../system/README.md) supervisor process and
the M2-level `Thread` / `Task` types.

[← back to tests overview](../README.md)

## What's in here

| File | Role |
|---|---|
| `Makefile.in` | Build glue (templated) |
| `schur-2.m2` | A small Schur-ring computation runnable concurrently as a smoke test |

The suite is **intentionally small** today. The supervisor is
stable, so the regression-test surface is narrow. Most threading
edge cases are caught by the supervisor's own unit tests in
[`../../system/`](../../system/README.md).

## What threading covers

When an M2 user spawns a `Thread`, the supervisor:

1. Allocates a worker thread and registers it with bdwgc.
2. Initialises a per-thread error / interrupt state.
3. Runs the supplied M2 closure under that thread.
4. Cleans up when the thread terminates.

Tests in this directory verify that:

- Concurrent computations produce consistent results.
- Threads can be interrupted safely.
- GB / resolution computations release thread resources on
  completion.

## Triggering

```sh
ctest -R "threads" --output-on-failure
```

## Why so few tests

The supervisor predates much of the surrounding engine and has been
stable for years. New thread-related work tends to happen at the M2
level (`Task`, `schedule`, `cancel`) and gets its own unit tests in
[`../normal/`](../normal/README.md).

If the supervisor's internals change substantially, this suite is
the right place for additional tests.

## Related

- [`../README.md`](../README.md) — overall test-suite overview.
- [`../../system/README.md`](../../system/README.md) — supervisor
  source.
- [`../../d/threads.dd`](../../d/README.md) — interpreter binding for
  threads.
- [`../../m2/threads.m2`](../../m2/README.md) — M2-side
  `Thread`/`Task` wrappers.
