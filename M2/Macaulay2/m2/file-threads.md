# `threads.m2` — `AtomicInt` and threading primitives

`threads.m2` is the M2-side wrapper for the engine's threading
machinery — `Task`, `schedule`, `cancelTask`, plus the
**`AtomicInt`** type for thread-safe integer arithmetic.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## `AtomicInt`

```m2
-----------------------------------------------------------------------------
-- AtomicInt
-----------------------------------------------------------------------------

AtomicInt.synonym = "atomic integer"

scan({symbol +=, symbol -=, symbol &=, symbol |=, symbol ^^=},
    op -> typicalValues#(op, AtomicInt) = ZZ)

store = method()
```

`AtomicInt` is a **thread-safe integer cell**. Multiple threads
can read and update it without locks, using atomic operations.

The `scan(...)` block registers `+=`, `-=`, `&=`, `|=`, `^^=` (XOR)
as operations that return a regular `ZZ`. This means:

```m2
x = atomicInt(0)
x += 1      -- atomic increment; returns the new value
```

The atomic write returns the integer result so the user can chain
operations safely.

## Task API

The rest of `threads.m2` exposes:

- **`createTask f`** — wrap a function as a `Task`.
- **`schedule task`** — submit the task for execution.
- **`taskResult task`** — block until completion and return result.
- **`cancelTask task`** — request cancellation.
- **`isReady task` / `isDone task`** — non-blocking status checks.

The implementation routes everything to the engine's
[`system/` supervisor](../system/README.md) via
[`../d/threads.dd`](../d/README.md).

## Concurrency model

Macaulay2 supports **cooperative multi-tasking** at the M2 level:

- A task runs until it blocks (e.g., on I/O or a long computation
  that voluntarily yields).
- The supervisor manages a thread pool.
- bdwgc handles GC across threads (each thread is registered).

True parallelism is supported but rare in M2 code — most parallel
work happens *inside* the engine (F4, NCF4, schreyer-resolution)
via TBB. The user-level `Task` API is for orchestration, not
embarrassingly-parallel arithmetic.

## Used by

- M2 packages that schedule background work
  (`NumericalAlgebraicGeometry`'s parallel path tracking, etc.).
- The supervisor's own bookkeeping.
- [`file-examples.md`](file-examples.md) — uses tasks to time-limit
  example execution.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../system/README.md`](../system/README.md) — supervisor source.
- [`../d/threads.dd`](../d/README.md) — interpreter binding.
- [`../e/file-m2tbb.md`](../e/file-m2tbb.md) — engine TBB wrapper.
- [`../tests/threads/README.md`](../tests/threads/README.md) —
  threading tests.
