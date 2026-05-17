# `threads.dd` — M2-level threading primitives

`threads.dd` provides the **M2-level threading primitives** —
`Task`, `schedule`, `cancelTask`, `taskResult`, plus the `AtomicInt`
type. It is the `.dd` companion to the supervisor in
[`../system/`](../system/README.md) that exposes threading to M2
user code.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
use pthread;
```

The file imports the lower-level [`pthread.d`](README.md) bindings
and builds the M2-facing API on top.

## What `threads.dd` exposes

The full `Task` API the M2 layer uses:

- **`Task`** — M2 type representing a unit of background work.
- **`createTask(f)`** — wrap a function as a task.
- **`schedule(task)`** — submit to the supervisor.
- **`taskResult(task)`** — block until completion, return result.
- **`cancelTask(task)`** — request cancellation.
- **`isReady(task)`, `isDone(task)`** — non-blocking checks.

Plus atomic primitives:

- **`AtomicInt`** — thread-safe integer cell.
- **Atomic operations** — `+=`, `-=`, compare-and-swap.

## Supervisor handoff

The actual scheduling and worker-thread management happen in
[`../system/`](../system/README.md). `threads.dd` is the
interpreter-side veneer that turns M2 calls into supervisor
operations:

- `schedule(task)` → `supervisor_schedule_task(...)`.
- `cancelTask(task)` → `supervisor_cancel(...)`.
- `taskResult(task)` → blocks on supervisor's per-task condvar.

## `.dd` not `.d`

The C++-only form is needed because:

- The supervisor's C++ API uses STL containers.
- Some pthread features require C++ exception-safety.
- The `AtomicInt` implementation uses `std::atomic`.

## Used by

- [`../m2/file-threads.md`](../m2/file-threads.md) — M2-side `Task`
  type.
- M2 user code calling `schedule`, `createTask`, etc.
- The example / test runner ([`../m2/file-examples.md`](../m2/file-examples.md))
  uses tasks for time-limited execution.

## Related

- [`README.md`](README.md) — d/ overview.
- [`../system/README.md`](../system/README.md) — supervisor.
- `pthread.d`, `pthread0.d` — lower-level POSIX threads bindings.
- [`../m2/file-threads.md`](../m2/file-threads.md) — M2-side
  consumer.
- [`../e/file-m2tbb.md`](../e/file-m2tbb.md) — engine's TBB wrapper
  (orthogonal parallelism story).
