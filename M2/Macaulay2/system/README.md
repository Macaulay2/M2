# `M2/Macaulay2/system/` — thread supervisor

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (three-layer architecture, `ThreadTask` lifecycle, thread-pool sizing, GC integration, per-thread file-handle state, C/C++ split, synchronisation primitives, "how to extend").

The Macaulay2 binary runs under a small **supervisor process** that manages a
pool of worker threads and provides the boundary across which user-interrupt
signals are delivered. The supervisor sources live here.

## Files

| File | Role | Deep dive |
|---|---|---|
| `supervisor.{hpp,cpp}`, `supervisorinterface.h` | Worker-pool manager backing M2 `Task` / `schedule` / `taskResult` | [`file-supervisor.md`](file-supervisor.md) |
| `m2file.{hpp,cpp}`, `m2fileinterface.h` | `M2File` per-thread file-handle state with sync/unsync modes | [`file-m2file.md`](file-m2file.md) |
| `mutex.h`, `mutexclass.hpp`, `pthread-methods.hpp`, `gc_std.hpp` | Spinlocks, mutex class, pthread portability, GC-aware STL allocators | [`file-mutex.md`](file-mutex.md) |
| `m2util.hpp` | Supervisor-side helpers for constructing M2-shaped values | [`file-m2util.md`](file-m2util.md) |
| `tests.cpp` | Standalone supervisor self-tests | [`file-tests.md`](file-tests.md) |
| `Makefile.in`, `Makefile.files`, `CMakeLists.txt` | Build glue | — |

**Coverage:** every source file in this directory has a dedicated deep-dive doc.

## Why a separate process?

The supervisor exists primarily to:

- Insulate the interpreter from signal-handler nastiness across pthread
  implementations.
- Provide a single point where worker threads can be safely cancelled.
- Centralise the bookkeeping needed to make Boehm GC cooperate with multiple
  threads (each thread must be registered with bdwgc).

The thread-related top-level work is exposed to M2 code through
[`d/threads.dd`](../d/README.md) and ultimately
[`m2/threads.m2`](../m2/README.md).

## Related

- bdwgc (vendored as a [submodule](../../submodules/README.md)) — the GC the
  supervisor cooperates with.
- [`Macaulay2/bin/`](../bin/README.md) — links the supervisor into the final
  binary.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
