# `M2/Macaulay2/system/` — thread supervisor

The Macaulay2 binary runs under a small **supervisor process** that manages a
pool of worker threads and provides the boundary across which user-interrupt
signals are delivered. The supervisor sources live here.

## Files

| File | Role |
|---|---|
| `supervisor.hpp`, `supervisor.cpp` | The supervisor itself — a pthread-based task scheduler that the interpreter delegates parallel work to |
| `supervisorinterface.h` | C-callable interface used from the `.d`/`.dd` interpreter (specifically [`d/threads.dd`](../d/README.md)) |
| `m2file.hpp`, `m2file.cpp`, `m2fileinterface.h` | Thread-safe file/stream abstraction layered on top of stdio |
| `gc_std.hpp` | C++ standard-library allocator that goes through bdwgc, so STL containers are GC-friendly |
| `mutex.h`, `mutexclass.hpp` | Mutex primitives |
| `pthread-methods.hpp` | Helper templates for pthread-based primitives |
| `m2util.hpp` | Misc utilities shared with the supervisor |
| `tests.cpp` | Standalone tests for the supervisor |
| `Makefile.in`, `Makefile.files`, `CMakeLists.txt` | Build glue |

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
