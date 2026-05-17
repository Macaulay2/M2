# `supervisor.{cpp,hpp}`, `supervisorinterface.h` — the thread supervisor

`supervisor.cpp` / `supervisor.hpp` / `supervisorinterface.h`
together implement **M2's thread supervisor** — the worker-pool
manager that backs M2-level `Task`, `schedule`, `taskResult`,
and `cancelTask`.

Part of the [`system/` directory](README.md).

[← system/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```cpp
#include <stdlib.h>
#include <assert.h>

#include <atomic>
#include <iostream>
#include <chrono>
#include <thread>
#include <mutex>
using std::lock_guard;

// The maximum number of concurrent threads
const static unsigned int numCores = std::thread::hardware_concurrency();
// We allocate between 5 and 17 threads initially, to save trouble with memory allocation.
const static int maxNumThreads = ((numCores < 4) ? 4 : (16 < numCores ? 16 : numCores)) + 1;
```

Right at the top: the **thread-pool sizing heuristic**. M2
allocates `max(4, min(16, numCores)) + 1` threads at startup.
The "+1" is for the main interpreter thread (which is I/O bound,
not CPU bound).

## The `ThreadTask` struct

```cpp
struct ThreadTask
{
  ...
};
```

Each user-submitted unit of work is a `ThreadTask`. The struct
tracks:

- The function to call (`ThreadTaskFunctionPtr`).
- The argument.
- Status (pending / running / done / cancelled).
- The result.
- A condition variable to signal completion.
- Any cancellation request.

The supervisor maintains a **queue of pending `ThreadTask`s** and
a **set of running tasks**. Workers consume from the queue;
results live with the `ThreadTask` until the caller reads them.

## `supervisorinterface.h` — C-callable bridge

```c
#ifndef _system_supervisorinterface_h_
#define _system_supervisorinterface_h_

#include <M2/gc-include.h>

typedef void* (*ThreadTaskFunctionPtr)(void*);

#define GETSPECIFICTHREADLOCAL
#ifdef GETSPECIFICTHREADLOCAL
#define THREADLOCAL(x,typ) (*((typ*)TS_Get_Local(x##_id)))
#define THREADLOCALDECL(typ,x) int x##_id
#define THREADLOCALINIT(x) TS_Add_ThreadLocal(&x##_id,#x)
#else
#define THREADLOCAL(x,typ) x
#define THREADLOCALDECL(typ,x) typ x
#define THREADLOCALINIT(x) x
#endif

#ifdef __cplusplus
extern "C" {
```

The header defines the **C-callable API** the interpreter (via
[`../d/file-pthread.md`](../d/file-pthread.md)) calls into.

The `THREADLOCAL` macro indirection is interesting: in the
"GETSPECIFICTHREADLOCAL" mode, each declared thread-local actually
goes through a `TS_Get_Local(id)` call that resolves via the
supervisor's own pthread-key registry. This lets the supervisor
introspect what each thread thinks its locals are — useful for
debugging and the M2 debugger's thread-aware breakpoints.

## Why a supervisor at all

Without one, M2's threading would map directly to pthreads. But:

- **GC interaction**: Boehm GC needs to know about all threads,
  and stop them at GC pauses. Centralised thread management makes
  this easier.
- **Cancellation**: pthreads' `pthread_cancel` is fragile across
  platforms; the supervisor implements its own polite cooperative
  cancellation.
- **M2-level API consistency**: M2 wants `Task`/`schedule`/...
  with simple semantics regardless of the underlying threading
  primitives.

## Lifecycle of a task

```
M2: t = schedule(f, args)
   ↓ (via d/pthread.d's taskCreatePush)
Supervisor: enqueue ThreadTask
   ↓
Worker: pop ThreadTask, run f(args)
   ↓
Worker: store result, signal completion
   ↓
M2: taskResult(t)
   ↓ (via d/pthread.d's taskWait)
Supervisor: wait for completion, return result
```

## Used by

- [`../d/file-pthread.md`](../d/file-pthread.md) — interpreter's
  pthread bindings.
- [`../d/file-threads.md`](../d/file-threads.md) — M2-level
  `Task` API on top.
- [`../bin/file-main.md`](../bin/file-main.md) — boots the
  supervisor.

## Related

- [`README.md`](README.md) — system/ overview.
- [`file-m2file.md`](file-m2file.md) — sister file-handle
  state-management module.
- [`file-mutex.md`](file-mutex.md) — mutex primitives.
- [`file-tests.md`](file-tests.md) — supervisor self-tests.
