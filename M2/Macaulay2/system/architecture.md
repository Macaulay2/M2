# Thread supervisor architecture

This document is the **architectural reference** for
`M2/Macaulay2/system/` — the **thread supervisor** that backs
M2's user-visible `Task` / `schedule` / `taskResult` API.
Coordinates worker threads, GC-cooperates with Boehm GC, and
manages per-thread file-handle state.

[← system/ overview](README.md) · [← repository TOC](../../../README.md)

## Position in the four-language stack

```
.d / .dd     ──scc1──▶   .c / .cpp     ──C/C++──▶   M2-interpreter ──▶ M2
                                                          ▲
                                                          │ linked
                                                      M2-engine (C++)
                                                          │
                                                          │ uses
                                                          ▼
                                              M2-supervisor (system/)
                                              ← you are here
```

The supervisor is a **separate static library** linked into the
final M2 binary. It sits below the engine and interpreter,
providing threading and per-thread state to both.

## Why a dedicated supervisor

Without one, M2's threading would map directly to pthreads. But:

1. **GC interaction** — Boehm GC needs to know about all threads
   and stop them at GC pauses. Centralised thread management
   makes this easier.
2. **Cancellation** — pthreads' `pthread_cancel` is fragile
   across platforms; the supervisor implements its own
   cooperative cancellation.
3. **Per-thread file-handle state** — `M2File` lets multiple
   threads share `stdout` / `stderr` without scrambling each
   other's output.
4. **M2-level API consistency** — the user-visible `Task` /
   `schedule` API should have simple semantics regardless of
   underlying threading primitives.

## Three-layer architecture

```
┌──────────────────────────────────────────────────────┐
│   M2-level API surface                                │
│   (called from d/threads.dd, d/pthread.d)             │
├──────────────────────────────────────────────────────┤
│   C-callable interface                                │
│   supervisorinterface.h, m2fileinterface.h            │
│   m2util.hpp (M2 value construction)                  │
├──────────────────────────────────────────────────────┤
│   C++ implementation                                  │
│   supervisor.{cpp,hpp} — task scheduling              │
│   m2file.{cpp,hpp} — per-thread file state            │
│   mutex.h, mutexclass.hpp — sync primitives           │
│   pthread-methods.hpp — pthread portability           │
│   gc_std.hpp — GC-aware STL allocators                │
└──────────────────────────────────────────────────────┘
```

## The `ThreadTask` lifecycle

```
M2 user:  t = schedule(f, args)
   ↓ (interpreter, d/pthread.d's taskCreatePush)
Supervisor: enqueue ThreadTask into ready queue
   ↓
Worker:    pop ThreadTask, run f(args)
   ↓
Worker:    store result, signal completion
   ↓
M2 user:   taskResult(t)
   ↓ (interpreter, d/pthread.d's taskWait)
Supervisor: block on per-task condvar, return result
```

Each `ThreadTask` carries:

- The function to call (`ThreadTaskFunctionPtr`).
- The argument.
- Status (pending / running / done / cancelled).
- The result.
- A condition variable to signal completion.
- Any cancellation request.

## Thread-pool sizing

```cpp
const static unsigned int numCores = std::thread::hardware_concurrency();
const static int maxNumThreads = ((numCores < 4) ? 4 : (16 < numCores ? 16 : numCores)) + 1;
```

The pool is `clamp(4, 16, numCores) + 1` threads. The `+1` is the
**main interpreter thread** — I/O bound, doesn't compete with
the CPU-bound workers.

Why clamp:

- **Floor of 4** — even on a single-core machine, M2 benefits
  from a handful of workers for I/O parallelism.
- **Ceiling of 16** — beyond this, memory pressure (each worker
  has its own thread stack + per-thread state) outweighs
  speedup.

## GC integration

Boehm GC needs every thread registered. The supervisor
**registers worker threads** with the GC at creation time and
**unregisters** at destruction. This means:

- The GC's "stop the world" pause stops all worker threads.
- The GC scans each thread's stack for roots.
- New `getmem` allocations from any worker work transparently.

Without supervisor coordination, M2 would deadlock under GC
pressure (worker threads not in the stop-set would race with the
collector).

The GC-aware allocators in `gc_std.hpp` (`gc_map<T,U>`,
`gc_set<T>`) ensure STL containers used by the supervisor are
themselves GC-scanned. Otherwise pointers into GC memory could
be lost.

## Per-thread file-handle state

[`m2file.{cpp,hpp}`](file-m2file.md) provides `M2File`:

```cpp
class M2File {
  int currentThreadMode;
  stdio0_fileOutputSyncState unsyncState;
  pthread_cond_t ownerChangeCondition;
  ...
};
```

Two modes:

| Mode | Behaviour |
|---|---|
| Sync | Acquires an exclusive lock per write; output never interleaves mid-message |
| Unsync | Each thread maintains its own `Net` (2D output state); flushes are explicit |

`stdout` and `stderr` default to sync. Per-task output that the
user wants tagged-per-thread uses unsync.

## C-callable bridge: `supervisorinterface.h`

```c
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
```

The `THREADLOCAL` macro indirection lets thread-locals route
through the supervisor's own pthread-key registry. This gives
the supervisor **introspection** over what each thread thinks
its locals are — useful for the M2 debugger's thread-aware
breakpoints.

## Synchronisation primitives

Three primitives, three trade-offs:

| Primitive | Source | Use |
|---|---|---|
| Spinlock | [`file-mutex.md`](file-mutex.md), `mutex.h` | Short critical sections, low contention |
| `pthreadMutex` | `mutexclass.hpp` | Long waits, RAII-managed |
| Boost atomic / `std::atomic` | inline | Lock-free flags, counters |

Plus `pthread-methods.hpp` for pthread portability across MinGW64
/ glibc / others.

## C / C++ split

The supervisor is **C++-first** (uses `std::atomic`, RAII,
condvars). But it has a **C interface** (`supervisorinterface.h`,
`m2fileinterface.h`) because:

- The interpreter is `scc1`-translated to C — can't easily call
  C++ functions directly.
- The C interface lets future ports (e.g., to a non-C++ JIT) bind
  without reimplementing the supervisor.

The C interface wraps the C++ supervisor in `extern "C"` functions
that internally cast `void*` task pointers to the C++ `ThreadTask`
type.

## How threads coordinate

```
                           ┌────────────────────┐
                           │  task ready queue  │
                           └─────────┬──────────┘
                                     │ pop
            ┌──────────┐  schedule   ▼  schedule  ┌──────────┐
            │ worker 1 │ ◀───────  pool  ───────▶ │ worker N │
            └────┬─────┘                          └─────┬────┘
                 │ run task                             │ run task
                 ▼                                      ▼
            result store                          result store
                 │                                      │
                 │ signal                               │ signal
                 ▼                                      ▼
            ┌──────────────────────────────────────────────┐
            │   parent thread waits on task condvar         │
            └──────────────────────────────────────────────┘
```

The ready queue is a `std::deque<ThreadTask*>` protected by a
mutex. Workers wait on a condvar when the queue is empty; the
condvar wakes them as new tasks arrive.

## Sandboxed testing

[`file-tests.md`](file-tests.md) — `tests.cpp` is a **standalone
test binary** that exercises the supervisor without engine or
interpreter dependencies. Catches:

- Lost tasks, duplicate runs, race conditions, deadlocks.
- Cancellation correctness.
- Result passing.

CI runs this on every build.

## How to extend

Adding a new threading primitive:

1. Add to `mutex.h` (C interface) and `mutexclass.hpp` (C++ wrapper).
2. Add a test in `tests.cpp`.
3. Use from the supervisor or M2File.

Adding a new task mode:

1. Add to the `ThreadTask` struct.
2. Update the supervisor's `dispatch_task` to honour it.
3. Expose via `supervisorinterface.h`.
4. Wire through `d/threads.dd` or `d/pthread.d`.

## File-by-file

| Component | File doc |
|---|---|
| Worker pool + scheduling | [`file-supervisor.md`](file-supervisor.md) |
| Per-thread file state | [`file-m2file.md`](file-m2file.md) |
| Sync primitives | [`file-mutex.md`](file-mutex.md) |
| M2 value construction | [`file-m2util.md`](file-m2util.md) |
| Standalone tests | [`file-tests.md`](file-tests.md) |

## Related

- [`README.md`](README.md) — system/ navigation hub.
- [`../d/file-threads.md`](../d/file-threads.md) — M2-side
  `Task` / `schedule` API surface.
- [`../d/file-pthread.md`](../d/file-pthread.md),
  [`../d/file-atomic.md`](../d/file-atomic.md) — interpreter
  bindings that call into the supervisor.
- [`../bin/file-main.md`](../bin/file-main.md) — boots the
  supervisor.
- [`../e/architecture.md`](../e/architecture.md) — the engine,
  which runs as workloads on supervisor-managed threads.
- [`../e/file-m2tbb.md`](../e/file-m2tbb.md) — engine's TBB
  wrapper (orthogonal parallelism within one engine workload).
