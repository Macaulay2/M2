# Threading model

This document unifies M2's **concurrency story** across three
independent threading mechanisms that coexist in the binary:

1. **Supervisor + pthreads** — manages worker threads for M2-level
   `Task` / `schedule`.
2. **Intel TBB** — engine-internal parallelism, primarily in
   resolutions.
3. **Per-thread state machinery** — error flags, interrupt flags,
   `M2File`, etc.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md) · [Memory](MEMORY.md) · [Startup](STARTUP.md)

## The three threading models

```
┌──────────────────────────────────────────────────────────┐
│   M2 user level                                            │
│       Task, schedule, taskResult — d/file-threads.md       │
│       │                                                    │
│       ▼                                                    │
│   M2 supervisor (system/)                                  │
│       Worker thread pool, M2File, pthread coordination     │
│       │                                                    │
│       ▼                                                    │
│   POSIX pthreads + Boehm GC thread registration            │
└──────────────────────────────────────────────────────────┘

                       SEPARATELY:

┌──────────────────────────────────────────────────────────┐
│   Engine internal parallelism                              │
│       schreyer-resolution/res-dep-graph.cpp via TBB        │
│       │                                                    │
│       ▼                                                    │
│   Intel TBB (flow::graph)                                  │
│       │                                                    │
│       ▼                                                    │
│   Engine's own thread pool (separate from supervisor's)    │
└──────────────────────────────────────────────────────────┘
```

These two stacks **don't communicate directly**. The supervisor
parallelises *across* engine workloads (e.g., compute three GBs
in parallel); TBB parallelises *within* one workload (e.g., one
resolution's level-degree DAG cells).

## Stack 1: Supervisor + M2-level Task

### M2-level API

**Source**:
[`M2/Macaulay2/d/file-threads.md`](M2/Macaulay2/d/file-threads.md).

The user-visible API:

```m2
t = createTask(f)        -- f is a function with zero args
schedule(t)              -- submit to worker pool
result = taskResult(t)   -- block until done, return result
cancelTask(t)            -- cooperative cancellation
isReady(t), isDone(t)    -- non-blocking checks
```

Plus atomic primitives:

```m2
a = new AtomicInt        -- thread-safe integer
a + 1                    -- atomic increment
```

### Interpreter binding

**Source**:
[`M2/Macaulay2/d/file-pthread.md`](M2/Macaulay2/d/file-pthread.md).

The `.d` files wrap the supervisor's C interface
(`supervisorinterface.h`). Each M2-level operation maps to one or
two supervisor calls:

```
M2 schedule(task)
   ↓ d/threads.dd
   taskCreatePush(...)
   ↓ d/pthread.d
   Ccode(...) → supervisor's taskCreatePush()
   ↓ system/supervisor.cpp
   enqueue ThreadTask
```

### Supervisor

**Source**:
[`M2/Macaulay2/system/architecture.md`](M2/Macaulay2/system/architecture.md).

The supervisor is a **static library** linked into the M2 binary.
It manages:

- A **worker thread pool** of `clamp(4, 16, numCores) + 1` threads.
- A **task queue** (ready queue + running set).
- **Cancellation flags** per task.
- **Per-thread state** (M2File, GC registration).

Each worker is registered with Boehm GC at creation; GC pauses
work correctly across the pool.

### What runs on a worker

The function passed to `createTask` runs **on a supervisor worker
thread**. The worker:

1. Pops a `ThreadTask*` from the ready queue.
2. Calls the task's function with its argument.
3. Stores the return value as the task's result.
4. Signals the completion condvar.
5. Loops.

A task's function can call any M2-level operation — it has access
to engine, interpreter, and all global state. The supervisor
doesn't constrain *what* can run, only *when* it runs.

### When the supervisor model is used

Whenever the M2 user writes `schedule(...)`. Common cases:

- Running benchmark tasks in parallel.
- Running per-package tests concurrently (the test harness uses
  this).
- Running independent computations and gathering results.

The example runner in
[`M2/Macaulay2/m2/file-examples.md`](M2/Macaulay2/m2/file-examples.md)
uses tasks for time-limited example execution.

## Stack 2: Engine TBB parallelism

### What TBB is

[Intel TBB](https://github.com/oneapi-src/oneTBB) is a parallel
programming library. M2 uses primarily:

- **`tbb::flow::graph`** — task DAGs (used by resolution).
- **`tbb::parallel_for`** — parallel loops (in some matrix paths).
- **`tbb::concurrent_unordered_map`** — concurrent hash tables.

### Engine wrapper

**Source**:
[`M2/Macaulay2/e/file-m2tbb.md`](M2/Macaulay2/e/file-m2tbb.md).

`m2tbb.hpp` is a **conditional include**: if TBB is available at
build time, it pulls in the real TBB headers; otherwise it
defines minimal sequential stubs.

```cpp
#ifdef WITH_TBB
  #include <tbb/tbb.h>
#else
  // sequential fallback
  template <typename F>
  void parallel_for(int begin, int end, F f) {
    for (int i = begin; i < end; ++i) f(i);
  }
#endif
```

This lets engine code use TBB primitives without sprinkling
`#ifdef` everywhere.

### Where TBB is used in the engine

Primary user: **Schreyer-resolution dependency graph**.

**Source**:
[`M2/Macaulay2/e/schreyer-resolution/file-res-dep-graph.md`](M2/Macaulay2/e/schreyer-resolution/file-res-dep-graph.md)
and [`schreyer-resolution/architecture.md`](M2/Macaulay2/e/schreyer-resolution/architecture.md).

A resolution computes many `(level, degree)` cells. Cells form a
**DAG**: cell `(i, d)` depends on `(i-1, d')` for various `d'`.
TBB schedules independent cells across threads.

```cpp
tbb::flow::graph G;
std::vector<std::vector<NodePtr>> nodes;  // nodes[level][degree]
// ... build the DAG ...
G.wait_for_all();
```

The standalone sandbox
([`schreyer-resolution/file-res-tasking-example.md`](M2/Macaulay2/e/schreyer-resolution/file-res-tasking-example.md))
explores this pattern in isolation.

### Other TBB users (minor)

- Some `DMat<R>` linear-algebra paths can use `parallel_for` for
  row reductions.
- The F4 GB engine (newer `gb-f4/`) has TBB hooks but most
  workloads stay sequential.

### TBB does *not* talk to the supervisor

The TBB pool is **completely independent** of the supervisor
worker pool. TBB has its own internal threads, picked up at
process startup via TBB's auto-detection.

This means a parallel resolution can run *inside* a supervisor
task. The supervisor sees one task; TBB sees N parallel cell
evaluations.

It also means **total thread count** under parallel workloads can
be `supervisor workers × TBB threads` — usually `16 × 16 = 256`
threads, which the OS scheduler must handle. In practice this is
fine; M2's parallel workloads rarely stress this.

## Stack 3: Per-thread state

Each thread has its own:

### Error flag

**Source**: [`M2/Macaulay2/d/file-err.md`](M2/Macaulay2/d/file-err.md).

Thread-local boolean: "has an error occurred in this thread?"
Set by error-reporting functions; checked by callers after every
operation. Replaces C++ exceptions across the C ABI.

```d
threadLocal errorOccurred = false;
```

If a worker thread errors, the error stays *in that thread*. The
parent thread polling `isReady(task)` will eventually see the
task transition to "done with error."

### Interrupt flag

**Source**:
[`M2/Macaulay2/d/file-interrupts.md`](M2/Macaulay2/d/file-interrupts.md).

Thread-local boolean: "user pressed Ctrl-C?" The signal handler
sets it on the **main thread**; long-running operations poll it
regularly.

In a parallel workload, Ctrl-C interrupts the main thread; worker
threads don't see it directly. The main thread cancels pending
tasks (via the supervisor) and worker threads see the
cancellation via their own thread-local flag.

### M2File (per-thread output state)

**Source**:
[`M2/Macaulay2/system/file-m2file.md`](M2/Macaulay2/system/file-m2file.md).

Each `M2File` (wrapping `stdout`/`stderr`/etc.) carries per-thread
state for output buffering:

- **Sync mode** — exclusive lock per write; outputs never
  interleave mid-message.
- **Unsync mode** — each thread maintains its own `Net` (2D output
  state); flushes are explicit.

The default for `stdout`/`stderr` is sync. Per-task output that
the user wants tagged-per-thread uses unsync.

### Symbol resolution scopes

The interpreter's symbol-table lookup uses thread-local scope
chains during execution. A worker thread's lookups happen in its
own scope without contending with other threads. See
[`M2/Macaulay2/d/file-binding.md`](M2/Macaulay2/d/file-binding.md).

## GC + threads interaction

**Critical**: Boehm GC needs every thread registered so its
"stop the world" pause works correctly. Threads not in the
stop-set race with the collector → crashes.

The supervisor handles registration:

```cpp
// Worker thread startup
GC_register_my_thread(&base);   // tell GC about this thread
// ... do work ...
GC_unregister_my_thread();      // before exit
```

TBB threads — Intel's `parallel_for` workers — are
**auto-registered by TBB's GC-aware machinery** in M2's build
configuration. Without this, parallel resolutions would crash on
GC pauses.

If you ever see a crash that looks like "use of freed memory"
inside an engine inner loop *only when threading is enabled*,
suspect a missing thread registration. See
[`MEMORY.md`](MEMORY.md) for the broader memory story.

## Atomic primitives

Where shared state needs atomic access:

| Primitive | Source | Use |
|---|---|---|
| `std::atomic<T>` | C++11 stdlib | counters, flags |
| `__sync_*` builtins | GCC/Clang | low-level spinlocks |
| `pthread_mutex` | pthread.h | larger critical sections |
| `pthread_cond` | pthread.h | wait/notify |

See [`M2/Macaulay2/system/file-mutex.md`](M2/Macaulay2/system/file-mutex.md)
for the engine-side primitives and
[`M2/Macaulay2/d/file-atomic.md`](M2/Macaulay2/d/file-atomic.md)
for the interpreter-side.

The M2-level `AtomicInt` is layered on `std::atomic<long>`.

## When to use which threading mechanism

| Workload | Use |
|---|---|
| Independent M2-level computations | Supervisor `schedule` |
| Bounded-time speculative execution | Supervisor `schedule` + timeout |
| Per-package test runs | Supervisor `schedule` |
| Resolution cell DAG | Engine TBB `flow::graph` |
| Engine matrix row-reduction | Engine TBB `parallel_for` (where wired) |
| New per-thread state | Add to thread-local with `threadLocal` keyword in `.d` |
| Tight inner loop sharing memory | `std::atomic` directly |
| Larger critical section | `pthreadMutex` |

Cross-mechanism rules:

- **Don't call TBB from M2 user code.** TBB is engine-internal.
- **Don't call supervisor from TBB workers.** Reentrancy is not
  guaranteed.
- **Engine code can be called from supervisor workers** without
  special precautions (most engine operations are reentrant).

## Common pitfalls

### Lost-update on shared global

```cpp
static int counter = 0;
// ... worker A: counter++  -- worker B: counter++
// final counter may be 1, not 2
```

Fix: `std::atomic<int>` or hold a mutex.

### Held mutex across an interpreter call

```cpp
mutex.lock();
// ... call into interpreter, which calls back ...
mutex.lock();  // DEADLOCK
```

The interpreter doesn't know about your mutex. Either release
before calling into the interpreter or use a recursive mutex.

### Use-after-cancel

A cancelled task may have already started running. Cancellation
is **cooperative** — the task polls and decides when to bail
out. If your task allocates memory and then is cancelled, that
memory is still reachable (GC tracked) and stays until the GC
reaps it.

### Thread-local in a class

```cpp
class Foo {
    static thread_local int data;  // OK
};
// vs
class Foo {
    thread_local int data;  // INVALID
};
```

Thread-locals must be `static` (or namespace-scope). M2's
`.d` files use the `threadLocal` keyword which compiles to
`thread_local` in the right places.

## Debugging concurrency issues

### Race conditions

1. **Reproduce with TSAN**: rebuild with
   `-fsanitize=thread` and re-run. ThreadSanitizer flags
   data races precisely.
2. **Use Valgrind's helgrind** if TSAN isn't available.
3. **Check thread registration** with GC if crashes correlate
   with parallel workloads.

### Deadlocks

1. **GDB attach** to a stuck process: `gdb -p $(pidof M2)`.
2. **`thread apply all bt`** in gdb to see every thread's stack.
3. **Look for two threads each waiting on a mutex the other
   holds.**

### Hangs in `taskResult`

A common pattern: the task is running but never finishes. Most
often:

- An infinite loop in the task's function.
- The task is blocked on a lock the parent holds.
- The task is waiting on a condvar that was already signalled
  before the wait started (a race).

`gdb` attached to M2 shows the worker thread's stack.

## How threading interacts with other concerns

| Concern | Interaction |
|---|---|
| Memory ([`MEMORY.md`](MEMORY.md)) | GC must register all threads; STL containers need `gc_allocator` if holding GC pointers across threads |
| Startup ([`STARTUP.md`](STARTUP.md)) | Phase 6 = supervisor init; TBB threads come up later on first use |
| Testing ([`TESTING.md`](TESTING.md)) | `system/tests.cpp` stresses supervisor in isolation; `tests/threads/` does M2-level thread tests |
| Build ([`BUILD.md`](BUILD.md)) | TBB is a configure-time dependency; without it, `m2tbb.hpp` falls back to sequential |

## Used by

- Engine developers writing parallel algorithms.
- Anyone debugging M2 under parallel workloads.
- Newcomers wondering "why are there three different threading
  systems."

## Related

- [`README.md`](README.md) — repository TOC.
- [`MEMORY.md`](MEMORY.md) — GC + thread interaction.
- [`STARTUP.md`](STARTUP.md) — phase 6 (supervisor init).
- [`TESTING.md`](TESTING.md) — `tests/threads/` and
  `system/tests.cpp`.
- [`M2/Macaulay2/system/architecture.md`](M2/Macaulay2/system/architecture.md)
  — supervisor in detail.
- [`M2/Macaulay2/d/file-threads.md`](M2/Macaulay2/d/file-threads.md)
  — interpreter `Task` API.
- [`M2/Macaulay2/d/file-pthread.md`](M2/Macaulay2/d/file-pthread.md),
  [`M2/Macaulay2/d/file-atomic.md`](M2/Macaulay2/d/file-atomic.md),
  [`M2/Macaulay2/d/file-interrupts.md`](M2/Macaulay2/d/file-interrupts.md)
  — interpreter primitives.
- [`M2/Macaulay2/e/file-m2tbb.md`](M2/Macaulay2/e/file-m2tbb.md)
  — engine TBB wrapper.
- [`M2/Macaulay2/e/schreyer-resolution/file-res-dep-graph.md`](M2/Macaulay2/e/schreyer-resolution/file-res-dep-graph.md)
  — primary TBB use in the engine.
- [`M2/Macaulay2/system/file-supervisor.md`](M2/Macaulay2/system/file-supervisor.md),
  [`M2/Macaulay2/system/file-m2file.md`](M2/Macaulay2/system/file-m2file.md),
  [`M2/Macaulay2/system/file-mutex.md`](M2/Macaulay2/system/file-mutex.md)
  — per-file deep-dives.
