# `mutex.h`, `mutexclass.hpp`, `pthread-methods.hpp`, `gc_std.hpp` — synchronisation primitives

These four small headers provide **synchronisation primitives**
the supervisor and `M2File` use: spinlocks, mutex C++ classes,
pthread-portability helpers, and GC-aware STL allocators.

Part of the [`system/` directory](README.md).

[← system/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## `mutex.h` — C-callable spinlock

```c
struct spinlockStructure
{
  volatile int m_MutexInt;
};
typedef struct spinlockStructure spinLock;
static const spinLock uninitializedSpinLock = {0};
static inline void initializeSpinLock(struct spinlockStructure* sls)
{
  __sync_lock_release(&sls->m_MutexInt);
}
static inline void acquireSpinLock(struct spinlockStructure* sls)
```

A **C-callable spinlock** built on GCC's `__sync_*` builtins:

- **`initializeSpinLock`** — sets to released state.
- **`acquireSpinLock`** — spin-busy-loop until acquired.
- **`releaseSpinLock`** — atomic store-release.

Spinlocks are cheap when contention is rare (short critical
sections). For longer waits the supervisor uses pthread mutexes
(via `mutexclass.hpp`).

The `#undef ERROR` near the top:

```c
#include <pthread.h>
#undef ERROR			/* undo mingw64 damage */
```

is a portability hack — MinGW64's headers define a macro `ERROR`
that collides with M2's enum. Forcing an `#undef` here keeps the
collision contained.

## `mutexclass.hpp` — C++ mutex wrapper

```cpp
class pthreadMutex
{
public:
  pthreadMutex()
  {
    if(pthread_mutex_init(&m_Mutex,NULL))
      abort();
  }
  void lock()
  {
    while(pthread_mutex_lock(&m_Mutex));
  }
  void unlock()
  {
    ...
  }
};
```

RAII pthread mutex with explicit `lock()` / `unlock()` methods.
Compared to `std::mutex`:

- **Always uses pthreads** — important when M2 explicitly cares
  about pthread keys / cancellation.
- **`abort()` on init failure** — fast-fail rather than letting
  uninitialised mutex propagate.
- **Loops `pthread_mutex_lock`** — robust against EINTR.

## `pthread-methods.hpp` — pthread-type portability

```cpp
#ifdef __MINGW32__
  #define clearThread(t) t.p = NULL, t.x = 0
  static inline int operator==(pthread_t t, int zero) { return t.p == (void *)zero; }
  ...
#else
  #define clearThread(t) t = 0
#endif
```

`pthread_t` is **not portable**:

- POSIX leaves it intentionally opaque. Could be `unsigned long`,
  could be a struct.
- MinGW64 defines it as a struct `{ p, x }`.
- Linux glibc uses `unsigned long`.

`pthread-methods.hpp` makes the supervisor's `pthread_t`
comparisons / clears compile uniformly across all platforms.

## `gc_std.hpp` — GC-aware STL allocators

```cpp
#include <M2/gc-include.h>
#include <gc/gc_allocator.h>
#define gc_map(T,U) std::map<T,U,std::less<T>,gc_allocator<std::pair<const T,U>>>
#define gc_set(T)   std::set<T,std::less<T>,gc_allocator<T>>
```

Two macros that produce `std::map` / `std::set` types using
**Boehm GC's allocator**. Why this matters:

- The supervisor stores `ThreadTask*` pointers in `std::map`s and
  `std::set`s.
- Those pointers point to GC-managed memory.
- If the STL container's *internal storage* uses `malloc`, the GC
  doesn't scan it — and might reclaim the `ThreadTask*` while
  it's only referenced from the supervisor's std::map.

Using `gc_allocator` makes the container's internal storage
GC-scanned, fixing the issue.

## Used by

- [`file-supervisor.md`](file-supervisor.md) — primary consumer.
- [`file-m2file.md`](file-m2file.md) — uses `pthreadMutex`.
- C-side code via `mutex.h`'s spinlocks.

## Related

- [`README.md`](README.md) — system/ overview.
- pthread library — POSIX threads.
- Boehm GC — garbage collector used here.
