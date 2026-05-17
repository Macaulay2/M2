# `mem.{cpp,hpp}` — `stash` (size-class slab allocator)

`mem.cpp` implements the engine's **size-class slab allocator** —
called `stash` — used for fast allocation of fixed-size objects.
A `stash` maintains a free list per size class and a chain of large
slabs that those objects are carved out of.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Configuration constants

```cpp
#include <cassert>
#include "newdelete.hpp"
#include "../system/mutex.h"  // for spinLock

class buffer;

// 2 * 2^NDOUBLES = largest stash size.
const int NDOUBLES   = 25;
const int slab_size  = 2032;  // bytes per slab
```

Two constants set the engine-wide allocator parameters:

- **`NDOUBLES = 25`** — the largest stash size is `2 * 2^25 ≈ 64 MB`.
  Past this, allocations fall through to the GC heap.
- **`slab_size = 2032`** — each slab is ~2 KB, fitting comfortably in
  L1 cache. Comments record alternative values that were tried
  (`262134`, etc.).

## The `stash` class

A `stash` is a per-size-class free list. Operations:

- **`new_elem()`** — return the next free element (allocate a new
  slab if needed).
- **`delete_elem(ptr)`** — push back onto the free list (no actual
  free).
- **`reset_stash()`** — purge the free list and slabs.

Allocation is `O(1)`; deallocation is `O(1)`. The catch: a `stash`
only manages a single size class. Engines typically allocate one
`stash` per type that's allocated heavily.

## Why a custom allocator

Three reasons engine code uses `stash` over plain GC:

1. **Per-allocation overhead** — bdwgc's `GC_malloc` has higher
   per-call cost than a single linked-list pop. For objects allocated
   in tight loops (e.g. S-pairs in [`file-spair.md`](file-spair.md)),
   the savings are substantial.
2. **Determinism** — `stash` doesn't trigger garbage collection. In
   real-time or test contexts this matters.
3. **Tracking** — each `stash` can report how many elements it has
   allocated, useful for benchmarking and leak detection.

The downside: objects allocated through `stash` are not GC-managed;
the caller is responsible for explicit `delete_elem` (or for using a
short-lived `stash` that gets destroyed all at once).

## Spinlock

The `#include "../system/mutex.h"` pulls in the engine's spinlock
type. `stash` uses a spinlock on its free list — necessary because the
allocator can be called from multiple threads via the supervisor.

## Used by

- [`file-spair.md`](file-spair.md) — S-pair allocation.
- `gb_elem` allocation in [`file-gb-default.md`](file-gb-default.md).
- Various per-object stashes scattered through the codebase.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-MemoryBlock.md`](file-MemoryBlock.md) — modern bump allocator
  alternative.
- [`file-newdelete.md`](file-newdelete.md) — GC-backed default.
- [`file-myalloc.md`](file-myalloc.md) — debug-only allocator wrapper.
- `../system/` — supplies the spinlock.
