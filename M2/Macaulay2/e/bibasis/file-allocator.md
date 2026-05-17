# `allocator.hpp`, `allocator.cpp` — `FastAllocator` slab pool

`FastAllocator` is BIBasis's **custom slab allocator** for the
massive number of small objects (monomials, polynomials, triples)
the involutive-basis algorithm creates and destroys.

Part of [`bibasis/`](README.md).

[← bibasis/ overview](README.md) · [← engine overview](../README.md)

## Why a custom allocator

BIBasis computations create **enormous numbers of small objects**:

- Each monomial in a boolean ring is a few bytes.
- Each polynomial is a sequence of monomials.
- Each "triple" or "pair" carries a few small fields.

Allocating these via `new`/`delete` (or even Boehm GC) incurs
per-object overhead:

- Memory-allocator bookkeeping per call.
- Cache misses jumping around the heap.
- Synchronisation cost in multi-threaded paths.

A **slab allocator** wins by:

- Allocating large blocks once.
- Subdividing into fixed-size chunks.
- Free list reuses chunks cheaply.
- Cache-friendly (chunks stay near each other).

For BIBasis-shaped workloads the speedup is significant —
sometimes 2-3× overall.

## What's exposed

```cpp
namespace BIBasis
{
    class FastAllocator
    {
    public:
        static void* Allocate(size_t bytes);
        static void Deallocate(void* p, size_t bytes);
    };
}
```

Two static methods. The size is **passed in to both** — this lets
the allocator pick the right slab pool without storing per-object
size headers.

## Implementation

```cpp
#include <cstdlib>
#include <cmath>
#include <string>

#include "allocator.hpp"
#include "error.h"
```

A typical implementation:

- Maintain per-size-class free lists.
- On `Allocate(bytes)`: pop from the free list for that size; if
  empty, slab-allocate.
- On `Deallocate(bytes)`: push back to the free list.
- Never give memory back to the OS (just reuse forever).

The "never free" trade-off is fine because BIBasis runs are
finite — at the end of the run, the OS reclaims the process heap.

## How the rest of bibasis uses it

```cpp
class Triple
{
    void* operator new(size_t size) {
        return FastAllocator::Allocate(size);
    }
    void operator delete(void* p, size_t size) {
        FastAllocator::Deallocate(p, size);
    }
    ...
};
```

Every class that wants slab allocation overloads `operator new` /
`operator delete` to route through `FastAllocator`.

## GC interaction

Bibasis predates / sidesteps M2's Boehm GC for these small
objects — the engine's `our_new_delete` patterns don't apply
inside this subsystem. The allocator manages its own memory; the
GC sees the big slabs as black-box allocations.

## Used by

- Every BIBasis class that stores many small instances:
  `Triple`, `Polynom`, the various `MonomXXX` types.

## Related

- [`README.md`](README.md) — bibasis/ overview.
- [`file-bibasis-internals.md`](file-bibasis-internals.md) — heavy user.
- [`file-polynom.md`](file-polynom.md) — heavy user.
- [`file-monom.md`](file-monom.md) — heavy user.
