# `myalloc.{cpp,hpp}` — `StatsAllocator` (debug / benchmark allocator)

`myalloc.hpp` defines **`StatsAllocator`** — a tiny instrumented
allocator wrapper used during **debugging** and **benchmarking** to
count and report allocations of a specific type. It is *not* used in
production builds; the class is single-threaded and intentionally
trivial.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Status (from the header)

```cpp
// This class is static as it appears easiest if all allocator objects
// are essentially identical.  It could be a static member of StatsAllocator,
// but then each type T would have a different stats object.
//
// This class is meant for debugging/benchmark use only.
// This class is not thread safe.
//
// TODO: perhaps include mathicgb logging facility, or perhaps even boost.
```

The header is explicit:

- **Single-threaded** — no locking, no atomics.
- **Static** — all instances share counters, which makes per-type
  statistics easier to read.
- **Debug only** — should never appear in the normal build path.

The TODO notes the planned upgrade path: switch to mathicgb's logging
or Boost's accumulators for a more featureful instrumented allocator.

## How it's used

A typical pattern:

```cpp
class MyHotClass {
    static StatsAllocator allocator;
    void *operator new(size_t s)   { return allocator.alloc(s); }
    void  operator delete(void *p) { allocator.dealloc(p);      }
};
```

After running a benchmark, the user dumps `StatsAllocator`'s counters
to see how many `MyHotClass` instances were created and how much
memory they consumed. The class itself doesn't actually free anything
in some configurations — useful for detecting double-frees or for
producing total-allocation summaries.

## Not for production

The header's "not thread safe" disclaimer rules out using
`StatsAllocator` in any path the supervisor parallelises. Production
code uses [`file-newdelete.md`](file-newdelete.md)'s `our_new_delete`
or [`file-mem.md`](file-mem.md)'s `stash`.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-newdelete.md`](file-newdelete.md) — production GC allocator.
- [`file-mem.md`](file-mem.md) — production stash allocator.
- [`file-MemoryBlock.md`](file-MemoryBlock.md) — bump allocator.
- mathicgb submodule — logging facility the TODO points toward.
