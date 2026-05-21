# `newdelete.hpp` — `our_new_delete` GC-friendly allocation hook

`newdelete.hpp` (in subdirectories) declares **`our_new_delete`** and
**`our_new_gc`** — the two `operator new` / `operator delete` overloads
that route engine-side allocations through bdwgc, the Boehm-Demers-Weiser
garbage collector. Inheriting from these gives a class GC-managed
storage at no per-allocation cost beyond the bdwgc allocator's own.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## The pattern

```cpp
class Foo : public our_new_delete {
    // any allocation of Foo via `new Foo(...)` goes through bdwgc
};
```

Inheriting from `our_new_delete` overrides:

- `operator new(size_t)` → `GC_malloc(size_t)` (collected, may contain
  pointers — the GC will scan the bytes).
- `operator new[](size_t)` → same for arrays.
- `operator delete(void *)` → typically a no-op; bdwgc reclaims when
  unreferenced.

`our_new_gc` is the variant for objects that may contain pointers,
matching `GC_malloc`; `our_new_delete_atomic` (when needed) wires up
`GC_malloc_atomic` for pointer-free byte buffers.

## Why subclasses rather than global override

The engine **doesn't** globally override `operator new` for two reasons:

1. **Mixed memory regimes** — some engine state has to live in the
   system heap (e.g. `std::vector`-managed buffers, third-party
   library handles). Subclassing lets each class decide individually.
2. **External libraries** — FLINT, MPFR, GMP all do their own
   allocations. A global override would conflict.

The subclass-based mechanism is opt-in: every engine class that wants
GC management says "I'm `our_new_delete`."

## Why the header lives in subdirectories

The convention: each subdirectory of `e/` has its own copy of
`newdelete.hpp`, customised for that subsystem's needs. Subdirectories
that follow a sufficiently different pattern (memtailor-based, for
instance) may declare a different version.

This pattern predates module-style namespacing in C++; modern code
could probably get by with one header. The duplicate copies stay for
compatibility.

## What `our_new_delete` looks like underneath

```cpp
class our_new_delete {
public:
    void *operator new(size_t s)    { return GC_malloc(s); }
    void *operator new[](size_t s)  { return GC_malloc(s); }
    void  operator delete(void *p)  { /* GC reclaims */ }
    // ...
};
```

The class has no data — the only thing that matters is the operator
overloads.

## Universal usage

`our_new_delete` (or its variants) is the base class of:

- [`EngineObject`](file-hash.md) and `MutableEngineObject` (themselves
  the bases of nearly every engine class).
- Direct subclasses that bypass `EngineObject` for special reasons
  (e.g. `gb_elem` in [`file-spair.md`](file-spair.md)).
- Anonymous helper structs allocated inside engine code.

Bdwgc submodule lives at [`../../submodules/bdwgc/`](../../submodules/README.md).

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-hash.md`](file-hash.md) — `EngineObject` uses
  `our_new_delete` as its allocation base.
- [`file-MemoryBlock.md`](file-MemoryBlock.md) — non-GC alternative for
  hot loops.
- bdwgc submodule.
