# Memory model

This document is the **end-to-end memory-management reference**
for M2. The garbage collector, pool allocators, slab allocators,
finalisers, GC-aware STL containers, and overflow-checked
arithmetic all interlock — this is the unified narrative.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md) · [Startup](STARTUP.md)

## The big picture

```
┌────────────────────────────────────────────────────────────┐
│  Layer 1: Boehm-Demers-Weiser conservative GC (bdwgc)       │
│  The substrate; tracks every "pointer-bearing" allocation.  │
├────────────────────────────────────────────────────────────┤
│  Layer 2: M2 allocator hooks                                │
│  our_new_delete, getmem, GC_MALLOC_ATOMIC, finalize.hpp     │
├────────────────────────────────────────────────────────────┤
│  Layer 3: Pool / slab allocators                            │
│  MemoryBlock, FastAllocator, stash (size-class slabs)       │
├────────────────────────────────────────────────────────────┤
│  Layer 4: External library wrappers + GC-aware STL          │
│  gmp_*, mpfr_*, fmpz_*, gc_allocator<T>                     │
├────────────────────────────────────────────────────────────┤
│  Layer 5: Overflow-checked arithmetic                       │
│  safe::add, safe::mul, safe::pow                            │
└────────────────────────────────────────────────────────────┘
```

Each layer has its own concerns. Together they let M2 manage
gigabytes of mathematical data without manual `free()` calls or
memory leaks.

## Layer 1: Boehm GC

**Source**: vendored submodule
[`M2/submodules/file-submodules.md`](M2/submodules/file-submodules.md)
(bdwgc).

**What it is**: a **conservative mark-sweep garbage collector**.
"Conservative" means it doesn't need precise type info — it
scans every word in the heap and treats anything that *looks
like* a pointer to GC memory as a root.

**When it runs**: opportunistically (after every N allocations)
or explicitly via `GC_collect`. The collector pauses every
thread, scans all roots (stacks + globals), traces reachability,
sweeps unreachable allocations.

**API**:

```c
void *p = GC_malloc(size);          // pointer-bearing — scanned
void *p = GC_malloc_atomic(size);   // leaf data — not scanned
GC_free(p);                          // explicit early free (rare)
```

**Why conservative**: M2 mixes hand-written C++ with
`scc1`-generated C with external libraries. A precise GC would
require type info from all of them; conservative just works.
The cost: a few percent of CPU overhead and occasional false
retention.

**Failure modes**: if memory you expect to be reclaimed isn't,
the most likely cause is a "**false root**" — some integer that
happens to look like a pointer keeps the allocation alive.
Diagnosis via `GC_print_heap_usage()`.

## Layer 2: M2 allocator hooks

How everything else routes through Layer 1.

### `our_new_delete` (engine C++)

**Source**: [`M2/Macaulay2/e/file-newdelete.md`](M2/Macaulay2/e/file-newdelete.md).

Every engine class that contains GC-managed data inherits from
`our_new_delete`:

```cpp
class Matrix : public our_new_delete { ... };
class FreeModule : public our_new_delete { ... };
```

`our_new_delete` overloads `operator new` / `operator delete` to
route through `GC_malloc`. No explicit `delete` needed — the GC
reclaims unreachable instances.

For **mutable** objects that should also be **identity-tracked**
(e.g., `MutableMatrix`), inherit from `MutableEngineObject`
instead — see [`M2/Macaulay2/e/file-hash.md`](M2/Macaulay2/e/file-hash.md).

### `getmem` family (interpreter C)

**Source**: [`M2/Macaulay2/d/file-c-glue.md`](M2/Macaulay2/d/file-c-glue.md).

`scc1`-generated C code can't use `our_new_delete` (it's C, not
C++). It uses the C-side wrappers in `M2mem.{c,h}`:

```c
void *p = getmem(n);             // pointer-bearing
void *p = getmem_atomic(n);      // leaf data
```

Same underlying GC, different API for the language.

### `getmem_atomic` for leaf data

**Atomic data** has no pointers — e.g., a packed `unsigned long`
array of bit-packed integers, a `string`, a `double[]`. The GC
doesn't need to scan it.

Tagging an allocation as atomic:

- Slight speedup (no scan cost).
- Critical for large allocations like a 100MB string.
- Wrong choice (atomic for a pointer-bearing struct) = lost
  pointers = use-after-free.

### Finalisers

**Source**: [`M2/Macaulay2/e/file-finalize.md`](M2/Macaulay2/e/file-finalize.md).

For objects that wrap external-library state (GMP `mpz_t`, MPFR
`mpfr_t`, FLINT `fmpz_t`, Python `PyObject*`, libxml2 trees), the
GC reclaiming the wrapper isn't enough — the library's own
`free` function must run.

`finalize.hpp` provides:

```cpp
template <typename T>
void register_finalizer(T *obj, void (*cleanup)(T*));
```

When the GC reclaims `obj`, `cleanup(obj)` runs first. Used
extensively in `aring-zz-gmp.hpp`, `aring-zz-flint.hpp`, etc.

**Failure mode**: a missing finaliser → external library leak.
The GC doesn't notice; the leak grows silently.

## Layer 3: Pool / slab allocators

For hot loops allocating billions of small objects, going
through the GC for each one is too slow. Solution: pool
allocators.

### `MemoryBlock<T>` (engine)

**Source**: [`M2/Macaulay2/e/file-MemoryBlock.md`](M2/Macaulay2/e/file-MemoryBlock.md).

A **bump-pointer pool**:

```cpp
MemoryBlock<Node> pool;
Node *n1 = pool.allocate();   // O(1), no GC interaction
Node *n2 = pool.allocate();
// ... millions of allocations ...
// no individual deallocations
// when `pool` goes out of scope, all memory released en masse
```

Used by:

- F4 GB engine ([`M2/Macaulay2/e/f4/file-memblock.md`](M2/Macaulay2/e/f4/file-memblock.md))
  — per-step bump allocator for polynomial terms.
- Schreyer resolution
  ([`M2/Macaulay2/e/schreyer-resolution/file-res-memblock.md`](M2/Macaulay2/e/schreyer-resolution/file-res-memblock.md))
  — per-cell bump allocator.

**When NOT to use**: when objects outlive the pool, or when you
need to free individual objects mid-computation. Stick to
`our_new_delete` then.

### `FastAllocator` (BIBasis)

**Source**: [`M2/Macaulay2/e/bibasis/file-allocator.md`](M2/Macaulay2/e/bibasis/file-allocator.md).

A **size-class slab allocator** specialised for BIBasis's
billions of `Triple` / `Polynom` / `Monom` objects:

```cpp
class Triple {
    static void* operator new(size_t s) {
        return FastAllocator::Allocate(s);
    }
    static void operator delete(void* p, size_t s) {
        FastAllocator::Deallocate(p, s);
    }
};
```

Per-size-class free lists; never gives memory back to the OS.
Trade-off: monotonic growth, but `O(1)` alloc/dealloc and
cache-friendly.

### `stash` (engine size-class slabs)

**Source**: [`M2/Macaulay2/e/file-mem.md`](M2/Macaulay2/e/file-mem.md).

A general-purpose **size-class slab allocator** in the engine.
Used where the allocation pattern is predictable but the
lifetime exceeds a single computation step. Less hot than
`MemoryBlock`; coexists with the GC.

### When to pick which

| Situation | Use |
|---|---|
| Object lives the whole program | `our_new_delete` (default) |
| Pointer-bearing, normal lifetime | `our_new_delete` |
| Leaf data, normal lifetime | `getmem_atomic` |
| Hot loop, predictable lifetime, per-step | `MemoryBlock<T>` |
| Hot loop, monotonic growth, fixed-size | `FastAllocator` (BIBasis) |
| Per-thread thread-local pool | inherit + custom slab |
| Wraps external library state | `our_new_delete` + finaliser |

## Layer 4: External library wrappers + GC-aware STL

### Wrapping external types

Each external numeric library has its own allocator. M2 wraps
them in GC-managed handles with finalisers:

| Library | Wrapper convention |
|---|---|
| GMP (`mpz_t`, `mpq_t`) | `gmp_ZZ`, `gmp_QQ` — wrapped struct |
| MPFR (`mpfr_t`) | `gmp_RR` — wrapped struct |
| MPC (`mpc_t`) | `gmp_CC` — wrapped struct |
| FLINT (`fmpz_t`, etc.) | `aring-zz-flint`'s `ElementType` |
| NTL (`NTL::ZZ`, etc.) | NTL glue (`file-ntl-glue.md`) |
| Python (`PyObject*`) | `file-python.md` — reference-count managed |

The wrapper carries the library's native handle plus a finaliser
that calls the library's free function.

**Sources**:
[`M2/Macaulay2/e/file-aring-zz-gmp.md`](M2/Macaulay2/e/file-aring-zz-gmp.md),
[`M2/Macaulay2/e/file-aring-zz-flint.md`](M2/Macaulay2/e/file-aring-zz-flint.md),
[`M2/Macaulay2/d/file-python.md`](M2/Macaulay2/d/file-python.md),
[`M2/Macaulay2/d/file-ffi.md`](M2/Macaulay2/d/file-ffi.md).

### GC-aware STL allocators

**Source**: [`M2/Macaulay2/system/file-mutex.md`](M2/Macaulay2/system/file-mutex.md)
(documents `gc_std.hpp`).

STL containers (`std::map`, `std::set`) allocate **their internal
storage** via the allocator template parameter. By default that's
`std::allocator<T>` which calls `malloc`. If the container holds
GC-tracked pointers, the GC won't scan that internal storage —
the pointers become "lost roots."

Solution: `gc_std.hpp` provides:

```cpp
#define gc_map(T, U)  std::map<T, U, std::less<T>, gc_allocator<std::pair<const T, U>>>
#define gc_set(T)     std::set<T, std::less<T>, gc_allocator<T>>
```

Used by the supervisor's `ThreadTask*` containers
([`M2/Macaulay2/system/file-supervisor.md`](M2/Macaulay2/system/file-supervisor.md))
to keep task pointers scannable.

**Failure mode**: using plain `std::map<int, GC_Object*>` in code
that holds the only reference to those objects → premature
collection → crashes.

## Layer 5: Overflow-checked arithmetic

**Source**: [`M2/Macaulay2/e/file-overflow.md`](M2/Macaulay2/e/file-overflow.md).

Not memory allocation per se, but it shares the same
"silent-corruption" failure mode. Monomial degrees are `int`s;
multiplying two monomials adds their exponents. With small
exponents this is fine; with degree-30000 monomials in 5
variables, a multiplication may overflow `int32_t`.

`safe::add` / `safe::mul` / `safe::pow` raise an error on
overflow instead of silently producing nonsense:

```cpp
int d = safe::mul(deg, count);    // throws on overflow
```

Used pervasively in monoid / degree code. Without it, an
overflow would corrupt a GB without any visible error.

**The "monomial overflow" class of bugs is so dangerous M2
treats overflow detection as a memory-correctness concern.**

## Per-thread memory

When M2 runs tasks on worker threads (via the supervisor —
[`M2/Macaulay2/system/architecture.md`](M2/Macaulay2/system/architecture.md)),
each thread has:

- Its own stack (thousands of bytes, scanned by GC).
- Its own thread-local `M2File` state for per-thread output
  buffering.
- Its own thread-local error / interrupt flags.
- Shared access to the GC heap.

The supervisor **registers each worker with Boehm GC** at thread
creation. Without this, "stop the world" pauses would race with
workers, producing crashes.

## The `ring_elem` boundary

Values cross the engine/interpreter boundary as `ring_elem`
([`M2/Macaulay2/e/file-ringelem.md`](M2/Macaulay2/e/file-ringelem.md))
— a **tagged union** of `int`, pointer-to-`mpz_t`,
pointer-to-`Nterm`, etc.

`ring_elem` itself is **just a 64-bit word** (the union). When
the bits represent a pointer, that pointer is a GC-tracked
allocation. When they represent a packed integer (e.g., a small
`Z/p` element), no allocation involved.

The 64-bit-width is critical: tagged-pointer schemes need a
known size to dispatch. Cross-platform consistency comes from
the `hash_t` typedef pinning it to `uint64_t` always.

## Memory leaks: why M2 generally doesn't have them

Three mechanisms protect against leaks:

1. **GC reclaims unreachable objects.** Forgetting to "free"
   is impossible because there's no free to forget.
2. **Pool allocators release en masse.** When a `MemoryBlock`
   goes out of scope, all its allocations vanish together.
3. **Finalisers cover external libs.** GMP / FLINT / NTL state
   gets freed when its wrapper is collected.

Where leaks *do* happen:

- **Missing finaliser** — external lib state lingers.
- **False root** — some long-lived int holds a pointer-shaped value.
- **Container leak** — a global `std::map<int, T*>` with no
  cleanup.
- **Daemon thread** — a worker thread never gets joined.

Diagnosis: `GC_print_heap_usage()`, valgrind with
[`M2/files/file-files-content.md`](M2/files/file-files-content.md)'s
`M2-suppressions.supp`.

## Why not `malloc` / `free`?

The historical reasons M2 chose Boehm GC over manual memory
management:

1. **Math objects have intricate sharing patterns.** A polynomial
   can be a sub-term of many other polynomials, of a matrix
   entry, of an ideal generator, ... Tracking ownership by hand
   is brittle.
2. **The interpreter has dynamic typing.** A value's type isn't
   known statically; ad-hoc reference counting would need runtime
   type dispatch on every assign.
3. **scc1-generated code.** Adding `free` calls automatically
   from scc1 would be hard.
4. **C++ exceptions don't unwind through C boundaries.** Manual
   memory needs careful RAII; with C in the mix, RAII alone
   isn't enough.

The trade-off (~5% perf cost for Boehm GC) is acceptable in
exchange for memory-safety guarantees.

## Debugging memory issues

| Symptom | First step |
|---|---|
| OOM under normal workload | Check for missing finalisers (Layer 2) |
| OOM under specific input | Check pool allocator scope (Layer 3) |
| Crash with "use of freed memory" | Check `gc_allocator` usage in containers (Layer 4) |
| Crash with "wrong answer" | Check `safe::*` usage in monomial code (Layer 5) |
| Crash early in startup | Check that `GC_INIT()` is first in `main()` ([STARTUP.md](STARTUP.md) phase 2) |
| External-lib state corruption | Check the wrapper's finaliser (Layer 2) |

## Used by

- Engine developers tracking memory bugs.
- Anyone curious why M2 has no explicit `delete` calls.
- Newcomers trying to understand the allocator choices.

## Related

- [`README.md`](README.md) — repository TOC.
- [`STARTUP.md`](STARTUP.md) — GC init is phase 3 of startup.
- [`GLOSSARY.md`](GLOSSARY.md) — Boehm GC, `our_new_delete`,
  `MemoryBlock`, `FastAllocator`, finaliser, `safe::*` entries.
- [`TOUR.md`](TOUR.md) — Path F (engine extension) covers
  memory-conscious extension.
- Engine architecture overview
  ([`M2/Macaulay2/e/architecture.md`](M2/Macaulay2/e/architecture.md))
  has its own "Memory model" section.
- Per-file deep dives for each allocator layer:
  [`file-newdelete.md`](M2/Macaulay2/e/file-newdelete.md),
  [`file-hash.md`](M2/Macaulay2/e/file-hash.md),
  [`file-MemoryBlock.md`](M2/Macaulay2/e/file-MemoryBlock.md),
  [`file-mem.md`](M2/Macaulay2/e/file-mem.md),
  [`file-finalize.md`](M2/Macaulay2/e/file-finalize.md),
  [`file-overflow.md`](M2/Macaulay2/e/file-overflow.md),
  [`bibasis/file-allocator.md`](M2/Macaulay2/e/bibasis/file-allocator.md),
  [`d/file-c-glue.md`](M2/Macaulay2/d/file-c-glue.md).
