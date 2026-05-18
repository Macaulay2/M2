# `engine.cpp` — engine global definitions

Tiny (≈70-line) translation unit that owns a handful of **engine-wide
singletons** that don't have a better home — anything that needs exactly
one definition somewhere lives here.

Distinct from [`engine.h`](file-engine-h.md) (the public C-callable
header) and [`engine-includes.hpp`](file-engine-includes-hpp.md) (the
common include prelude). Those define an API surface; this file
provides storage for runtime state.

## What it defines

| Symbol | Type | Purpose |
|---|---|---|
| `MutableEngineObject::mNextMutableHashValue` | `unsigned int`, initially `13` | Counter for handing out hash values to newly created mutable engine objects (see [`hash.hpp`](file-hash.md)). Mutable objects can't use content-based hashing because their contents change; each gets a stable, unique integer instead. |
| `heap_size[GEOHEAP_SIZE]` | `const int` array | Bucket sizes for **geometric heaps** — the data structure used to accumulate large polynomial sums incrementally. Successive bucket capacities `4, 16, 64, 256, 1024, …, 16777216, 67108864`, a quadrupling sequence. Consumed by [`geopoly.hpp`](file-geopoly-hpp.md), [`geovec.hpp`](file-geovec.md), [`geobucket.hpp`](file-geobucket.md), [`schur-poly-heap.hpp`](file-schur-poly-heap.md), and [`gbring.cpp`](file-gbring.md). |
| `doubles` | `stash *` | Global stash for fixed-size `double` allocations. |
| `doubling_stash` | `stash *` | Stash for doubling-array allocations (used by anything that grows like `std::vector` but inside Boehm-managed memory). |

`GEOHEAP_SIZE` itself (the number `15`) is defined in
[`style.hpp`](file-style.md). The constant lives there because it's a
compile-time tuning knob; `engine.cpp` provides the bucket-size table
that the constant indexes into.

## Why it's tiny

Most engine state lives inside a `Ring` / `Computation` /
`PolynomialRing` / `Monoid` instance — there are very few truly
process-global singletons in the engine, so this file stays small.
When you need to add a new engine-wide singleton, this is the right
home.

## See also

- [`hash.hpp`](file-hash.md) — `MutableEngineObject` class hierarchy
- [`mem.hpp`](file-mem.md) — `stash` type for the global allocators
- [`geopoly.hpp`](file-geopoly-hpp.md) — primary consumer of `heap_size`
- [`style.hpp`](file-style.md) — where `GEOHEAP_SIZE` is defined
- [`architecture.md`](architecture.md) — engine architecture overview
